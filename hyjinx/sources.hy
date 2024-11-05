"
This module provides functions that generate text (markdown)
from a variety of non-plaintext sources on the web.
"

(require hyrule.argmove [-> ->>])

(import hyrule [inc dec])
(import hyjinx.lib [first is-url])

(import httpx)
(import locale)
(import lxml)
(import os)
(import re)

(import lxml-html-clean [Cleaner])
(import markdownify [markdownify])
(import urllib.parse [urlparse])

(import arxiv [Search :as arxiv-search])
(import wikipedia :as wiki)
(import youtube_transcript_api [YouTubeTranscriptApi])
(import youtube_transcript_api.formatters [TextFormatter])


;; * Remote / web sources
;; ----------------------------------------------------

(defn youtube-meta [youtube-id]
  "Return the title and source of the youtube video."
  (let [url f"https://www.youtube.com/oembed?url=https://www.youtube.com/watch?v={youtube-id}&format=json"
        response (.get httpx url)]
    (match response.status-code
      200 (let [data (.json response)
                title (:title data "No title provided")
                author (:author-name data "No author provided")]
            {#** data
             "title" title
             "author" author})
      otherwise (.raise_for_status response))))

(defn youtube-dict [youtube-id [punctuate False]]
  "Load (and optionally punctuate) youtube transcript.
  Youtube 'transcripts' are normally just a long list of words with no
  punctuation or identification of the speaker.
  We can apply punctuation filter, which can give much higher quality text,
  but this takes VRAM (1-2GB) and requires pytorch.
  To do so, pass `puncuate` as `True`.
  Defaults to user's locale, this may not be desirable for summarization."
  (let [languages [(get (locale.getlocale) 0) "en" "en-GB"]
        avail-transcripts (.list-transcripts YouTubeTranscriptApi youtube-id)
        transcript (.fetch (.find-transcript avail-transcripts languages))
        formatter (TextFormatter)
        text (.format_transcript formatter transcript)
        meta-info (youtube-meta youtube-id)]
    (if punctuate
      (do
        ; lazy import here because not everyone will want to spend the VRAM.
        (import deepmultilingualpunctuation [PunctuationModel])
        {"transcript" (.restore-punctuation (PunctuationModel) text)
         #** meta-info})
      {"transcript" text #** meta-info})))

(defn youtube [youtube-id #** kwargs]
  "Load (and optionally punctuate) youtube transcript as text."
  (let [d-yt (youtube-dict youtube-id #** kwargs)]
    (.format "### {title}
Title: {title}
Author: {author}
youtube id: {youtube-id}
### transcript
{transcript}" :youtube-id youtube-id #** d-yt)))

(defn url [url]
  "Fetch a URL's content as cleaned markdown text."
  ;; could use trafilatura instead, but this is not bad
  (if (is-url url)
      (let [response (.get httpx url)
            cleaner (Cleaner :javascript True :style True)]
        (match response.status-code
          200 (-> response.text
                  (lxml.html.fromstring) 
                  (cleaner.clean_html)
                  (lxml.html.tostring)
                  (markdownify :heading-style "ATX" :strip "style")
                  (.replace "\r\n" "\n")
                  (.replace "\r" "\n")
                  (.strip))
          otherwise (.raise_for_status response)))
      (raise (ValueError f"Fetching {url} failed (implausible url)."))))

(defn filename-from-url [url]
  "Sanitise a url into a filename."
  (let [parsed_url (urlparse url)
        netloc parsed_url.netloc
        path parsed_url.path
        fname f"{netloc}_{(os.path.basename path)}"]
    (+ (re.sub r"[^a-zA-Z0-9_.-]" "_" fname)
       "_" (short-id fname))))

(defn clean-web-md [text * [bad "#`|"]]
  "Web-sourced markdown strings often have multiple bad characters
  and repeated newlines.
  This function rewrites a string with each line stripped,
  and (stripped) lines starting with bad characters removed."
  (re.sub r"\n\n\n[\n]+" "\n" 
    (.join "\n"
      (lfor line (.split text "\n")
        ;; allow blank lines, but not 'bad' lines
        :if (if line
              (not (in (first line) bad))
              True)
          (.strip line)))))

;; TODO: full text of a single arXiv paper - maybe just chat over file?
;;       consider arXiv latex -> markdown directly (with pandoc)

(defn arxiv [topic [n 12]]
  "Get relevant arxiv summaries on a topic (as text)."
  (let [results (.results (arxiv-search :query topic :max-results n))]
    (.join "\n\n---\n\n"
           (lfor paper results
                 (let [authors (.join ", " (map str paper.authors))]
                   f"Document:
### {paper.title}
#### Details
Title: {paper.title}
Authors: *{authors}*
Date: {paper.published}
{paper.entry_id}  DOI: {paper.doi}
#### Summary
{paper.summary}")))))

(defn wikipedia [topic [index 0]]
  "Get the full Wikipedia page on a topic (as text)."
  (try
    (let [pages (wiki.search topic)
          best (get pages index)
          summary (wiki.summary best :auto-suggest False)
          page (wiki.page best :auto-suggest False)]
        (.join "\n"
               [f"### Wikipedia page {page.title}"
                f"Title: {page.title}"
                f"URL: {page.url}"
                f"{page.content}"
                "\nRelated wikipedia pages:"
                (.join ", " pages)]))
    (except [wiki.exceptions.DisambiguationError]
      (wikipedia topic :index (inc index)))))
