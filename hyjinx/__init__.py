"""
🦑 - Functions and macros for modern living in the Hy REPL.

▌      ▖▗       
▛▀▖▌ ▌▗▖▄ ▛▀▖▚▗▘
▌ ▌▚▄▌ ▌▐ ▌ ▌▗▚ 
▘ ▘▗▄▘▄▘▀▘▘ ▘▘ ▘

hyjinx.lib          - a smorgasbord of convenience functions.
hyjinx.source       - code inspection.
hyjinx.hjx_inspect  - code inspection, similar to python's inspect.
hyjinx.doc          - peruse hy documentation.
hyjinx.actors       - a convenient asyncio agent model
hyjinx.screen       - a convenient ncurses wrapper.
hyjinx.result       - Result type for explicit error handling.

(depends on numpy and/or jax)
hyjinx.mat          - numpy pretty-printing for humans.

(depends on zmq, ecdsa, zstandard)
hyjinx.[zmq_client, zmq_server, crypto, wire] - lazy-pirate zmq RPC architecture.

(depends on openai and/or anthropic)
hyjinx.llm          - discuss code with a Large Language Model (AI). Anthropic, TabbyAPI and OpenAI-compatible APIs are supported.

"""

import hy

# hyjinx.lib — convenience functions
from hyjinx.lib import (
    # modules
    mreload,
    # async
    sync_await,
    coroutine,
    # functions
    named_partial,
    compose,
    # time
    timestamp,
    days_ago,
    yesterday,
    tomorrow,
    now,
    # OS
    mkdir,
    hyx_Xexclamation_markX,
    pwd,
    cd,
    ls,
    shell,
    username,
    # strings
    grepp,
    get_numeric,
    sstrip,
    similar,
    decimal_align,
    unicode_search,
    camel_to_underscore,
    is_url,
    # numeric
    as_float,
    isnumeric,
    sign,
    round_to,
    hyx_posXquestion_markX,
    hyx_negXquestion_markX,
    hyx_zeroXquestion_markX,
    hyx_numberXquestion_markX,
    dice,
    prod,
    # output
    pp,
    hash_color,
    progress,
    # collections
    sieve,
    shift,
    get_in,
    group,
    # config / files
    config,
    slurp,
    slurp_result,
    spit,
    template,
    pload,
    psave,
    extract_json,
    jload,
    jload_result,
    jsave,
    jappend,
    jsonl_append,
    jprint,
    filetype,
    filenames,
    # ids / hashing
    hash_id,
    short_id,
    db_url,
)

# hyjinx.source — code inspection
from hyjinx.source import (
    edit,
    get_source_details,
    print_source,
    interact,
    inject_exception_hook,
)

# hyjinx.docs — documentation browser
from hyjinx.docs import (
    install,
    doc_hy,
    doc_hyrule,
    doc_toolz,
    doc,
)

# hyjinx.result — explicit error handling
from hyjinx.result import (
    ok,
    err,
    as_result,
    unwrap,
    unwrap_or,
    map_ok,
    map_err,
    collect_results,
)

# numpy may not be installed
try:
    from hyjinx.mat import (
        last_col,
        drop_first_rows,
        drop_first_cols,
        drop_last_cols,
        take_last_rows,
        take_last_cols,
        ppa,
        describe,
    )
except ModuleNotFoundError:
    pass

# require all the macros
hy.macros.require('hyjinx.macros', None, assignments='ALL', prefix='')

# set the package version
# the major.minor version simply match the assumed Hy version
__version__ = "1.2.3"
__version_info__ = __version__.split(".")
