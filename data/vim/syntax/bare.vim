set expandtab

syn keyword     bareKW        type enum struct enum union

syn match       bareComment "#.*" contains=bareTodo

syn match       bareNum      "\<[0-9]\+\>"

syn match       bareKW      ":"
syn match       bareKW      "="
syn match       bareKW      "|"

syn keyword bareType   uint int u8 i8 u16 i16 u32 i32 u64 i64 f32 f64 map str data bool void

syn keyword  bareTodo  contained TODO BUG FIX FIXME NOTE

if version >= 508 || !exists("did_bare_syntax_inits")
  if version < 508
    let did_tptp_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink bareComment     Comment
  HiLink bareKW          Keyword
  HiLink bareNum         Constant
  HiLink bareType        Type
  HiLink bareTodo        Todo
  delcommand HiLink
end

let b:current_syntax = "bare"
