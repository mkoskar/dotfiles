" vim: fdm=marker

if exists('b:current_syntax') | finish | endif
let s:cpo_save = &cpo
set cpo&vim

" ----------------------------------------

setl fdm=syntax cole=3 cocu=nv

syn case ignore
syn sync fromstart

syn match netinfoDev /dev \S\+/
syn match netinfoEth /\(\x\x:\)\{5}\x\x/
syn match netinfoIPv4 /\(\d\{1,3}\.\)\{3}\d\{1,3}\(\/\d\+\)\?/
syn match netinfoIPv6 /[[:xdigit:]:]*::[[:xdigit:]:]*\(\/\d\+\)\?/
syn match netinfoIPv6FQ /\(\x\{1,4}:\)\{7}\x\{1,4}\(\/\d\+\)\?/
syn match netinfoLinkFlags /<.*>/
syn match netinfoLinkKind /\v^\s+\zs(amt|bareudp|bond|bond_slave|bridge|bridge_slave|can|dsa|dummy|erspan|geneve|gre|gretap|gtp|hsr|ifb|ip6erspan|ip6gre|ip6gretap|ip6tnl|ipip|ipoib|ipvlan|ipvtap|lowpan|macsec|macvlan|macvtap|netdevsim|nlmon|rmnet|sit|vcan|veth|virt_wifi|vlan|vrf|vti|vxcan|vxlan|wireguard|xfrm)\ze\s+/
syn match netinfoLinkState /state \S\+/
syn match netinfoLinkType /^\s\+\zslink\/\S\+/
syn match netinfoMetric /metric \d\+/

syn region netinfoInet6Table start=/inet6 table/ end=/^$/ contains=ALL fold
syn region netinfoModeline start=/\%1l# vim:/ end=/$/ conceal

hi def link netinfoDev Identifier
hi def link netinfoEth PreProc
hi def link netinfoIPv4 Constant
hi def link netinfoIPv6 Constant
hi def link netinfoIPv6FQ Constant
hi def link netinfoLinkFlags Statement
hi def link netinfoLinkKind Identifier
hi def link netinfoLinkState Statement
hi def link netinfoLinkType Identifier
hi def link netinfoMetric Special
hi def link netinfoModeline Comment

" ----------------------------------------

let &cpo = s:cpo_save
let b:current_syntax = 'netinfo'
