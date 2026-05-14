# Changelog

## 0.2.8

Pure infrastructure modernization. No source or API changes.

### Changed

- Switched `uuid` dep from a git ref to the `uuid_erl` hex package
  (`uuid_erl 2.0.7` — okeuday's Erlang UUID library is published to
  hex under a renamed slot; the OTP app name stays `uuid`). The
  previous git ref tracked an unpinned `master` branch, which was
  unsafe for reproducible builds. The hex move also unblocks
  publishing swirl itself to hex.
- `fprofx` test dep moved from `ransomr/fprofx` to `lpgauth/fprofx`
  (`otp_19` branch), matching the rest of the ecosystem.
- CI matrix bumped from OTP 24-26 to OTP 25-28.
- `actions/checkout` upgraded to v5.
- Documentation migrated from `edown` to `rebar3_ex_doc`.
- `vsn` in `swirl.app.src` bumped 0.2.5 → 0.2.8 (was lagging behind
  the git tags).

### Removed

- `edoc` Makefile target.

(Tags 0.2.6 and 0.2.7 exist in git but `swirl.app.src`'s `vsn` field
had been stuck at `0.2.5` through both of them. 0.2.8 brings the
app metadata in sync with the tag.)
