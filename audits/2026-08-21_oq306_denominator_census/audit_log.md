# audit_log — OQ-306 denominator census
OPEN  HEAD: d7b4d4f83bfe4d745f74f9ba988a5bb252695d77 (2026-08-21, C1 start)
corpus md5 (testsets, n=285): 616972623c8f8d85df19cd5ddb9b98c4  — pinned across BOTH halves of the C1 diff pair
clean-side  pipeline_output.json md5: 2b287aab2878ee6b043c4df4e286a767  run_at 2026-08-21T14:23:47Z code_dirty=false
golden baseline md5: 238b6603aa00bce0625f13d181bfb7a9  ids=285  (blessed from the clean-side artifact)
CLOSE HEAD: 97c4735a (2026-08-21, C1 landed)

STAMP PAIR COMPARISON (operator ruling 2026-08-17): OPEN d7b4d4f8 != CLOSE 97c4735a.
The single intervening commit is this audit's OWN C1 commit — `git log --oneline
d7b4d4f8..97c4735a` yields exactly 97c4735a. No third-party writer during the C1
window, and the corpus md5 above is identical across BOTH halves of the diff pair,
which is the affirmative form of that claim rather than an inference from a clean
`git status`.

EARLIER IN THE SESSION the stamps DID move under a real concurrent writer: an
untagged c-orchestrator run completed at 04:23 and took the live leg 279 -> 285
and the stratum 26 -> 27 while this session was doing read-only substrate
verification. That is OQ-297's falsifier, and the branch that fired is
ACTIONABLE-CATCH: it was caught BEFORE any diff pair was taken, so every witness
in this audit is over the post-move, md5-pinned corpus. Detection sufficed here.
Note what did the catching, though — a process check run for an unrelated reason,
not the stamp pair, which would only have compared d7b4d4f8 against itself.
