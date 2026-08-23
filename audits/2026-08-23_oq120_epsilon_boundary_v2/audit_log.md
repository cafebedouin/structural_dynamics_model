# audit_log — OQ-120 Phase 0 **v2**

## OPEN stamp (v2)

- **OPEN HEAD:** `f88c8c3cf4040e5994c8c1af11e40dcb6c484cf0`
- **OPEN date:** 2026-08-23
- **v2 PREREGISTRATION.md md5:** `7d0a85d93ae1b9e540ac54d2d4cc4ba7`
- **v1 PREREGISTRATION.md md5:** `b181e1a2a9cd42b86d190be09f61d400` — the genuinely-prior one,
  preserved unedited at `audits/2026-08-21_oq120_epsilon_boundary/`.

**v2's prereg is NOT frozen-before-results and says so on its own face.** v1 completed first and
produced G1b; v2's specification repairs criteria that v1 proved unsatisfiable, vacuous, non-total
or underspecified. Recording the md5 here preserves *this* document's integrity going forward — it
does not, and is not claimed to, confer the ordering property v1's prereg actually has.

**Substrate identical to v1** — 18 live legs at the same file counts, `testsets_nemotron_think`
and `testsets_glm` still empty, HEAD unmoved at `f88c8c3c`. v2's sweep is therefore a
re-derivation under final code plus the new per-stratum quantities. **A transition-level difference
from v1 would be a determinism bug, not a new result** — this doubles as a determinism check on a
fork that was patched mid-flight during v1.

## Gate baseline at v2 OPEN

Carried forward and re-observed: pristine `f88c8c3c` is **GREEN** (28 rows). The v1 directory now
holds a `WRITEUP.md` with its `**Fired:**` line, so v1 no longer reds the gate; **v2's own
directory now does**, for the same transient reason, until v2's WRITEUP lands. Re-observed at CLOSE.

## Assumed substrate

S1–S13 verified first-hand during v1 and unchanged at v2 OPEN (same HEAD, same substrate):
`audits/2026-08-21_oq120_epsilon_boundary/substrate_check.md`. The one row that was FALSE as
written (S8) is re-stated above.

---

## Results (nothing above this line was written after a v2 sweep ran)

