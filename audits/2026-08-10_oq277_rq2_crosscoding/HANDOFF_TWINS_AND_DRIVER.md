# HANDOFF — redaction twins, then the driver, then the freeze

**Written:** 2026-08-11, by the instance that closed extractor A's half and built the controls.
**Read first:** this file, then `verdict_grammar_amendment.md` **in full** (it is binding
pre-registration content and is incorporated into `PREREGISTRATION.md` verbatim), then
`HANDOFF.md` for the carried rulings and writeup obligations.
**Superseded entry points:** `HANDOFF_EXTRACTOR_A2.md` and `HANDOFF_EXTRACTOR_B.md` are both
CLOSED. Their conventions still bind; their task lists are done.

## State — what is DONE and witnessed

| | |
|---|---|
| Direction (i) units | **DONE** — `packets/wu_units.json` (22), coder packet `packets/coder_direction_i.json` |
| Direction (ii) units | **DONE** — `packets/our_units/01–26`, both extractors, 0 leak hits |
| Cell accounting | 22 matrix units (A 13 + B 9); 4 floor-only. Quarantine on **`matrix_unit`**, never on `role`, never on `overlap_source` alone (§I.2) |
| Anchors | **DONE** — `controls/anchors.json`, 3/direction, 0 hits under BOTH lexicons |
| Decoys | **DONE** — `controls/decoys.json`, 2, clean under both lists |
| Planted leak | **DONE** — `controls/planted.json`, fires 9 hits (i) / 11 (ii) |
| Planted broken unit | **DONE** — `controls/planted.json`, meaning-inverted, ground truth recorded |
| Control verification | `controls/verify_controls.txt` — re-run it, do not trust this table |
| **Redaction twins** | **NOT DONE — your first task** |
| Driver | NOT DONE |
| `PREREGISTRATION.md` + md5 | NOT DONE |
| Spend-go | NOT REQUESTED (operator gate, at freeze) |
| Any model call | **NEVER MADE** — `payloads/` and `responses/` are empty by design |

## 1. Redaction twins — 3 per direction

The pairs are **PRE-DECLARED** in `controls/redaction_pairs_predeclared.json`. **Do not choose
them**, and do not re-derive them "to check" — they were fixed by a mechanical rule before the
units existed, which is the strongest available form of pre-registration for this control.

Each pair = the SAME incident coded twice, **redacted** and **unredacted**, k=3 each,
QUARANTINED from all matrices, EXEMPT from H2's leak rule (an unredacted unit necessarily
contains source vocabulary — that is the point). Report Δ("other" rate) = the redaction floor.

- **Direction (ii)** — the redacted arm already exists (units `04`, `10`, and B's `20`). You write
  the **unredacted** arm for each.
- **Direction (i)** — the redacted arm exists in `packets/wu_units.json`
  (`kb_evening_fallback_quota_chain`, `whatsapp_silent_death`, `movespeed_tcc_sandbox`). The
  unredacted arm restores Wu's own vocabulary from his md5-pinned source rows.

**The unredacted arm stays inside the never-code boundary — but only if you hold this line:**
it preserves the vocabulary **the source document itself uses**, at the points the source uses it,
and **adds nothing**. You are un-stripping, not labelling. If a source does not name a pattern, its
unredacted twin carries no pattern vocabulary and the pair's delta is legitimately small. **Writing
in an attribution the source does not make is coding, and it would make the floor measure your
labelling instead of redaction's cost.**

The three direction-(ii) directories were selected on vocabulary density (21, 5 and 4 hits), so the
arm is non-empty **by construction rather than by luck** — that is what the pre-declaration bought.

**Sanity check when done, both halves:** the unredacted arm MUST fail the leak sweep (it is exempt,
not clean) and the redacted arm MUST pass it. A pair where both arms pass is a pair where you did
not actually un-redact anything, and it would report a floor of zero — "redaction costs nothing" —
by construction. Assert both directions of that before moving on.

## 2. Driver — `python/audits/oq277_crosscoding_driver.py`

- Wrap `call_with_retry` (the single choke point) and **dump every assembled payload BEFORE send**,
  to `payloads/`. `llm_call` has no payload logging; capture is caller-side.
- **Import the leak-grep from `python/audits/oq277_lexicon.py`. Do NOT write a second matcher.**
- Coder is **Sonnet 5 only**, stateless single-turn SDK calls (subagents CANNOT be coders — the
  pattern list is injected into every harness instance).
- k=3 same-input redraws per unit per direction; label = unanimous 3/3 else UNSTABLE (own row,
  excluded from cells).
- Interleave anchors and decoys source-blind. Include the planted-leak fixtures in the payload dump
  so the grep is exercised **over the real dumped-payload path**, not only over strings.

**Before Phase 3, in this order — the order is the control:**
1. **Assert the payload-capture count EQUALS the expected call count.** A capture bug writing zero
   payloads yields a clean leak-grep and a green H2 — success-shaped absence, the exact failure
   this experiment studies. **Count first.**
2. Then grep. Overlap and quarantined units' calls still count toward the expected total.

## 3. Freeze, then ask

`PREREGISTRATION.md`, incorporating `verdict_grammar_amendment.md` **verbatim**; its md5 into
`audit_log.md` **physically ABOVE the first result line**; then request operator spend-go. The
spend-go is at **prereg freeze**, not at plan approval.

## What the amendment obliges you to carry (do not re-derive these — read them)

§I/§I.2 cell accounting · §I.1 four-measured-units limit · §I.4 floor asymmetry (a sub-4/4 floor is
an UPPER BOUND and §E fails CLOSED) · §J/§J.1 the git channel and the ruling-as-artifact · §L the
count-what-the-rule-produces check · §M k=0 leaves the quarantine mechanism UNTESTED · §N pooled
`incident_location` · **§O the declared calibration residue — no P6 anchor in direction (i), no
multi-membership anchor in direction (ii); any P6 result is UNCALIBRATED and the E↔P6 row lacks
anchor support on our side** · §P the self-comparison family and the compositional gap.

## The standing hazard, restated because it is the one that recurs

**Three self-comparisons have been caught in this arc, each in a different instrument** (§P). The
shape is always an apparatus measuring agreement between two things that are not independent, and
**reporting the agreement at full confidence, because agreement is what a working version produces
too.** When you build the driver, the live version of this is: an anchor, a decoy, or a redaction
twin that is secretly the same text as something else in the run. Check identity, not just labels.
