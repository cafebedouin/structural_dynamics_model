# HANDOFF — redaction twins, then the driver, then the freeze

> ## ⚠ STATE UPDATE 2026-08-11 — still the entry point; four things below have MOVED
>
> This file is **not** superseded — its conventions, §2 driver spec, §2.5 no-live-call rule and
> §3 freeze order all still bind. Four changes since it was written, each operator-ruled:
>
> | # | change | authority |
> |---|---|---|
> | 1 | **Scope grew.** Direction **(iii′)** (n=10, not ~15) and the **n=8 escape check** are IN this freeze. Both were in the frozen design and scheduled by no handoff. | `RULING_2026-08-11_freeze_scope.md` |
> | 2 | **Control (c) direction-(ii) now has FIVE pairs, in two sets.** The declared three stand; two more added. The **corrected set carries the both-residue row**; the declared set is reported alongside. | `controls/redaction_pair_selection_defect.md` (RULED section) |
> | 3 | **§1's "do not re-derive to check" is CORRECTED.** Checking is permitted and expected; *reselecting* is mechanically prevented (`controls/recheck_predeclared_counts.py` exits non-zero if the selection moves). The original wording collapsed verification into reselection. | amendment §Q.2 |
> | 4 | **Escape extraction is a separate instance** under operator condition C1. | `HANDOFF_ESCAPE_EXTRACTOR.md` |
>
> **DONE since:** direction-(i) twins (3 arms, `controls/redaction_twins_direction_i.json`, 9/9
> two-sided checks pass via `controls/verify_redaction_twins.py`); the ruling + defect artifacts;
> amendment §Q.
>
> **NEXT, in order:**
> 1. **Direction-(ii) twins — 5 unredacted arms.** Units `04` (declared+corrected), `10`, `20`
>    (declared), `05`, `07` (corrected). `verify_redaction_twins.py` is RED until they land.
>    **Pairs 2 and 3 have ZERO taxonomy vocabulary in their `files_read`** — their arms restore
>    only source-identifying vocabulary, and that is the honest outcome, not a reason to write in
>    an attribution the source does not make. Unit `04` alone has a source naming the pattern for
>    its own incident (`AUDIT.md:144`).
> 2. **(iii′) units — 10 exemplar incidents**, enumerated with line numbers in
>    `RULING_2026-08-11_freeze_scope.md` §2.1. **P3 has ZERO exemplars** — that row's coverage is
>    P1/P2/P4/P5/P6 only, declared in advance.
> 3. Packets, driver (stub transport ONLY), prompts, prereg, then **stop for spend-go**.
>
> **Still true and unchanged: no model call has ever been made.** `payloads/` and `responses/`
> are empty by design.


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
| Control verification | **`controls/verify_controls.py`** — re-run it, do not trust this table. Currently ALL CONTROLS PASS, exit 0; output in `controls/verify_controls.txt`. It is a STANDING check: re-run after any change to units, controls, or the lexicon |
| **Redaction twins** | **NOT DONE — your first task** |
| **Coder packets** | **NOT ASSEMBLED — see §1.5.** `packets/coder_direction_i.json` predates the controls and contains NO anchors and NO decoys; direction (ii) has no assembled packet at all |
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

## 1.5 Assemble the coder packets — a task the earlier handoffs did not name

`packets/coder_direction_i.json` was frozen at step 2, **before the controls existed**: it holds
Wu's 22 redacted units and contains **no anchors and no decoys**. Direction (ii) has **no assembled
packet at all** — its units are 26 per-file JSONs, of which only the 22 with `matrix_unit: true`
are cells.

Decide and RECORD where interleaving happens — packet or driver — and keep it in one place:

- **If the driver interleaves at send time**, say so in the prereg, because a later reader
  auditing "what was sent" will otherwise open the packet and get an incomplete picture. The
  payload dump is then the authoritative record and the packet is only a unit source.
- **If you assemble full packets**, the packet is the record and the driver just sends it.

Either is defensible; leaving it implicit is not. Whichever you choose, the direction-(ii) packet
must be built from `matrix_unit: true` units **plus** anchors, decoys and — separately labelled and
quarantined — the redaction twins.

**Declared and checked overlap, so you do not rediscover it as a surprise:** the three Wu units
serving as direction-(ii) anchors (`governance_silent_error`, `dream_self_referential`,
`heartbeat_md_pa_self_silencing`) are ALSO three of the 22 units coded in direction (i). That is
intended and clean — different label spaces, different runs, a stateless coder, and the anchors are
quarantined from direction-(ii) cells — but it means those three texts appear in both runs. Do not
"fix" it by dropping them from direction (i); that would shrink the coded set to tidy up an overlap
that costs nothing.

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

## 2.5 STOP — do not make a live model call, including to test the driver

**No model call has ever been made in this audit. `payloads/` and `responses/` are empty BY
DESIGN, and they stay empty until the operator says otherwise.**

The hazard is not that you would decide to run the experiment early. It is that **building a driver
and smoke-testing it with one real call is the most natural thing in the world**, and it would
break two things at once: it spends against a gate the operator holds, and it puts a result on disk
before `PREREGISTRATION.md`'s md5 sits above the first result line — which is the whole point of
that ordering. A single "just checking the transport works" call is a prereg violation, not a
warm-up.

**Test the driver against a STUBBED transport** — a fake `call_with_retry` returning canned
responses. That exercises everything that can actually be wrong at this stage: payload assembly,
the pre-send dump, the capture count, the leak-grep over dumped files, k=3 bookkeeping, unanimity
and UNSTABLE routing, and the quarantine on `matrix_unit`. None of that needs the network.

If you believe a live call is genuinely required before freeze, **stop and ask** — do not decide it.

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
