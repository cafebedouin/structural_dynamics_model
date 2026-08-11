# OQ-277 HANDOFF — read this, then `RECON.md`, then start at "Your next step"

**Written:** 2026-08-10, by the instance that executed step 0, step 1, and Phase 1 direction (i).
**Why a handoff:** direction-(ii) extraction needs ~184 KB of audit prose read *carefully*.
Skimming to fit a full context window would produce thin units — and thin units bias toward
`other`, which is exactly what control (c) exists to measure. A degraded extraction would
confound the control meant to catch it. A fresh extractor reading only the frozen substrate is
a **better** extractor than a context-loaded one, not merely a cheaper one.

**You are the EXTRACTOR. You never code.** Coding is stateless SDK calls (`agent/llm_call.py`)
because `CLAUDE.md` — which contains P1–P6 — is injected into every harness instance, including
yours. If you find yourself assigning a pattern to a unit, you have broken the experiment.
Assigning Wu's *true class* is not coding: those labels come from Wu's own frozen files, never
from your judgement.

---

## State: what is DONE and witnessed

| Step | Status | Witness |
|---|---|---|
| 0. Transcript snapshot | **DONE** | 90 members == 90 live glob; md5 `289534248baeae333db49c5d00ef185d`; manifest sidecar. Earliest content stamp `2026-07-03` |
| 1. Four OQs minted | **DONE** | `280 parsed, 0 malformed`; gate GREEN 13/13; commit `8d7e5aba` |
| Frame frozen | **DONE** | 174 dirs = 73 + 101, partition exact; per-listing md5s in `frame/frame_manifest.txt` |
| Samples drawn | **DONE** | `frame/sample.json`, seed 20260810, n=22 primary + n=8 escape |
| Direction (i) source | **DONE** | `packets/wu_source/` — catalog + labeled dataset, md5s in `FETCH_MANIFEST.txt` |
| RECON (findings R1–R4) | **DONE** | `RECON.md` |
| **Direction (i) units** | **NOT DONE** | the n=17 paper-only draft was DELETED as superseded; rebuild at n=22 from the catalog |
| Direction (ii) units | **NOT DONE** | ← your next step |
| Anchors / decoys / controls | **NOT DONE** | |
| Driver | **NOT DONE** | |
| PREREGISTRATION.md + md5 | **NOT DONE** | |
| Spend-go | **NOT REQUESTED** | operator gate, at freeze |
| Any model call | **NEVER MADE** | `payloads/` and `responses/` are empty by design |

---

## Four things fired before any spend (all already in substrate)

1. **Self-inclusion.** This audit's own directory landed in the escape-check stratum — the
   coder would have been asked to code the experiment coding it, and the payload could never
   have passed its own leak-grep (this dir carries the full P-lexicon *and* Wu's name).
   Excluded by a named pre-sample rule in `frame/freeze_frame.sh`; the manifest counts the
   exclusion so it can never be silent.
2. **Empty-directory denominator.** `audits/2026-08-10_oq78_idiom_close/` was empty and
   untracked yet sat in Appendix B's §4.5 denominator. 73/175 → 73/174; headline 42% unchanged.
3. **Substring leak-matcher false positive.** "permission **class b**y default" matched banned
   `Class B`. Fixed to word-boundary regex; the fixed matcher's positive control fires 4/4 on a
   planted leak. **Use word-boundary matching in the driver's leak-grep.**
4. **R2** — Wu's two records disagree on 45% of class assignments. See `RECON.md`; it changes
   the interpretation table, not just the row index.

---

## Your next step (do these in order)

### 1. Rebuild direction (i) units at n=22
Source: `packets/wu_source/failure_modes_catalog.md`, the per-class case tables (Class A ~line
64, B ~90, C ~110, D ~130, E ~148). Each row gives case file, version, one-line symptom, true
root cause, and linked meta-rules. Write `packets/wu_units.json` with the pinned unit format
`{id, symptom, mechanism_as_described, detection_path, consequence}` plus metadata
`{catalog_class, dataset_class, agreeing (bool), manifestations}`.

- **Unit = INCIDENT, never manifestation.** Class A is ONE case
  (`whatsapp_client_display_folding`); its six quirks are sub-events with no independent case
  documents and must NOT become units.
- **`catalog_class` is the primary row index; `dataset_class` is the robustness index.** Both
  are recorded per unit. Do not collapse them.
- The catalog is in Chinese with English technical terms. Translate faithfully into the unit
  format; the *mechanism* must survive translation, since that is what gets coded.

### 2. Redact both directions
Strip Wu's taxonomy vocabulary from direction-(i) units (class letters + names,
`fail-plausible`, `gray failure`, trigger/amplifier/concealer, meta-rule ids) and the P-lexicon
from direction-(ii) units. **Do NOT strip the shared subject matter** (`silent`, `never fired`,
`green`) — it belongs to neither taxonomy and removing it destroys codeability, which is the
bias control (c) is measuring. Freeze both banned lists verbatim in the prereg.

### 3. Direction (ii) units from the 22 sampled dirs
`frame/sample.json` → `primary_sample`. Entry files are heterogeneous (some dirs' largest `.md`
is 1 KB and the real content is in a sibling) — read enough to get a real mechanism, not the
first file you find. Same unit format, no P-vocabulary.

### 4. Controls, then driver, then freeze
Anchors (3/direction), decoys (2), redaction-bias pairs (3/direction, quarantined and
leak-exempt), planted leak, planted broken unit. Then
`python/audits/oq277_crosscoding_driver.py` wrapping `call_with_retry` to dump every assembled
payload **before** send. Then `PREREGISTRATION.md`, md5 into `audit_log.md` **above** the first
result line, then request spend-go.

**Before Phase 3, assert the driver's payload-capture count equals the expected call count
(operator amendment 4).** The leak-grep's positive control proves a grep that *fails to flag*
would be caught; it says nothing about a grep run over **empty input**. A capture bug that
writes zero payloads yields a clean leak-grep and a green H2 — success-shaped absence, the
exact Pattern-6 shape this experiment studies. Count first, then grep.

---

### 5. Carry R5's leak-ban additions into the driver
`RECON.md` §R5 records a **pre-registered directional expectation** that our own instability
should concentrate on P6 (the pattern doing mechanism work in a layer-sorted set). It is
extractor-facing and scored by a rule fixed in advance. **It must never reach a coder
payload** — add `parasitic`, `cross-cutting`, and `layer` to the direction-(ii) banned lexicon
so a leaked hint is *caught* rather than assumed absent, and include one planted-leak payload
carrying those terms.

---

## The frame command now has a positive control — run it, don't assume it

`frame/control_frame_command.py` (operator amendment, 2026-08-10). "174 = 73 + 101, partition
exact" is an arithmetic identity `comm -23` cannot violate — it witnesses nothing about whether
the census *classifies* correctly. The control plants 6 directories with known correct
classifications and asserts each lands in its bucket and stays out of the other, reproduces the
empty-directory defect, and keeps the 77/175 unit error live as a regression check.
**Currently PASSES (6/6 + both regression controls).** Re-run it if the census command, the
keyword list, or the directory layout changes.

Two things it surfaced that you must not undo:

- **The census is a positional parse.** `cut -d/ -f2` extracts the directory only because the
  pinned target is `audits/` (yielding `audits/dir/file.md`). From inside `audits/` with an
  unprefixed target it extracts filenames and subdirectory names instead. **Always use the
  explicit `audits/` target from the repository root; never `.`.** This is the same mechanism
  Wu's Class B names — positional parsing of tool output — sitting inside the instrument that
  measures our own incidence rate. Worth a line in the writeup.
- **`grep` is a shell FUNCTION in the interactive harness shell**, not `/usr/bin/grep`, and the
  two differ on whether a `./` prefix is emitted — which moves `cut`'s field by one. The
  control pins `/usr/bin/grep` explicitly for this reason. **Any script computing a census
  figure should pin the binary**, or its answer depends on which shell happened to run it.
  (The frozen frame is unaffected and was verified: both greps return the same 74 → 73 after
  self-exclusion, and the frozen listing matches. The pinned `audits/` target is what saves it.)

---

## Carried rulings you must not re-litigate

- **Code against the published six** (`CLAUDE.md` / v0.3 §4.3), definitions frozen VERBATIM in
  the prereg with file + commit hash. This scopes the experiment; it does **not** resolve the
  OQ-278 fork.
- **Class A is VERDICT-INELIGIBLE** under the catalog index (n=1).
- **The 12-unit agreeing stratum's selection rule is frozen with its cost declared** — higher
  expressibility there is partly a codeability artifact, never "the cleaner number" (`RECON.md`
  §R2 point 3).
- **The asymmetric (i)-vs-(ii) row ships as typed OPEN** — the same-family confound is
  registered but unmeasured, and registering a confound does not license reading through it.
- **Extension changes n and NOTHING else.** Any other change is a new experiment.

## Writeup obligations already accrued (carry to `WRITEUP.md`)

- `**Fired:** live` — three catches landed before any spend (list above). The bit is honest
  regardless of what the coding produces.
- **R2b:** Wu's 55% self-agreement is a finding about the comparison set, independent of the
  cross-coding, and weakens Wu as a §12 priority threat in a way that *supports* §5.3.
- **R2a:** the disagreement is systematic (E-hub, 8/10; B/E modal at 4/10; only 5 of 10 pairs
  occupied). The pre-registered C/D guess was **wrong** and is recorded as wrong.
- **Denominator line (operator amendment 4):** two unit-level defects have now been found in
  §4.5's denominator by two independent checks, and the figure survived both at 42%. **That
  survival is the reassurance — not the corrections being complete. Assume a third defect
  exists.**
- **PROPOSED Ω_C mapping table** awaiting operator ruling — including the tempting but
  unasserted E-hub ↔ P6 correspondence.
- **R5's directional expectation, scored against its pre-registered rule** — including the
  "uninformative" branch if the unstable row has fewer than 4 units. If it is disconfirmed,
  say so as plainly as the C/D guess was.
- **The census-as-positional-parse finding** (above): the instrument measuring our
  silent-failure incidence carries the mechanism Wu's Class B names, and it took building a
  positive control to see it.
- **Freeze-time vs run-time catches (ISSUES OQ-276).** All four of this arc's catches fired
  at *freeze* time, before any measurement existed to contaminate — which is the cleanest
  available evidence for §9.3's *epistemic* efficacy, the one it can least instrument. The
  `Fired:` bit cannot currently express the distinction; a candidate `at-freeze | at-run |
  post-hoc` axis is recorded in OQ-276 as NOT adopted, pending the first catch-rate reading.
