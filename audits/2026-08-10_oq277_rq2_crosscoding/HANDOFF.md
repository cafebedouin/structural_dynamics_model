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
