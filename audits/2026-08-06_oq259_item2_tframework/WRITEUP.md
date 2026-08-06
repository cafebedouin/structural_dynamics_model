# OQ-259 item 2 — T Framework GRADUATED as second meta-layer file (kernel presence 3/3 blinded + P1 unanimity 3/3); baseline re-minted from pinned recipe; P2 ruled out as mechanical gate

**Executed:** 2026-08-06
**OQ:** OQ-259 (item 2 — conversion provenance + P2 calibration + Part C graduation dry-run)
**Verdict:** Three staged parts, one day: (A) the committed baseline (597,374 B, `51caeb36…`) is NOT reproduced by the pinned recipe (pandoc 2.9.2.1 `-f docx -t gfm --wrap=none`; markup-shape diff only, cause withheld) — fresh baseline minted, md5 `a365da8aa11e5039807275bcc662f956`; (P2) no token-mechanical meta-layer gate is buildable (ceiling never exceeds floor across three variants) — operator ruled P1-only promotion; (C) three serialized same-input dry-runs each minted a contested kernel (P1 3/3) whose churned-id kernel descriptions were adjudicated one SAME subject+stance group under a blinded call with a passing planted control — **"graduated second meta-layer file" per the frozen grammar**; ingestion remains a separate operator decision, and the P1 leg carries a base-rate bound (the already-graduated AT Fiat also measures P1 3/3 on its existing OQ-264 draws — zero-spend comparator — so P1 3/3 is a meta-layer-general pattern, not T-Framework-specific evidence; C_RESULTS.md).
**Substrate:** no pipeline run (dry-run manifests only; corpus untouched, witnessed per run)
**Evidence map:**
- `tframework_repro_pandoc2921.md` — pinned-recipe output from the committed docx (672,832 B, md5 `a365da8a…`); byte-identical to the newly minted baseline at `agent/analysis/originals/k_files/T Framework - Michigan 2026 BCFP.md`
- `superseded_baseline_51caeb36.md` — the retired committed baseline, retained verbatim (597,374 B, md5 `51caeb36…`)
- `pandoc_stderr.log` — empty; conversion exited 0
- `RECON.md` — provenance narrowing (sole commit, clean worktree) with commands
- `P2_CALIBRATION.md` — Part C prereg work: two-sided P2 calibration verdict (no
  token-mechanical form opens a gap; escalated; operator RULED P1-only)
- `p2_calibration.py` + `p2_calibration_v{1,2,3}_output.txt` — the calibration script
  and the three swept-variant outputs witnessing the inverted/collapsed ordering
- `PREREGISTRATION.md` — Part C prereg, frozen `4f862bee` before run 1 (P1-only
  promotion, symmetric confirmatory-draw staging, HALT, k=3 grammar)
- `TAG_INVENTORY_TFRAMEWORK.txt` — mechanical header extraction of the pinned
  baseline (39 lines; descriptive reference layer, non-gating)
- `tframework_c_run{1,2,3}.log` + the three `*.manifest.json` — the staged dry-run
  draws (P1 pass ×3; input md5 + corpus-untouched witnessed around each)
- `PRESENCE_CALL.md` / `build_presence_packet.py` / `PRESENCE_PACKET.md` — the frozen
  presence-clause protocol (packet md5 `afbaaa55…`, withheld mapping md5 `e6a26bed…`,
  planted AT Fiat control, instrument-validity rule)
- `PRESENCE_CALLS_RAW.md` — verbatim blinded calls, committed `7581cf98` before the
  mapping
- `presence_mapping.json` — the reveal (md5 matches pin)
- `C_RESULTS.md` — per-run table, staged path, presence verdict, graduation verdict
  with its pinned scope

## What ran

1. Provenance narrowing (read-only): both `.docx` and `.md` frozen since sole commit
   `1bd57a84`; worktree clean vs HEAD for both files. Non-reproduction is therefore
   conversion-side relative to the committed pair, not a post-commit edit.
2. Pinned recipe run: `pandoc -f docx -t gfm --wrap=none` (pandoc 2.9.2.1) on the
   committed docx → `tframework_repro_pandoc2921.md`, exit 0, stderr empty.
3. Comparison (witnessed in-session):
   - repro: 672,832 B, md5 `a365da8aa11e5039807275bcc662f956`, 1,351 lines
   - committed baseline: 597,374 B, md5 `51caeb369d147849d07b45f1ba0926b6`, 1,351 lines
   - `diff` = 1,424 lines across the pair.

## Diff shape (descriptive only — cause claim intentionally withheld)

- Line counts identical (1,351 vs 1,351); the difference is intra-line markup
  representation, not content addition/removal.
- Dominant component: the repro carries `<span class="underline">…</span>` runs that the
  committed baseline lacks. Stripping exactly those span tags from the repro reduces the
  diff from 1,424 lines to 8.
- Residual (2 changed lines): superscript representation — repro emits `<sup>NN</sup>`;
  baseline carries Unicode superscript characters (e.g. `²⁰`, `¹³`, `²¹`, `³¹`).
- Both components are markup-encoding differences over the same text. What produced the
  committed baseline (different pandoc version, post-processing, or another converter) is
  NOT determined here and is not claimed.

## Disposition

- Fresh baseline minted from the pinned recipe: `agent/analysis/originals/k_files/T
  Framework - Michigan 2026 BCFP.md` is now md5 `a365da8a…` (672,832 B). This md5 is the
  pinned input for OQ-259 item-2 Part C (graduation dry-run prereg).
- Superseded baseline retained in this directory (`superseded_baseline_51caeb36.md`).
- Both md5s recorded in KNOWN_STATE (2026-08-06 entry).
- OQ-259 item-2 note (a) is RESOLVED as "confirmed non-reproduction, provenance
  re-anchored to the pinned recipe" — the note's precondition (resolve conversion
  provenance before any emphasis-aware variant) is discharged by the re-mint.

## P2 two-sided calibration (Part C prereg work, executed same day — see P2_CALIBRATION.md)

Swept three token-mechanical strictness variants of the reading↔TAG match-rate
predicate over the AT Fiat k=3 triple (ceiling) and the Biopower NW + Cap K NW triples
(floor): the ceiling NEVER exceeds the floor (v1/v2 inverted, v3 collapsed to ~0). P2
is unbuildable as a mechanical gate on current instruments — the quantity exists only
at the judged level (OQ-264's blinded 6/6). ESCALATED to the C checkpoint per the
plan's pinned clause; no threshold pinned; Part C run 1 must not start until the
operator picks the P2 form (judged step / P1-only / other). Side finding: the Cap K r2
manifest has an empty `commitment_system_recognition` — P2 must be conditioned on
P1-passing draws.

## Part C — EXECUTED (operator go + P1-only ruling, 2026-08-06)

Prereg frozen `4f862bee` before run 1 (P1-only promotion per the operator's P2 ruling;
symmetric confirmatory-draw staging — run 2 iff run 1 mints, run 3 iff both — chosen
over the declared-n=1 alternative because the 3/3 grammar makes it strictly dominant).
Three serialized dry-runs, each witnessed (input md5 `a365da8a…` stable before/after;
corpus snapshot diff empty; ~220,720 tok/run): P1 PASS 3/3 under three churned kernel
ids and churned reading-set sizes (3/6/5). The presence clause was adjudicated BLIND
per the frozen protocol (close call; unblinded-reads-run-generous tripwire): planted
different-subject control called DIFFERENT 3/3 (instrument valid), the three draws'
kernel descriptions one SAME group. **Verdict per the frozen grammar: graduated second
meta-layer file.** Full detail and the claim's pinned scope: `C_RESULTS.md`.
Ingestion (stories into the corpus) is NOT part of this verdict — separate operator
spend decision, now permitted by the (f) verdict's meta-layer exception.
