# READOUT — AT Fiat k=3 reproduce-rate arm (Phase C, sole spend)

Date: 2026-08-06. Pre-registration: PROPOSAL_ADDENDUM.md §4 (committed `ac2650ae`
BEFORE any call). Deciding pass: name-blind subject+stance matching of each re-run's
kernel readings + selected axes against the six pinned baseline descriptors.

## Result: reproduce-rate 6/6, 6/6, 6/6 — full recovery in all three re-runs

| Pinned baseline reading | Run 1 (142033) | Run 2 (142156) | Run 3 (142314) |
|---|---|---|---|
| 1 empirical precedent (BDS/Vietnam/Chicago) | `empirical_precedent_reading` | `empirical_efficacy_reading` | `empirical_precedent_reading` |
| 2 knowledge-generation / scholarship of consequence | `scholarship_of_consequence_reading` | `scholarly_knowledge_reading` | `scholarship_of_consequence_reading` |
| 3 truth-procedure (Badiou/McGee) | `truth_event_reading` | `event_ontology_reading` | `axiomatic_truth_event_reading` |
| 4 predictive synthesis (Bagg/Dewey) | `predictive_synthesis_reading` | `predictive_theory_reading` | `predictive_synthesis_reading` |
| 5 empathy simulation (Mauri/HCI) | `empathy_simulation_reading` | `empathy_sandbox_reading` | `empathy_simulation_reading` |
| 6 utopian fiction / social criticism (McGee & Romanelli) | `bounded_fiction_reading` | `utopian_fiction_reading` | `social_criticism_fiction_reading` |
| **Reproduce-rate** | **6/6** | **6/6** | **6/6** |

This closes the 2026-08-05 ruling's standing rider ("AT Fiat has NO Arm-0 measurement")
with the OPPOSITE of its stated expectation: the precision rider guessed "smallest file,
proportional per-reading noise likely worse" — measured, the smallest file is the ONLY
one with perfect per-reading redraw stability.

## Run witnesses (per-run HALT checks all clean)

| Run | Input md5 (== `1bd57a84` baseline) | Ingest line | Manifest (copies in this dir) |
|---|---|---|---|
| 1 | `8d2224c8…` ✓ | `33,793 tok / cap 975,616 (headroom 941,823) [claude-sonnet-5]` | `fiat_pedagogy_kernel_2026_20260806_142033` |
| 2 | `8d2224c8…` ✓ | identical | `fiat_utility_debate_2026_20260806_142156` |
| 3 | `8d2224c8…` ✓ | identical | `fiat_efficacy_kernel_2026_20260806_142314` |

All three: whole-doc single-prompt ingest (0 chunk/window lines); corpus untouched
(listing-diff on `prolog/testsets/` and `json/` empty vs the pre-run snapshot after
every run); no `*_brief.md`; contested kernel minted every time; provenance identical
(`claude-sonnet-5`, prompt `d179423d`). Logs: `atfiat_k3_run{1,2,3}.log`. Spend: 3 of
≤3+1 calls, ≈101K input tok.

## What churns even at reproduce-rate 1.0 (feeds the standard)

- **Names churn while identities hold:** the kernel id took three values across four
  same-input draws (`fiat_efficacy_kernel` → `simulated_action_efficacy` /
  `fiat_value_kernel` / `simulated_action_efficacy`) and every reading was renamed in
  every draw — kind-level identity (subject+stance) is the stable stratum, names are
  not (consistent with the standing kind-level-only doctrine).
- **The unit population still churns:** runs 1–2 minted a SEVENTH reading
  (`localist_rejection_reading` / `localism_critique_reading`) by promoting the
  baseline's deferred `ethical_localism_trap` territory to reading altitude; run 3
  re-deferred it (`localism_universalism_scale_tension`). D varies 6→7→7→6 at full
  reproduce-rate — the denominator finding (PHASE0_REPORT correction block item 2)
  reproduces here in miniature: presence of the BASELINE set is stable, the
  population's boundary is not.
- **Genre-flag observation (descriptive, non-gating, per §4 firewall):** no re-run
  omega meets the OQ-259 item-3 (i)+(ii) standard (no strategic-selection +
  fidelity-consequence pair; the omegas are reading-collapse/localism-reflexivity
  bookkeeping) — as expected for a single-voice non-arsenal file; carries no weight on
  item 3.

## Interpretation (scoped)

Per-reading redraw stability is FILE-STRUCTURE-DEPENDENT, now measured at three points:
~0.33–0.50 (Cap K NW, 340K-tok arsenal), ~0.67–0.83 (Biopower NW, 103K arsenal),
**1.00 ×3 (AT Fiat, 34K single-voice answers file)**. The obvious reading — one
discrete card = one reading is a stable extraction; arsenal block-structure forces
synthesis choices that churn — is a hypothesis consistent with n=3 files, not a
finding. Consequence for the minted standard: no single global churn floor exists;
presence-claim standards must either measure the file's own reproduce-rate or assume
the worst measured class.
