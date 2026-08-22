% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Reciprocity Bargain as Read by Non-Nuclear Weapon States (Article VI Binding)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the Nuclear Non-Proliferation
 *   Treaty text — the nnws_reading — as a clean, epsilon-invariant
 *   constraint. The standing arrangement under contest is the treaty regime
 *   as it actually operates: the non-weapons-state majority delivers
 *   verified, inspected, funded restraint, while the five recognized weapons
 *   states owe good-faith disarmament negotiations under Article VI with no
 *   timeline, no verification, and no enforcement body able to compel them.
 *   Under this reading, Article VI is binding law and non-weapons-state
 *   restraint is conditional consideration exchanged for weapons-state
 *   performance; because the counter-performance remains undelivered while
 *   the restraint side is actively enforced, the arrangement couples a
 *   genuine coordination function (proliferation prevention, crisis
 *   stability, peaceful commerce under a common verification baseline) with
 *   asymmetric delivery, held together by enforcement machinery that runs
 *   against only one side. Per the epsilon-invariance principle, the
 *   colloquial label 'the NPT bargain' decomposes into at least three
 *   structurally distinct claims; this file authors one of them, with its own
 *   epsilon, beneficiaries, and classification, linked to its siblings
 *   through the network block. The claimed type and the authored metrics are
 *   independent facts: the claim records what this reading takes the
 *   structure to be; the metrics record what the arrangement's operation
 *   looks like from the documentary record.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary beneficiary and co-agenda-setter (institutional/arbitrage) — collects verified restraint and status legitimacy, faces no symmetric verification, shapes the review agenda through the P5 process
 *   - non_nuclear_weapon_states_parties: Primary target bloc (organized/constrained) — delivers verified restraint and funding, holds an unenforced reciprocity claim, exits only through costly withdrawal
 *   - allied_nnws_under_extended_deterrence: Intermediate seat (powerful/constrained) — pays restraint, collects umbrella security, resists enforcement of the reciprocity claim hardest
 *   - iaea_safeguards_secretariat: Verification administrator (institutional/constrained) — enforces the restrained side only, reports violations to a council that enforces selectively
 *   - tpnw_states_coalition: Excluded challenger bloc (organized/mobile) — built a parallel instrument after concluding review politics cannot deliver, remains outside consensus decisions
 *   - non_party_threshold_states: Excluded outsiders (powerful/mobile) — armed outside the treaty, cited by both camps, seated in no review decision
 *   - icj_international_lawyers: Analytical observer (analytical/analytical) — affirms the obligation's legal reality while its content stays open
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.62).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.45).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Reciprocity Bargain as Read by Non-Nuclear Weapon States (Article VI Binding)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, 'c9eb2fd5-f966-4604-bda5-7a9985a761a9').
narrative_ontology:cs_kernel_codification('c9eb2fd5-f966-4604-bda5-7a9985a761a9', fixed_text).
narrative_ontology:cs_authority_grounding('c9eb2fd5-f966-4604-bda5-7a9985a761a9', lineage).
narrative_ontology:cs_interpretation_layer_present('c9eb2fd5-f966-4604-bda5-7a9985a761a9').
narrative_ontology:cs_reading_relation('c9eb2fd5-f966-4604-bda5-7a9985a761a9', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9eb2fd5-f966-4604-bda5-7a9985a761a9', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('c9eb2fd5-f966-4604-bda5-7a9985a761a9', foundational, article_vi_binding_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c9eb2fd5-f966-4604-bda5-7a9985a761a9', article_vi_binding_obligation, conventional).
narrative_ontology:cs_axiom('c9eb2fd5-f966-4604-bda5-7a9985a761a9', foundational, restraint_as_conditional_consideration).
narrative_ontology:cs_axiom_status(restraint_as_conditional_consideration, holdable).
narrative_ontology:cs_axiom_grounding('c9eb2fd5-f966-4604-bda5-7a9985a761a9', restraint_as_conditional_consideration, conventional).
narrative_ontology:cs_reference_frame('c9eb2fd5-f966-4604-bda5-7a9985a761a9', reciprocal_bargain_1968).
narrative_ontology:cs_drift_state('c9eb2fd5-f966-4604-bda5-7a9985a761a9', contemporary_tpnw_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c9eb2fd5-f966-4604-bda5-7a9985a761a9', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, non_nuclear_weapon_states_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states_parties).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, allied_nnws_under_extended_deterrence).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, allied_nnws_under_extended_deterrence).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, pacta_sunt_servanda_reciprocity).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, icj_1996_article_vi_affirmation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states recognized by the treaty as possessing nuclear weapons at entry into force. They retain and modernize arsenals, run the P5 process that shapes each review cycle's agenda, and submit voluntary reporting on disarmament steps that no body can verify or compel. Their security planning presupposes the arrangement's continuation: it legitimizes their status, dampens proliferation among allies and rivals, and costs them nothing beyond what they themselves agree to describe as interim.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, nuclear_weapon_states, agenda_setter).

% The treaty's majority bloc. They accept comprehensive safeguards, facility access, and export restrictions on their own programs, and fund the verification system through assessed contributions. In exchange they hold a legal commitment from the weapons states to pursue disarmament negotiations, plus peaceful-use cooperation. Their collective leverage runs through review conferences and consensus politics; individual exit runs through lawful withdrawal, which since the North Korean case carries reputational cost and sanctions exposure.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states_parties, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, non_nuclear_weapon_states_parties, beneficiary).

% Wealthy non-weapons states — NATO members, Japan, South Korea, Australia — that forgo acquisition while relying on United States nuclear guarantees. They defend the regime publicly but resist binding timelines or proximity to the parallel prohibition treaty, because enforceable disarmament pressure threatens the umbrella their defense planning rests on. Their restraint is real and inspected; their enthusiasm for the reciprocity claim is calibrated to alliance maintenance.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, allied_nnws_under_extended_deterrence, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, allied_nnws_under_extended_deterrence, beneficiary).

% Administers the verification system: negotiates safeguards agreements, conducts inspections, and reports non-compliance to its Board and the Security Council. Member states set its mandate and budget; its inspection rights reach deep into non-weapons-state programs while extending to the five recognized weapons states only as far as those states volunteer. When it reports violations, consequences follow only if the Security Council concurs.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, iaea_safeguards_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% A bloc of mostly non-weapons states, joined by humanitarian organizations and survivor movements, that negotiated a parallel prohibition treaty after concluding the review process would never deliver disarmament. They attend review conferences as observers and organize voting pressure, but stand outside the consensus decisions that govern the original treaty, and the weapons states boycott their instrument outright.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, tpnw_states_coalition, excluded,
    organized, generational, mobile, global).

% States that acquired arsenals outside the treaty — India, Pakistan, Israel — and never accepted its obligations. They engage the regime's export-control and safety apparatus selectively while rejecting the membership bargain; their existence is cited by both camps in the reciprocity dispute, and they hold a seat in no review-conference decision.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_party_threshold_states, excluded,
    powerful, generational, mobile, regional).

% The International Court of Justice, treaty-law scholars, and commission rapporteurs who adjudicate and theorize what the treaty's obligations mean. The Court's 1996 opinion affirmed that good-faith negotiation toward disarmament is a legal duty while leaving its concrete content open; subsequent scholarship and litigation supply the doctrinal pressure that review politics alone cannot generate.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, icj_international_lawyers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nnws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Halts proliferation cascades among parties by universalizing verified restraint; provides a standing multilateral forum for disarmament diplomacy; underwrites peaceful nuclear commerce and safety standards through a common verification baseline.
% TRANSFER_FUNCTION: Moves verified restraint — foregone weapons options, intrusive inspection access, programmatic transparency, assessed funding — from the non-weapons-state majority into a common security pool; moves status legitimacy and strategic stability to the weapons states. The promised counter-transfer, disarmament performance, remains undelivered, so the net flow under this reading runs one way.
% ABSENT_VOICES: Non-party threshold states would object that the bargain entrenches a permanent caste line drawn without them; they sit outside every review decision. Hibakusha and downwind communities were absent from the state-centric room for five decades until the humanitarian initiative carried their testimony in; they object that indefinite possession normalizes harms done to them. Both objections now enter only through pressure channels the consensus rule can disregard.
% DISAPPEARANCE_RATIONALE: Export-control arrangements, alliance deterrence planning, the verification agency's mandate, and regional proliferation decisions across East Asia and the Middle East all presuppose the treaty's architecture. Overnight removal would trigger hedging cascades in industrialized states, immediate alliance renegotiation, and collapse of the common verification baseline — the security order would rearrange around whatever replaced it.
% FOUNDING_PROBLEM: The early-1960s projection that dozens of states would acquire nuclear weapons within a decade. The treaty was built to freeze the spread at the existing five while binding those five to negotiate their arsenals away, so that restraint would be a temporary condition rather than a permanent caste.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the ICJ's 1996 advisory opinion affirms the disarmament obligation as live law; successive General Assembly resolutions and the parallel treaty's negotiating record attest that the weapons-state side remains undelivered; hibakusha testimony and humanitarian-initiative documentation supply non-state attestation. The weapons states themselves attest the opposite — that the danger persists and they comply in good faith — which is precisely why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the net transfer under this reading runs one way in delivery — verified restraint, inspection access, programmatic transparency, and assessed funding flow from the majority while the owed counter-performance stays undelivered — but the regime also returns real goods (cascade prevention, commerce, safety standards) and its coercive reach is limited, tempering the score below predatory levels. Suppression 0.45: enforcement exists and hardened materially (the 93+2 program, the Additional Protocol) but applies to the restrained side only and ultimately depends on Security Council unanimity, which the weapons states' veto caps. Theater ratio 0.52: review-cycle consensus documents have grown increasingly ceremonial — final documents that recite disarmament language while binding nothing — while the safeguards apparatus remains functionally real, splitting the ratio near even. Accessibility collapse 0.50: alternatives remain partly usable (the parallel prohibition treaty, lawful withdrawal, latent hedging), so understanding the arrangement does not foreclose exit the way a natural limit would. Resistance 0.65: sustained, organized, cross-regional resistance — unusual for a treaty regime — expressed through the humanitarian initiative, the parallel instrument, and repeated review-conference blockades. Trajectory notes: the 1995 step-change reflects indefinite extension removing the twenty-five-year renewal leverage the majority originally held; the 2000 dip reflects the thirteen practical steps reform pulse; the 2017 rise tracks the parallel treaty's adoption exposing the delivery gap. The suppression series is tracked deliberately because enforcement capacity genuinely changed over the interval (hardening through 2000, then plateauing as Additional Protocol adoption stalls and budgets strain) — this is an enforcement-capacity story, not a static picture. Fixing cost is prohibitive: amendment runs through the very agenda-setters whose obligations would tighten, so no fixer captures enough benefit to outweigh the cost they personally bear.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seat compute differently from identical text. From the weapons-state seat the arrangement is stewardship they voluntarily report on; from the majority seat it is a contract whose other signatory stopped performing while keeping the collection machinery running. The allied non-weapons states occupy a genuinely intermediate position with an identity-lock dynamic: decades of extended-deterrence dependence have fused non-acquisition with their security self-concept, so exit is unthinkable short of alliance rupture, and they resist enforcement of the reciprocity claim harder than anyone — if the umbrella's credibility broke, their seat would migrate toward the target pole and organized resistance would spike. The verification administrator bears administrative cost without capturing gains; the excluded challengers experience the consensus rule itself as the operative barrier. Same-power divergence: the weapons states and the verification agency both hold institutional power over the same text, yet one collects from it and the other administers it — differentiated by role and exit, not by standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries drive the derivation: nuclear_weapon_states sit near the beneficiary end (they collect restraint and legitimacy; their arbitrage-grade exit — they shape interpretation and face no symmetric verification — pushes them further toward subsidy). Declared victims drive the opposite pole: non_nuclear_weapon_states_parties sit near the full-target end (they deliver the transfer; constrained exit — costly withdrawal, sanctions exposure since the North Korean precedent — traps them there). The allied non-weapons states are intentionally left out of both arrays: their position is genuinely mixed (they pay restraint but collect umbrella security), and forcing them into either array would falsify the structure. The challenger coalition and the threshold states sit outside the transfer path entirely — one contests it from a parallel venue, the other never entered it. The vindicated propositions (reciprocity doctrine, the court's affirmation) are listed separately and collect no rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two halves: halt the spread, and negotiate the existing arsenals away. The first half remains live and substantially achieved among parties; the second half is atrophying — the delivery gap widens while review-cycle language grows more ceremonial. The tangled-rope classification guards against both mislabels: against pure extraction (the coordination function is genuine — no party-state cascade since 1970, commerce and safety cooperation flow) and against pure coordination (the delivery asymmetry is documented and growing, not a rounding error). The rising theater series alongside a plateauing suppression series is the drift signature to watch: if review maintenance becomes fully performative while nobody profits enough to repair it and the majority's cost of exit stays prohibitive, the arrangement drifts toward inertial maintenance of a bargain whose second half nobody enforces. The genealogy consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges as a zombie-risk flag — the founding problem's disarmament half may be dead while the arrangement persists — without asserting that outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel npt_treaty_text (reading: nnws_reading). Would instantiating the nws_reading instead — non-proliferation as binding on NNWS, disarmament as aspirational without enforcement — change the structural classification, and where exactly does the disagreement bite?',
    'Compile and classify the sibling stories (npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading) and compare per-seat classifications and epsilon over the identical standing arrangement.',
    'If the nws_reading computes as low-extraction coordination, the extraction signal in this story localizes entirely to the reciprocity dispute — who bears binding obligations — rather than to the regime''s coordination function; the shared text then classifies differently by reading, confirming the decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this file is one reading of the NPT kernel; sibling readings instantiate different constraints over the same text.').

omega_variable(
    article_vi_content_indeterminacy,
    'What concrete conduct does Article VI''s good-faith obligation require, such that weapons-state behavior could be measured as compliance or breach?',
    'Development of ICJ jurisprudence, Marshall Islands-line litigation outcomes, or negotiated benchmarks (fissile-material cutoff, test-ban entry into force, arsenal ceilings) that operationalize the standard.',
    'If measurable benchmarks exist and are unmet, the weapons states sit in material breach and effective extraction from the restrained majority rises sharply; if the obligation is genuinely indeterminate, part of the measured imbalance is the price of legal indeterminacy rather than deliberate default.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_content_indeterminacy, empirical, 'Whether the disarmament obligation has determinate content against which delivery can be judged.').

omega_variable(
    tpnw_stigma_effectiveness,
    'Does the parallel prohibition treaty''s stigmatization actually alter weapons-state posture — budget lines, doctrine, extended-deterrence bargains — or does it harden resistance while imposing costs on the advocating majority?',
    'Track weapons-state defense-planning changes, alliance burden-sharing renegotiations, and review-conference voting blocs across successive cycles.',
    'If stigmatization works, this reading''s pressure instruments acquire enforcement-like force and the arrangement migrates toward enforceable reciprocity; if it backfires, resistance climbs and the review process drifts toward purely ceremonial maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tpnw_stigma_effectiveness, empirical, 'Whether regime-external pressure converts into behavioral change among the weapons states.').

omega_variable(
    customary_law_crystallization,
    'Is the Article VI obligation crystallizing into customary international law binding even non-parties, as litigation arguments and General Assembly resolution patterns suggest?',
    'State-practice and opinio-juris surveys; domestic court treatment of the obligation; International Law Commission identification-of-customary-law workstream outputs.',
    'Crystallization would open a new enforcement channel (raising suppression while narrowing the delivery asymmetry); failure leaves this reading dependent on review-conference pressure alone, sustaining the current weak-enforcement profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_crystallization, empirical, 'Whether political pressure on the disarmament obligation is hardening into justiciable law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nnws_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1978, npt_treaty_text__nnws_reading, theater_ratio, 1978, 0.25).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nnws_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__nnws_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nnws_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nnws_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(npt__tr_t2017, npt_treaty_text__nnws_reading, theater_ratio, 2017, 0.47).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_text__nnws_reading, theater_ratio, 2025, 0.52).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nnws_reading, base_extractiveness, 1970, 0.34).
narrative_ontology:measurement(npt__be_t1978, npt_treaty_text__nnws_reading, base_extractiveness, 1978, 0.4).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nnws_reading, base_extractiveness, 1985, 0.46).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__nnws_reading, base_extractiveness, 1995, 0.54).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nnws_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nnws_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(npt__be_t2017, npt_treaty_text__nnws_reading, base_extractiveness, 2017, 0.6).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_text__nnws_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nnws_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(npt__su_t1978, npt_treaty_text__nnws_reading, suppression_requirement, 1978, 0.28).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__nnws_reading, suppression_requirement, 1985, 0.32).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_text__nnws_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nnws_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nnws_reading, suppression_requirement, 2010, 0.46).
narrative_ontology:measurement(npt__su_t2017, npt_treaty_text__nnws_reading, suppression_requirement, 2017, 0.46).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_text__nnws_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the NPT bargain' covers at least three structurally distinct claims: (1) this file's nnws_reading — Article VI binding, restraint as conditional consideration; (2) npt_treaty_text__nws_reading — non-proliferation binding on the majority, disarmament aspirational; (3) npt_treaty_text__withdrawal_threshold_reading — the Article X exit-threshold contest. Each carries its own epsilon, beneficiary/victim structure, and classification over the same standing arrangement; the nws_reading currently controls the enforcement levers and thus sits upstream of this reading's operating environment. Family links declared via affects_constraints per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
