% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe as Necessary Selector for Organizational Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In high-reliability industries (nuclear, aviation, offshore drilling,
 *   rail), a recurring organizational doctrine holds that only a real
 *   catastrophic event — with its attendant chaos, mortality salience, and
 *   institutional trauma — can genuinely recalibrate an organization's risk
 *   perception after a long incident-free period. This reading treats
 *   simulation and drills as structurally inferior proxies that create false
 *   confidence, and treats long peacetime stretches as inevitably producing
 *   competence decay that only an actual disaster can correct. The doctrine
 *   is not baseless: normalization of deviance during quiet periods is a
 *   well-documented failure mode. But the doctrine also concentrates
 *   authority in those who survived a disaster and in the post-disaster
 *   remediation industry, while discounting the judgment of peacetime staff,
 *   junior engineers, and simulation-based training as inherently
 *   second-tier, and it implicitly treats the next catastrophe as
 *   functionally necessary rather than as a failure to be prevented.
 *
 * KEY AGENTS:
 *   - post_disaster_consultancy_firms: primary beneficiary (organized/arbitrage) — market position depends on catastrophes having occurred
 *   - veteran_operators_with_disaster_experience: beneficiary and agenda-setter (powerful/constrained) — authority partly constituted by survived disaster
 *   - frontline_operators_during_peacetime: primary target (moderate/constrained) — judged as inevitably complacent absent a real event
 *   - junior_engineers_denied_real_stakes_training: primary target (powerless/trapped) — judgment discounted until they survive a real disaster
 *   - communities_near_high_hazard_facilities: diffuse victim (powerless/trapped) — bear the physical risk of the doctrine's implied necessary catastrophe
 *   - regulatory_safety_boards: analytical observer (institutional/analytical) — can compel alternative investment
 *   - simulation_and_drill_vendors: excluded voice (moderate/constrained) — structurally devalued by the doctrine's core claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.58).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.42).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe as Necessary Selector for Organizational Competence").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '2f40a90f-cad6-4319-bec8-5bfb5f3aff5d').
narrative_ontology:cs_kernel_codification('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d', distributed).
narrative_ontology:cs_authority_grounding('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d', practice).
narrative_ontology:cs_interpretation_layer_present('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d').
narrative_ontology:cs_reading_relation('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d', foundational, only_lived_catastrophe_recalibrates_risk_perception).
narrative_ontology:cs_axiom_status(only_lived_catastrophe_recalibrates_risk_perception, holdable).
narrative_ontology:cs_axiom_grounding('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d', only_lived_catastrophe_recalibrates_risk_perception, empirically_contingent).
narrative_ontology:cs_axiom('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d', secondary, simulation_produces_false_confidence_not_genuine_competence).
narrative_ontology:cs_axiom_status(simulation_produces_false_confidence_not_genuine_competence, holdable).
narrative_ontology:cs_axiom_grounding('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d', simulation_produces_false_confidence_not_genuine_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d', post_incident_institutional_memory_era).
narrative_ontology:cs_drift_state('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d', extended_contemporary_peacetime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f40a90f-cad6-4319-bec8-5bfb5f3aff5d', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, incident_investigation_specialists).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, post_disaster_consultancy_firms).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, veteran_operators_with_disaster_experience).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators_during_peacetime).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, communities_near_high_hazard_facilities).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, junior_engineers_denied_real_stakes_training).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, normalization_of_deviance_thesis).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, generational_forgetting_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build practice and reputation around post-catastrophe root-cause analysis and remediation contracts. Their expertise and market position depend on catastrophes having occurred and on the belief that only lived disaster teaches an organization anything real. They advise industry that simulation cannot substitute for the genuine article, which sustains demand for their retrospective services.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, post_disaster_consultancy_firms, beneficiary,
    organized, biographical, arbitrage, national).

% Hold elevated institutional standing and decision authority because they personally lived through a plant failure, a crash, a collapse. They set safety doctrine and training priorities, often asserting that people who have not been through a real event cannot truly understand the hazard. Their authority is partly constituted by having survived what others have not, which gives them an interest in the doctrine that only catastrophe teaches.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, veteran_operators_with_disaster_experience, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, veteran_operators_with_disaster_experience, agenda_setter).

% Work shifts for years or decades without a major incident and are told, implicitly and explicitly, that their competence is degrading regardless of training quality because no real catastrophe has recalibrated the organization's risk perception. They bear the anxiety of being judged as inevitably complacent, and bear the actual risk when the doctrine's prediction of decay becomes self-fulfilling through under-investment in non-catastrophic learning tools.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators_during_peacetime, payer,
    moderate, biographical, constrained, national).

% Enter high-hazard industries and are told by veteran staff that their simulator-based and drill-based training is fundamentally inferior and cannot be trusted to produce real competence. They cannot access the 'real' catastrophe experience that would grant full standing, so their judgment is discounted until and unless a genuine disaster occurs on their watch — a bar they have no way to clear on their own initiative.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, junior_engineers_denied_real_stakes_training, payer,
    powerless, biographical, trapped, national).

% Live adjacent to plants, rail corridors, or ports whose operating doctrine implicitly treats the next disaster as the mechanism that will recalibrate organizational safety. They bear the actual physical risk of the black swan event the doctrine treats as necessary and, in a sense, overdue, without having any say over whether alternative competence-maintenance investments are made instead.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, communities_near_high_hazard_facilities, payer,
    powerless, generational, trapped, regional).

% Investigate incidents after the fact and issue findings about organizational memory, drift, and competence decay. They can compel investment in simulation, near-miss reporting, or staffing, and their post-incident reports either reinforce or challenge the catastrophe-as-teacher doctrine depending on what they find.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulatory_safety_boards, observer,
    institutional, generational, analytical, national).

% Sell high-fidelity simulators and drill programs premised on the claim that rehearsed crisis exposure can substitute for real catastrophe. Under this reading's doctrine their product is structurally devalued — treated as necessarily inferior to lived disaster — so their voice in the industry's competence debate is discounted even where their evidence is strong.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_and_drill_vendors, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizations do need some mechanism to keep hazard perception calibrated over long stretches without incident, since real risk awareness genuinely can atrophy when nothing bad has happened for years — this reading correctly identifies that a coordination problem (competence maintenance under safety) exists.
% TRANSFER_FUNCTION: The doctrine transfers authority and institutional credibility toward those who possess disaster experience and toward the post-disaster remediation industry, while transferring anxiety, discounted judgment, and unaddressed residual risk onto peacetime frontline staff, junior engineers, and nearby communities who have no legitimate path to the credential the doctrine prizes.
% ABSENT_VOICES: Simulation and drill vendors, and the operators who have gone through high-fidelity training without a real disaster, would object that the doctrine's dismissal of rehearsed exposure is not evidence-based and forecloses cheaper, non-destructive competence-maintenance strategies; they are structurally discounted by the very doctrine under review and rarely sit on the safety boards that adjudicate it.
% DISAPPEARANCE_RATIONALE: If belief in catastrophe-as-necessary-selector vanished overnight, veteran operators would lose a distinct source of institutional standing and consultancy firms would lose a rhetorical anchor for their market; regulatory bodies would need to establish alternative, harder-to-verify competence metrics. Some structures (deference to disaster survivors, underinvestment in simulation) would rearrange; but the underlying calibration problem the doctrine points at would not disappear, which is why the verdict is contested rather than clean.
% FOUNDING_PROBLEM: Organizations that go long stretches without incidents genuinely do drift toward complacency and normalize small deviations from safe practice (documented repeatedly in post-incident investigations), so some means of keeping hazard perception sharp is a real and recurring problem.
% FOUNDING_PROBLEM_CORROBORATION: Safety researchers outside the veteran-operator and consultancy communities (independent human-factors and organizational-safety academics) corroborate that competence decay during long incident-free periods is real, but a substantial body of that same independent literature disputes the stronger claim that only actual catastrophe — as opposed to well-designed simulation or near-miss learning — can supply the needed selection pressure. No source outside the beneficiary set corroborates the strong exclusivity claim that catastrophe is necessary and simulation cannot substitute.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-rising (0.34 to 0.58) because the coordination function (keeping hazard perception calibrated) is real and partially served, but an increasing share of the doctrine's institutional weight rewards disaster-survivor status and remediation contracting rather than actual competence improvement. Theater ratio rises faster (0.30 to 0.61) because as peacetime stretches lengthen without incident, the doctrine increasingly manifests as ritualized deference to survivor narratives and consultancy-driven post-mortems rather than falsifiable competence testing — a classic proxy-goal substitution (Goodhart drift) where 'has someone here survived a disaster' substitutes for 'is this organization actually competent.' Suppression is comparatively low and rises gently (0.28 to 0.42): the doctrine operates less through overt coercion than through discounting non-disaster credentials and channeling institutional prestige, though this hardens somewhat as veteran cohorts consolidate authority over training budgets.
 *
 * PERSPECTIVAL GAP:
 *   From the veteran-operator seat, the doctrine reads as hard-won wisdom: an organization that hasn't been tested cannot know its own weaknesses, and that humility is itself safety-positive. From the junior-engineer or simulation-vendor seat, the same doctrine reads as an unfalsifiable status hierarchy that discounts real evidence of competence (drill performance, near-miss reporting, simulator mastery) in favor of a credential nobody can earn by preparation alone. The engine's per-seat computation should reflect that the coordination function (calibration against complacency) is genuinely served for the organization as a whole, while the extraction (status and market capture riding on that function) is concentrated on specific seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Veteran operators and consultancy firms sit near the beneficiary end: their standing and revenue are partly constituted by catastrophe having occurred and by the belief that lived disaster is irreplaceable. Frontline peacetime operators and junior engineers sit near the target end: they bear the reputational discount of unproven competence and the anxiety of an unfalsifiable decay narrative, with junior engineers particularly trapped since no amount of simulator hours can grant them the credential the doctrine actually prizes. Communities near facilities are targets by proxy — they carry the physical tail risk of a doctrine that treats catastrophe as a necessary teacher rather than a failure outcome, without having agency over the organization's competence-maintenance choices.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine competence decay during long incident-free periods — remains partially live, which is why this is tangled_rope rather than snare: there is a real coordination function underneath the extraction. But the doctrine's strong exclusivity claim (only real catastrophe suffices) increasingly functions as a mandate that outlives its evidentiary support once high-fidelity simulation and near-miss reporting systems mature — at that point the doctrine's persistence owes more to the standing it confers on disaster survivors and consultancy firms than to any remaining necessity. Distinguishing 'a real calibration problem exists' from 'only catastrophe can solve it' is exactly the seam this classification exists to hold open rather than collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_necessity_vs_selector_bias,
    'Is catastrophic experience actually a superior competence-maintenance mechanism, or does survivor-bias make disaster-experienced staff appear more competent simply because incompetent staff and organizations that failed catastrophically are removed from the sample?',
    'Longitudinal comparison of organizations that experienced a real disaster versus matched organizations that maintained rigorous simulation and near-miss reporting programs without a real disaster, tracking subsequent safety performance over 10-20 years.',
    'If simulation-trained organizations perform comparably, the exclusivity premise collapses and the constraint''s coordination claim is substantially weakened relative to its extraction — pushing the classification toward snare. If catastrophe-experienced organizations reliably outperform, the coordination function is more robust and the tangled_rope reading is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_necessity_vs_selector_bias, empirical, 'Whether disaster-derived competence is causally superior or a survivorship artifact.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (catastrophe_as_necessary_selector) of the kernel catastrophe_avoidance_retention. The sibling readings (simulation_as_proxy_catastrophe, hybrid_near_miss_learning) disagree specifically about whether simulated or distributed near-miss exposure can supply the same selection pressure as a real catastrophe. Where exactly is the disagreement located structurally?',
    'Compare organizational outcomes across industries that have adopted high-fidelity simulation regimes (aviation) versus those still substantially reliant on real-incident learning (some process-industry sectors) to locate whether the disagreement is empirical (about simulation fidelity) or definitional (about what counts as ''genuine'' selection pressure).',
    'If the disagreement is purely empirical, resolving simulation fidelity data could shift the whole kernel toward the simulation or hybrid reading, deprecating this reading''s exclusivity claim. If definitional, the readings may remain permanently coexisting rather than resolvable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating whether the kernel''s contested readings differ empirically or definitionally.').

omega_variable(
    veteran_authority_capture_extent,
    'To what extent has veteran-operator authority over training doctrine become self-reinforcing (survivors gatekeeping the definition of competence in ways that favor survivor status) versus reflecting genuine, transferable expertise?',
    'Audit training-budget and promotion decisions in organizations with strong catastrophe-as-teacher cultures against decisions in organizations with more distributed, simulation-heavy competence models, controlling for actual safety outcomes.',
    'High capture would strengthen the case that the beneficiary structure (veteran operators, consultancy firms) is doing more extractive than coordinative work; low capture would support the doctrine''s self-presentation as merit-based wisdom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(veteran_authority_capture_extent, empirical, 'Whether veteran authority reflects transferable expertise or self-reinforcing gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t8, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 8, 0.38).
narrative_ontology:measurement(cata_tr_t16, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 16, 0.46).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 24, 0.52).
narrative_ontology:measurement(cata_tr_t32, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 32, 0.57).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(cata_be_t8, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(cata_be_t16, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(cata_be_t32, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(cata_su_t8, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(cata_su_t16, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(cata_su_t32, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.1).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the catastrophe_avoidance_retention kernel: the shared kernel question is what selection pressure suffices to maintain organizational safety competence across long incident-free intervals. This reading (catastrophe_as_necessary_selector) claims only real catastrophic events supply adequate pressure and forecloses the simulation_as_proxy_catastrophe reading's claim that high-fidelity drills are functionally equivalent — the two premises directly contradict each other on whether simulated exposure can genuinely substitute for lived disaster. This reading stands in an influences relationship (not foreclosure) with hybrid_near_miss_learning, since the hybrid reading's claim that distributed near-miss learning contributes to competence maintenance is compatible with catastrophe also being necessary as an occasional hard reset, even though the hybrid reading assigns catastrophe a smaller share of the mechanism. Each reading is authored with its own epsilon, beneficiary/victim structure, and metrics per the epsilon-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
