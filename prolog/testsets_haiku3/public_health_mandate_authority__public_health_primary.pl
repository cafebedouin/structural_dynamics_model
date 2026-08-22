% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate Authority—Collective Protection Primary
 *   domain: public_health/constitutional_rights/bioethics
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.68).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.71).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate Authority—Collective Protection Primary").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, '9dd6a27c-a098-4774-a6fc-676adccda875').
narrative_ontology:cs_kernel_codification('9dd6a27c-a098-4774-a6fc-676adccda875', formalized).
narrative_ontology:cs_authority_grounding('9dd6a27c-a098-4774-a6fc-676adccda875', extraction).
narrative_ontology:cs_interpretation_layer_present('9dd6a27c-a098-4774-a6fc-676adccda875').
narrative_ontology:cs_reading_relation('9dd6a27c-a098-4774-a6fc-676adccda875', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('9dd6a27c-a098-4774-a6fc-676adccda875', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('9dd6a27c-a098-4774-a6fc-676adccda875', foundational, collective_protection_foundational).
narrative_ontology:cs_axiom_status(collective_protection_foundational, holdable).
narrative_ontology:cs_axiom_grounding('9dd6a27c-a098-4774-a6fc-676adccda875', collective_protection_foundational, deontological).
narrative_ontology:cs_axiom('9dd6a27c-a098-4774-a6fc-676adccda875', secondary, externality_imposition_justifies_coercion).
narrative_ontology:cs_axiom_status(externality_imposition_justifies_coercion, holdable).
narrative_ontology:cs_axiom_grounding('9dd6a27c-a098-4774-a6fc-676adccda875', externality_imposition_justifies_coercion, instrumental).
narrative_ontology:cs_reference_frame('9dd6a27c-a098-4774-a6fc-676adccda875', population_health_primacy_framework).
narrative_ontology:cs_drift_state('9dd6a27c-a098-4774-a6fc-676adccda875', endemic_phase_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9dd6a27c-a098-4774-a6fc-676adccda875', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_infrastructure).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, vulnerable_commons).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, employment_displaced_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot mount immune response to pathogens; depend on collective immunity thresholds maintained through population-wide mandates. Their survival requires others to comply; they cannot negotiate or exit. They benefit from mandate enforcement because it sustains the protective commons they cannot generate individually.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Hospitals, clinics, and critical care systems face collapse when surge demand exceeds capacity during disease outbreaks. Mandates that reduce transmission reduce surge risk, enabling the system to function within design limits. The infrastructure benefits from reduced strain; mandates are the stabilizing mechanism.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_infrastructure, beneficiary,
    organized, generational, constrained, national).

% The shared epidemiological state—herd immunity thresholds, pathogen prevalence, transmission chains—is a collective good that individuals cannot protect unilaterally. The mandate coordinates the externality: individual non-compliance imposes harm on the commons and on those who depend on it.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, vulnerable_commons, beneficiary,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(public_health_mandate_authority__public_health_primary, vulnerable_commons).

% Refuse vaccination or other interventions on grounds of bodily autonomy, medical skepticism, or distrust of authority. They bear the costs of mandate enforcement: employment loss, service exclusion, social stigma. Exit would require identity-rupture (renouncing their autonomy framework or medical convictions). The mandate's enforcement machinery is directed at them.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals, payer,
    moderate, biographical, identity_locked, national).

% Face termination from employment when vaccine-mandate requirements clash with their (stated or genuine) medical contraindications or conscience objections. They lack the resources to litigate exemptions, lack alternative employment paths in tight labor markets, and bear the externality cost: income loss, family destabilization, healthcare access interruption.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, employment_displaced_populations, payer,
    powerless, immediate, trapped, national).

% Declares and enforces the mandate, backed by law and employment/service rules. Justifies it as protecting the commons and the immunocompromised. Sets thresholds, duration, exemption criteria. Carries responsibility for both the protection function and the coercive enforcement.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, constrained, national).

% Mobilizes resistance to mandates on bodily autonomy grounds. They would argue that no collective benefit justifies non-consensual medical intervention and that individual choice must be preserved even if herd immunity falls. Their voice is excluded from the mandate-setting process; they are framed as spreading disinformation rather than holding a legitimate alternative framework.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, political_opposition, excluded,
    organized, biographical, constrained, national).

% Argue for sliding-scale justification: mandate legitimacy depends on threat severity, alternative availability, coercion magnitude, and duration. They are structurally excluded because this reading treats the collective protection obligation as foundational and non-negotiable; proportionality framing would permit mandate-lifting even if commons protection remained incomplete.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, proportionality_advocates, excluded,
    moderate, biographical, constrained, national).

% Other jurisdictions' mandate regimes provide comparison data: empirical outcomes under stricter or lighter enforcement, observed compliance patterns, measured health equity impacts, and political economy consequences. They generate evidence for whether the public_health_primary framing produces better population outcomes than alternative readings.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, comparative_public_health_regimes, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, public_health_authority).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains collective immunity thresholds and protects healthcare capacity by coordinating individual medical interventions into a population-wide protection strategy. Solves the tragedy-of-the-commons problem: individual non-compliance imposes negative externality on the immunocompromised and system stability, but individuals cannot internalize that cost unilaterally.
% TRANSFER_FUNCTION: Moves bodily autonomy rights and labor-market participation from resistant individuals to collective protection capacity. The mandate-resistant bear the cost of compliance or the cost of exclusion; the immunocompromised and infrastructure gain predictability and protection. The transfer is asymmetric: the beneficiaries gain protective services; the payers lose choice and sometimes income.
% ABSENT_VOICES: Political opposition (bodily autonomy primary advocates) and proportionality advocates are structurally excluded from mandate-setting deliberation. They would frame the mandate as unjustified coercion or as requiring sliding-scale justification, not foundational obligation. Their exclusion from the consensus is the signature of this reading's dominance—the protected commons is asserted as beyond-negotiation rather than as a live contention.
% DISAPPEARANCE_RATIONALE: If the mandate disappeared, unvaccinated populations would re-enter labor markets and service access; immunocompromised individuals would face newly elevated personal protection burdens or involuntary isolation; healthcare surge capacity would become brittle; and collective immunity would drift below protective thresholds for populations requiring it. The constraint's removal would reorganize who bears risk and who is protected.
% FOUNDING_PROBLEM: Infectious disease poses collective threat to public health and healthcare system stability. Individuals acting in isolation cannot sustain protective immunity thresholds; free-rider problems emerge when some benefit from collective protection without contributing to it. Healthcare systems face surge-collapse risk when disease prevalence rises faster than capacity.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and epidemiological literature attest the founding problem is live and requires mandates to solve. Political opposition and bodily-autonomy advocates attest the problem is overstated, that alternatives (voluntary incentives, tiered access, voluntary high-risk isolation) could address surge risk without coercion. Medical practitioners outside mandate-setting attest the problem varies by disease (COVID-19 transmission dynamics vs. smaller-risk diseases) and moment (early pandemic vs. endemic phase). No single external party corroborates the public_health_primary reading unambiguously.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_protection_necessity,
    'Is population-wide mandate enforcement necessary to maintain protective immunity thresholds, or would voluntary incentives and high-risk voluntary isolation achieve the same epidemiological outcomes?',
    'Natural experiments from jurisdictions that adopted voluntary-only strategies: empirical tracking of herd immunity levels, healthcare surge outcomes, and protection of immunocompromised populations.',
    'If outcomes are equivalent, the mandate''s extraction is not balanced by necessary coordination function—extraction becomes primary. If voluntary strategies fail, the mandate''s coordination function is necessary and the extraction is coordination cost. If outcomes are intermediate, the mandate is justified at some threat/extraction ratio but not others, supporting proportionality_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_protection_necessity, empirical, 'Whether mandates are structurally necessary for collective protection or whether alternatives achieve equivalent outcomes.').

omega_variable(
    suppression_internalization_trajectory,
    'Is the measured suppression (0.71) primarily structural (legal/employment barriers) or internalized (individuals have adopted the mandate''s framing that resistance is dangerous)?',
    'Post-mandate empirical trajectory: if suppression persists after legal mandates are lifted, internalization is substantial; if suppression evaporates, it was structural. Ethnographic/survey research on motivation for compliance: do individuals comply because they fear penalties or because they believe resistance endangers others?',
    'If primarily structural, the constraint is a coercive apparatus that would release upon mandate-lifting. If internalized, the constraint has become internalized collective norm even after legal mandate ends—a deeper capture. Identity-locked exit classification depends partly on this: identity rupture requires internalization to occur.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Structural vs. internalized character of suppression in mandate-resistant populations.').

omega_variable(
    commons_framing_contestation,
    'Is ''vulnerable commons'' a coherent collective category whose protection justifies individual mandate costs, or is it a rhetorical bundling of distinct populations (immunocompromised, healthcare workers) whose needs could be met through targeted rather than universal mandates?',
    'Comparative policy analysis: jurisdictions that adopted targeted mandates (mandatory for healthcare workers, voluntary for general population) and tracked health equity outcomes separately for each population.',
    'If ''commons'' is coherent and universal mandates are necessary to protect it, public_health_primary reading is supported and extraction is justified coordination cost. If commons is rhetorical bundling and targeted mandates achieve protection, the universal mandate is overreach—extraction on mandate-resistant is not necessary for the stated coordination function, and snare dynamics emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_framing_contestation, conceptual, 'Whether ''vulnerable commons'' is a coherent collective or a rhetorical bundling of distinct populations.').

omega_variable(
    reading_foreclosure_structure,
    'Does the public_health_primary reading logically foreclose the bodily_autonomy_primary reading, or do they coexist as irreconcilable value frameworks?',
    'Analytic: if public_health_primary asserts ''collective obligation is foundational,'' it directly contradicts bodily_autonomy''s ''individual choice is foundational.'' Can both axioms be held in a single normative framework? If not, one forecloses the other. If yes, they coexist as different parties'' frameworks.',
    'Forecloses relation: one reading is logically impossible if the other is adopted. Coexists_with: both readings remain live for different parties. This affects kernel-level dynamics and whether judicial, legislative, or political processes can adjudicate between readings or must instead manage permanent contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Logical relationship between public_health_primary and bodily_autonomy_primary readings of the kernel.').

omega_variable(
    immunocompromised_agency_erasure,
    'Are immunocompromised populations correctly characterized as beneficiaries without agency (trapped/powerless, passive recipients), or do they have negotiating power and could they be authors of the mandate rather than passive beneficiaries?',
    'Political-economy analysis: historical record of immunocompromised advocacy during mandate development; empirical power to veto or modify mandates; whether immunocompromised populations would support mandates if autonomy costs were higher or alternatives emerged.',
    'If beneficiaries are genuinely agency-erased, the beneficiary/payer structure accurately models the constraint''s asymmetry and tangled_rope classification holds. If immunocompromised populations have substantial agency and could negotiate mandate terms, the constraint might be better modeled as a negotiated coordination structure where both beneficiary and payer seats hold some power—rebalancing d across both seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immunocompromised_agency_erasure, empirical, 'Whether immunocompromised populations are agency-erased passive beneficiaries or active negotiating agents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(publ_tr_t0, observed).
narrative_ontology:measurement(publ_tr_t5, public_health_mandate_authority__public_health_primary, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(publ_tr_t5, observed).
narrative_ontology:measurement(publ_tr_t10, public_health_mandate_authority__public_health_primary, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(publ_tr_t10, observed).
narrative_ontology:measurement(publ_tr_t15, public_health_mandate_authority__public_health_primary, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(publ_tr_t15, observed).
narrative_ontology:measurement(publ_tr_t20, public_health_mandate_authority__public_health_primary, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(publ_tr_t20, observed).
narrative_ontology:measurement(publ_tr_t25, public_health_mandate_authority__public_health_primary, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(publ_tr_t25, observed).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__public_health_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(publ_tr_t30, observed).
narrative_ontology:measurement(publ_tr_t40, public_health_mandate_authority__public_health_primary, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(publ_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(publ_be_t0, observed).
narrative_ontology:measurement(publ_be_t5, public_health_mandate_authority__public_health_primary, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(publ_be_t5, observed).
narrative_ontology:measurement(publ_be_t10, public_health_mandate_authority__public_health_primary, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(publ_be_t10, observed).
narrative_ontology:measurement(publ_be_t15, public_health_mandate_authority__public_health_primary, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(publ_be_t15, observed).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__public_health_primary, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(publ_be_t20, observed).
narrative_ontology:measurement(publ_be_t25, public_health_mandate_authority__public_health_primary, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(publ_be_t25, observed).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__public_health_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(publ_be_t30, observed).
narrative_ontology:measurement(publ_be_t40, public_health_mandate_authority__public_health_primary, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(publ_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(publ_su_t0, observed).
narrative_ontology:measurement(publ_su_t5, public_health_mandate_authority__public_health_primary, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(publ_su_t5, observed).
narrative_ontology:measurement(publ_su_t10, public_health_mandate_authority__public_health_primary, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(publ_su_t10, observed).
narrative_ontology:measurement(publ_su_t15, public_health_mandate_authority__public_health_primary, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(publ_su_t15, observed).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__public_health_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(publ_su_t20, observed).
narrative_ontology:measurement(publ_su_t25, public_health_mandate_authority__public_health_primary, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(publ_su_t25, observed).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__public_health_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(publ_su_t30, observed).
narrative_ontology:measurement(publ_su_t40, public_health_mandate_authority__public_health_primary, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(publ_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(publ_grid_01, public_health_mandate_authority__public_health_primary, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement(publ_grid_02, public_health_mandate_authority__public_health_primary, accessibility_collapse(class), 40, 0.73).
narrative_ontology:measurement(publ_grid_03, public_health_mandate_authority__public_health_primary, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(publ_grid_04, public_health_mandate_authority__public_health_primary, accessibility_collapse(individual), 40, 0.68).
narrative_ontology:measurement(publ_grid_05, public_health_mandate_authority__public_health_primary, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(publ_grid_06, public_health_mandate_authority__public_health_primary, accessibility_collapse(organizational), 40, 0.74).
narrative_ontology:measurement(publ_grid_07, public_health_mandate_authority__public_health_primary, accessibility_collapse(structural), 0, 0.71).
narrative_ontology:measurement(publ_grid_08, public_health_mandate_authority__public_health_primary, accessibility_collapse(structural), 40, 0.78).
narrative_ontology:measurement(publ_grid_09, public_health_mandate_authority__public_health_primary, resistance(class), 0, 0.54).
narrative_ontology:measurement(publ_grid_10, public_health_mandate_authority__public_health_primary, resistance(class), 40, 0.48).
narrative_ontology:measurement(publ_grid_11, public_health_mandate_authority__public_health_primary, resistance(individual), 0, 0.48).
narrative_ontology:measurement(publ_grid_12, public_health_mandate_authority__public_health_primary, resistance(individual), 40, 0.38).
narrative_ontology:measurement(publ_grid_13, public_health_mandate_authority__public_health_primary, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(publ_grid_14, public_health_mandate_authority__public_health_primary, resistance(organizational), 40, 0.52).
narrative_ontology:measurement(publ_grid_15, public_health_mandate_authority__public_health_primary, resistance(structural), 0, 0.28).
narrative_ontology:measurement(publ_grid_16, public_health_mandate_authority__public_health_primary, resistance(structural), 40, 0.22).
narrative_ontology:measurement(publ_grid_17, public_health_mandate_authority__public_health_primary, stakes_inflation(class), 0, 0.44).
narrative_ontology:measurement(publ_grid_18, public_health_mandate_authority__public_health_primary, stakes_inflation(class), 40, 0.68).
narrative_ontology:measurement(publ_grid_19, public_health_mandate_authority__public_health_primary, stakes_inflation(individual), 0, 0.38).
narrative_ontology:measurement(publ_grid_20, public_health_mandate_authority__public_health_primary, stakes_inflation(individual), 40, 0.62).
narrative_ontology:measurement(publ_grid_21, public_health_mandate_authority__public_health_primary, stakes_inflation(organizational), 0, 0.41).
narrative_ontology:measurement(publ_grid_22, public_health_mandate_authority__public_health_primary, stakes_inflation(organizational), 40, 0.64).
narrative_ontology:measurement(publ_grid_23, public_health_mandate_authority__public_health_primary, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(publ_grid_24, public_health_mandate_authority__public_health_primary, stakes_inflation(structural), 40, 0.71).
narrative_ontology:measurement(publ_grid_25, public_health_mandate_authority__public_health_primary, suppression(class), 0, 0.42).
narrative_ontology:measurement(publ_grid_26, public_health_mandate_authority__public_health_primary, suppression(class), 40, 0.71).
narrative_ontology:measurement(publ_grid_27, public_health_mandate_authority__public_health_primary, suppression(individual), 0, 0.52).
narrative_ontology:measurement(publ_grid_28, public_health_mandate_authority__public_health_primary, suppression(individual), 40, 0.74).
narrative_ontology:measurement(publ_grid_29, public_health_mandate_authority__public_health_primary, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(publ_grid_30, public_health_mandate_authority__public_health_primary, suppression(organizational), 40, 0.68).
narrative_ontology:measurement(publ_grid_31, public_health_mandate_authority__public_health_primary, suppression(structural), 0, 0.38).
narrative_ontology:measurement(publ_grid_32, public_health_mandate_authority__public_health_primary, suppression(structural), 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__public_health_primary, 0.18).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories share the kernel public_health_mandate_authority but instantiate different readings. Public_health_primary (this file): collective protection is foundational; extraction on resistant individuals is justified coordination cost. Bodily_autonomy_primary: bodily sovereignty is foundational; mandates foreclose this reading and are categorically impermissible. Proportionality_reading: mandate legitimacy slides with threat/coercion ratio; mandates are neither foundational nor categorically impermissible but contextually justified. Each reading has distinct ε (public_health_primary ε=0.68 reflects extraction on resistant; bodily_autonomy_primary ε would be near 1.0 reflecting total violation; proportionality_reading ε would vary with measured threat/alternative availability). The readings coexist as different parties' frameworks in contemporary public discourse; no single reading forecloses another within the legal/political system—all three are simultaneously held by different institutional actors and political coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__public_health_primary, powerless, 0.9).
constraint_indexing:directionality_override(public_health_mandate_authority__public_health_primary, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
