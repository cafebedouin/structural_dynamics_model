% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation to Admit Refugees (Reading)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint represents the reading of border legitimacy that
 *   emphasizes a state's humanitarian obligation to admit those fleeing
 *   persecution or disaster, while maintaining the right to exclude general
 *   economic migrants. It is a 'tangled rope' because it genuinely
 *   coordinates international protection for vulnerable populations
 *   (beneficiaries: refugees, humanitarian organizations) but simultaneously
 *   extracts from and suppresses economic migrants (victims: economic
 *   migrants) through active enforcement of the distinction. The distinction
 *   itself creates a bifurcated victim set and allows for moderate extraction
 *   from those deemed 'economic'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.45).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.7).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation to Admit Refugees (Reading)").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, 'e85d39aa-40c5-480f-b16e-576b37695715').
narrative_ontology:cs_kernel_codification('e85d39aa-40c5-480f-b16e-576b37695715', formalized).
narrative_ontology:cs_authority_grounding('e85d39aa-40c5-480f-b16e-576b37695715', lineage).
narrative_ontology:cs_interpretation_layer_present('e85d39aa-40c5-480f-b16e-576b37695715').
narrative_ontology:cs_reading_relation('e85d39aa-40c5-480f-b16e-576b37695715', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('e85d39aa-40c5-480f-b16e-576b37695715', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('e85d39aa-40c5-480f-b16e-576b37695715', foundational, non_refoulement_principle).
narrative_ontology:cs_axiom_status(non_refoulement_principle, holdable).
narrative_ontology:cs_axiom_grounding('e85d39aa-40c5-480f-b16e-576b37695715', non_refoulement_principle, deontological).
narrative_ontology:cs_axiom('e85d39aa-40c5-480f-b16e-576b37695715', foundational, state_right_to_control_entry_of_non_refugees).
narrative_ontology:cs_axiom_status(state_right_to_control_entry_of_non_refugees, holdable).
narrative_ontology:cs_axiom_grounding('e85d39aa-40c5-480f-b16e-576b37695715', state_right_to_control_entry_of_non_refugees, conventional).
narrative_ontology:cs_reference_frame('e85d39aa-40c5-480f-b16e-576b37695715', post_wwii_refugee_convention_framework).
narrative_ontology:cs_drift_state('e85d39aa-40c5-480f-b16e-576b37695715', contemporary_global_migration_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e85d39aa-40c5-480f-b16e-576b37695715', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, receiving_states_security).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, international_humanitarian_organizations).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_with_weak_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, refugees_and_asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States define and enforce the distinction between refugees/asylum seekers and economic migrants, admitting the former under international law while excluding the latter. They benefit from perceived control over borders and national security, but bear costs of processing and integration.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, receiving_states_security, agenda_setter,
    institutional, generational, constrained, national).

% These individuals are granted protection and entry, escaping persecution or disaster. Their lives depend on this distinction being upheld, but they are vulnerable to arbitrary interpretations and delays.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, refugees_and_asylum_seekers, beneficiary,
    powerless, immediate, trapped, global).

% These individuals are systematically excluded based on their motivation for migration. They face significant barriers, risks, and often exploitation in attempts to cross borders, bearing the full cost of the distinction.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, trapped, global).

% These organizations advocate for the rights of refugees and provide aid. Their mandate is strengthened by the recognition of humanitarian obligations, but they are constrained by state sovereignty and resource limitations.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, international_humanitarian_organizations, beneficiary,
    organized, generational, constrained, global).

% These groups monitor state compliance with international law and advocate for broader protections, often challenging the strictness of the refugee/migrant distinction.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to provide protection to vulnerable populations fleeing persecution or disaster, preventing states from unilaterally denying entry to those in dire need, while allowing for managed migration flows.
% TRANSFER_FUNCTION: Transfers the burden of care and integration from countries of origin/first asylum to receiving states for refugees, while transferring the cost of exclusion (e.g., border enforcement, human suffering) to economic migrants.
% ABSENT_VOICES: Economic migrants, who are denied a legitimate pathway to entry, would argue for a more expansive view of human mobility and the right to seek a better life, challenging the moral legitimacy of the distinction itself.
% DISAPPEARANCE_RATIONALE: If the humanitarian obligation vanished, states would likely close borders more aggressively, leading to increased suffering for those fleeing persecution/disaster, and a breakdown of international protection regimes. If the distinction itself vanished, states would face immense pressure to admit all migrants, leading to a complete reorganization of border policies and national demographics.
% FOUNDING_PROBLEM: The post-WWII era saw massive displacement and a recognition that states had a moral and legal obligation to protect those fleeing persecution, leading to the 1951 Refugee Convention.
% FOUNDING_PROBLEM_CORROBORATION: International law, UN agencies, and human rights organizations consistently corroborate the ongoing necessity and live status of this obligation, citing persistent global conflicts and disasters. While some states contest the scope or implementation, the core principle remains widely affirmed by external bodies.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).
:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while it provides vital protection, the distinction is often applied in ways that create hardship and exploitation for those deemed 'economic migrants', who are still fleeing dire circumstances. Suppression is high (0.7) due to the active border enforcement, detention, and deportation mechanisms required to maintain the distinction. Theater ratio is low (0.2) as the humanitarian function is real, but the increasing securitization of borders and the politicization of migration debates introduce some performative elements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of receiving states, this is a necessary and legitimate balance between sovereignty and humanitarianism. From the perspective of economic migrants, it is an arbitrary and unjust distinction that denies fundamental human mobility. The engine will compute these divergent classifications based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving states act as agenda-setters, balancing security and humanitarian concerns. Refugees are clear beneficiaries, while economic migrants are clear targets/victims. International humanitarian organizations benefit from the framework's existence but are constrained in their advocacy. The bifurcated victim set is a key structural feature of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distinction_arbitrariness,
    'Is the distinction between ''refugee'' and ''economic migrant'' genuinely clear and morally defensible, or is it an arbitrary construct used to manage migration flows?',
    'Empirical analysis of ''mixed migration flows'' and the lived experiences of individuals, coupled with philosophical inquiry into the moral weight of different motivations for movement.',
    'If arbitrary, the constraint''s extractiveness and suppression would be re-evaluated as higher, as the coordination function would be revealed as a cover for broader exclusion. If robust, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distinction_arbitrariness, conceptual, 'Ambiguity in the moral and practical clarity of the refugee/migrant distinction.').

omega_variable(
    state_capacity_vs_obligation,
    'To what extent does a state''s capacity (economic, social, infrastructural) legitimately limit its humanitarian obligation, and how is this capacity measured?',
    'Development of internationally agreed-upon metrics for ''absorptive capacity'' and ''fair share'' principles, or legal rulings clarifying the limits of non-refoulement in mass influx situations.',
    'If capacity is a strong, measurable limit, the constraint''s extractiveness might be seen as lower (more justified). If capacity is often used as a pretext, extractiveness would be higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_capacity_vs_obligation, empirical, 'Uncertainty regarding the legitimate limits of state humanitarian obligation based on capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(bord_tr_t1970, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1951, 0.3).
narrative_ontology:measurement(bord_be_t1970, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1951, 0.5).
narrative_ontology:measurement(bord_su_t1970, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'border_legitimacy' kernel. It focuses on the humanitarian obligation, which influences but does not foreclose the sovereignty-based and freedom-of-movement readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
