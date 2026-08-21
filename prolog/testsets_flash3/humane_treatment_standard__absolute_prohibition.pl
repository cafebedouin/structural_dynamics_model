% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Absolute Prohibition of Torture (Common Article 3 Reading)
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'absolute prohibition' reading of Common
 *   Article 3 of the Geneva Conventions, which establishes non-derogable
 *   minimum standards for humane treatment during armed conflict. Under this
 *   reading, no circumstances, including national security imperatives,
 *   permit torture or degrading treatment. It is framed as a Mountain due to
 *   its claimed non-derogable, universal nature, with negligible extraction
 *   and suppression, as it asserts a fundamental moral and legal limit on
 *   state action. Detainees are the beneficiaries, while state parties and
 *   their personnel bear the 'cost' of absolute adherence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.1).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.05).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.1).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, mountain).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Absolute Prohibition of Torture (Common Article 3 Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/human_rights").

domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, 'cae3aad5-7460-40f8-8e3a-25563e37aa54').
narrative_ontology:cs_kernel_codification('cae3aad5-7460-40f8-8e3a-25563e37aa54', fixed_text).
narrative_ontology:cs_authority_grounding('cae3aad5-7460-40f8-8e3a-25563e37aa54', lineage).
narrative_ontology:cs_interpretation_layer_present('cae3aad5-7460-40f8-8e3a-25563e37aa54').
narrative_ontology:cs_reading_relation('cae3aad5-7460-40f8-8e3a-25563e37aa54', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('cae3aad5-7460-40f8-8e3a-25563e37aa54', humane_treatment_standard__proportionality_balancing, forecloses).
narrative_ontology:cs_axiom('cae3aad5-7460-40f8-8e3a-25563e37aa54', foundational, human_dignity_is_absolute).
narrative_ontology:cs_axiom_status(human_dignity_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('cae3aad5-7460-40f8-8e3a-25563e37aa54', human_dignity_is_absolute, deontological).
narrative_ontology:cs_axiom('cae3aad5-7460-40f8-8e3a-25563e37aa54', foundational, non_derogability_of_core_rights).
narrative_ontology:cs_axiom_status(non_derogability_of_core_rights, holdable).
narrative_ontology:cs_axiom_grounding('cae3aad5-7460-40f8-8e3a-25563e37aa54', non_derogability_of_core_rights, deontological).
narrative_ontology:cs_reference_frame('cae3aad5-7460-40f8-8e3a-25563e37aa54', post_geneva_conventions_absolute_prohibition).
narrative_ontology:cs_drift_state('cae3aad5-7460-40f8-8e3a-25563e37aa54', contemporary_counterterrorism_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('cae3aad5-7460-40f8-8e3a-25563e37aa54', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, interrogators_and_detention_personnel).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, universal_human_dignity).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, non_derogable_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals held in custody during armed conflict, whose fundamental human dignity and right to humane treatment are protected absolutely by this standard, regardless of their status or perceived threat. They have no means of exit from detention.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees, beneficiary,
    powerless, immediate, trapped, global).

% States that have ratified the Geneva Conventions and are bound by Common Article 3. This reading requires them to uphold absolute prohibitions against torture and degrading treatment, even when facing security threats. Their 'exit' is to withdraw from the conventions, which carries immense diplomatic and legal costs.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, constrained, global).

% Individuals directly responsible for the custody and interrogation of detainees. This reading imposes strict limits on their methods, requiring adherence to humane treatment standards without exception. Non-compliance carries legal and professional penalties.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, interrogators_and_detention_personnel, payer,
    moderate, biographical, constrained, local).

% Organizations and individuals who monitor state compliance with international human rights law, document violations, and advocate for the strict adherence to the absolute prohibition against torture. They provide external scrutiny and pressure.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-derogable baseline for humane treatment of persons hors de combat, ensuring a minimum standard of dignity and preventing a race to the bottom in conflict zones.
% TRANSFER_FUNCTION: Transfers the absolute right to humane treatment to all detainees, imposing an absolute obligation on state parties to refrain from torture or degrading treatment, regardless of security imperatives.
% ABSENT_VOICES: Those who advocate for 'enhanced interrogation techniques' or 'security exceptions' to human rights law are structurally excluded from this reading's framework, as it asserts the non-derogable nature of the prohibition.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished, the legal and moral landscape of armed conflict would fundamentally shift. States would face fewer constraints on interrogation, potentially leading to widespread abuses, and the international human rights framework would lose a foundational pillar.
% FOUNDING_PROBLEM: The historical reality of widespread atrocities, torture, and inhumane treatment of combatants and civilians during armed conflicts, necessitating a universal, non-derogable standard of protection.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN bodies, and numerous legal scholars consistently attest to the ongoing relevance of this prohibition, citing persistent violations and the need for its continued enforcement. The problem of inhumane treatment in conflict remains live.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, ExtMetricName, E),
    domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.1) because this reading asserts a fundamental moral and legal limit, not a mechanism for rent collection. The 'cost' to states is adherence to a universal standard, which is framed as a duty rather than an extraction. Suppression is low (0.05) because the constraint's persistence relies on its inherent moral force and widespread acceptance as international law, rather than active coercion against dissenting parties. Theater ratio is 0.0 as this reading posits no performative aspect; its function is direct and absolute. Accessibility collapse is high (0.9) because this reading asserts that alternatives to humane treatment are morally and legally foreclosed. Resistance is low (0.05) because, within this reading, the prohibition is widely accepted as a fundamental norm, even if sometimes violated in practice.
 *
 * PERSPECTIVAL GAP:
 *   While this reading asserts a universal, non-derogable standard, other readings (contextual_necessity, proportionality_balancing) would experience the same underlying legal text as more flexible or extractive, reflecting a fundamental disagreement on the nature of the constraint itself. This story captures only the absolute prohibition perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees are full beneficiaries (d=0.0) as the constraint directly protects their fundamental rights. State parties are agenda-setters who bear the obligation of upholding the standard (d=0.5, symmetric, as the benefit is to the international order they participate in). Interrogators are payers (d=1.0) as they are directly constrained in their methods. Human rights advocates are observers (d=0.5, analytical).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_compliance_gap,
    'Does the actual practice of state parties consistently align with the absolute prohibition, or are there widespread, unacknowledged violations?',
    'Comprehensive, independent monitoring and reporting by international bodies and NGOs, coupled with robust accountability mechanisms for violations.',
    'If violations are widespread and unacknowledged, the constraint''s effective extractiveness (from detainees) and suppression (of alternatives to humane treatment) would be higher than stated, and its ''mountain'' classification would be challenged as a false summit or a snare for detainees.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_compliance_gap, empirical, 'Gap between the declared absolute prohibition and actual state practice.').

omega_variable(
    natural_law_vs_convention,
    'Is the absolute prohibition against torture a reflection of natural law (emerges_naturally: true) or a purely conventional legal norm that could be revised?',
    'Philosophical and legal debate on the foundations of international law, and analysis of state behavior during periods of extreme stress or regime change.',
    'If purely conventional, its ''mountain'' classification is conceptually weaker, and its persistence depends more on active enforcement and political will than on inherent truth. The presence of beneficiaries (detainees) on a ''natural'' law also flags it for false summit detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_convention, conceptual, 'Whether the prohibition is a natural moral truth or a human construct.').

omega_variable(
    reading_framing_contest,
    'Is this ''absolute prohibition'' reading the most accurate interpretation of Common Article 3, or do ''contextual necessity'' or ''proportionality balancing'' readings offer a more faithful account of its intent and application?',
    'Judicial rulings by international courts, consensus among legal scholars, and evolving state practice that explicitly rejects or embraces one reading over others.',
    'If a sibling reading gains dominance, the structural properties of the constraint (e.g., extractiveness from detainees, suppression of state discretion) would shift dramatically, leading to a reclassification of the underlying constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_contest, conceptual, 'Contest over the correct interpretation of Common Article 3.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__absolute_prohibition, theater_ratio, 1949, 0.0).
narrative_ontology:measurement(huma_tr_t1970, humane_treatment_standard__absolute_prohibition, theater_ratio, 1970, 0.0).
narrative_ontology:measurement(huma_tr_t1990, humane_treatment_standard__absolute_prohibition, theater_ratio, 1990, 0.0).
narrative_ontology:measurement(huma_tr_t2001, humane_treatment_standard__absolute_prohibition, theater_ratio, 2001, 0.0).
narrative_ontology:measurement(huma_tr_t2010, humane_treatment_standard__absolute_prohibition, theater_ratio, 2010, 0.0).
narrative_ontology:measurement(huma_tr_t2024, humane_treatment_standard__absolute_prohibition, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement(huma_be_t1970, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(huma_be_t1990, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(huma_be_t2001, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2001, 0.1).
narrative_ontology:measurement(huma_be_t2010, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2010, 0.1).
narrative_ontology:measurement(huma_be_t2024, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1949, 0.05).
narrative_ontology:measurement(huma_su_t1970, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1970, 0.05).
narrative_ontology:measurement(huma_su_t1990, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(huma_su_t2001, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2001, 0.05).
narrative_ontology:measurement(huma_su_t2010, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2010, 0.05).
narrative_ontology:measurement(huma_su_t2024, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, proportionality_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'humane_treatment_standard' kernel (Common Article 3). It asserts an absolute prohibition, influencing and coexisting with other readings that propose contextual exceptions or balancing tests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
