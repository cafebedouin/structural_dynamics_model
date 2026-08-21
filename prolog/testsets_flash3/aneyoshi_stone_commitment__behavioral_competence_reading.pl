% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Commitment: Behavioral Competence Reading
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'behavioral competence' reading of the
 *   Aneyoshi tsunami stone commitment. In this reading, the stone functions
 *   as a live, intergenerational land-use rule, actively constraining
 *   building location decisions. Its efficacy was dramatically demonstrated
 *   by the community's survival during the 2011 Tohoku tsunami, where
 *   adherence to the stone's directive to build above a certain elevation
 *   saved lives. This reading emphasizes the stone's operational force as a
 *   regulatory mechanism, rather than merely a commemorative artifact.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone Commitment: Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, '305473f6-f53e-40eb-9f10-8f0a0dfc8194').
narrative_ontology:cs_kernel_codification('305473f6-f53e-40eb-9f10-8f0a0dfc8194', fixed_text).
narrative_ontology:cs_authority_grounding('305473f6-f53e-40eb-9f10-8f0a0dfc8194', lineage).
narrative_ontology:cs_reading_relation('305473f6-f53e-40eb-9f10-8f0a0dfc8194', aneyoshi_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('305473f6-f53e-40eb-9f10-8f0a0dfc8194', foundational, intergenerational_behavioral_transmission).
narrative_ontology:cs_axiom_status(intergenerational_behavioral_transmission, holdable).
narrative_ontology:cs_axiom_grounding('305473f6-f53e-40eb-9f10-8f0a0dfc8194', intergenerational_behavioral_transmission, empirically_contingent).
narrative_ontology:cs_axiom('305473f6-f53e-40eb-9f10-8f0a0dfc8194', foundational, local_ecological_knowledge_as_regulatory_force).
narrative_ontology:cs_axiom_status(local_ecological_knowledge_as_regulatory_force, holdable).
narrative_ontology:cs_axiom_grounding('305473f6-f53e-40eb-9f10-8f0a0dfc8194', local_ecological_knowledge_as_regulatory_force, empirically_contingent).
narrative_ontology:cs_reference_frame('305473f6-f53e-40eb-9f10-8f0a0dfc8194', ancestral_tsunami_survival_directive).
narrative_ontology:cs_drift_state('305473f6-f53e-40eb-9f10-8f0a0dfc8194', contemporary_2011_tohoku_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('305473f6-f53e-40eb-9f10-8f0a0dfc8194', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_risk_transmission).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, local_ecological_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of Aneyoshi, whose ancestors erected the stone, continue to abide by its directive to build above a certain elevation. This commitment has demonstrably saved lives in subsequent tsunamis, making them direct beneficiaries of the constraint's operational force. Their identity is deeply tied to the land and its history of disaster.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents, beneficiary,
    powerless, generational, identity_locked, local).

% Local officials implicitly uphold the stone's directive through zoning and building permit processes, even if not explicitly citing the stone. They benefit from reduced disaster response costs and increased community resilience. Their actions reinforce the stone's behavioral competence.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, local_government_officials, agenda_setter,
    institutional, biographical, constrained, local).

% Academics and practitioners who study the Aneyoshi stone as a case study in effective, long-term, community-based disaster risk reduction. They analyze its mechanisms and outcomes, corroborating its behavioral impact.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, disaster_risk_reduction_experts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land-use decisions across generations to ensure settlement occurs above the historical tsunami inundation line, preventing loss of life and property.
% TRANSFER_FUNCTION: Transfers knowledge and a behavioral imperative from past generations to present and future ones, ensuring safety at the cost of restricting prime coastal land for development.
% ABSENT_VOICES: Developers or new residents who might prioritize economic development or scenic views over historical risk mitigation are implicitly excluded by the community's strong adherence to the stone's directive and local planning norms.
% DISAPPEARANCE_RATIONALE: If the commitment embodied by the Aneyoshi stone vanished, future generations might build in hazardous zones, leading to catastrophic loss of life and property in subsequent tsunamis. The community's long-term survival strategy would collapse.
% FOUNDING_PROBLEM: Repeated devastating tsunamis wiped out coastal settlements, leading ancestors to seek a permanent, intergenerational solution for safe habitation.
% FOUNDING_PROBLEM_CORROBORATION: The 2011 Tohoku tsunami, which devastated neighboring areas but left Aneyoshi largely unharmed due to adherence to the stone's elevation, provides direct empirical corroboration from outside the immediate community. Disaster anthropologists and historical records also attest to the problem's persistence and the stone's efficacy.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_commitment__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because the constraint primarily serves to protect the community, with minimal cost beyond land-use restriction. Suppression is low (0.1) as adherence is largely voluntary and culturally ingrained, not coercively enforced. Theater ratio is also very low (0.05) because the stone's function is demonstrably real and effective, not performative. Accessibility collapse is high (0.9) as the alternative (building in unsafe zones) is understood to be catastrophic. Resistance is negligible (0.02) due to the clear and repeatedly demonstrated benefits.
 *
 * PERSPECTIVAL GAP:
 *   This reading posits the stone as an active, life-saving constraint. A contrasting 'commemorative husk' reading would see it as a symbolic artifact with no behavioral force, leading to a very different classification (likely Piton or even Mountain with high theater if its 'naturalness' is contested). The engine's classification will depend on which reading's metrics are accepted.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are direct beneficiaries, as the stone's directive ensures their safety. Local government officials also benefit from reduced disaster impact. Disaster risk reduction experts are observers, analyzing its efficacy. No identifiable victims exist in this reading, as the constraint is seen as purely protective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_commemorative_ambiguity,
    'Is the Aneyoshi stone commitment primarily a live behavioral constraint on land use, or has it decayed into a commemorative artifact with symbolic but no operational force?',
    'Longitudinal ethnographic study of land-use decisions and building practices in Aneyoshi, coupled with analysis of local government zoning and permitting processes, to determine if the stone''s directive is actively referenced or implicitly followed in practice.',
    'If primarily behavioral, the constraint is a Mountain (as claimed). If commemorative, it would likely reclassify as a Piton, with higher theater_ratio and lower accessibility_collapse, as its function would be performative rather than protective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_commemorative_ambiguity, empirical, 'Ambiguity between the stone''s active behavioral influence and its symbolic commemorative role.').

omega_variable(
    natural_law_vs_cultural_norm,
    'Is the ''naturalness'' of the stone''s directive (build above the tsunami line) a physical Mountain, or is its persistence as a behavioral constraint a culturally constructed Rope?',
    'Analysis of other tsunami-prone communities: if similar directives are universally adopted and adhered to without explicit cultural artifacts, it leans towards natural law. If adherence is highly variable and dependent on specific cultural transmission mechanisms, it leans towards a constructed Rope.',
    'If a pure natural law, the ''emerges_naturally'' flag is fully justified. If a culturally constructed Rope, the low extractiveness and suppression are still valid, but the ''naturalness'' claim is weaker, potentially triggering an FSM if beneficiaries are identified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_cultural_norm, conceptual, 'Whether the stone''s directive is a natural law (physics of tsunamis) or a culturally transmitted norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 2011, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'aneyoshi_stone_commitment' kernel. This 'behavioral competence' reading emphasizes the stone's active role in land-use decisions, contrasting with the 'commemorative husk' reading which views it as a symbolic artifact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
