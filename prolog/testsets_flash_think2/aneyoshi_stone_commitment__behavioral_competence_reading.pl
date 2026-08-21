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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Aneyoshi Stone Land-Use Commitment (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the 'behavioral competence' reading of the
 *   Aneyoshi Stone commitment, which views the stone as an active, effective
 *   land-use rule. It describes the stone's function as a direct,
 *   low-extraction coordination mechanism that successfully guided building
 *   location decisions for 78 years, culminating in the survival of the
 *   community during the 2011 tsunami. This reading contrasts with the
 *   'commemorative husk' reading, which sees the stone as purely symbolic.
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
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone Land-Use Commitment (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, 'c268e0b8-93a5-4266-af68-4b74cfb05261').
narrative_ontology:cs_kernel_codification('c268e0b8-93a5-4266-af68-4b74cfb05261', fixed_text).
narrative_ontology:cs_authority_grounding('c268e0b8-93a5-4266-af68-4b74cfb05261', practice).
narrative_ontology:cs_reading_relation('c268e0b8-93a5-4266-af68-4b74cfb05261', aneyoshi_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('c268e0b8-93a5-4266-af68-4b74cfb05261', foundational, tsunami_hazard_is_ever_present).
narrative_ontology:cs_axiom_status(tsunami_hazard_is_ever_present, holdable).
narrative_ontology:cs_axiom_grounding('c268e0b8-93a5-4266-af68-4b74cfb05261', tsunami_hazard_is_ever_present, empirically_contingent).
narrative_ontology:cs_axiom('c268e0b8-93a5-4266-af68-4b74cfb05261', foundational, intergenerational_wisdom_is_binding).
narrative_ontology:cs_axiom_status(intergenerational_wisdom_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('c268e0b8-93a5-4266-af68-4b74cfb05261', intergenerational_wisdom_is_binding, conventional).
narrative_ontology:cs_reference_frame('c268e0b8-93a5-4266-af68-4b74cfb05261', intergenerational_survival_mandate).
narrative_ontology:cs_drift_state('c268e0b8-93a5-4266-af68-4b74cfb05261', pre_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c268e0b8-93a5-4266-af68-4b74cfb05261', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, disaster_preparedness_efficacy).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_wisdom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in safety by adhering to the land-use rule encoded in the stone, building their homes above the designated tsunami inundation line. Their survival in the 2011 tsunami directly validated this compliance.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents, beneficiary,
    powerless, biographical, constrained, local).

% Maintains the physical stone markers and reinforces the land-use rule through local planning and public education. Benefits from the community's safety and resilience, reducing disaster response costs.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, local_government, agenda_setter,
    institutional, generational, constrained, local).

% Their collective experience and wisdom regarding tsunami hazards are embodied in the stone's directive. They are the original authors of the commitment, whose foresight continues to protect the community.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, past_generations_of_aneyoshi, observer,
    analytical, civilizational, analytical, local).

% These are individuals or groups who view the Aneyoshi stone primarily as a historical memorial or cultural artifact, rather than a binding, active land-use regulation. From the perspective of this 'behavioral competence' reading, their views are excluded from the operational understanding of the stone's function.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, commemorative_husk_reading_adherents, excluded,
    moderate, biographical, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land-use decisions within the Aneyoshi community to ensure collective survival against recurrent tsunami threats by establishing a clear, high-elevation building boundary.
% TRANSFER_FUNCTION: Transfers safety, resilience, and intergenerational knowledge from past generations to current and future residents by restricting development to areas above the stone's designated height.
% ABSENT_VOICES: Adherents of the 'commemorative husk' reading, who would argue for more flexible land-use policies based on the stone's symbolic rather than regulatory function, are absent from the operational decision-making process this reading describes.
% DISAPPEARANCE_RATIONALE: If the commitment vanished, future generations might disregard the stone's warning, leading to construction in vulnerable areas. The next tsunami would then result in catastrophic loss of life and the potential dissolution of the community, fundamentally altering its existence and demographic continuity.
% FOUNDING_PROBLEM: Repeated catastrophic loss of life and destruction of settlements from tsunamis in the Aneyoshi region, necessitating a clear and enduring directive for safe habitation.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of devastating tsunamis (e.g., 1896, 1933), geological evidence of inundation, and the observed survival of Aneyoshi residents in the 2011 Great East Japan Earthquake (due to compliance with the stone's directive) corroborate the ongoing threat and the efficacy of the commitment. This corroboration comes from historical accounts, scientific data, and direct empirical observation, not solely from the benefiting parties.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness is very low (0.05) because it primarily serves to prevent harm, with the 'cost' of compliance being minimal compared to the benefit of survival. Suppression is low (0.10) as compliance is largely self-enforcing due to the clear and catastrophic consequences of non-compliance, rather than active coercion. Theater ratio is very low (0.05) because the stone's function is direct and demonstrably effective. Accessibility collapse is high (0.90) because the alternative (building below the stone) is understood to be existentially risky. Resistance is low (0.05) due to the proven efficacy of the rule, especially after the 2011 tsunami.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this 'behavioral competence' reading, the stone is a clear, effective, and low-extraction coordination mechanism. However, adherents of the 'commemorative husk' reading would perceive the stone as having no active behavioral force, leading to a fundamental divergence in how the constraint's operational status is understood.
 *
 * DIRECTIONALITY LOGIC:
 *   The Aneyoshi residents are the primary beneficiaries, receiving safety and survival. The local government acts as an agenda-setter, maintaining the commitment and benefiting from community resilience. Past generations are observers whose wisdom is vindicated. Adherents of the 'commemorative husk' reading are excluded from the operational understanding of the stone's function, as their interpretation would undermine its behavioral force.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_rule_vs_memorial,
    'Is the Aneyoshi stone primarily an active, binding land-use rule, or has its function decayed to that of a purely commemorative memorial?',
    'Longitudinal ethnographic study of community decision-making processes, land-use planning documents, and building permit applications in the decades following the 2011 tsunami. If building patterns consistently adhere to the stone''s directive, it supports the ''behavioral competence'' reading.',
    'If resolved as an active rule, the constraint''s classification as a Rope is reinforced. If resolved as a memorial, its classification would shift towards a Piton, reflecting a degraded function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_rule_vs_memorial, empirical, 'Distinguishing the stone''s active regulatory force from its symbolic value.').

omega_variable(
    compliance_mechanism_ambiguity,
    'Is compliance with the stone''s directive driven by a deep-seated behavioral competence (internalized understanding of hazard), or by social pressure and institutional reinforcement?',
    'Comparative analysis with communities lacking such explicit markers but facing similar hazards. If compliance is higher and more resilient in Aneyoshi, it suggests a stronger internalized behavioral competence. Post-disaster interviews could also reveal motivations.',
    'If primarily behavioral competence, the constraint''s low suppression and high accessibility collapse are more robust. If primarily social pressure, the constraint might be more fragile to shifts in community norms or institutional capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_mechanism_ambiguity, empirical, 'Understanding the primary driver of compliance with the stone''s land-use rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aney_tr_t15, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(aney_tr_t30, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(aney_tr_t45, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 45, 0.05).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 78, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aney_be_t15, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement(aney_be_t30, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(aney_be_t45, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 45, 0.05).
narrative_ontology:measurement(aney_be_t60, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 78, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(aney_su_t15, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement(aney_su_t30, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(aney_su_t45, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 45, 0.1).
narrative_ontology:measurement(aney_su_t60, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(aney_su_t78, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 78, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Aneyoshi Stone commitment kernel. The 'commemorative_husk_reading' is a sibling constraint that interprets the stone as a symbolic artifact rather than an active land-use rule, leading to a higher extractiveness and a different classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
