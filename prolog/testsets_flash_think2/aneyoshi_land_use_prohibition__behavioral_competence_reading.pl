% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'behavioral competence' reading of the
 *   Aneyoshi tsunami stone land-use prohibition. In this reading, the stone
 *   is understood as a live, actively enforced land-use rule, where the
 *   prohibition against building below a certain elevation has been
 *   operationally maintained through social practice and community adherence
 *   for 78 years. The constraint is grounded in the physical reality of
 *   tsunami hazards, with social enforcement ensuring its efficacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.2).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:requires_active_enforcement(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, '2c982345-4122-483d-944d-9f51b006dc41').
narrative_ontology:cs_kernel_codification('2c982345-4122-483d-944d-9f51b006dc41', fixed_text).
narrative_ontology:cs_authority_grounding('2c982345-4122-483d-944d-9f51b006dc41', practice).
narrative_ontology:cs_interpretation_layer_present('2c982345-4122-483d-944d-9f51b006dc41').
narrative_ontology:cs_reading_relation('2c982345-4122-483d-944d-9f51b006dc41', aneyoshi_land_use_prohibition__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('2c982345-4122-483d-944d-9f51b006dc41', foundational, tsunami_risk_is_ever_present).
narrative_ontology:cs_axiom_status(tsunami_risk_is_ever_present, holdable).
narrative_ontology:cs_axiom_grounding('2c982345-4122-483d-944d-9f51b006dc41', tsunami_risk_is_ever_present, empirically_contingent).
narrative_ontology:cs_axiom('2c982345-4122-483d-944d-9f51b006dc41', foundational, adherence_ensures_survival).
narrative_ontology:cs_axiom_status(adherence_ensures_survival, holdable).
narrative_ontology:cs_axiom_grounding('2c982345-4122-483d-944d-9f51b006dc41', adherence_ensures_survival, instrumental).
narrative_ontology:cs_reference_frame('2c982345-4122-483d-944d-9f51b006dc41', community_survival_through_adherence).
narrative_ontology:cs_drift_state('2c982345-4122-483d-944d-9f51b006dc41', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2c982345-4122-483d-944d-9f51b006dc41', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_hazard_awareness).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, community_resilience_through_adherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere to the land-use prohibition, settling above the tsunami inundation line. They are the primary beneficiaries of the safety and survival the rule provides, but also bear the costs of restricted land use and potentially longer commutes to fishing grounds or other resources.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents, beneficiary,
    powerless, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents, payer).

% Maintain the tsunami stones, educate new generations about the hazard and the rule, and integrate the prohibition into local planning and disaster preparedness. They enforce the rule through social norms and administrative guidance rather than overt coercion.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, local_authorities, agenda_setter,
    institutional, generational, mobile, local).

% Individuals or groups who might view the tsunami stones primarily as historical memorials or symbols, rather than active land-use rules. In this 'behavioral competence' reading, their interpretation is not the dominant operational one, and their voices are effectively marginalized in community decision-making regarding land use.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, commemorative_husk_reading_adherents, excluded,
    moderate, biographical, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land use and settlement patterns within the Aneyoshi community to avoid high-risk tsunami inundation zones, thereby ensuring collective safety and long-term survival in a hazard-prone region.
% TRANSFER_FUNCTION: Transfers the cost of restricted coastal land use (e.g., building further inland, longer access to resources) from individual residents to the collective benefit of community safety, resilience, and survival from tsunamis.
% ABSENT_VOICES: Those who might prioritize convenience, economic development, or aesthetic preferences for coastal living over strict adherence to the prohibition are effectively excluded from the operational interpretation of the rule. Their perspectives are overridden by the community's historical experience and shared commitment to survival.
% DISAPPEARANCE_RATIONALE: If the prohibition and its social enforcement vanished overnight, settlement patterns would gradually drift back into hazardous coastal zones over generations, increasing vulnerability to future tsunamis and leading to catastrophic loss of life and property, fundamentally reorganizing the community's relationship with its environment.
% FOUNDING_PROBLEM: Repeated devastating tsunamis throughout history, which destroyed coastal settlements and caused immense loss of life in the Aneyoshi region, necessitating a permanent, intergenerational solution for community survival.
% FOUNDING_PROBLEM_CORROBORATION: Geological records of past tsunami deposits, historical accounts of devastation, and ongoing scientific assessments of seismic activity and tsunami risk in the region, all corroborated by disaster anthropologists, seismologists, and local historical societies outside the immediate beneficiary group.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because the rule primarily serves a collective safety function, with minimal rent-seeking. Suppression is low (0.20) as adherence is largely voluntary and maintained through strong social norms and intergenerational transmission of knowledge, rather than overt coercion. The theater ratio is also very low (0.05) because the rule is genuinely functional and directly contributes to community survival, with little performative maintenance. Accessibility collapse is high (0.90) due to the undeniable physical hazard of tsunamis, which makes alternatives to safe settlement effectively non-existent. Resistance is low (0.05) because the rule is widely accepted as essential for survival.
 *
 * PERSPECTIVAL GAP:
 *   This 'behavioral competence' reading contrasts sharply with the 'commemorative husk' reading, which views the stone as a historical memorial with decayed behavioral force. From the perspective of this reading, the rule is a vital, functional component of community life, whereas the alternative reading would see it as largely symbolic. The engine's classification will highlight this divergence based on the differing metric profiles of each reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are both beneficiaries (receiving safety) and payers (bearing the cost of restricted land use), resulting in a directionality near symmetric. Local authorities act as agenda-setters, maintaining the rule and educating the community. The tsunami hazard itself is the underlying physical constraint that makes the social rule necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the Aneyoshi prohibition is unequivocally live. The founding problem (recurrent devastating tsunamis) persists, and the constraint continues to effectively solve the problem of community vulnerability. There is no evidence of mandatrophy in this reading; the constraint's function remains critical and actively fulfilled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_symbolic_function,
    'Is the Aneyoshi stone''s prohibition a live, behaviorally enforced land-use rule, or primarily a historical memorial with decayed behavioral force?',
    'Longitudinal ethnographic studies of settlement patterns, land-use decisions, and community responses to hazard warnings over multiple generations. Analysis of local planning documents and disaster preparedness protocols.',
    'If the prohibition is found to be a ''commemorative husk'' (as per the sibling reading), its effective extractiveness would be lower (as there''s no real behavioral cost), and its classification might shift towards Piton or even Mountain (if purely symbolic). If it remains behaviorally competent, its classification as a Rope is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_symbolic_function, empirical, 'Distinguishing the operational function of the tsunami stone from its symbolic role.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aney_tr_t20, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(aney_tr_t40, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 78, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aney_be_t20, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(aney_be_t40, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(aney_be_t60, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(aney_be_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 78, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(aney_su_t20, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(aney_su_t40, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(aney_su_t60, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(aney_su_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 78, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two readings of the 'aneyoshi_land_use_prohibition' kernel. This 'behavioral competence' reading emphasizes the active, functional role of the stone as a land-use rule, while the 'commemorative husk' reading (a sibling constraint) interprets it as a historical memorial with decayed behavioral force. Both constraints are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
