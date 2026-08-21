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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the Aneyoshi tsunami stone land-use prohibition
 *   from the 'behavioral competence' reading. In this view, the stone is a
 *   live, operationally enforced rule, not merely a memorial. The
 *   prohibition, based on the physical reality of tsunami hazards, has been
 *   actively maintained through social practice for 78 years, ensuring the
 *   community's survival. This reading emphasizes the active, behavioral
 *   force of the rule and its direct impact on land-use decisions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.25).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:requires_active_enforcement(aneyoshi_land_use_prohibition__behavioral_competence_reading).
domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, '9d12c2f5-5283-4bdd-b9f1-8e2e5ced1018').
narrative_ontology:cs_kernel_codification('9d12c2f5-5283-4bdd-b9f1-8e2e5ced1018', fixed_text).
narrative_ontology:cs_authority_grounding('9d12c2f5-5283-4bdd-b9f1-8e2e5ced1018', practice).
narrative_ontology:cs_reading_relation('9d12c2f5-5283-4bdd-b9f1-8e2e5ced1018', aneyoshi_land_use_prohibition__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('9d12c2f5-5283-4bdd-b9f1-8e2e5ced1018', foundational, tsunami_hazard_is_live).
narrative_ontology:cs_axiom_status(tsunami_hazard_is_live, holdable).
narrative_ontology:cs_axiom_grounding('9d12c2f5-5283-4bdd-b9f1-8e2e5ced1018', tsunami_hazard_is_live, empirically_contingent).
narrative_ontology:cs_axiom('9d12c2f5-5283-4bdd-b9f1-8e2e5ced1018', foundational, intergenerational_adherence_is_active).
narrative_ontology:cs_axiom_status(intergenerational_adherence_is_active, holdable).
narrative_ontology:cs_axiom_grounding('9d12c2f5-5283-4bdd-b9f1-8e2e5ced1018', intergenerational_adherence_is_active, conventional).
narrative_ontology:cs_reference_frame('9d12c2f5-5283-4bdd-b9f1-8e2e5ced1018', community_survival_through_adherence).
narrative_ontology:cs_drift_state('9d12c2f5-5283-4bdd-b9f1-8e2e5ced1018', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9d12c2f5-5283-4bdd-b9f1-8e2e5ced1018', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, ancestral_community).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, future_generations).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_hazard_awareness).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, intergenerational_risk_transmission).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, community_resilience_through_adherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere to the land-use prohibition, foregoing development in high-risk areas. They bear the cost of restricted land use but directly benefit from the safety and continued existence of their community, having learned from ancestral experience.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents, beneficiary).

% Established the land-use prohibition after catastrophic tsunamis, inscribing the rule on stone markers. Their legacy and wisdom are preserved through the community's continued adherence, ensuring the survival of their descendants.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, ancestral_community, agenda_setter,
    institutional, civilizational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__behavioral_competence_reading, ancestral_community, beneficiary).

% Inherit the safety and continued existence of the Aneyoshi community due to the land-use prohibition. They are bound by the physical hazard and the intergenerational commitment to the rule, benefiting from the foresight of their ancestors.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Would seek to develop land in the prohibited zones for economic gain, but are effectively excluded by the community's strong adherence to the ancestral rule and the clear, present danger of tsunami risk. Their proposals are not considered viable within the community's framework.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, economic_developers, excluded,
    powerful, immediate, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates land use and settlement patterns to align with the immutable physical reality of tsunami hazards, ensuring the long-term survival and safety of the Aneyoshi community.
% TRANSFER_FUNCTION: Transfers safety and community continuity across generations, from the adherence to the prohibition by current residents, who in turn bear the cost of restricted land use.
% ABSENT_VOICES: Economic developers or external interests who might advocate for building in the prohibited zones are effectively absent from the community's decision-making, their arguments foreclosed by the overwhelming historical and physical evidence of tsunami risk and the community's lived experience.
% DISAPPEARANCE_RATIONALE: If the prohibition and its social enforcement vanished overnight, it is highly probable that future generations, lacking direct memory or adherence, would build in vulnerable areas. This would lead to catastrophic loss of life and property during the next major tsunami, fundamentally altering the community's existence and potentially leading to its demise.
% FOUNDING_PROBLEM: Repeated catastrophic loss of life and community due to devastating tsunamis, leading to the realization that certain coastal areas were inherently unsafe for permanent settlement and required a permanent, intergenerational prohibition.
% FOUNDING_PROBLEM_CORROBORATION: Geological evidence of past tsunami inundation, historical records of the destruction of previous settlements, and the continued seismic activity in the region all corroborate the founding problem. The community's continued existence in a high-risk zone, sustained by this rule, further attests to its live status, as confirmed by disaster anthropologists and local historians.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain due to its grounding in immutable tsunami physics and its very low extractiveness (0.05) – the 'cost' of restricted land use is directly offset by the benefit of survival. Suppression (0.25) is present as social enforcement, but it aligns with a clear, existential threat, making it a necessary component of collective safety rather than coercive extraction. Theater ratio is negligible (0.05) as the rule's function is direct and vital. Accessibility collapse is high (0.9) because the physical hazard genuinely eliminates alternatives for safe settlement. Resistance is low (0.1) because the necessity of the rule is widely accepted within the community. The measurement series reflect a stable, consistently enforced constraint over the 78-year period.
 *
 * PERSPECTIVAL GAP:
 *   This 'behavioral competence' reading fundamentally diverges from the 'commemorative husk' reading. While the latter views the stone as a decayed symbol without active behavioral force, this reading asserts the stone's continued operational efficacy as a land-use rule. The engine's classification will highlight this divergence based on the distinct metric profiles and structural declarations of each reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are both payers (restricted land use) and beneficiaries (safety), resulting in a largely symmetric directionality. The ancestral community acts as the agenda-setter, having established the rule, and benefits from the preservation of their legacy. Future generations are clear beneficiaries, inheriting safety. Economic developers are excluded, as their interests conflict with the core purpose of the prohibition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fsm_natural_law_vs_construct,
    'Is the Aneyoshi land-use prohibition a genuine natural law (derived from tsunami physics) or a constructed constraint (social rule) that benefits identifiable agents (community survival)?',
    'Analysis of community decision-making processes: if the rule is actively debated and re-affirmed based on social consensus rather than solely on physical necessity, it leans towards a constructed constraint. If adherence is seen as an inevitable response to an external physical threat, it leans towards natural law.',
    'If primarily a constructed constraint, its classification as a Mountain would be a ''false summit,'' potentially reclassifying it as a Rope or Tangled Rope, depending on the degree of extraction and enforcement dynamics. If a genuine natural law, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_natural_law_vs_construct, conceptual, 'Ambiguity between natural law and socially constructed rule for the Aneyoshi prohibition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the social enforcement of the prohibition primarily structural (e.g., community norms, historical memory, collective vigilance) or internalized (e.g., individual belief in the necessity, identity fusion with ancestral wisdom)?',
    'Post-migration behavior analysis: if individuals who leave the community continue to adhere to similar safety practices in new contexts, it suggests internalized suppression. If adherence is strongly tied to community presence and social pressure, it suggests structural suppression.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as the adherence mechanism is carried by the individual. If purely structural, removing the community context might lead to a collapse of adherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the Aneyoshi land-use prohibition.').

omega_variable(
    conceptual_framing_ambiguity,
    'Is the Aneyoshi tsunami stone primarily a live, behaviorally enforced land-use rule (behavioral competence reading) or a historical memorial whose prohibition has decayed to a symbol (commemorative husk reading)?',
    'Empirical observation of land-use decisions and community discourse: if new construction proposals in prohibited zones are consistently rejected based on the stone''s authority, it supports the behavioral competence reading. If such proposals are debated without reference to the stone''s active authority, it supports the commemorative husk reading.',
    'If the behavioral competence reading is correct, the constraint is a Mountain (as authored). If the commemorative husk reading is correct, the constraint would likely be reclassified as a Piton (atrophied function, theatrical maintenance) or even a Rope (if it still serves a coordination function as a symbol).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_framing_ambiguity, conceptual, 'The core ambiguity between the behavioral competence and commemorative husk readings of the Aneyoshi tsunami stone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 13, 0.05).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 26, 0.05).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 39, 0.05).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 52, 0.05).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 65, 0.05).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 78, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aney_be_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 13, 0.05).
narrative_ontology:measurement(aney_be_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 26, 0.05).
narrative_ontology:measurement(aney_be_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 39, 0.05).
narrative_ontology:measurement(aney_be_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 52, 0.05).
narrative_ontology:measurement(aney_be_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 65, 0.05).
narrative_ontology:measurement(aney_be_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 78, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(aney_su_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 13, 0.25).
narrative_ontology:measurement(aney_su_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 26, 0.25).
narrative_ontology:measurement(aney_su_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 39, 0.25).
narrative_ontology:measurement(aney_su_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 52, 0.25).
narrative_ontology:measurement(aney_su_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 65, 0.25).
narrative_ontology:measurement(aney_su_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 78, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This story is one of two distinct readings of the 'aneyoshi_land_use_prohibition' kernel. This 'behavioral competence' reading emphasizes the stone as a live, enforced rule, while the 'commemorative husk' reading views it as a symbolic memorial. Their differing ε values and structural properties necessitate separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
