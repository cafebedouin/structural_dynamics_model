% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone Inscription as Live Land-Use Prohibition
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   A coastal village maintains a stone inscription marking the upper safe
 *   boundary for settlement decades after a catastrophic tsunami. The
 *   constraint is read here as behavioral_competence: daily spatial
 *   practiceâwalking, farming, buildingâactively enforces the
 *   prohibition, and the community accepts the economic cost of steep
 *   terrain. This is one reading of a contested kernel; the sibling reading
 *   (commemorative_husk) would treat the same stone as a decayed memorial
 *   lacking behavioral force. The authored metrics reflect a low-extraction
 *   coordination mechanism sustained by intergenerational practice.
 *
 * KEY AGENTS:
 *   - coastal_community_residents (organized/identity_locked) â net beneficiaries whose daily practice enforces the prohibition
 *   - prospective_lowland_users (moderate/mobile) â excluded voices who would prefer development in the prohibited zone
 *   - disaster_anthropology_researcher (analytical/analytical) â observes the institutional memory mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.18).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.12).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.18).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Inscription as Live Land-Use Prohibition").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, 'd7eeae59-ec87-4973-8ce4-574154be35ad').
narrative_ontology:cs_kernel_codification('d7eeae59-ec87-4973-8ce4-574154be35ad', fixed_text).
narrative_ontology:cs_authority_grounding('d7eeae59-ec87-4973-8ce4-574154be35ad', lineage).
narrative_ontology:cs_interpretation_layer_present('d7eeae59-ec87-4973-8ce4-574154be35ad').
narrative_ontology:cs_reading_relation('d7eeae59-ec87-4973-8ce4-574154be35ad', stone_land_use_rule__commemorative_husk, forecloses).
narrative_ontology:cs_axiom('d7eeae59-ec87-4973-8ce4-574154be35ad', foundational, stone_inscription_constitutes_binding_prohibition).
narrative_ontology:cs_axiom_status(stone_inscription_constitutes_binding_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('d7eeae59-ec87-4973-8ce4-574154be35ad', stone_inscription_constitutes_binding_prohibition, conventional).
narrative_ontology:cs_axiom('d7eeae59-ec87-4973-8ce4-574154be35ad', secondary, spatial_habit_is_active_enforcement).
narrative_ontology:cs_axiom_status(spatial_habit_is_active_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('d7eeae59-ec87-4973-8ce4-574154be35ad', spatial_habit_is_active_enforcement, conventional).
narrative_ontology:cs_reference_frame('d7eeae59-ec87-4973-8ce4-574154be35ad', ancestor_mandated_upland_settlement).
narrative_ontology:cs_drift_state('d7eeae59-ec87-4973-8ce4-574154be35ad', contemporary_development_pressure_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d7eeae59-ec87-4973-8ce4-574154be35ad', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, coastal_community_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inhabit a coastal village where a stone inscription marks the upper safe boundary for settlement. Daily movementâfarming, fetching water, visiting neighborsâhabitually respects the line. Building or cultivating below the stone is unthinkable because the spatial pattern is woven into bodily memory, kinship narrative, and intergenerational identity. The community pays the economic cost of steep terrain as a taken-for-granted exchange for safety.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, coastal_community_residents, beneficiary,
    organized, generational, identity_locked, local).

% Would prefer to build or cultivate in the flat, fertile lowland below the stone, but are absent from the intergenerational practice that sustains the prohibition. Their economic logic favors the lowland; their exclusion from the village's normative conversation keeps the zone vacant.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, prospective_lowland_users, excluded,
    moderate, biographical, mobile, local).

% Studies how the village maintains disaster avoidance through inscribed stones and embodied practice. Documents settlement patterns, interviews residents, and compares this case with communities where similar markers have decayed into mere memorials. Does not participate in the land-use rule itself.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_anthropology_researcher, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates safe settlement location across generations by anchoring a land-use boundary in a persistent physical marker and weaving compliance into routine movement and construction practice, solving the collective-action problem of resisting short-term economic temptation to occupy hazardous lowland.
% TRANSFER_FUNCTION: Moves the community's habitation and daily activity from flat, desirable lowland to steep, safe upland; the transfer is of risk-avoidance paid for through accepted physical inconvenience and reduced agricultural convenience.
% ABSENT_VOICES: Prospective lowland developers, younger residents who might prefer easier terrain, and municipal planners who view the lowland as untapped real estate are not present in the intergenerational conversation that reproduces the stone's authority.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished, houses and fields would migrate to the flat lowland within a generation, the steep climb would be abandoned as unnecessary labor, and the settlement pattern would reorganize around immediate convenienceâreaccumulating catastrophic risk.
% FOUNDING_PROBLEM: A prior tsunami or catastrophic flood destroyed lowland settlement, creating the need for a persistent rule that would survive the decay of personal memory and formal institutions.
% FOUNDING_PROBLEM_CORROBORATION: Geological evidence of past inundation and independent disaster-risk assessments corroborate the hazard; oral histories collected by outside researchers confirm the founding event, attesting from outside the beneficiary community that the problem the stone addresses remains real.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the arrangement moves the community to safety without a capturer; suppression is low (0.12) because enforcement is woven into daily practice rather than imposed by an apparatus; theater is minimal (0.08) because the spatial pattern is functionally live. Accessibility collapse is moderate (0.45): once the stone's rule is known and embodied, building in the lowland becomes practically unthinkable, but alternatives exist in theory. Resistance is minimal (0.08) across 78 years of sustained compliance. The measurement series tracks a slight drift upward in extractiveness and theater as modernization pressure accumulates, but the constraint remains structurally a rope.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (coastal residents) experiences the constraint as identity-fused safety practice; the excluded seat (prospective lowland users) would experience it as a barrier to economic opportunity. The engine computes this divergence from identical structural data through directionality: residents are structurally coordinated, while excluded developers are structurally barred.
 *
 * DIRECTIONALITY LOGIC:
 *   The community residents are beneficiaries of the coordination (d near the beneficiary end); their identity-locked exit reflects fusion with the place and its practice. Prospective lowland users are effectively targets (d near the target end) because the constraint bars them from the economic use they would otherwise make of the flatland, though they are not present in the governance conversation. The observer seat sits at analytical scope with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophic flood risk) is corroborated as live by outside geological and disaster-management sources, preventing a mandatrophy mislabel. The constraint persists because the problem persists, not because an agenda setter extracts from it. Were the risk to disappear while the prohibition remained, the constraint would drift toward piton or husk; the measurement series would show rising theater_ratio and extractiveness without live coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the stone currently function as a live land-use prohibition (behavioral_competence) or as a decayed memorial without behavioral force (commemorative_husk)?',
    'Longitudinal ethnographic study comparing spatial practice with stated norms across age cohorts, plus observation of actual construction decisions near the boundary.',
    'If the practice is inert habit without normative binding, this constraint dissolves and the sibling reading becomes the correct classification; if binding, this reading remains valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between live prohibition and memorial husk readings of the same kernel.').

omega_variable(
    economic_pressure_threshold,
    'At what level of economic incentive (flatland development value, population pressure) does the accepted cost of steep-hill compliance become unacceptable?',
    'Natural experiment or policy change that alters land value; observation of compliance rates and normative articulation under increased pressure.',
    'If compliance breaks under moderate pressure, the constraint is weaker than a rope and may require active enforcement to persist; if it holds, the rope is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_pressure_threshold, empirical, 'Threshold at which economic pressure overwhelms the coordination benefit.').

omega_variable(
    enforcement_mechanism_diffusion,
    'Is compliance maintained purely by diffuse daily practice, or is there a latent sanction apparatus (social shunning, elder correction) that acts as an invisible enforcement mechanism?',
    'Fine-grained ethnographic observation of normative breaches and community response, including interviews about perceived consequences of violation.',
    'If a latent apparatus exists, suppression is higher than authored and the constraint edges toward tangled_rope; if pure practice, it remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_diffusion, empirical, 'Whether enforcement is truly diffuse or hidden apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ston_tr_t13, stone_land_use_rule__behavioral_competence, theater_ratio, 13, 0.05).
narrative_ontology:measurement(ston_tr_t26, stone_land_use_rule__behavioral_competence, theater_ratio, 26, 0.06).
narrative_ontology:measurement(ston_tr_t39, stone_land_use_rule__behavioral_competence, theater_ratio, 39, 0.06).
narrative_ontology:measurement(ston_tr_t52, stone_land_use_rule__behavioral_competence, theater_ratio, 52, 0.07).
narrative_ontology:measurement(ston_tr_t65, stone_land_use_rule__behavioral_competence, theater_ratio, 65, 0.07).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.08).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ston_be_t13, stone_land_use_rule__behavioral_competence, base_extractiveness, 13, 0.15).
narrative_ontology:measurement(ston_be_t26, stone_land_use_rule__behavioral_competence, base_extractiveness, 26, 0.16).
narrative_ontology:measurement(ston_be_t39, stone_land_use_rule__behavioral_competence, base_extractiveness, 39, 0.16).
narrative_ontology:measurement(ston_be_t52, stone_land_use_rule__behavioral_competence, base_extractiveness, 52, 0.17).
narrative_ontology:measurement(ston_be_t65, stone_land_use_rule__behavioral_competence, base_extractiveness, 65, 0.17).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__behavioral_competence, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, commemorative_husk).

% DUAL FORMULATION NOTE:
% The stone_land_use_rule kernel decomposes into two structurally distinct constraints: behavioral_competence (this file), which reads the stone as a live prohibition enforced by daily practice, and commemorative_husk, which reads it as a decayed memorial without behavioral force. They share the physical artifact but differ in epsilon, beneficiary structure, and functional status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
