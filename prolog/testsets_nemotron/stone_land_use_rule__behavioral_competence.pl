% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone Marker Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   In a Japanese mountain valley, a stone marker placed at the 1945 debris
 *   flow's maximum extent bears the inscription: 'Do not build below this
 *   stone.' For 78 years, the community has maintained zero settlement on the
 *   alluvial fan — not through zoning enforcement, but through daily spatial
 *   practice. Residents route their lives around the line: homes on steep
 *   slopes, fields on safe ground, children taught the boundary by walking it
 *   with elders. The municipal planning authority defers to this practice.
 *   The constraint is claimed as mountain (natural law of the terrain) but
 *   beneficiaries exist (the community, future generations), triggering FSM
 *   evaluation. The epsilon is low (0.12) — the cost is the steep hill climb,
 *   not extraction.
 *
 * KEY AGENTS:
 *   - disaster_affected_community: Primary beneficiary (organized/identity_locked) — sustains the constraint through embodied practice
 *   - future_generations: Primary beneficiary (powerless/trapped) — inherits protection without voice
 *   - municipal_planning_authority: Agenda setter (institutional/constrained) — could override but defers
 *   - disaster_researchers: Observer (analytical/analytical) — documents the anomaly
 *   - developers_outsiders: Excluded (moderate/mobile) — would build if permitted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.12).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.15).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.12).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, mountain).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Marker Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(stone_land_use_rule__behavioral_competence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, '48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f').
narrative_ontology:cs_kernel_codification('48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f', fixed_text).
narrative_ontology:cs_authority_grounding('48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f', lineage).
narrative_ontology:cs_interpretation_layer_present('48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f').
narrative_ontology:cs_reading_relation('48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f', stone_land_use_rule__commemorative_husk, coexists_with).
narrative_ontology:cs_axiom('48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f', foundational, embodied_practice_constitutes_constraint).
narrative_ontology:cs_axiom_status(embodied_practice_constitutes_constraint, holdable).
narrative_ontology:cs_axiom_grounding('48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f', embodied_practice_constitutes_constraint, empirically_contingent).
narrative_ontology:cs_axiom('48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f', foundational, intergenerational_transmission_requires_daily_enactment).
narrative_ontology:cs_axiom_status(intergenerational_transmission_requires_daily_enactment, holdable).
narrative_ontology:cs_axiom_grounding('48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f', intergenerational_transmission_requires_daily_enactment, empirically_contingent).
narrative_ontology:cs_reference_frame('48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f', survivor_vow_1945).
narrative_ontology:cs_drift_state('48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('48a8c1fb-22e9-4bf6-bfe0-803fb6ad906f', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, disaster_affected_community).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, future_generations).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, embodied_institutional_memory_prevents_repeat_casualty).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, spatial_practice_as_living_archive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of the valley who lost family and homes in the 1945 debris flow. They maintain the stone's prohibition through daily routing decisions — avoiding the alluvial fan for housing, farming only the safe slopes, teaching children the boundary by walking it. The constraint is not external to them; it is constituted in their collective muscle memory. Leaving the valley is possible but means abandoning the ancestral commitment that defines their relationship to the land.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_affected_community, beneficiary,
    organized, generational, identity_locked, local).

% Those not yet born who inherit the constraint's protection. They have no voice in its maintenance but are its ultimate beneficiaries — the prohibition prevents them from being placed in the debris flow's path. Their exclusion is structural: they cannot consent to or contest the arrangement that saves their lives.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, future_generations, beneficiary,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, future_generations, excluded).

% The formal land-use regulator that could override the prohibition through zoning changes or development permits. In practice, it defers to the community's spatial practice because the stone's authority is recognized as superior to bureaucratic process. The authority's exit from deference would require political will to confront a 78-year unbroken compliance record.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, municipal_planning_authority, agenda_setter,
    institutional, biographical, constrained, regional).

% Anthropologists, geologists, and disaster scholars who study the stone as a rare case of sustained behavioral compliance without state enforcement. They document the constraint's operation but do not participate in its maintenance. Their presence adds external validation but no coercive force.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_researchers, observer,
    analytical, generational, analytical, global).

% External actors who would build on the alluvial fan if permitted. They are structurally excluded — the community's daily practice and the planning authority's de facto recognition create a barrier they cannot overcome without breaking the constraint. Their exclusion is not by identity but by the constraint's operational success.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, developers_outsiders, excluded,
    moderate, immediate, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents settlement on a known debris-flow alluvial fan by encoding the hazard boundary into daily spatial practice. The stone marks the line; the community's routine movements — where they build, farm, walk, teach children — enact the prohibition continuously. No periodic decision or enforcement action is needed; the constraint lives in the muscle memory of the valley's inhabitants.
% TRANSFER_FUNCTION: Transfers the cost of foregone development on the alluvial fan (prime flat land, water access) from the community to the constraint itself. The community accepts steeper, less convenient building sites on the valley slopes — a persistent economic cost — in exchange for the certainty that no one sleeps in the flow path. The transfer is from individual economic optimization to collective intergenerational survival.
% ABSENT_VOICES: Future generations are the primary absent voice — they bear the benefit but cannot participate in the practice that sustains it. Developers and speculators who would challenge the prohibition are excluded by the constraint's operational success, not by a formal ban. The municipal authority's potential dissent (if it ever chose to override) is also absent, held in check by the constraint's moral weight.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight — the stone remained but the community's spatial practice dissolved — the alluvial fan would be developed within a decade. The next debris flow (geologically certain on a 50-100 year cycle) would kill residents living in the flow path. The world rearranges catastrophically: the constraint is the only thing preventing a repeat of 1945.
% FOUNDING_PROBLEM: The 1945 debris flow killed 68 people in a settlement built on the alluvial fan. Survivors placed the stone at the flow's maximum extent and vowed: no one sleeps below this line. The founding problem was not 'how to regulate land use' but 'how to ensure our children never die this way again' — a problem of intergenerational transmission, not bureaucratic coordination.
% FOUNDING_PROBLEM_CORROBORATION: The 1945 event is documented in prefectural disaster records, geological surveys of the fan stratigraphy, and the oral testimony of three surviving witnesses (as of 2020). The community's unbroken compliance is attested by the planning authority's own land-use maps showing zero residential permits issued for the fan since 1945. No corroboration comes from beneficiaries alone — the geological and administrative records are independent.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, ExtMetricName, E),
    domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(stone_land_use_rule__behavioral_competence),
    narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the economic cost (foregone flat land) is the price of survival, not a transfer to a beneficiary. Suppression is negligible (0.15) — no active enforcement prevents development; the community's own practice is the barrier. Theater ratio is near zero (0.08) — the practice is functional, not performative. Accessibility collapse is high (0.87) — once you understand the stone, the alternative (building on the fan) is unthinkable, not merely difficult. Resistance is near zero (0.05) — no one contests the prohibition from within. The metrics describe a constraint that operates like a natural law but is sustained by social practice.
 *
 * PERSPECTIVAL GAP:
 *   From the community's seat (identity_locked, generational horizon), the constraint is a mountain — it is the terrain itself made visible. From the municipal authority's seat (institutional, biographical), it is a rope they voluntarily defer to — coordination without coercion. From developers' seat (mobile, immediate), it appears as a snare — a barrier to profit with no formal legal basis. The engine computes these divergences from the structural data; the claimed_type (mountain) reflects the community's lived reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The disaster_affected_community is both beneficiary and agenda_setter — they sustain the constraint and collect its protection. Directionality d is near 0.0 (full beneficiary) because the constraint subsidizes their survival; the economic cost is self-imposed as the price of that subsidy. Future_generations are pure beneficiaries (d = 0.0, trapped). Municipal_authority has d ≈ 0.3 — they bear administrative friction but gain legitimacy from deference. Developers have d ≈ 0.9 — they would extract value from the fan but are blocked. The identity_locked exit of the community is key: their self-concept is fused with the constraint; exit means ceasing to be who they are.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing repeat casualty) remains live — the geological hazard has not diminished. The constraint has not outlived its function; its mandate is renewed with each generation that walks the boundary. No mandatrophy exists. The low epsilon is not atrophy but efficiency: the constraint achieves its purpose with minimal coercive overhead because the practice is internalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_prohibition,
    'Is the stone''s prohibition a genuine natural-law constraint (the fan''s geology makes settlement physically impossible to survive) or a constructed social prohibition that happens to align with geology?',
    'Counterfactual test: if the community''s spatial practice were erased but the geology remained, would the prohibition persist? If yes, natural law; if no, constructed. Historical test: did the 1945 survivors have a choice to resettle the fan with engineering mitigation, and did they reject it?',
    'If natural law, the constraint is a Mountain with epsilon near zero regardless of beneficiaries. If constructed, the sustained compliance is a social achievement with non-zero epsilon (the economic cost of foregone development), and FSM evaluation applies — the ''naturalness'' framing may conceal a coordination cost the community bears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_prohibition, conceptual, 'Whether the prohibition''s authority derives from geology or from the community''s sustained practice.').

omega_variable(
    commemorative_husk_divergence,
    'At what point does behavioral competence degrade into commemorative husk — and has this reading already crossed that threshold?',
    'Longitudinal measurement of compliance markers: (a) new construction permits on the fan, (b) agricultural intensification on the fan, (c) transmission fidelity — do children still learn the boundary by walking it, or only by being told? A decline in (c) while (a) and (b) remain zero signals the transition.',
    'If the constraint is already a commemorative husk, its epsilon is rising (the community pays the cost of maintaining the practice without the behavioral guarantee), and the claimed_type should shift toward piton or snare. The behavioral_competence reading asserts the constraint remains live; the commemorative_husk sibling reads it as hollow. This omega names the boundary between them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commemorative_husk_divergence, empirical, 'Whether the constraint''s behavioral force has already decayed to symbolic gesture.').

omega_variable(
    kernel_reading_identity,
    'Does the behavioral_competence reading foreclose, coexist with, or influence the commemorative_husk reading?',
    'Structural analysis: if behavioral competence requires the constraint to have active behavioral force, and commemorative_husk asserts that force has decayed to zero, can a single framework hold both? Or do different parties hold them simultaneously (community vs. outside observers)?',
    'Determines the cs_structure.reading_relations entry. If forecloses, the readings are mutually exclusive in any one commitment framework. If coexists_with, the community holds behavioral_competence while researchers hold commemorative_husk. If influences, the behavioral reading''s demonstrated success creates pressure on the commemorative reading to acknowledge residual force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship between the two declared readings of the stone_land_use_rule kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 1945, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_tr_t1945, stone_land_use_rule__behavioral_competence, theater_ratio, 1945, 0.02).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_tr_t1965, stone_land_use_rule__behavioral_competence, theater_ratio, 1965, 0.03).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_tr_t1985, stone_land_use_rule__behavioral_competence, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_tr_t2003, stone_land_use_rule__behavioral_competence, theater_ratio, 2003, 0.07).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_tr_t2023, stone_land_use_rule__behavioral_competence, theater_ratio, 2023, 0.08).

% Extraction over time
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_be_t1945, stone_land_use_rule__behavioral_competence, base_extractiveness, 1945, 0.08).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_be_t1965, stone_land_use_rule__behavioral_competence, base_extractiveness, 1965, 0.1).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_be_t1985, stone_land_use_rule__behavioral_competence, base_extractiveness, 1985, 0.11).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_be_t2003, stone_land_use_rule__behavioral_competence, base_extractiveness, 2003, 0.12).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_be_t2023, stone_land_use_rule__behavioral_competence, base_extractiveness, 2023, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_su_t1945, stone_land_use_rule__behavioral_competence, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_su_t1965, stone_land_use_rule__behavioral_competence, suppression_requirement, 1965, 0.12).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_su_t1985, stone_land_use_rule__behavioral_competence, suppression_requirement, 1985, 0.14).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_su_t2003, stone_land_use_rule__behavioral_competence, suppression_requirement, 2003, 0.15).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_su_t2023, stone_land_use_rule__behavioral_competence, suppression_requirement, 2023, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__behavioral_competence, 0.06).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% This constraint and its sibling commemorative_husk are two readings of the same kernel (stone_land_use_rule). They differ in epsilon (0.12 vs. estimated 0.35+ for the husk reading, where maintenance cost persists but behavioral force has decayed), in theater ratio (0.08 vs. >0.5), and in the identity_locked vs. constrained exit options for the community. The behavioral_competence reading claims the constraint remains a Mountain; the commemorative_husk reading classifies it as Piton or Snare. Both cannot be true of the same operational reality — the kernel's ε-invariance requires they be separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stone_land_use_rule__behavioral_competence, organized, 0.05).
constraint_indexing:directionality_override(stone_land_use_rule__behavioral_competence, powerless, 0.0).
constraint_indexing:directionality_override(stone_land_use_rule__behavioral_competence, institutional, 0.25).
constraint_indexing:directionality_override(stone_land_use_rule__behavioral_competence, moderate, 0.85).
constraint_indexing:directionality_override(stone_land_use_rule__behavioral_competence, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
