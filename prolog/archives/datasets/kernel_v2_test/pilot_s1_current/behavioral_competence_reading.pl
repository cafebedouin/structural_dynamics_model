% ============================================================================
% CONSTRAINT STORY: behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_competence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: behavioral_competence_reading
 *   human_readable: Aneyoshi Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/tsunami_mitigation
 *
 * SUMMARY:
 *   The aneyoshi stone prohibition (behavioral competence reading): the stone
 *   marks a land-use boundary established after the 1896 Meiji tsunami. The
 *   boundary was operationally enforced through 78 years of consistent
 *   land-use practice, with no recorded violations and no formal written law.
 *   The village maintained the boundary across generational turnover, two
 *   world wars, and post-war development pressure. Under the behavioral
 *   competence reading, this constraint is a solution to a specific
 *   institutional problem: how to encode and transmit knowledge of a
 *   low-probability, high-consequence natural hazard (tsunami recurrence at
 *   ~100-150 year intervals) in a form that persists across individual
 *   lifespans and generational memory. The stone itself is not a physical
 *   barrier — it is a behavioral marker. Compliance is costless (building
 *   elsewhere does not reduce resident welfare), and no party collects
 *   extraction from the boundary. The constraint's 78-year persistence
 *   reflects not coercive suppression but alignment between the rule and
 *   ecological reality: building past the stone exposes structures to tsunami
 *   risk. The behavioral competence reading interprets the stone's functional
 *   logic as: embody knowledge in a persistent physical marker → encode the
 *   marker in cultural practice → practice transmits knowledge across
 *   generations without requiring centralized enforcement or written codes.
 *
 * KEY AGENTS:
 *   - Village Community: Organized collective (organized/mobile) — practices the boundary through cultural transmission; benefits from distributed knowledge preservation
 *   - Individual Resident: Moderate agent (moderate/constrained) — faces low-cost compliance; benefits from tsunami-risk avoidance; constrained by property boundaries and development opportunities
 *   - Tsunami Physics (The Environment): Non-agent entity — the true cost-bearer of violations; the constraint does not extract from residents, it prevents residents from being harmed by the environment
 *   - Analytical Observer: Universal perspective (powerful/analytical) — sees the constraint as a coordination mechanism that solves the problem of knowledge persistence across generational timescales
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_competence_reading, 0.08).
domain_priors:suppression_score(behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(behavioral_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(behavioral_competence_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_competence_reading, rope).
narrative_ontology:human_readable(behavioral_competence_reading, "Aneyoshi Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(behavioral_competence_reading, "disaster_anthropology/institutional_memory/tsunami_mitigation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(behavioral_competence_reading, 'e005e429-4f57-4fcd-85c1-1ab402dc562a').
narrative_ontology:cs_kernel_codification('e005e429-4f57-4fcd-85c1-1ab402dc562a', distributed).
narrative_ontology:cs_authority_grounding('e005e429-4f57-4fcd-85c1-1ab402dc562a', practice).
narrative_ontology:cs_interpretation_layer_present('e005e429-4f57-4fcd-85c1-1ab402dc562a').
narrative_ontology:cs_reading_relation('e005e429-4f57-4fcd-85c1-1ab402dc562a', behavioral_competence_reading__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('e005e429-4f57-4fcd-85c1-1ab402dc562a', foundational, behavioral_knowledge_persistence).
narrative_ontology:cs_axiom_status(behavioral_knowledge_persistence, holdable).
narrative_ontology:cs_axiom_grounding('e005e429-4f57-4fcd-85c1-1ab402dc562a', behavioral_knowledge_persistence, empirically_contingent).
narrative_ontology:cs_axiom('e005e429-4f57-4fcd-85c1-1ab402dc562a', foundational, coordination_without_extraction).
narrative_ontology:cs_axiom_status(coordination_without_extraction, holdable).
narrative_ontology:cs_axiom_grounding('e005e429-4f57-4fcd-85c1-1ab402dc562a', coordination_without_extraction, instrumental).
narrative_ontology:cs_reference_frame('e005e429-4f57-4fcd-85c1-1ab402dc562a', knowledge_encoding_in_practice).
narrative_ontology:cs_drift_state('e005e429-4f57-4fcd-85c1-1ab402dc562a', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('e005e429-4f57-4fcd-85c1-1ab402dc562a', '2026-02-26T14:23:18Z').
narrative_ontology:cs_kernel_id(behavioral_competence_reading, aneyoshi_land_use_prohibition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE COMMUNITY — ROPE (Behavioral Competence) — The stone's prohibition solves a genuine coordination problem: how to maintain behavioral memory across generational discontinuity. The practice of observing the stone-marked boundary is low-cost, benefits all residents equally, and requires no asymmetric extraction. The binding mechanism is cultural transmission, not coercion. Exit is possible but unnecessary — the rule aligns with residents' actual survival interests.
constraint_indexing:constraint_classification(behavioral_competence_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: INDIVIDUAL RESIDENT — ROPE (Behavioral Competence) — The prohibition is encoded in practice (not in written law or enforced punishment). Violating it is costly only in the sense that it exposes the resident to tsunami risk — the true cost-bearer is the environment, not a human enforcer. The resident is not paying a fine to a beneficiary; they are losing protection. Extraction is negligible because no actor collects from compliance.
constraint_indexing:constraint_classification(behavioral_competence_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER — ROPE (Behavioral Competence) — From a universal perspective, the constraint is a solution to an asymmetric information problem: how a community encodes knowledge of low-probability, high-consequence events (tsunamis occurring at ~100-150 year intervals) such that the knowledge persists across the lifespan of any individual or generation. The stone is a behavioral mechanism that eliminates the need for centralized enforcement or written codes. The constraint's persistence across 78 years reflects not coercive suppression but alignment with ecological fact.
constraint_indexing:constraint_classification(behavioral_competence_reading, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_competence_reading_tests).
:- end_tests(behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The behavioral competence reading assumes no beneficiary structure — no party collects from the boundary. The low extractiveness value reflects that the constraint's function is alignment with tsunami risk, not transfer of value. Suppression (0.12): Low. The prohibition is maintained through cultural practice and internalized understanding, not enforcement machinery. Residents are not coerced into compliance; they choose to avoid the hazard zone because they understand (or inherit understanding of) the risk. Theater ratio (0.15): Low. The behavioral competence reading interprets the stone and boundary as functional devices for knowledge transmission, not as performance of tradition. The theater measurement reflects minor degradation over 78 years as direct memory of the 1896 tsunami fades, but the functional logic (avoid the zone because tsunami risk) persists.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives (village community, individual resident, analytical observer) arrive at the same classification (Rope) and low extractiveness values under the behavioral competence reading. This uniformity reflects that the reading asserts a genuine coordination problem (knowledge persistence) with genuine solution (behavioral marker → cultural practice → generational transmission) and no asymmetric extraction. The perspectival gap exists not within this reading but BETWEEN readings: the commemorative husk reading would produce Piton classification with higher theater_ratio (ε ≈ 0.15, theater_ratio ≈ 0.70+), emphasizing the ritual and commemorative dimensions at the expense of the functional coordination logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the behavioral competence reading, directionality (d) is not derived from beneficiary/victim declarations because no actor is identified as beneficiary or victim. The constraint is not extractive — residents comply because it aligns with their interests (avoid tsunami risk). The analytical observer's perspective uses d ≈ 0.5 (symmetric, neither benefiting nor paying through the constraint itself) because the observer is analyzing the constraint's structural function rather than experiencing its costs or benefits. Beneficiary/victim derivation would apply to the commemorative husk reading (where beneficiaries might include scholars, cultural heritage institutions, or commemorative practice communities), but is absent from the behavioral competence reading.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNELS AND READINGS: This story instantiates ONE reading of a contested kernel (aneyoshi_land_use_prohibition). The behavioral competence reading claims the stone's prohibition is a coordination mechanism encoding knowledge of tsunami risk. The sibling reading (commemorative_husk_reading) claims the prohibition persists as ritual and performance after the functional justification has atrophied. The two readings do not foreclose each other — they coexist in the empirical community's actual practice: residents cite both functional tsunami-risk reasoning AND ancestral/commemorative motivation for maintaining the boundary. The readings differ at the axiom level (behavioral_knowledge_persistence vs. ritual_memory_continuation) and will exhibit different drift profiles if measured over longer timescales. Mandatrophy does not apply to this constraint because the behavioral competence reading asserts no mandate that has outlived its function — the function (tsunami-hazard knowledge persistence) is live and ongoing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the aneyoshi prohibition a behavioral coordination mechanism that survives because it works (behavioral competence reading), or a commemorative vestige maintained by tradition even though the original survival function has become decorative (commemorative husk reading)?',
    'Observational test: measure compliance cost to residents. If compliance carries asymmetric extraction cost (residents pay, beneficiary collects), drift toward snare/tangled_rope classification. If compliance is costless practice (residents follow because it aligns with genuine safety interest), behavioral competence reading holds. Secondary test: what happens if enforcement capacity erodes? Behavioral competence frame predicts compliance persists due to internalized practice; husk frame predicts compliance collapses when enforcement (or commemorative performance) ceases.',
    'Behavioral competence reading: constraint classifies as Rope with minimal extractiveness (ε ≈ 0.08). Commemorative husk reading: constraint reclassifies as Piton with high theater_ratio (ε ≈ 0.15, theater_ratio ≈ 0.70+). The two readings diverge at the structural function level — coordination that works vs. performance of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether the prohibition functions as behavioral coordination or commemorative vestige').

omega_variable(
    suppression_naturality_boundary,
    'What portion of observed compliance is suppression (internalized prohibition, social shame, fear of violation) versus alignment (residents understand tsunami risk and choose the boundary placement)?',
    'Interview-based study: ask residents WHY they avoid building past the stone. If answers emphasize tsunami risk prediction, behavioral competence reading is supported (suppression ≈ 0.05-0.15). If answers emphasize tradition, taboo, or ancestor mandate without reference to tsunami mechanics, husk reading is supported (suppression ≈ 0.25-0.40).',
    'If suppression is primarily alignment with ecological fact, extractiveness drops (ε ≈ 0.05). If suppression is internalized taboo, extractiveness rises and the constraint may reclassify toward Snare from the individual resident perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_naturality_boundary, empirical, 'Ratio of suppression (internalized prohibition) to alignment (resident understanding of tsunami risk)').

omega_variable(
    generational_knowledge_loss_rate,
    'How does the stone''s effectiveness at transmitting tsunami-hazard knowledge vary with generational distance from the last observed tsunami?',
    'Longitudinal ethnographic study tracking resident knowledge of tsunami risk across generations (t=0 being the 1896 tsunami, through 78-year interval, to present). Measure knowledge retention: does understanding of why the stone exists persist, or does the reason become forgotten while the practice remains? Does knowledge loss trigger classification drift from Rope to Piton?',
    'If knowledge persists: behavioral competence frame supported, Rope classification holds. If knowledge erodes while practice continues: transition from Rope (early interval) to Piton (late interval) observed in measurements, supporting the temporal drift that mandatrophy analysis must track.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_knowledge_loss_rate, empirical, 'Rate of knowledge loss across generational distance from the 1896 tsunami').

omega_variable(
    committer_frame_sibling_distinction,
    'This reading (behavioral competence) claims the stone''s prohibition is a coordination mechanism that works because it aligns with physical reality (tsunami risk). The sibling reading (commemorative husk) claims the prohibition persists as performance of tradition even though its functional justification has atrophied. What structural evidence would distinguish these readings at the kernel level (aneyoshi_land_use_prohibition)?',
    'The kernel ''aneyoshi_land_use_prohibition'' is contested at the level of whether the core mechanism is behavioral coordination or commemorative performance. This is a case of coexisting readings: different parties genuinely hold both interpretations (behavioral competence held by residents who cite tsunami risk; commemorative husk held by scholars who emphasize the ritual dimension). The test is not to eliminate one reading but to measure how the kernel''s reference frame and drift state differ when interpreted through each reading''s axioms.',
    'Behavioral competence reading: reference frame is ''knowledge_of_tsunami_risk_encoded_in_practice''; drift_state shows stable alignment (practice persists because it matches physical reality). Commemorative husk reading: reference frame is ''ancestral_mandate_through_ritual''; drift_state shows practice_drift (ritual decouples from original function). Both readings coexist in the empirical community''s actual practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_sibling_distinction, conceptual, 'Structural distinction between behavioral competence and commemorative husk readings at kernel level').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(behav_comp_theater_1896, behavioral_competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(behav_comp_theater_1916, behavioral_competence_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(behav_comp_theater_1936, behavioral_competence_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(behav_comp_theater_1956, behavioral_competence_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(behav_comp_theater_1974, behavioral_competence_reading, theater_ratio, 78, 0.15).

% Extraction over time
narrative_ontology:measurement(behav_comp_extract_1896, behavioral_competence_reading, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(behav_comp_extract_1916, behavioral_competence_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(behav_comp_extract_1936, behavioral_competence_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(behav_comp_extract_1956, behavioral_competence_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(behav_comp_extract_1974, behavioral_competence_reading, base_extractiveness, 78, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(behav_comp_suppress_1896, behavioral_competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(behav_comp_suppress_1916, behavioral_competence_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(behav_comp_suppress_1936, behavioral_competence_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(behav_comp_suppress_1956, behavioral_competence_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement(behav_comp_suppress_1974, behavioral_competence_reading, suppression_requirement, 78, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(behavioral_competence_reading, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi prohibition kernel decomposes into two constraint stories corresponding to two distinct readings: behavioral_competence_reading (this file) interprets the stone as a coordination mechanism for knowledge persistence; commemorative_husk_reading interprets the same prohibition as ritualized performance after functional justification has faded. The two readings have different ε values (behavioral competence: ~0.08; commemorative husk: ~0.15+), different primary type classifications (Rope vs. Piton), and different beneficiary/victim structures (none vs. potential ritual beneficiaries). They are linked as siblings under a common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
