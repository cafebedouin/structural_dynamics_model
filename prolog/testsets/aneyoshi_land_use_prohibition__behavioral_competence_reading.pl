% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_behavioral_competence, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Prohibition: Behavioral Competence Reading
 *   domain: disaster_anthropology/commitment_systems/land_use_governance
 *
 * SUMMARY:
 *   Aneyoshi is a 340-resident fishing village in Iwate Prefecture, Japan,
 *   situated 144 meters above the Pacific Ocean on a hillside marked by a
 *   1.5-meter stone monument erected in 1933. The stone, inscribed '津波記念碑'
 *   (Tsunami Memorial Stone), marks the high-water line of the 1933 Showa
 *   tsunami and carries the directive: 'Do not build below this stone.' For
 *   78 years (1933–2011), Aneyoshi residents maintained settlement
 *   exclusively above this marker despite economic pressure to develop lower
 *   land. In March 2011, the 9.0-magnitude Great East Japan Earthquake
 *   triggered a tsunami. Aneyoshi, sheltered by its adherence to the stone
 *   directive, experienced zero deaths. Neighboring Otsuchi Township, 25 km
 *   away, with settlements on lower terrain, suffered 1,184 deaths—the
 *   highest death toll of any municipality in the disaster. This constraint
 *   models Aneyoshi's stone as a live behavioral commitment—a directive that
 *   operationally determines settlement patterns and demonstrably saves
 *   lives. The constraint is resolved through this BEHAVIORAL COMPETENCE
 *   READING, which treats the stone as a causally efficacious coordination
 *   mechanism. A sibling reading, the COMMEMORATIVE HUSK reading, treats the
 *   same artifact as primarily a memorial to ancestors—ritual honoring rather
 *   than operative behavioral directive. Both readings parse the same
 *   physical artifact and social practice; they differ in how they account
 *   for the binding mechanism.
 *
 * KEY AGENTS:
 *   - Aneyoshi residents (powerless/constrained): Primary actors who enact the settlement prohibition. Benefit from hazard avoidance coordination; constrained by economic opportunity cost of not developing lower land. Internalize the directive across 78-year non-catastrophe interval.
 *   - Younger generation (moderate/mobile): Demographic cohort with geographic mobility and modern economic opportunities; experience slight extractive pressure from the prohibition (inconvenience of distance to shore/commerce) but internalize hazard-avoidance coordination.
 *   - Disaster-resilience institutions (institutional/arbitrage): National frameworks for disaster mitigation and community-based adaptation. Benefit from Aneyoshi's behavioral persistence as proof-of-concept for long-interval hazard encoding.
 *   - Ancestral authority lineage (institutional/implicit): The 1933 settlers and their kin transmit the stone directive across generations. Not explicitly organized but functionally operative through kinship and community practice.
 *   - Otsuchi Township (comparative victim): Neighboring municipality lacking equivalent ancestral directive or stone marker. Settlement on lower terrain results in 1,184 deaths in 2011, providing stark empirical contrast validating Aneyoshi's commitment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.02).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Stone Prohibition: Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'aca00621-1183-4068-a9ac-0a980c8552b6').
narrative_ontology:cs_kernel_codification('aca00621-1183-4068-a9ac-0a980c8552b6', fixed_text).
narrative_ontology:cs_authority_grounding('aca00621-1183-4068-a9ac-0a980c8552b6', lineage).
narrative_ontology:cs_interpretation_layer_present('aca00621-1183-4068-a9ac-0a980c8552b6').
narrative_ontology:cs_reading_relation('aca00621-1183-4068-a9ac-0a980c8552b6', aneyoshi_land_use_prohibition__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('aca00621-1183-4068-a9ac-0a980c8552b6', foundational, stone_directive_causally_determines_settlement_location).
narrative_ontology:cs_axiom_status(stone_directive_causally_determines_settlement_location, holdable).
narrative_ontology:cs_axiom_grounding('aca00621-1183-4068-a9ac-0a980c8552b6', stone_directive_causally_determines_settlement_location, empirically_contingent).
narrative_ontology:cs_axiom('aca00621-1183-4068-a9ac-0a980c8552b6', secondary, multi_generational_memory_requires_material_anchor).
narrative_ontology:cs_axiom_status(multi_generational_memory_requires_material_anchor, holdable).
narrative_ontology:cs_axiom_grounding('aca00621-1183-4068-a9ac-0a980c8552b6', multi_generational_memory_requires_material_anchor, empirically_contingent).
narrative_ontology:cs_reference_frame('aca00621-1183-4068-a9ac-0a980c8552b6', ancestral_hazard_knowledge_encoded_in_stone).
narrative_ontology:cs_drift_state('aca00621-1183-4068-a9ac-0a980c8552b6', contemporary_post_2011, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aca00621-1183-4068-a9ac-0a980c8552b6', '2026-02-26T14:33:22Z').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_residents).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, downstream_community_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: ANEYOSHI RESIDENT (ROPE, generational) — The stone directive coordinates survival behavior across time scales longer than individual memory. Residents are constrained by resource availability (topography, distance to resources) but the coordination function is genuine: the constraint enables collective continuation rather than extracting from any group. The stone operates as a persistent coordination signal — low extraction, high coordination benefit.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__behavioral_competence_reading, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% Perspective 2: YOUNGER GENERATION (ROPE, biographical/mobile) — At individual biographical scale, younger Aneyoshi residents have geographic mobility (can relocate to jobs in Kamaishi or beyond) and could exit the prohibition's binding. But they classify as Rope rather than Snare because the directive imposes minimal extractive burden — building on lower land is inconvenient, not prohibitive, and the coordination benefit (tsunami safety) is clear and internalized. Mobile exit options available; low suppression of alternatives.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__behavioral_competence_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% Perspective 3: DISASTER-RESILIENCE INSTITUTION (ROPE, immediate) — National disaster-mitigation frameworks benefit from the stone directive as a case of successful long-term behavioral persistence. Aneyoshi's 2011 survival while neighboring Otsuchi Township (25 km away) suffered 1,184 deaths provides empirical validation of the coordination function. Institutional actors experience the stone as enabling coordination that produces measurable public goods — low extraction, high coordination payoff.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__behavioral_competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: ANALYTICAL OBSERVER (MOUNTAIN, civilizational/universal) — From a civilizational scale, the stone directive instantiates an immutable constraint: tsunami cycles exceed generational memory; communities that fail to encode hazard knowledge across the non-catastrophe interval do not persist; those that do persist. Aneyoshi's 78-year interval without tsunami (1933 to 2011) represents the exact timescale at which institutional memory atrophies and only material anchors (stone) maintain operative constraint. The analytical perspective sees the binding mechanism as a structural necessity, not contingent institutional practice.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__behavioral_competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__behavioral_competence_reading, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (ε = 0.05): Very low. The stone directive coordinates behavior toward a collective benefit (tsunami hazard avoidance) with minimal asymmetric extraction. No beneficiary-victim split; all residents share the benefit of survival. The only extractive component is the opportunity cost imposed by the prohibition: residents cannot freely choose to settle on lower land for economic convenience. But this is not extraction in the technical sense — it is the legitimate cost of coordination. The measured value (0.05) reflects this minimal asymmetry. The measurement trajectory shows slight increases during the 1973–1993 period as opportunity costs accumulate (younger residents lack direct memory of 1933 tsunami), then a sharp drop in 2011 when the tsunami validates the directive's causal efficacy, re-synchronizing individual incentives with collective benefit. SUPPRESSION (0.02): Minimal. The prohibition is normatively endorsed; collective memory of the 1933 tsunami provides immediate justification; violation is socially unthinkable for most of the interval. Suppression requirement is low because consensus is high. The slight increase in the 1973–1993 period (to 0.03) reflects passive social sanction of the few residents who question the prohibition—a low-level suppression of dissent rather than coercion. THEATER RATIO (0.15): Low. The stone directive operates with high functional clarity: explicit behavioral rule, tied to material evidence, with causal mechanism understood (tsunami hazard → settlement location → survival). The modest theater component (0.15 rather than near-zero) reflects the interpretive layer that accumulates as direct memory fades—younger residents learn the prohibition through tradition and authority rather than causal reasoning. But the functional layer dominates; the behavioral outcome (no settlement below stone) is driven by understood causality, not performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   The behavioral_competence_reading produces Rope from all four perspectives because the constraint genuinely coordinates behavior toward collective benefit with minimal extraction. The perspectival gap is not between Rope and Snare (as in typical extraction scenarios), but between operationally live Rope and the sibling reading's classification as Piton or Snare. The behavioral_competence reading sees the stone as causally efficacious; the commemorative_husk reading would see it as an inert memorial ritual that happens to correlate with adaptive settlement patterns. The empirical question—resolved by 2011—determines which reading is correct. The analytical observer's mountain perspective might be tempted to naturalize the constraint ('long-interval tsunamis necessarily require multi-generational memory encoding'), but the behavioral_competence reading anchors the constraint in contingent institutional practice (the specific community choice to erect a stone, maintain the tradition, and enforce settlement above it). The mount is false; the rope is real.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from beneficiary status and exit options. (1) Aneyoshi residents: powerless/constrained + beneficiary (hazard protection) → low d → low χ (Rope). (2) Younger generation: moderate/mobile + beneficiary (hazard protection) + exit options (could relocate) → moderate d, but beneficiary status dominates → moderate χ (still Rope). (3) Disaster-resilience institution: institutional/arbitrage + beneficiary (validation of coordination model) → low d → low χ (Rope). (4) Analytical observer: analytical/analytical (observer position, not agent position) → canonical d for analytical context → produces mountain perspective (natural-law framing). The sibling reading (commemorative_husk) would produce higher d for residents (treating them as bearers of ritual burden rather than beneficiaries of protection), pushing toward Snare or Tangled Rope. The difference in directionality between readings reflects their different accounts of the binding mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_persistence_mechanism,
    'Does the stone directive operate through direct causal constraint on settlement behavior, or through ritualized commemoration that happens to reinforce adaptive behavior?',
    'Longitudinal observational study: interview residents on their conscious motivations for settlement location (explicit knowledge of tsunami hazard vs. tradition/parent authority vs. aesthetic preference); track settlement decisions in newly migrated residents without embedded kinship ties to the prohibition; model statistical correlation between proximity to stone and actual tsunami mortality in 2011 against counterfactual settlement patterns in neighboring communities without such directive artifacts.',
    'If direct causal: behavioral_competence_reading is correct (Rope from all perspectives, low ε). If primarily ritual-based with adaptive outcome: commemorative_husk_reading is more accurate (Piton or Snare, higher ε). If mixed: depends on proportion — strong ritual framing with occasional explicit hazard instruction may split into two constraints (behavioral_competence for explicit-instruction agents; commemorative_husk for tradition-transmission agents).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_persistence_mechanism, empirical, 'Whether stone directive operates through direct behavioral causation or through adaptive ritual').

omega_variable(
    post_2011_behavioral_shift,
    'Has the 2011 tsunami validation strengthened or weakened the stone directive''s operative force among younger residents?',
    'Comparative study: (a) pre-2011 resident interviews on stone significance and settlement decision factors; (b) post-2011 resident interviews on same factors; (c) construction permit applications and settlement pattern changes in Aneyoshi''s reconstruction phase (2011-2020); (d) intergenerational transmission of explicit hazard-awareness narratives (did 2011 validation make hazard-talk more frequent or more specific in parental instruction?).',
    'If strengthened: behavioral_competence_reading shows durable empirical grounding across episodic validation. If weakened (residents view it as ''we got lucky once''): suggests commitment system''s authority derives from continuous re-encoding rather than single validation event — may shift toward commemorative_husk reading. If split by cohort (older strengthen, younger weaken): indicates two constraints operating simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_2011_behavioral_shift, empirical, 'Post-2011 trajectory of stone directive''s operative force').

omega_variable(
    committer_frame_ambiguity,
    'Is this constraint ONE behavioral commitment to land-use prohibition encoded in stone, or TWO distinct commitments (behavioral adaptation to long-interval tsunami hazard PLUS commemorative practice honoring ancestors/traditional authority)?',
    'Kernel analysis: identify the single codified commitment whose behavioral reading I am instantiating (behavioral_competence) and the sibling reading''s kernel (commemorative_husk). They parse the same artifact — Aneyoshi''s stone marker inscribed ''津波記念碑'' (Tsunami Memorial Stone, 1933) with height marking — differently: as live behavioral directive vs. as memorial to ancestors. The resolution: is there one kernel (the stone directive about settlement location) or two kernels (one behavioral, one commemorative)? This omega documents why they are one kernel with two readings rather than two distinct constraints.',
    'If one kernel, two readings: behavioral_competence_reading and commemorative_husk_reading are siblings in a contested kernel; the engine''s reading_relations and axioms in cs_structure capture their relationship. If two kernels: each constraint should be decomposed as a separate JSON file per ε-invariance principle — behavioral ε ≈ 0.05, commemorative ε ≈ 0.40-0.60 (institutional theater maintaining memorial function). Current authoring treats it as one kernel, two readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Whether the stone artifact grounds one kernel with two readings or two distinct constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0, 83).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_bc_tr_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aneyoshi_bc_tr_t40, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(aneyoshi_bc_tr_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 78, 0.12).

% Extraction over time
narrative_ontology:measurement(aneyoshi_bc_be_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(aneyoshi_bc_be_t20, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 20, 0.03).
narrative_ontology:measurement(aneyoshi_bc_be_t40, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 40, 0.04).
narrative_ontology:measurement(aneyoshi_bc_be_t60, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(aneyoshi_bc_be_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 78, 0.03).
narrative_ontology:measurement(aneyoshi_bc_be_t83, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 83, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_bc_su_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0, 0.01).
narrative_ontology:measurement(aneyoshi_bc_su_t60, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 60, 0.03).
narrative_ontology:measurement(aneyoshi_bc_su_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 78, 0.01).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The Aneyoshi stone constraint family consists of two readings of a single kernel (the 1933 stone directive and settlement prohibition). The behavioral_competence_reading (ε ≈ 0.05) models the stone as causally determining settlement location through explicit behavioral coordination. The commemorative_husk_reading (ε ≈ 0.40–0.60, likely Piton or Snare) models the stone as primarily performing commemorative function—ritual honoring of ancestors and 1933 victims—with behavioral persistence as a side effect. Both readings use identical artifact (the stone marker) and identical historical outcome (2011 survival). They differ in causal attribution and in how they account for the binding mechanism. The readings are linked because the empirical validation (2011) provides evidence for or against each causal model. The behavioral_competence reading's low ε depends on the claim that the prohibition directly causes settlement location decisions; the commemorative_husk reading's higher ε depends on the claim that ritual maintenance and identity fusion are primary, and behavioral adaptation is emergent. Network link is directional: behavioral_competence_reading influences commemorative_husk_reading because if behavioral causation is demonstrated, the commemorative reading must account for how ritual can be performative while still coordinating adaptive behavior.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
