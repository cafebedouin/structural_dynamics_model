% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Stone Land-Use Rule (Behavioral Competence Reading)
 *   domain: institutional/anthropological
 *
 * SUMMARY:
 *   A stone boundary line on an upland slope marks a land-use prohibition
 *   that has persisted for 78 years through daily spatial practice without
 *   written codification or state enforcement. The rule prohibits plowing,
 *   building, or intensive cultivation above the line; compliance is
 *   sustained through bodily knowledge of the boundary's location, collective
 *   memory of why it exists, and social friction when violations occur. The
 *   behavioral-competence reading treats this as a binding land-use
 *   constraint sustained by the internalization of practice — agents comply
 *   because they have learned the rule through embodied experience and
 *   because violating it triggers costly collective repair and social
 *   sanction. This reading emphasizes that the rule's persistence depends on
 *   continuous behavioral competence (knowing where the stone lies,
 *   understanding why, practicing avoidance) distributed across the entire
 *   community. An alternative reading (commemorative_husk) sees the same
 *   stone as a decayed memorial artifact: the original rule may have had
 *   behavioral force, but over time the meaning has drained away; what
 *   persists is a vestigial practice maintained more by inertia than by
 *   understood necessity.
 *
 * KEY AGENTS:
 *   - Land conservation collective (organized, generational time horizon, constrained exit) — multigenerational community of stewards maintaining the boundary through daily practice
 *   - Agricultural pressures (moderate power, biographical horizon, constrained exit) — farmers and landowners bearing the opportunity cost of boundary compliance
 *   - Settlement stewards (organized, generational horizon, mobile exit) — institutional memory holders who preserve and transmit the rule
 *   - Erosion prevention beneficiary (analytical, civilizational horizon) — the physical outcome the rule sustains
 *   - External markets and pressures (institutional, biographical horizon, trapped) — excluded from negotiating boundary relaxation
 *   - Intergenerational knowledge preservation (analytical, civilizational horizon) — the pedagogical function of the rule
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
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, mountain).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Land-Use Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "institutional/anthropological").

domain_priors:emerges_naturally(stone_land_use_rule__behavioral_competence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, 'd801d257-9400-422f-9a20-569c7d3ef552').
narrative_ontology:cs_kernel_codification('d801d257-9400-422f-9a20-569c7d3ef552', implicit).
narrative_ontology:cs_authority_grounding('d801d257-9400-422f-9a20-569c7d3ef552', practice).
narrative_ontology:cs_interpretation_layer_present('d801d257-9400-422f-9a20-569c7d3ef552').
narrative_ontology:cs_reading_relation('d801d257-9400-422f-9a20-569c7d3ef552', stone_land_use_rule__commemorative_husk, coexists_with).
narrative_ontology:cs_axiom('d801d257-9400-422f-9a20-569c7d3ef552', foundational, behavioral_rule_persists_through_learned_practice).
narrative_ontology:cs_axiom_status(behavioral_rule_persists_through_learned_practice, holdable).
narrative_ontology:cs_axiom_grounding('d801d257-9400-422f-9a20-569c7d3ef552', behavioral_rule_persists_through_learned_practice, empirically_contingent).
narrative_ontology:cs_axiom('d801d257-9400-422f-9a20-569c7d3ef552', foundational, erosion_prevention_remains_functionally_necessary).
narrative_ontology:cs_axiom_status(erosion_prevention_remains_functionally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('d801d257-9400-422f-9a20-569c7d3ef552', erosion_prevention_remains_functionally_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('d801d257-9400-422f-9a20-569c7d3ef552', learned_boundary_practice).
narrative_ontology:cs_drift_state('d801d257-9400-422f-9a20-569c7d3ef552', contemporary_seven_eight_years_post_establishment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d801d257-9400-422f-9a20-569c7d3ef552', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, land_conservation_collective).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, intergenerational_knowledge_preservation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, agricultural_pressures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The multigenerational community of stewards — farmers, shepherds, and residents — who maintain the stone boundary and practice daily compliance with its prohibitions. They benefit from the stable land-use pattern it sustains: the cleared upland remains productive pasture, erosion is minimized, and the constraint's clarity reduces transaction costs of land negotiation. They experience the rule through bodily practice: knowing where the stone lies, steering equipment away, teaching children the boundary, incorporating avoidance into seasonal routine.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, land_conservation_collective, beneficiary,
    organized, generational, constrained, local).

% Farmers and landowners who would gain marginal productive capacity if they could plow or build beyond the stone line. The boundary forecloses higher-yield cropping patterns and eliminates options for field expansion during demographic or market upswings. The cost is measured in lost acreage and the opportunity cost of steeper hillside agriculture elsewhere. Exit: they could violate the rule, but doing so incurs persistent social friction and eventual collective repair enforcement.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, agricultural_pressures, payer,
    moderate, biographical, constrained, local).

% The physical outcome enabled by the constraint: soil retention on the cleared slope, reduced sedimentation in the river network, and preserved arable land downstream. This is not an agent — it is the coordination outcome the rule sustains. Named here as beneficiary because the constraint's persistence is justified by the actual ecological stabilization it produces.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, erosion_prevention_beneficiary, beneficiary,
    analytical, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(stone_land_use_rule__behavioral_competence, erosion_prevention_beneficiary).

% The succession of village elders, land councils, and institutional memory holders who maintain the rule's narrative, repair violations when they occur, and transmit the practice to new residents and landowners. They do not 'run' the constraint in an enforcement sense — the constraint runs through daily spatial practice — but they preserve the rule structure and adjudicate boundary disputes. Their exit: migration or loss of collective memory would dissolve the rule's operation within a generation.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, settlement_stewards, agenda_setter,
    organized, generational, mobile, local).

% Capital-intensive agriculture, tourism development, and outsider investment have historically sought to break the boundary — to clear more land, build vacation infrastructure, or extract stone itself. They are excluded by the rule's enforcement through local practice and by the settlement's collective resistance. They would benefit from boundary relaxation but cannot participate in negotiating it (the rule is not a commodity that changes hands; it is a sustained practice they cannot enter).
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, external_markets_and_pressures, excluded,
    institutional, biographical, trapped, global).

% The abstract good of transmitted institutional memory: knowing where the boundary lies, why it matters, how to repair it, and how to resolve disputes when they arise. The rule sustains this knowledge through practice — each act of compliance reinforces why the boundary exists. Named as beneficiary because the constraint's pedagogical function — teaching each generation about land stewardship and collective decision-making — is part of what the rule accomplishes.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, intergenerational_knowledge_preservation, beneficiary,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(stone_land_use_rule__behavioral_competence, intergenerational_knowledge_preservation).

% Researchers and institutional analysts studying the rule as a case of institutional durability and behavioral enforcement. They document how the constraint persists without written codification or state enforcement, sustained purely through daily spatial practice and collective commitment. Their position is analytical only; they do not enforce, benefit, or pay.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, anthropological_observers, observer,
    analytical, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes land use by marking a clear, physically enforced boundary that prevents erosion-driving overexploitation of the slope, protects downstream arable land, and reduces transaction costs of knowing where land-use negotiation stops and common interest begins. The stone itself becomes the coordinating device — it is not a rule written on paper but embodied in the landscape.
% TRANSFER_FUNCTION: Moves opportunity costs (forgone acreage expansion, marginal yield losses) from the collective conservation benefit (soil stability, downstream protection, intergenerational knowledge) to individual farmers and landowners who encounter the boundary in their daily practice. The transfer is not monetary — it is enforced through social friction and collective repair labor when violations occur.
% ABSENT_VOICES: Commercial agricultural interests, infrastructure developers, and outside investors who would profit from boundary relaxation are systematically excluded — not invited to negotiate because the rule is not treated as negotiable. They would argue for market-driven land allocation but cannot participate in a system that operates through sustained practice rather than institutional negotiation.
% DISAPPEARANCE_RATIONALE: If the stone were removed and the rule ceased to bind, the upslope land would be plow-cultivated within years, erosion would accelerate, sedimentation downstream would increase, and the institutional knowledge of boundary maintenance would atrophy. The landscape would physically transform; the settlement's relationship to land stewardship would degrade. The world is not rearranged by human decision but by the actual ecological and social consequences of rule collapse.
% FOUNDING_PROBLEM: Early-medieval land clearance in upland regions created erosion risk: cleared slopes shed water and soil, threatening downstream agriculture and settlement. The problem was not explicit at first but emerged empirically — communities observed that certain boundary stones coincided with stable land use and that crossing them triggered erosion and social friction. The rule emerged from observational knowledge, not as a designed policy.
% FOUNDING_PROBLEM_CORROBORATION: Geomorphologists studying the region confirm active erosion on cleared slopes above and below the stone boundary, and soil retention above it. Regional land councils and farm associations document continued slope instability where the rule has been abandoned. Settlement elders attest the founding problem persists; external environmental analysts corroborate that the rule's persistence is justified by actual erosion prevention.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   This reading claims the constraint is a mountain — a natural outcome that emerges from the actual ecological and behavioral dynamics of slope stability and collective coordination. The extractiveness is low (0.18) and remarkably flat across 78 years because the rule's operation is not dependent on increasing coercion or capital-intensive enforcement. The suppression requirement is minimal (0.12) because agents comply through learned practice, not through fear of punishment. The theater ratio is very low (0.08) because the rule's functional activity (steering around the stone, incorporating it into seasonal planning, teaching children the boundary) is mostly real coordination work, not theatrical maintenance. The accessibility collapse is very high (0.89) because once the rule is learned and internalized through embodied practice, alternatives become unthinkable — a farmer who learns the boundary's location and why it matters experiences it as a natural feature of the landscape, not as an imposed constraint. Resistance is near-zero (0.05) because the rule aligns with genuine ecological interest (preventing erosion that would harm all parties downstream). The measured flatness of extractiveness and suppression across 78 years is the diagnostic signature of a mountain: the constraint's persistence is not sustained by accumulating enforcement intensity or by increasing extraction — it is sustained by the rule's alignment with how the world actually works. The slight uptick in theater at years 26 and 65 may reflect generational transitions when institutional memory must be more actively reasserted, but the effect is small. This reading treats the stone as a landmark that has become genuinely natural through practice — agents experience it as a fixed feature they have learned to navigate, not as an imposed rule they must resist or that must be constantly justified.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (settlement stewards), the rule is a knowledge practice that must be actively preserved and transmitted — they experience the burden of institutional memory work and know the rule is sustained by their effort. From the agricultural-pressure seat (constrained farmers), the rule is a boundary they encounter daily and have learned to work around — they experience it as a real constraint on land use but also as simply 'how farming works here.' From the external-market seat (excluded investors), the rule is an opaque local institution that blocks profitable development. The engine computes these divergences from the structural data: the agenda-setter has mobile exit and organized power (can choose to transmit or let the rule decay), while the farmer has constrained exit and moderate power (must comply but cannot unilaterally change the rule). These differences in power and exit options produce different directionalities — the agenda-setter approaches d = 0.4 (moderate beneficiary, does coordination work), while the constrained farmer approaches d = 0.7 (moderate target, bears opportunity costs). The mountain claim asserts that despite these divergences, the rule's persistence is explained by alignment with actual ecological stability, not by coercion or extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The land-conservation collective benefits from the stable land-use pattern and from the intergenerational knowledge the rule sustains (d near beneficiary end, ~0.25). Agricultural pressures bear the opportunity cost of boundary compliance (d near target end, ~0.7). Settlement stewards do coordination and memory work, receiving diffuse legitimacy but no concentrated benefit (d near middle, ~0.45). The behavioral-competence reading assumes that agents comply primarily because they have internalized the rule through practice, not because they are coerced. This assumption places the rule closer to the beneficiary end for those who understand its purpose (erosion prevention, downstream stability) and closer to neutral for those who experience it as a simple landscape feature. The mountain classification requires that directionality not depend on who is enforcing the rule (no one is — it is self-enforcing through practice) and that the rule persist regardless of who the nominal beneficiary is (it would persist even if external investors took over local land management, because the erosion gradient remains true). The claim rests on the presumption that the rule's persistence is independent of power asymmetries — it exists because agents have learned that boundaries at certain elevations correlate with slope stability.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (erosion risk from upslope clearance) remains live — geomorphological data confirm active erosion on uncontrolled slopes and soil stability above the stone line. The rule's function has not atrophied; the constraint persists precisely because it is solving the problem it was built for. The mandatrophy question does not apply in the standard form because there is no gap between the founding problem and the current function. The alternative reading (commemorative_husk) would argue that the founding problem is contested or dead — that modern drainage and terracing have solved the erosion risk, and the stone persists only as a symbolic reminder of a problem no longer present. That reading would claim mandatrophy. This reading (behavioral_competence) asserts that the founding problem is demonstrably live and that the rule solves it through its actual operation, not through theatrical maintenance. The measurement data support this: extractiveness and suppression are flat and low across 78 years, which is inconsistent with a rule that has lost its function and persists through theater; if the rule were a theatrical zombie, we would expect increasing suppression (to maintain compliance as meaning decays) and possibly rising extraction (as newer enforcement mechanisms are grafted on). The flatness of the metrics is the data the mandatrophy question reads — mandatrophy would show up as a rising theater_ratio or an accelerating suppression requirement as the rule's real function fades and theater takes over.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_internalization_vs_coercive_compliance,
    'Is the measured compliance with the stone boundary sustained by internalized behavioral competence (agents understand why the rule exists and practice avoidance as a learned skill) or by implicit threat of social sanction (agents comply primarily to avoid friction, not because they endorse the rule)?',
    'Post-enforcement ethnography: interview agents about their reasoning for compliance, observe whether compliance persists when external supervision is absent, and compare compliance patterns for agents with strong vs. weak understanding of the rule''s ecological justification.',
    'If behavioral internalization is dominant, the rule''s persistence is explained by the mountain reading (agents have learned the rule and act on it without coercion). If compliance is primarily coercion-avoidance, the rule is sustained by suppression and approaches a tangled_rope or snare classification. The behavioral-competence reading requires that internalization is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_internalization_vs_coercive_compliance, empirical, 'Whether compliance depends on understood justification or coercive threat.').

omega_variable(
    natural_law_vs_constructed_norm,
    'Is the stone boundary''s persistence explained by agents'' discovery that certain slopes erode and certain boundaries prevent erosion (natural law alignment), or by agents'' construction of a norm that happens to correlate with erosion patterns but could be abandoned if social conditions change (constructed norm that happens to be adaptive)?',
    'Cross-case comparison: examine whether analogous upland communities with similar erosion gradients develop similar boundary rules even without direct knowledge transfer. If convergence occurs, this suggests alignment with natural law. If boundaries are idiosyncratic, this suggests local construction.',
    'Natural law alignment strengthens the mountain classification; constructed-norm findings strengthen the commemorative_husk reading and suggest the rule could be abandoned if the community chose.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_norm, conceptual, 'Whether the rule reflects discovered natural law or constructed local practice that happens to be adaptive.').

omega_variable(
    knowledge_transmission_mechanism_fragility,
    'How fragile is the behavioral competence the constraint depends on? If institutional memory were disrupted (by out-migration, generational discontinuity, or introduction of new residents unfamiliar with the rule), how quickly would compliance decay?',
    'Historical cases of rule abandonment in the region, or observation of how quickly new residents are incorporated into the boundary-maintenance practice. Testing: interview long-term vs. recent residents about their understanding of the rule.',
    'High fragility (knowledge lost within a generation without active transmission) would suggest the rule is more fragile than a true mountain and closer to a piton or scaffold. Low fragility would support the mountain reading. This is the FSM question for this constraint: the rule''s persistence may depend on beneficiary (institutional memory holders) actively maintaining knowledge, which could make it a false-summit mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_transmission_mechanism_fragility, empirical, 'Whether the rule''s behavioral competence persists across generational discontinuities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.06).
narrative_ontology:measurement(ston_tr_t13, stone_land_use_rule__behavioral_competence, theater_ratio, 13, 0.07).
narrative_ontology:measurement(ston_tr_t26, stone_land_use_rule__behavioral_competence, theater_ratio, 26, 0.08).
narrative_ontology:measurement(ston_tr_t39, stone_land_use_rule__behavioral_competence, theater_ratio, 39, 0.08).
narrative_ontology:measurement(ston_tr_t52, stone_land_use_rule__behavioral_competence, theater_ratio, 52, 0.08).
narrative_ontology:measurement(ston_tr_t65, stone_land_use_rule__behavioral_competence, theater_ratio, 65, 0.09).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.08).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.19).
narrative_ontology:measurement(ston_be_t13, stone_land_use_rule__behavioral_competence, base_extractiveness, 13, 0.18).
narrative_ontology:measurement(ston_be_t26, stone_land_use_rule__behavioral_competence, base_extractiveness, 26, 0.17).
narrative_ontology:measurement(ston_be_t39, stone_land_use_rule__behavioral_competence, base_extractiveness, 39, 0.18).
narrative_ontology:measurement(ston_be_t52, stone_land_use_rule__behavioral_competence, base_extractiveness, 52, 0.18).
narrative_ontology:measurement(ston_be_t65, stone_land_use_rule__behavioral_competence, base_extractiveness, 65, 0.19).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__behavioral_competence, suppression_requirement, 0, 0.11).
narrative_ontology:measurement(ston_su_t13, stone_land_use_rule__behavioral_competence, suppression_requirement, 13, 0.12).
narrative_ontology:measurement(ston_su_t26, stone_land_use_rule__behavioral_competence, suppression_requirement, 26, 0.13).
narrative_ontology:measurement(ston_su_t39, stone_land_use_rule__behavioral_competence, suppression_requirement, 39, 0.12).
narrative_ontology:measurement(ston_su_t52, stone_land_use_rule__behavioral_competence, suppression_requirement, 52, 0.11).
narrative_ontology:measurement(ston_su_t65, stone_land_use_rule__behavioral_competence, suppression_requirement, 65, 0.12).
narrative_ontology:measurement(ston_su_t78, stone_land_use_rule__behavioral_competence, suppression_requirement, 78, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, global_infrastructure).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__behavioral_competence, 0.16).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% The stone_land_use_rule kernel decomposes into two constraint readings. This reading (behavioral_competence) treats the stone as a live behavioral constraint sustained by internalized practice and actual ecological benefit. The sibling reading (commemorative_husk) treats the same stone as a vestigial memorial artifact maintained by inertia. The two readings share the referent (the physical stone and 78-year persistence) but diverge on mechanism: internalization + ecology vs. inertia + theater. This reading authors low, stable ε (extractiveness) and suppression because the rule is self-enforcing through practice; the sibling reading would author higher theater_ratio and higher suppression because it must be maintained theatrically once functional meaning decays. The ε-invariance principle requires two separate stories because ε changes when the referent shifts from 'the rule as live coordination' to 'the rule as decayed memorial.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
