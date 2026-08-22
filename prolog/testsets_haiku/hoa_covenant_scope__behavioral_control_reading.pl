% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant Behavioral Control Reading
 *   domain: property_law/collective_governance
 *
 * SUMMARY:
 *   This constraint instantiates the BEHAVIORAL CONTROL READING of the HOA
 *   covenant kernel: the covenant functions as a mechanism for enforcing
 *   aesthetic uniformity and lifestyle conformity as a
 *   property-value-maximization strategy, with enforcement scope extending
 *   into subjective aesthetic judgments, speech suppression (yard signs,
 *   flags), and visible-poverty markers. Under this reading, the covenant is
 *   a Snare — the coordination story (shared infrastructure maintenance) is
 *   subordinate to the extraction story (behavioral control and suppression
 *   of dissident aesthetics). The board-aligned majority benefits from
 *   conformity; nonconformists and marginal-aesthetic expressers bear the
 *   costs. The constraint is CLAIMED as Snare based on this reading's
 *   structural account, and the metrics author the extraction, suppression,
 *   and theater patterns that reading establishes.
 *
 * KEY AGENTS:
 *   - conformist_majority_homeowners: Primary beneficiary (power=moderate, exit=constrained) — aesthetic preferences align with covenant norms; enforcement falls on others; property-value narrative legitimizes their position
 *   - board_aligned_homeowners: Agenda-setter + beneficiary (power=moderate, exit=constrained) — interpret covenant norms, control enforcement, gain authority and prestige from board position
 *   - nonconformist_residents: Primary target/payer (power=powerless, exit=identity_locked) — face fines, speech suppression, pressure to conform; exit requires selling at potential loss; identity fusion with homeownership deepens trap
 *   - hoa_board: Enforcement apparatus (power=organized, exit=constrained) — administers discretionary regime, interprets undefined aesthetic standards, maintains enforcement machinery
 *   - dissenting_residents: Excluded (power=powerless, exit=identity_locked) — would dispute behavioral-control reading but are structurally barred from the framework-setting conversation
 *   - legal_challenge_mechanism: Observer seat (power=institutional, exit=analytical) — courts defer to board discretion on aesthetics; litigation expensive and rarely successful
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.42).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.68).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant Behavioral Control Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, 'e501b19b-1810-4679-ae74-a96caf3a6fa2').
narrative_ontology:cs_kernel_codification('e501b19b-1810-4679-ae74-a96caf3a6fa2', formalized).
narrative_ontology:cs_authority_grounding('e501b19b-1810-4679-ae74-a96caf3a6fa2', extraction).
narrative_ontology:cs_interpretation_layer_present('e501b19b-1810-4679-ae74-a96caf3a6fa2').
narrative_ontology:cs_reading_relation('e501b19b-1810-4679-ae74-a96caf3a6fa2', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e501b19b-1810-4679-ae74-a96caf3a6fa2', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('e501b19b-1810-4679-ae74-a96caf3a6fa2', foundational, aesthetic_uniformity_enforceable_via_property_sanctions).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_enforceable_via_property_sanctions, holdable).
narrative_ontology:cs_axiom_grounding('e501b19b-1810-4679-ae74-a96caf3a6fa2', aesthetic_uniformity_enforceable_via_property_sanctions, conventional).
narrative_ontology:cs_axiom('e501b19b-1810-4679-ae74-a96caf3a6fa2', foundational, behavioral_conformity_maximizes_property_value).
narrative_ontology:cs_axiom_status(behavioral_conformity_maximizes_property_value, holdable).
narrative_ontology:cs_axiom_grounding('e501b19b-1810-4679-ae74-a96caf3a6fa2', behavioral_conformity_maximizes_property_value, empirically_contingent).
narrative_ontology:cs_reference_frame('e501b19b-1810-4679-ae74-a96caf3a6fa2', board_discretionary_aesthetic_authority).
narrative_ontology:cs_drift_state('e501b19b-1810-4679-ae74-a96caf3a6fa2', contemporary_polarized_neighborhoods, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e501b19b-1810-4679-ae74-a96caf3a6fa2', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_residents).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_expressions).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, speech_restricted_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from neighborhood aesthetic uniformity that aligns with their preferences and perceived property value maintenance. They experience the covenant as protecting their investment and preserving neighborhood character. Their own behavior already aligns with covenant norms, so enforcement pressure falls on others, not them.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority_homeowners, beneficiary,
    moderate, biographical, constrained, local).

% Serve on or directly influence the HOA board that interprets and enforces the covenant. Set enforcement priorities, define what constitutes unacceptable aesthetic deviation, and control the complaint and sanctions apparatus. Directly benefit from the deference afforded board interpretations and the concentration of discretionary authority.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary).

% Bear enforcement pressure and potential fines for aesthetic or behavioral choices that deviate from board-endorsed norms: yard signs, flag displays, non-standard exterior colors, unconventional landscaping, visible poverty markers (older vehicles, weathered furniture), or lifestyle expressions deemed unacceptable. Exit requires selling (illiquid, costly, and may depress resale value given the covenant's restrictions). Identity fusion with homeownership and place makes exit psychologically costly beyond the financial barrier.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_residents, payer,
    powerless, biographical, identity_locked, local).

% Administers the covenant enforcement regime, interprets aesthetic standards, receives and acts on complaints, assesses fines, and maintains the covenant's discretionary authority structure. The board's power derives from the ambiguity and subjectivity of 'aesthetic uniformity' — broad, unquantified standards that require ongoing interpretive application.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board, agenda_setter,
    organized, biographical, constrained, local).

% Would dispute the covenant's behavioral control reading and argue for a narrower coordination reading focused on shared infrastructure. They are excluded from the decision-making apparatus because the covenant's legitimacy depends on treating aesthetic conformity as a settled good; dissent would require revisiting foundational premises the board cannot afford to contest in open forum.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, dissenting_residents, excluded,
    powerless, biographical, identity_locked, local).

% Face suppression of political, religious, or ideological speech through covenant enforcement (yard signs, flag displays, bumper stickers, visible symbols). The covenant frame converts speech into 'aesthetic violation,' invoking property rules rather than free speech protections. They bear the cost of self-censorship or sanctions.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, speech_restricted_homeowners, payer,
    powerless, biographical, identity_locked, local).

% Courts review covenant enforceability on narrow grounds (reasonableness, racial/religious animus, procedural fairness) but typically defer to board discretion on aesthetic judgments. Litigation is expensive and courts rarely override board decisions unless explicit unconstitutional discrimination is proven. This observer seat has limited power to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, legal_challenge_mechanism, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, no genuine coordination function exists. The covenant frame mimics coordination language (property value protection, community standards) but the actual function is behavioral conformity enforcement and suppression of dissident aesthetic/lifestyle choices.
% TRANSFER_FUNCTION: Transfers decision-making authority over personal property aesthetic expression from individual homeowners to the HOA board; transfers compliance burden and fine liability from conformist to nonconformist residents; transfers prestige and social legitimacy to conformist-aligned board members who define 'acceptable' neighborhood character.
% ABSENT_VOICES: Nonconformist residents, marginal-aesthetic advocates, and those with political/religious expressions the board disfavors are structurally excluded from the covenant-setting process. They would argue that aesthetic diversity is a legitimate value, that subjective beauty standards should not be enforceable through property sanctions, and that the covenant's breadth suppresses protected speech. They are absent because the covenant's legitimacy depends on treating aesthetic uniformity as self-evidently desirable — revisiting that premise in open forum would destabilize the enforcement regime.
% DISAPPEARANCE_RATIONALE: If the behavioral-control covenant vanished, individual homeowners would resume aesthetic autonomy; nonconformist residents would no longer face fines or speech suppression; the neighborhood's visual character would diversify; board authority would collapse to maintenance coordination only. The constraint's disappearance would directly enable the excluded voices and liberate the payer seats from suppression.
% FOUNDING_PROBLEM: Early suburbs faced genuinely disruptive externalities: garbage accumulation, deferred maintenance, structures in visible decay. Property-value protection and coordinated upkeep required some behavioral standards.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary HOA legal scholarship and resident surveys outside the board-aligned seats document that foundational maintenance and infrastructure problems are solved and stable. Boards have shifted enforcement to aesthetic conformity (color schemes, landscaping diversity, political signage, lifestyle visibility) uncoupled from the original externality problem. Independent analysis of enforcement data shows fines concentrated on speech-adjacent and marginal-aesthetic violations, not infrastructure maintenance.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).
:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) under this reading because the constraint captures compliance costs and fine revenue but does not extract productive output — nonconformists cannot buy their way out through increased productivity; they must conform or pay. Suppression is substantial (0.68) because the covenant's persistence depends on preventing nonconformist expression and keeping dissenting voices excluded from the legitimacy-setting conversation. Theater is moderate-high (0.41) because the board frames enforcement as 'property-value protection' and 'community standards,' but the actual pattern shows fines concentrated on speech-adjacent violations (yard signs, flags) and marginal-aesthetic choices (diverse landscaping, visible poverty markers) uncoupled from infrastructure maintenance. The measurement series shows extractiveness and suppression plateauing by year 20–25, suggesting the regime has stabilized into a steady-state enforcement pattern rather than escalating. Theater rising from 0.25 to 0.41 indicates the board's narrative work increasing as the founding coordination problem recedes and the aesthetic-control function becomes the operative behavior-shaping mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The board's reading of the covenant as legitimate property-value coordination sits at low d (beneficiary position, organized power, discretionary authority). The nonconformist resident's reading of the same covenant as speech-suppressing behavioral control sits at high d (target position, powerless, identity-locked exit). The constraint is the same arrangement; the directionality differs by structural position. This divergence is precisely what the per-seat computation captures — one constraint, multiple seats, multiple type classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Conformist-majority homeowners have d near 0.2–0.3 (beneficiary end): they benefit without enforcement pressure, their aesthetic preferences align with board norms, their exit options remain open (move if dissatisfied, but conformity removes dissatisfaction). Board-aligned homeowners have d near 0.15–0.25 (beneficiary + agenda-setter end): they set the rules, benefit from prestige and authority, face minimal enforcement pressure. Nonconformist residents have d near 0.8–0.9 (target end): they face active enforcement, fines, speech suppression, and identity-locked exit (selling triggers loss). The payer seats' high d values feed high effective extraction via the engine's χ computation. No directionality overrides needed; the structural data (beneficiary/victim declarations + power atoms + exit options) produce accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (disruptive externalities from deferred maintenance and visible decay) is documented as dead by year 10–15: infrastructure maintenance standards are met consistently, and contemporary enforcement data shows fines concentrated on aesthetic conformity and speech suppression rather than maintenance violations. Yet the covenant persists. This is a classic mandatrophy signature: the constraint lives past its founding function, maintained by the board's power to redefine 'acceptable' behavior and the conformist majority's preference for aesthetic uniformity. The behavioral-control reading prevents misclassification as coordination — under the coordination reading, the covenant would appear as a healthy rope; under the behavioral-control reading, it correctly classifies as snare with a dead founding mandate. The mandatrophy signal is the (founding_problem_status=dead) × (disappearance_verdict=world_rearranges) mismatch detected by the six-questions consumer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.68) structural (external enforcement machinery, fines, visible sanctions) or internalized (residents have internalized conformity norms and self-censor without active external pressure)?',
    'Post-exit suppression trajectory: survey residents who have sold and left the HOA jurisdiction to measure whether suppression of nonconformist aesthetics and speech persists after the enforcement machinery is removed. Persistent suppression indicates internalization; suppression dropping to near-zero indicates it was structural.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural enforcement measure suggests — the payer seats carry the suppression with them post-exit, making the constraint more durable and extractive than enforcement data alone indicates. If structural, the constraint''s hold weakens once enforcement machinery is absent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression mechanisms are external enforcement or internalized behavioral norms').

omega_variable(
    coordination_extraction_separation,
    'Are the genuine coordination functions (shared infrastructure maintenance, trash collection scheduling, common-area upkeep) structurally inseparable from behavioral control and aesthetic enforcement, or can infrastructure coordination operate independently?',
    'Comparative analysis of HOAs that narrowed covenant scope to infrastructure-only (eliminating aesthetic and lifestyle restrictions) while maintaining infrastructure coordination: do infrastructure problems emerge, or does coordination persist without behavioral control?',
    'If separable, the behavioral-control reading is confirmed — enforcement breadth exceeds coordination necessity and functions as pure suppression layered onto coordination. If inseparable, part of the measured extraction is genuine coordination cost rather than pure behavioral control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separation, empirical, 'Whether infrastructure coordination requires aesthetic/behavioral enforcement or operates independently').

omega_variable(
    reading_boundary_ambiguity,
    'Does the behavioral-control reading genuinely foreclose the coordination reading within a single HOA''s operational framework, or can both readings coexist as different parties'' interpretations of the same covenant?',
    'Discourse analysis of HOA board meetings, enforcement records, and covenant interpretation documents: are coordination language and behavioral-control rationales presented as mutually exclusive foundations, or as complementary justifications for the same rules?',
    'If readings foreclose each other, the coordinate boundary shifts; only one reading can be instantiated per constraint story. If coexisting, they are sibling readings under the same kernel, and both story files (coordination_reading and behavioral_control_reading) reflect live positions held by different parties or different moments in the same institution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether behavioral-control and coordination readings are logically distinct or complementary framings of the same covenant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(hoa__tr_t25, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(hoa__be_t25, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 25, 0.41).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(hoa__su_t25, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(hoa__su_t40, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__behavioral_control_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hoa_covenant_scope kernel. Under the behavioral-control reading, the covenant functions as suppression and aesthetic conformity enforcement (Snare, ε≈0.42, substantial suppression, dead founding mandate). The coordination_reading file models the same covenant as infrastructure-maintenance coordination (Rope, low ε, minimal suppression, live founding mandate). The extraction_reading file models the covenant as board revenue generation via fine proliferation (Snare, high ε, high suppression, revenue-capture function). All three stories share the same kernel (the enforceable aesthetic covenant itself) but author different ε values, beneficiary/victim structures, and type classifications reflecting the different structural logic each reading ascribes. The three files are linked via network.affects_constraints to indicate family membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
