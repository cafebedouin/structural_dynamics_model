% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__coordination_reading, []).

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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant as Shared Infrastructure Coordination (Coordination Reading)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint instantiates the coordination reading of a contested
 *   kernel: the legitimate scope and function of HOA covenants in residential
 *   development. Under the coordination reading, HOA covenants serve a
 *   genuine collective action function — coordinating shared infrastructure
 *   maintenance (roads, drainage, common areas) that create positive
 *   externalities for all homeowners. The covenant solves the problem that
 *   individual homeowners lack incentive to maintain shared infrastructure
 *   that benefits them symmetrically, and the municipality lacks capacity to
 *   manage micro-level infrastructure in private communities. This reading
 *   emphasizes that covenant enforcement is narrowly scoped to infrastructure
 *   standards and cost allocation, not behavioral control or homogeneity
 *   enforcement. The constraint exhibits low extractiveness (0.12) because
 *   coordination costs are transparent, proportional, and reciprocal —
 *   homeowners benefit symmetrically from collective action. Suppression is
 *   moderate (0.18) because enforcement mechanisms exist (liens, repair
 *   notices) but are proportional to actual coordination objectives and do
 *   not coerce behavior beyond infrastructure requirements. Theater ratio is
 *   low (0.35) because covenant rules specify material outcomes (roof
 *   replacement timelines, drainage maintenance standards) rather than
 *   performative compliance. This reading coexists with two sibling readings
 *   — the behavioral control reading (which emphasizes how covenants regulate
 *   lifestyle, aesthetics, and social composition, shifting focus from
 *   infrastructure to homogeneity) and the extraction reading (which
 *   emphasizes how HOA boards abuse enforcement powers for profit or
 *   control). All three readings invoke the same fixture (written HOA
 *   covenants and board enforcement), but diverge on whether the primary
 *   mechanism is infrastructure coordination, behavioral constraint, or
 *   institutional extraction.
 *
 * KEY AGENTS:
 *   - Homeowners (collective): Primary beneficiary and participant — benefit from coordinated infrastructure maintenance that individual incentives would not provide
 *   - Municipal Planning Authority: Secondary beneficiary — offloads legitimate infrastructure coordination functions to private collective action; reduces public budget burden
 *   - Free-riders/Shirkers: Enforcement target — homeowners who avoid maintenance costs or infrastructure contributions; constrained by proportional enforcement tied to objective standards
 *   - HOA Board: Institutional actor managing coordination mechanism — interprets covenant scope and administers enforcement; potential capture point under competing readings
 *   - Analytical Observer: Sees the coordination function clearly; examines whether the constraint remains coordination or drifts toward extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.12).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.18).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant as Shared Infrastructure Coordination (Coordination Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '19d1aacd-4c3d-4fe6-aad2-99999f7a437e').
narrative_ontology:cs_kernel_codification('19d1aacd-4c3d-4fe6-aad2-99999f7a437e', formalized).
narrative_ontology:cs_authority_grounding('19d1aacd-4c3d-4fe6-aad2-99999f7a437e', practice).
narrative_ontology:cs_interpretation_layer_present('19d1aacd-4c3d-4fe6-aad2-99999f7a437e').
narrative_ontology:cs_reading_relation('19d1aacd-4c3d-4fe6-aad2-99999f7a437e', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('19d1aacd-4c3d-4fe6-aad2-99999f7a437e', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('19d1aacd-4c3d-4fe6-aad2-99999f7a437e', foundational, covenants_coordinate_infrastructure_only).
narrative_ontology:cs_axiom_status(covenants_coordinate_infrastructure_only, holdable).
narrative_ontology:cs_axiom_grounding('19d1aacd-4c3d-4fe6-aad2-99999f7a437e', covenants_coordinate_infrastructure_only, empirically_contingent).
narrative_ontology:cs_axiom('19d1aacd-4c3d-4fe6-aad2-99999f7a437e', foundational, symmetric_cost_benefit_distribution).
narrative_ontology:cs_axiom_status(symmetric_cost_benefit_distribution, holdable).
narrative_ontology:cs_axiom_grounding('19d1aacd-4c3d-4fe6-aad2-99999f7a437e', symmetric_cost_benefit_distribution, conventional).
narrative_ontology:cs_reference_frame('19d1aacd-4c3d-4fe6-aad2-99999f7a437e', covenants_as_coordination_devices).
narrative_ontology:cs_drift_state('19d1aacd-4c3d-4fe6-aad2-99999f7a437e', contemporary_enforcement_scope_expansion, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('19d1aacd-4c3d-4fe6-aad2-99999f7a437e', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOMEOWNER PARTICIPANT (ROPE) — Views covenant as genuine coordination mechanism solving collective action problem: shared road maintenance, drainage, common area upkeep create positive externalities for all. Benefits from others' compliance; sees covenant as reciprocal (I maintain my share, you maintain yours). Cost of coordination (HOA fees) is transparent and proportional. Constrained exit because leaving means loss of property value (tied to covenant compliance), but not trapped — can sell and relocate. Experiences this as low-extraction coordination.
constraint_indexing:constraint_classification(hoa_covenant_scope__coordination_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: MUNICIPAL PLANNER (ROPE) — Views covenant infrastructure coordination as offloading legitimate municipal functions (stormwater management, road maintenance, common area upkeep) to private collective action. Benefits from reduced public burden; homeowners benefit from locally-responsive maintenance. Low extractiveness because the coordination function is genuine and the distribution of costs/benefits is symmetric. Arbitrage exit because the municipality can always choose to municipalize services instead.
constraint_indexing:constraint_classification(hoa_covenant_scope__coordination_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: FREE-RIDER (ROPE) — A homeowner who wants to avoid maintaining their property or contributing to shared costs faces enforcement (repair notices, liens, fines). But the enforcement is proportional and transparent — not coercive, but clear. Classification is rope rather than snare because: (a) enforcement scope is objectively defined (building codes, infrastructure standards), (b) exit option exists (sell the property), (c) the covenant's core function (infrastructure coordination) remains visible even from the free-rider position. The constraint coordinates; enforcement maintains the coordination.
constraint_indexing:constraint_classification(hoa_covenant_scope__coordination_reading, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — Views covenant through coordination lens: classically solves collective action problem in housing where individual incentives (defer maintenance, avoid costs) conflict with collective interest (preserve property value, functional infrastructure). Theater ratio is low (0.35) because the covenant's rules directly specify outcomes (roof replacement timeline, drainage maintenance, paint standards) tied to material infrastructure rather than performative compliance. Extractiveness is low (0.12) because cost allocation is transparent and tied to actual coordination costs, not to asymmetric power dynamics. This reading assumes covenants can function as pure coordination mechanisms absent extractive institutional capture.
constraint_indexing:constraint_classification(hoa_covenant_scope__coordination_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__coordination_reading_tests).
:- end_tests(hoa_covenant_scope__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Low. The coordination reading assumes that covenant enforcement is narrowly scoped to infrastructure costs and objective standards (building codes, drainage maintenance, road repair). Homeowners pay pro-rata shares of actual coordination costs; they benefit symmetrically from collective action outcomes. There is minimal asymmetric extraction because cost allocation is transparent and tied to real coordination requirements, not to board discretion or behavioral control. The measurement trajectory is nearly flat (0.10 → 0.14 over 20 years), indicating stable low extractiveness under the coordination reading. Suppression (0.18): Moderate-low. Enforcement mechanisms exist (repair notices, liens, fines) but are calibrated to achieve infrastructure objectives, not to maximize compliance or control. Homeowners have clear exit options (sell the property), though exit is costly (tied to property value). Suppression is bounded by the objective nature of infrastructure standards — boards cannot arbitrarily impose suppression. Theater ratio (0.35): Low. Covenant rules specify material outcomes (roof lifespan, gutter maintenance, drainage flow rates) rather than performative signals. Enforcement is proportional to actual infrastructure need, not ritualistic. The slight rise over 20 years (0.32 → 0.38) suggests minor drift toward more detailed specification of standards as complexity of community infrastructure increases, but remains well below the piton threshold (0.70) where theater would indicate degradation.
 *
 * PERSPECTIVAL GAP:
 *   The coordination reading produces unanimous or near-unanimous classification as Rope across most perspectives because all parties benefit from coordination. Homeowner and planner both see low extraction; free-riders see proportional enforcement, not coercion; the analytical observer sees genuine coordination function. The perspectival gap emerges only when comparing the coordination reading to the behavioral control reading (where homeowners perceive constraint classification differently depending on whether they emphasize infrastructure coordination or lifestyle regulation) or to the extraction reading (where the free-rider becomes a victim and enforcement becomes extractive). Within the coordination reading alone, the gap is minimal because the constraint appears to solve a genuine problem symmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners are declared as beneficiaries because the coordination reading assumes symmetric benefits — each homeowner benefits from others' infrastructure maintenance and pays pro-rata for coordination costs. No victims are declared because enforcement targets are viewed as free-riders within a coordination system, not as exploited parties. The free-rider perspective (Perspective 3) still classifies as Rope because exit is available (sell the property) and enforcement is proportional and transparent. Directionality d is derived from beneficiary status with constrained/mobile exit options — homeowners have some agency and can choose to comply or exit, reducing d toward the middle (d ≈ 0.35-0.45) rather than toward full targets (d > 0.85) or full beneficiaries (d < 0.15). The institutional planner's d is lower (toward 0.15-0.25) because they receive benefit (infrastructure offloading) without bearing enforcement burden.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the coordination reading produces internally consistent low-extractiveness classification (ε=0.12, Rope) IF the empirical assumption holds: that covenant enforcement is narrowly scoped to infrastructure coordination and objectively bounded. The three readings represent three different factual hypotheses about actual enforcement behavior, not three different interpretive framings of identical facts. The coordination reading is falsifiable — if empirical analysis (measuring actual enforcement scope) shows that boards systematically enforce behavioral rules beyond infrastructure standards, then the empirical basis for ε=0.12 collapses and the constraint drifts toward the behavioral control or extraction reading. The mandatrophy is not resolved by choosing a reading, but by examining evidence about actual enforcement patterns.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_scope_boundary_contestation,
    'Where is the boundary between legitimate infrastructure coordination (shared roads, drainage, common areas) and illegitimate behavioral control (aesthetic rules, lifestyle restrictions, privacy intrusion)?',
    'Comparative analysis of covenant enforcement scope across jurisdictions; historical tracking of which covenant rules are enforceable and which courts void as overreach; examination of which rules serve infrastructure coordination vs. which serve homogeneity/control',
    'If boundary is sharp and enforceable: coordination reading holds (ε ≈ 0.12). If boundary is contested and enforcement creeps: extraction reading becomes more accurate (ε > 0.40). If boundary is indeterminate: constraint is genuinely ambiguous between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_scope_boundary_contestation, conceptual, 'Boundary between legitimate infrastructure coordination and illegitimate behavioral control in covenant scope').

omega_variable(
    extractive_institutional_capture_likelihood,
    'Do HOA boards systematically become captured by developer interests or abuse enforcement powers to extract compliance from residents, even when the core coordination function is infrastructure-focused?',
    'Longitudinal study of enforcement patterns over time; comparison of enforcement intensity before and after developer exit; analysis of which rules are enforced and which are not (signals what the actual constraint mechanism is)',
    'If capture is rare and abuse is exceptional: coordination reading (Rope) holds empirically. If capture is systematic: extraction reading (Snare/Tangled Rope) is more empirically accurate, and the coordination reading becomes an ideological cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_institutional_capture_likelihood, empirical, 'Whether HOA boards systematically abuse enforcement powers or remain coordinating bodies').

omega_variable(
    reading_kernel_identity,
    'Is this constraint one reading of a contested kernel (the legitimate scope of HOA covenants), or are the competing readings describing structurally different constraints that should be decomposed?',
    'Examination of whether all three readings (coordination, behavioral control, extraction) invoke the same covenant structure and dispute its legitimate scope, or whether they describe causally distinct mechanisms that should be separate stories with different ε values',
    'If kernel reading: one fixture (covenant rules) has three interpretations. If decomposition needed: coordination reading (ε=0.12, Rope) is separate from behavioral control reading (ε=0.40+, Tangled Rope/Snare) based on actual scope of enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Whether this is a kernel reading or requires constraint decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_coord_tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(hoa_coord_tr_t10, hoa_covenant_scope__coordination_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(hoa_coord_tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(hoa_coord_be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hoa_coord_be_t10, hoa_covenant_scope__coordination_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(hoa_coord_be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.14).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The HOA covenant kernel is decomposed into three constraint stories corresponding to three readings of its legitimate scope. All three stories invoke the same written covenants and enforcement mechanisms, but model structurally different ε values based on different factual claims about enforcement scope. The coordination reading assumes enforcement is narrowly scoped to infrastructure (ε=0.12). The behavioral control reading assumes enforcement includes lifestyle/aesthetic regulation (ε ≈ 0.40-0.50). The extraction reading assumes enforcement is systematically abusive (ε ≥ 0.55). The three readings coexist in contemporary legal disputes; each is held as true by different parties and courts in different jurisdictions. Empirical resolution would require measuring actual enforcement patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
