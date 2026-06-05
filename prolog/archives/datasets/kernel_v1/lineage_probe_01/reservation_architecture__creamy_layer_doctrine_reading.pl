% ============================================================================
% CONSTRAINT STORY: reservation_architecture__creamy_layer_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reservation_architecture__creamy_layer_doctrine_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: reservation_architecture__creamy_layer_doctrine_reading
 *   human_readable: Creamy Layer Doctrine: Intra-Class Capture Suppression in Reservation Architecture
 *   domain: legal/constitutional/social_policy
 *
 * SUMMARY:
 *   The creamy layer doctrine in Indian constitutional law represents a
 *   mechanism for suppressing intra-class capture within reservation systems.
 *   The doctrine excludes 'advanced members' of backward classes — typically
 *   defined by income/property thresholds — from the benefits of quotas
 *   nominally allocated to remedial groups. The logical structure is:
 *   reservations are justified by structural deprivation; if the most
 *   advantaged members of a deprived group capture quota slots, the benefit
 *   reaches the wrong target and the justification collapses. The creamy
 *   layer doctrine polices against this capture, attempting to align quota
 *   allocation with the need it is designed to remedy. This constraint
 *   exhibits a perspectival cascade: the genuinely deprived see an extraction
 *   mechanism that fails to reach them (snare); the advanced members see
 *   mixed coordination and suppression (tangled rope); the elite see a
 *   performative constraint (piton); the constitutional framers' intent sees
 *   pure coordination (rope); the social justice movement sees both
 *   coordination and extractive surveillance (tangled rope); the analytical
 *   observer risks naturalizing the doctrine as an immutable feature of
 *   redistribution (mountain). The measurement trajectory shows rising
 *   suppression (0.35→0.52) as the doctrine's administrative apparatus
 *   matured, rising extractiveness (0.22→0.38) as intra-group heterogeneity
 *   increased with economic development, and rising theater (0.30→0.45) as
 *   the doctrine accumulated procedural complexity.
 *
 * KEY AGENTS:
 *   - Genuinely Deprived Stratum: Primary intended beneficiary (powerless/trapped) — those whose structural deprivation justifies the quota, but who may lack exam preparation, access to coaching, or information about application procedures; may be excluded by creamy layer thresholds that don't capture their deprivation
 *   - Advanced Members of Beneficiary Classes: Primary victims of creamy layer exclusion (moderate/constrained) — educated members of backward castes/classes whose families accumulated modest assets or education, now excluded from quota benefits by income/property tests
 *   - Creamy Layer Elite: Nominally excluded agents (institutional/arbitrage) — most advantaged members of beneficiary groups; continue accessing elite institutions through general merit category and resource arbitrage; experience exclusion as theatrical
 *   - Constitutional Framers' Equality Intent: Institutional beneficiary (institutional/arbitrage) — the principle of substantive equality and the intent to reach the most disadvantaged; creamy layer doctrine aligns policy with this commitment
 *   - Social Justice Movement Coalition: Organized agents (organized/constrained) — civil rights groups, Dalit movements, OBC organizing; need quotas but also experience means-testing surveillance and internalized stigma from creamy layer policing
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the doctrine as a logical necessity while missing its constructed features and extractive mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reservation_architecture__creamy_layer_doctrine_reading, 0.38).
domain_priors:suppression_score(reservation_architecture__creamy_layer_doctrine_reading, 0.52).
domain_priors:theater_ratio(reservation_architecture__creamy_layer_doctrine_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reservation_architecture__creamy_layer_doctrine_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(reservation_architecture__creamy_layer_doctrine_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reservation_architecture__creamy_layer_doctrine_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reservation_architecture__creamy_layer_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(reservation_architecture__creamy_layer_doctrine_reading, "Creamy Layer Doctrine: Intra-Class Capture Suppression in Reservation Architecture").
narrative_ontology:topic_domain(reservation_architecture__creamy_layer_doctrine_reading, "legal/constitutional/social_policy").

domain_priors:requires_active_enforcement(reservation_architecture__creamy_layer_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reservation_architecture__creamy_layer_doctrine_reading, 'bf0f414d-ddc1-4282-a87e-7be71271d640').
narrative_ontology:cs_kernel_codification('bf0f414d-ddc1-4282-a87e-7be71271d640', formalized).
narrative_ontology:cs_authority_grounding('bf0f414d-ddc1-4282-a87e-7be71271d640', lineage).
narrative_ontology:cs_interpretation_layer_present('bf0f414d-ddc1-4282-a87e-7be71271d640').
narrative_ontology:cs_reading_relation('bf0f414d-ddc1-4282-a87e-7be71271d640', reservation_architecture__mandal_expansion_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf0f414d-ddc1-4282-a87e-7be71271d640', reservation_architecture__substantive_equality_engine_reading, influences).
narrative_ontology:cs_axiom('bf0f414d-ddc1-4282-a87e-7be71271d640', foundational, deprivation_based_targeting).
narrative_ontology:cs_axiom_status(deprivation_based_targeting, holdable).
narrative_ontology:cs_axiom_grounding('bf0f414d-ddc1-4282-a87e-7be71271d640', deprivation_based_targeting, deontological).
narrative_ontology:cs_axiom('bf0f414d-ddc1-4282-a87e-7be71271d640', secondary, administrative_targeting_feasible).
narrative_ontology:cs_axiom_status(administrative_targeting_feasible, holdable).
narrative_ontology:cs_axiom_grounding('bf0f414d-ddc1-4282-a87e-7be71271d640', administrative_targeting_feasible, empirically_contingent).
narrative_ontology:cs_reference_frame('bf0f414d-ddc1-4282-a87e-7be71271d640', equity_through_precision_targeting).
narrative_ontology:cs_drift_state('bf0f414d-ddc1-4282-a87e-7be71271d640', contemporary_economic_heterogeneity_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bf0f414d-ddc1-4282-a87e-7be71271d640', '2026-02-27T00:00:00Z').
narrative_ontology:cs_kernel_id(reservation_architecture__creamy_layer_doctrine_reading, reservation_architecture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reservation_architecture__creamy_layer_doctrine_reading, genuinely_deprived_stratum).
narrative_ontology:constraint_beneficiary(reservation_architecture__creamy_layer_doctrine_reading, constitutional_framers_equality_intent).
narrative_ontology:constraint_victim(reservation_architecture__creamy_layer_doctrine_reading, advanced_members_of_beneficiary_classes).
narrative_ontology:constraint_victim(reservation_architecture__creamy_layer_doctrine_reading, creamy_layer_elite_candidates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENUINELY DEPRIVED STRATUM (SNARE) — The quota nominally targets this group, but without creamy layer exclusion, quota benefits leak to the advanced members of the same caste/class who can navigate exams, coaching, and application machinery. The genuinely deprived cannot exit this extraction — they remain structurally excluded even when the quota exists in their name. Maximum experienced extraction: the benefit that was supposed to reach them reaches others instead.
constraint_indexing:constraint_classification(reservation_architecture__creamy_layer_doctrine_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADVANCED MEMBERS OF BENEFICIARY CLASSES (TANGLED ROPE) — Experience genuine coordination benefit (quota enables their access to professional opportunities they might not have absent caste-based institutional barriers) alongside real extraction by creamy layer exclusion. The doctrine suppresses their opportunity within the quota by imposing income/property thresholds. They are constrained agents — educated, exam-capable, but subject to explicit exclusion. Mixed experience: benefit from quota structure, cost from the suppression mechanism.
constraint_indexing:constraint_classification(reservation_architecture__creamy_layer_doctrine_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREAMY LAYER ELITE (PITON) — Nominally excluded by the doctrine but continue to access elite institutions through open merit, private education pipelines, and resource arbitrage. Experience the creamy layer exclusion as performative constraint: they were never the target, and their exit pathways (general merit category, private universities, abroad) make the exclusion theatrical. The institutional apparatus appears to regulate them while functionally accommodating them.
constraint_indexing:constraint_classification(reservation_architecture__creamy_layer_doctrine_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL FRAMERS' EQUALITY INTENT (ROPE) — The creamy layer doctrine is a coordination mechanism that aligns quota allocations with the original constitutional commitment to reach the most disadvantaged. From this institutional perspective, the doctrine is pure coordination — translating the equality principle into operational policy that targets benefit where need is highest. No extraction, pure alignment of policy means to constitutional ends.
constraint_indexing:constraint_classification(reservation_architecture__creamy_layer_doctrine_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SOCIAL JUSTICE MOVEMENT COALITION (TANGLED ROPE) — Organized actors pushing for substantive equality (civil rights groups, Dalit movements, OBC organizing) experience the creamy layer doctrine as both coordination (it prevents elite capture of remedial quotas) and extraction (it imposes surveillance and means-testing on beneficiary communities, creating internalized stigma and administrative friction). Constrained by both the need for quota access and the policing mechanisms the doctrine requires.
constraint_indexing:constraint_classification(reservation_architecture__creamy_layer_doctrine_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, social stratification creates structural barriers to equal access — some mechanism must suppress intra-group capture or remedial allocations default to the already-privileged. The creamy layer doctrine might appear as an immutable logical requirement: any effective redistribution in a stratified society requires policing against elite capture. However, this perspective naturalizes what is a contingent institutional choice and may mask the doctrine's own extractive mechanisms (surveillance, means-testing, internalized shame).
constraint_indexing:constraint_classification(reservation_architecture__creamy_layer_doctrine_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reservation_architecture__creamy_layer_doctrine_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reservation_architecture__creamy_layer_doctrine_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reservation_architecture__creamy_layer_doctrine_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(reservation_architecture__creamy_layer_doctrine_reading, TR),
    TR >= 0.70.

:- end_tests(reservation_architecture__creamy_layer_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, increasing over time. The creamy layer doctrine's extractiveness is not uniform across beneficiary groups. For the genuinely deprived stratum, extractiveness is high (snare-level, ~0.72) because the doctrine fails to reach them — the quota exists in their name but benefits leak to more advantaged co-members. For the advanced members, extractiveness is moderate (~0.40) because they do benefit from the quota system itself, but face suppression through exclusion. The aggregate value (0.38) reflects a mixed experienced reality. The trajectory (0.22→0.38) shows rising extractiveness as India's economic development increased intra-group heterogeneity — wealthier families within backward groups could accumulate resources and education, making creamy layer thresholds more exclusionary. Suppression (0.52): Moderate-high, and rising. The suppression includes structural barriers (exam-based gatekeeping, coaching costs, application complexity) and doctrine-specific barriers (means-testing, income verification, family asset investigation). Theater ratio (0.45): Moderate, increasing slightly. The doctrine itself is largely functional (income thresholds do identify resource levels) but accumulates performative elements as the administrative apparatus grows — means-testing rituals, certification processes, appeals procedures. The rise from 0.30 to 0.45 reflects that while the core targeting mechanism remains functional, the documentation and compliance burden has theatricalized the doctrine's application.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the structural tension between the doctrine's stated purpose (reaching the genuinely deprived) and its actual allocation mechanism (targeting via income thresholds, which may miss structural deprivation not captured by income, and may exclude capable candidates from families with modest accumulated wealth). The genuinely deprived see extraction because the quota nominally targets them but benefits leak to the advanced members their family's modest education or property holdings are excluded by the creamy layer test. The advanced members see coordination (quota enabled their access that caste discrimination would have blocked) but also extraction (they are excluded by a suppression mechanism that may not accurately identify who is genuinely deprived within their caste). The elite see the doctrine as performative — they were never the target, and their arbitrage options make the exclusion theatrical. The constitutional framers' intent sees the doctrine as pure coordination — aligning allocations with the equality principle. The social justice movement sees both — the coordination function of preventing elite capture, but also the extractive machinery of surveillance and internalized shame that comes with means-testing regimes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and exit options. The genuinely deprived stratum bears costs (victims) and has no exit (trapped) → high d → high f(d) → maximum experienced extraction. The advanced members of beneficiary classes are both beneficiaries (quota access) and victims (creamy layer exclusion) with moderate constrained exit → moderate d → moderate experienced extraction. The creamy layer elite are nominally victims but have arbitrage options (open merit, private institutions) → low d → low experienced extraction. The constitutional intent is beneficiary with institutional arbitrage → low d → negative f(d). Each group's perspective reflects their position in the directionality pipeline. The perspectival gap is large: powerless agents see snare; moderate agents see tangled rope; institutional actors see rope or piton; organized agents see both coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING: This constraint is the creamy_layer_doctrine_reading of the reservation_architecture kernel. The mandatrophy is structured by the relationship between this reading and its siblings. The creamy_layer_doctrine_reading emphasizes precision targeting to suppress intra-class capture: the extraction is policed against the beneficiary group's own advanced members. The mandal_expansion_reading emphasizes quota growth from ~15% to ~50% and inclusion of numerically significant backward castes (OBCs), shifting the architecture from rescue of a few to allocation among the many — this reading coexists with the creamy layer reading (both are live positions in Indian constitutional politics) but applies the expansion logic to the entire quota pool, potentially undermining creamy layer precision. The substantive_equality_engine_reading grounds reservations in the constitutional meaning of equality itself — equality is not identical treatment but treatment calibrated to eliminate structural disadvantage — this reading influences the creamy layer reading by asking whether any targeting mechanism can achieve equality, or whether reservations must be understood as remaking the equality principle itself. The mandatrophy dissolves when the three readings are recognized as operating on different analytical frames: individual targeting precision (creamy layer), aggregate political allocation (mandal), and constitutional interpretation (substantive equality). No single frame subsumes the others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_targeting_accuracy,
    'Do income/property thresholds for creamy layer exclusion accurately identify the genuinely deprived within each beneficiary class, or do they exclude capable candidates whose families have accumulated modest assets through community effort without achieving generational wealth?',
    'Longitudinal tracking of quota beneficiaries: comparison of outcomes for those just above vs. just below creamy layer thresholds; analysis of asset accumulation patterns within beneficiary communities to establish whether threshold captures structural deprivation or penalizes community success',
    'If thresholds are accurate: creamy layer is pure coordination (rope), suppressing genuine intra-class capture. If thresholds are crude: creamy layer is extractive (snare from advanced members'' perspective), suppressing legitimate candidates whose families built modest prosperity through occupational effort rather than caste privilege.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_targeting_accuracy, empirical, 'Accuracy of creamy layer income thresholds in identifying structural deprivation').

omega_variable(
    reading_contingency_vs_necessity,
    'Is the creamy layer doctrine a contingent policy choice among multiple possible designs for suppressing intra-class capture, or a logically necessary element of any effective redistribution in a stratified society?',
    'Comparative institutional analysis: examine redistributive policies in other stratified societies (post-apartheid South Africa, post-colonial economies, class-based affirmative action jurisdictions) to identify whether creamy-layer-equivalent mechanisms are universal features or context-dependent choices',
    'If contingent: the doctrine''s naturalness is a false summit — the mountain perspective misidentifies constructed policy as natural law. If necessary: the mountain perspective captures a real structural constraint, and the question shifts to how to implement it with minimal extractive overhead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contingency_vs_necessity, conceptual, 'Whether creamy layer is logically necessary or institutionally contingent').

omega_variable(
    intra_class_heterogeneity_measurement,
    'Within each caste/class group targeted by reservations, what is the actual distribution of educational access, exam performance capacity, and family resource availability? Is intra-group inequality as large as inter-group inequality, or smaller?',
    'Empirical analysis: compare Gini coefficients and percentile distributions of resource access within vs. between beneficiary groups; identify whether creamy layer exclusion targets meaningful stratification within the group or artifacts of measurement',
    'If intra-group inequality is substantial: creamy layer targeting is addressing a real structural feature (coordination). If intra-group inequality is artifactual or measurement-driven: creamy layer is suppressing agents who are legitimately within the disadvantaged group (extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intra_class_heterogeneity_measurement, empirical, 'Measurement and comparison of intra-class vs. inter-class inequality').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the reservation_architecture kernel. What distinguishes this creamy_layer_doctrine_reading from the mandal_expansion_reading (which emphasizes quota growth and majoritarian politics) and the substantive_equality_engine_reading (which locates reservations in the equality principle itself)?',
    'Doctrinal analysis: this reading emphasizes suppression of intra-class capture and targeting toward the genuinely deprived; mandal reading emphasizes allocation growth and political coalitions; substantive equality reading emphasizes the constitutional meaning of equality. The readings coexist because they occupy different analytical frames — individual targeting, aggregate allocation, constitutional interpretation — and no single frame subsumes the others.',
    'Each reading produces different policy implications: creamy layer emphasizes precision targeting; mandal reading supports expansion to include new groups; substantive equality reading challenges the entire framing of reservations as benefit allocation rather than equality realization. The kernel contest is unresolved because the frames are incommensurable within a single institutional perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The reading''s relationship to sibling kernel interpretations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reservation_architecture__creamy_layer_doctrine_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(creamy_theater_t0_1950s, reservation_architecture__creamy_layer_doctrine_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(creamy_theater_t5_1980s, reservation_architecture__creamy_layer_doctrine_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(creamy_theater_t10_2010s, reservation_architecture__creamy_layer_doctrine_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(creamy_extract_t0_1950s, reservation_architecture__creamy_layer_doctrine_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(creamy_extract_t5_1980s, reservation_architecture__creamy_layer_doctrine_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(creamy_extract_t10_2010s, reservation_architecture__creamy_layer_doctrine_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(creamy_suppress_t0_1950s, reservation_architecture__creamy_layer_doctrine_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(creamy_suppress_t5_1980s, reservation_architecture__creamy_layer_doctrine_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(creamy_suppress_t10_2010s, reservation_architecture__creamy_layer_doctrine_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reservation_architecture__creamy_layer_doctrine_reading, resource_allocation).
narrative_ontology:affects_constraint(reservation_architecture__creamy_layer_doctrine_reading, reservation_architecture__mandal_expansion_reading).
narrative_ontology:affects_constraint(reservation_architecture__creamy_layer_doctrine_reading, reservation_architecture__substantive_equality_engine_reading).

% DUAL FORMULATION NOTE:
% The creamy layer doctrine is one reading of the contested reservation_architecture kernel. The other readings (mandal expansion, substantive equality engine) decompose the same constitutional commitment into different structural mechanisms. All three stories share the same kernel (the constitutional remedy for caste/class deprivation) but instantiate different doctrinal commitments. The creamy layer reading emphasizes precision targeting; the mandal reading emphasizes expansion and majoritarian inclusion; the substantive equality reading emphasizes the transformation of equality's constitutional meaning. Each has its own extractiveness value and beneficiary/victim structure because each reading implies a different allocation mechanism and different patterns of who benefits and who bears costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reservation_architecture__creamy_layer_doctrine_reading, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
