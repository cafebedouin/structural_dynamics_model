% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_coordination, []).

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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Statutory Boundary (Coordination Reading)
 *   domain: intellectual_property_law/technology_governance
 *
 * SUMMARY:
 *   The derivative-work statutory boundary in copyright law is a kernel that
 *   different legal interpretations read differently. This constraint
 *   instantiates the COORDINATION READING: the statutory boundary
 *   (distinguishing fixed recastings substantially incorporating original
 *   expression from transformative and intermediate uses) is designed to
 *   enable cultural innovation while protecting original creators' selective
 *   commercialization rights. Under this reading, the constraint imposes
 *   minimal extractiveness (ε=0.18, Rope) because both downstream creators
 *   and original creators benefit from clear rules: creators can transform
 *   freely without ex-ante licensing, while original creators retain
 *   attribution and licensing options. This reading treats the
 *   derivative-work doctrine as solving a genuine coordination problem — how
 *   to balance creator incentives with cultural evolution — rather than as an
 *   enclosure regime (enclosure_reading) or a hybrid permission system
 *   (hybrid_carveout_reading). The constraint's extractiveness has drifted
 *   upward slightly over time as enforcement mechanisms (takedown notices,
 *   litigation threats) have become more theatrical, but the core
 *   coordination function remains intact. The coordination reading is
 *   distinguished from the enclosure reading (which sees any use as potential
 *   infringement) and the hybrid reading (which permits non-commercial
 *   transformative use but requires licensing for commercial use). The
 *   constraint's theater ratio reflects the increasing performative character
 *   of copyright enforcement: DMCA takedown notices and cease-and-desist
 *   letters attempt to re-enclose what the fair-use doctrine has nominally
 *   opened.
 *
 * KEY AGENTS:
 *   - Downstream Creators: Transformative users (moderate/mobile) — benefit from clear non-infringing status of transformative use; can incorporate and remix without ex-ante licensing
 *   - Original Creators: Attribution + selective licensing (institutional/arbitrage) — benefit from statutory recognition and ability to license commercial derivatives; attribution norms provide visibility incentive
 *   - ML Training Ecosystem: Intermediate use operators (organized/mobile) — benefit from treating training as transformative use; can scale without licensing barriers
 *   - Open Culture Movement: Creative commons advocates (organized/constrained) — benefit from transformative-use scaffolding enabling remix and fan works; scaffolding has sunset as licensing norms mature
 *   - Copyright Enforcement Apparatus: Licensing and takedown infrastructure (institutional/arbitrage) — maintains performative enforcement even as statutory boundary limits actual liability; persists through institutional inertia
 *   - Analytical Observer: Civilizational/universal interpreter (analytical/analytical) — sees the coordination reading as the structurally coherent interpretation; distinguishes from enclosure and hybrid carveout readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.18).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.12).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Statutory Boundary (Coordination Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property_law/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, 'efa0c059-91df-463b-a22d-1e152764db2d').
narrative_ontology:cs_kernel_codification('efa0c059-91df-463b-a22d-1e152764db2d', fixed_text).
narrative_ontology:cs_authority_grounding('efa0c059-91df-463b-a22d-1e152764db2d', lineage).
narrative_ontology:cs_interpretation_layer_present('efa0c059-91df-463b-a22d-1e152764db2d').
narrative_ontology:cs_reading_relation('efa0c059-91df-463b-a22d-1e152764db2d', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('efa0c059-91df-463b-a22d-1e152764db2d', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('efa0c059-91df-463b-a22d-1e152764db2d', foundational, transformative_use_presumptively_noninfringing).
narrative_ontology:cs_axiom_status(transformative_use_presumptively_noninfringing, holdable).
narrative_ontology:cs_axiom_grounding('efa0c059-91df-463b-a22d-1e152764db2d', transformative_use_presumptively_noninfringing, conventional).
narrative_ontology:cs_axiom('efa0c059-91df-463b-a22d-1e152764db2d', foundational, downstream_creator_access_coordination_value).
narrative_ontology:cs_axiom_status(downstream_creator_access_coordination_value, holdable).
narrative_ontology:cs_axiom_grounding('efa0c059-91df-463b-a22d-1e152764db2d', downstream_creator_access_coordination_value, instrumental).
narrative_ontology:cs_reference_frame('efa0c059-91df-463b-a22d-1e152764db2d', fair_use_coordination_framework).
narrative_ontology:cs_drift_state('efa0c059-91df-463b-a22d-1e152764db2d', contemporary_digital_generative_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('efa0c059-91df-463b-a22d-1e152764db2d', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, downstream_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ml_training_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM CREATOR (ROPE) — Under the coordination reading, transformative use is non-infringing; downstream creators can incorporate copyrighted expression into new works without ex-ante licensing. The constraint coordinates two legitimate interests: original creator receives statutory recognition; downstream creator receives access to build upon cultural commons. Mobile exit option reflects that the creator can choose alternative approaches, rework content, or license if needed. This is genuine coordination with minimal coercive overhead.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__coordination_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: ORIGINAL CREATOR (ROPE) — The coordination reading protects original creators' interests through attribution norms, moral rights, and statutory recognition of their work's creative contribution. The creator benefits from the constraint: transformative use that cites the original creates value (visibility, influence) while preserving the creator's ability to license derivative works in markets where they choose exclusivity (commercial films, premium derivatives). Arbitrage exit reflects institutional capacity to license or negotiate selectively. Net coordination: both parties benefit from a clear boundary.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__coordination_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ML TRAINING ECOSYSTEM (ROPE) — The coordination reading treats ML training on copyrighted text as a transformative intermediate use: the model learns statistical patterns, not fixed recastings of original expression. The constraint coordinates two legitimate interests: creators' control over commercial derivatives; ML developers' access to training data. The ecosystem has mobile exit options (synthetic data, licensed datasets, public domain corpora). The constraint provides clear rules enabling large-scale training while preserving creator attribution and licensing options.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__coordination_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN CULTURE MOVEMENT (SCAFFOLD) — The coordination reading provides temporary scaffolding for creative collaboration: remix culture, fan works, academic adaptation, and accessibility improvements can legally exist as transformative uses while the cultural ownership model transitions from scarcity-based licensing to open attribution. The constraint has a sunset clause built into its logic: as open-source and creative-commons licensing mature, the legal need for fair-use scaffolding diminishes. Constrained exit reflects that movement actors must navigate the legal uncertainty; as clarity consolidates around transformative-use doctrine, the scaffolding becomes unnecessary.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__coordination_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COPYRIGHT ENFORCEMENT APPARATUS (PITON) — Under the coordination reading, copyright enforcement is largely performative and degraded: the transformative-use doctrine has rendered the statutory boundary porous. Courts must determine what counts as 'substantially incorporating original expression,' which is fact-intensive and unpredictable. Licensing (DMCA, cease-and-desist letters) attempts to re-enclose what the statute nominally permits. The apparatus persists through institutional inertia (copyright as default framework) even though its primary function (preventing unauthorized derivatives) has atrophied in practice. Theater ratio is high because enforcement generates theatrical performance (takedown notices, litigation threats) while underlying coordination happens through fair-use norms, not licensing.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__coordination_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational/universal view, the coordination reading sees the statutory boundary as solving a fundamental coordination problem: how to enable cultural evolution (downstream creators building on prior work) while protecting original creators' authorship claims and selective commercialization rights. This is not a natural law but a designed institutional solution. The constraint achieves minimal extractiveness because it preserves both parties' legitimate interests without ex-ante coercion. The analytical position treats the coordination reading as the structurally coherent interpretation of copyright's purpose, distinct from enclosure (maximizing creator rents) and hybrid carveout (ad-hoc permission boundaries).
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__coordination_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, TR),
    TR >= 0.70.

:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low, consistent with Rope classification. The coordination reading assumes the statutory boundary is designed to enable both creator protection and cultural innovation. Downstream creators experience low extraction because transformative use is non-infringing (no licensing requirement, no permission sought). Original creators experience low extraction because they retain attribution, moral rights, and selective licensing options — they benefit from visibility effects of transformative works citing them. The constraint imposes minimal coercive overhead: both parties' interests are protected within the stated boundary. Extractiveness has drifted upward slightly (0.12→0.18→0.22) as enforcement rhetoric has intensified, but the underlying coordination function remains. Suppression (0.12): Very low. The constraint explicitly permits transformative use without prior authorization, creating minimal barriers to downstream creation. Alternatives exist (licensing, independent creation, public domain use) and are genuinely accessible. Theater ratio (0.35): Moderate. The fair-use doctrine creates some performative activity (litigation risk, good-faith determination of transformativeness), but the coordination mechanism itself is not primarily theatrical. The theater has increased over time as enforcement mechanisms (DMCA, takedown letters) have become more prominent, even though the statutory boundary limits their legal effect.
 *
 * PERSPECTIVAL GAP:
 *   Unified across the coordination reading itself, but contested between readings. Under this reading, all perspectives classify as low-extraction types (Rope, Scaffold, Piton) because the constraint is genuinely coordinating both creator interests. Under the enclosure reading, downstream creators classify as Snare (trapped, powerless, experiencing high extraction) while original creators classify as Rope (beneficiary, arbitrage). The gap between readings is what matters: the coordination reading sees a win-win structure; the enclosure reading sees extraction benefiting original creators at downstream creators' expense; the hybrid reading sees conditional extraction (permitted for non-commercial, prohibited for commercial). This kernel contestation is irreducible within the legal system — different circuits may adopt different readings, creating regional variation.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the coordination reading, directionality values (d) are derived from the beneficiary structure. Downstream creators (transformative users) are primary beneficiaries — they gain access to copyrighted expression for creative building without licensing costs. This low-beneficiary status produces d ≈ 0.15-0.20, yielding low f(d) and thus low χ. Original creators are also beneficiaries — they gain attribution, visibility, and licensing options for commercial derivatives. The constraint does not extract from either party; it coordinates their interests. The lack of identified victims (empty `base_properties.victims[]`) confirms the coordination reading: no group bears extractive costs. This contrasts sharply with the enclosure reading (which identifies downstream creators as victims, producing d ≈ 0.90, high χ, Snare) and the hybrid reading (which identifies both as partial beneficiaries and partial victims, producing moderate d and Tangled Rope). The directionality chain — beneficiary structure → d derivation → χ computation → classification — makes the reading choice structurally precise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_use_boundaries,
    'What structural criteria distinguish transformative use (non-infringing) from fixed recasting (infringing derivative work)? Where is the boundary operationalized?',
    'Empirical analysis of fair-use case outcomes: codification of factors courts consistently apply to transformative-use determination; comparison with statutory language and legislative history',
    'If boundary is clear and predictable: coordination reading holds (low ε rope). If boundary remains fact-specific and unpredictable: judges function as gatekeepers making case-by-case licensing decisions, shifting toward enclosure reading (high ε snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_boundaries, empirical, 'Operationalization of transformative vs. fixed recasting boundary').

omega_variable(
    ml_training_transformativeness,
    'Does machine learning training on copyrighted text constitute transformative use (non-infringing intermediate use) or derivative work preparation (infringing)?',
    'Statutory interpretation of ''substantially incorporating original expression''; empirical analysis of whether ML models memorize vs. learn statistical patterns; legislative intent regarding technological intermediate uses',
    'If ML training is transformative use: coordination reading confirmed; generative AI ecosystem operates under rope logic (low extraction, clear rules). If ML training requires licensing: shifts toward hybrid carveout (commercial ML requires permission) or enclosure (all ML derivative).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ml_training_transformativeness, empirical, 'ML training status under transformative-use doctrine').

omega_variable(
    reading_contestation_in_statute,
    'Which reading (coordination, enclosure, hybrid carveout) is mandated by the statutory text ''derivative work'' and ''preparation of derivative work''? Is the boundary set by statute or by judicial interpretation of fair use?',
    'Statutory linguistics of 17 U.S.C. § 103 and § 106(2); legislative history of Copyright Act revisions (1976, 1998, digital); case law evolution showing whether courts narrow or expand ''substantially incorporating''',
    'If statute clearly instantiates coordination reading: lower dispute costs, predictable licensing. If statute is ambiguous: different courts adopt different readings, creating regional variation and forum shopping. If legislature can revise statute: coordination reading is contingent on political will, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_in_statute, conceptual, 'Statutory determination of derivative-work boundary reading').

omega_variable(
    cultural_commons_value_capture,
    'Who captures the economic value created when downstream creators produce transformative works? Does the coordination reading allow original creators to extract surplus from transformation value?',
    'Empirical analysis of licensing markets for derivative works; comparison of enforcement rates and settlement patterns across transformative-use categories; attribution value measurement (citation effects, visibility gains)',
    'If original creators capture significant value through licensing and attribution effects: coordination is genuine (both parties benefit). If downstream creators capture all transformation value while original creators receive only attribution: coordination reading masks extraction benefiting downstream creators (shift toward hybrid reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_commons_value_capture, empirical, 'Value distribution in transformative-use coordination').

omega_variable(
    kernel_reading_contestation,
    'Is the derivative-work statutory boundary best read as a coordination mechanism (this reading), an enclosure regime (enclosure reading), or a hybrid permission system (hybrid carveout reading)?',
    'Structural analysis of case outcomes, licensing practices, and enforcement patterns; comparison of relative extractiveness under each reading; examination of legislative intent and statutory language',
    'Different readings produce different ε values (0.18 rope vs. 0.68 snare vs. 0.48 tangled_rope) and different beneficiary/victim assignments. The reading chosen determines whether the constraint is experienced as enabling (rope), coercive (snare), or mixed (tangled_rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the derivative-work kernel is correct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deriv_coord_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(deriv_coord_tr_t10, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(deriv_coord_tr_t20, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(deriv_coord_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(deriv_coord_be_t10, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(deriv_coord_be_t20, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 20, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, information_standard).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine_operationalization).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, ml_training_intermediate_use).

% DUAL FORMULATION NOTE:
% The derivative-work statutory boundary kernel has three structurally distinct readings with different ε values: coordination_reading (0.18 Rope), enclosure_reading (0.68 Snare), and hybrid_carveout_reading (0.48 Tangled Rope). Each reading is a separate constraint story linked by network.affects_constraints. The epsilon values reflect different interpretations of the same statute, not different observables. Which reading is correct is a contested legal question; all three are live positions in copyright jurisprudence. This story is the coordination reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
