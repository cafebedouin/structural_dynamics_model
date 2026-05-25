% ============================================================================
% CONSTRAINT STORY: domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_domain_partition_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: domain_partition_reading
 *   human_readable: Domain Partition Reading: Kami-Buddha Functional Specialization
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   In Japanese religious practice, kami (Shinto deities governing
 *   this-worldly prosperity, health, and community welfare) and buddhas
 *   (Buddhist figures governing karmic liberation and afterlife salvation)
 *   coexist as complementary rather than competing authorities. The domain
 *   partition reading treats this simultaneous veneration as a coherent
 *   functional specialization: practitioners consult kami for material
 *   flourishing and buddhas for spiritual liberation without experiencing
 *   contradiction because the domains operate on different temporal axes
 *   (immediate vs. eschatological) and soteriological premises (flourishing
 *   vs. liberation). This reading models the constraint as pure coordination
 *   (rope) with minimal extraction overhead — the structure emerges logically
 *   from the partition of domains, not from institutional power asymmetries.
 *   The clerical establishment (Buddhist temples and Shinto shrines) benefits
 *   from this arrangement by capturing ritual and maintenance services from
 *   both communities, but with minimal coercive overhead because both
 *   communities genuinely need their respective domains. Theater ratio is
 *   very low (0.12) because the domain distinction is functionally
 *   transparent — practitioners and institutions do not require performative
 *   justification for why kami and buddhas should coexist; the reasons are
 *   self-evident from the logical partition of concerns.
 *
 * KEY AGENTS:
 *   - Individual Practitioners (Life Domain): Seek kami veneration for material prosperity; experience coordination mechanism with low extraction cost. Primary beneficiary of life-domain constraint.
 *   - Individual Practitioners (Death Domain): Seek buddha veneration for karmic liberation; experience coordination mechanism with low extraction cost. Primary beneficiary of death-domain constraint.
 *   - Buddhist Temples: Institutional beneficiary (institutional/arbitrage) — capture maintenance fees and ritual services from death-domain practitioners; benefit from coexistence with Shinto without requiring doctrinal merger.
 *   - Shinto Shrines: Institutional beneficiary (institutional/arbitrage) — capture maintenance fees and ritual services from life-domain practitioners; benefit from coexistence with Buddhism without requiring doctrinal merger.
 *   - Clerical Establishment (Integrated): The temple/shrine network as a unified system coordinating both domains; experiences the constraint as pure coordination with no internal extraction.
 *   - Comparative Religion Scholar: Analytical observer (analytical/analytical) — identifies the domain partition as a coherent functional solution to the problem of incorporating two distinct soteriologies into a single religious ecosystem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(domain_partition_reading, 0.08).
domain_priors:suppression_score(domain_partition_reading, 0.05).
domain_priors:theater_ratio(domain_partition_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(domain_partition_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(domain_partition_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(domain_partition_reading, theater_ratio, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(domain_partition_reading, rope).
narrative_ontology:human_readable(domain_partition_reading, "Domain Partition Reading: Kami-Buddha Functional Specialization").
narrative_ontology:topic_domain(domain_partition_reading, "religious_studies/comparative_religion/japanese_history").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(domain_partition_reading, practitioners_seeking_prosperity).
narrative_ontology:constraint_beneficiary(domain_partition_reading, practitioners_seeking_salvation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL PRACTITIONER - LIFE DOMAIN (ROPE) — The kami constraint governs this-worldly concerns (prosperity, health, family, business success). The practitioner experiences this as pure coordination: kami veneration solves the collective action problem of coordinating supernatural influence with material outcomes. Low extraction, low suppression. The practitioner can cease veneration without material penalty beyond foregone prosperity prayers.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDIVIDUAL PRACTITIONER - DEATH DOMAIN (ROPE) — The buddha constraint governs afterlife salvation and karmic resolution. The practitioner experiences this as pure coordination: buddha veneration solves the collective action problem of coordinating merit accumulation with karmic transfer to the next life. Low extraction, low suppression. The practitioner can cease veneration without material penalty in this life, though with perceived spiritual cost in the next.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: CLERICAL ESTABLISHMENT - INTEGRATED VIEW (ROPE) — Buddhist temples and Shinto shrines both benefit from the domain partition framework because it legitimizes their coexistence and complementary roles without requiring doctrinal merger. The institutional structure captures maintenance fees, ritual services, and land privileges from both domains. Extraction is minimal because both communities (kami and buddha practitioners) genuinely need their respective domains; the clerical system coordinates rather than exploits this need.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPARATIVE RELIGION SCHOLAR - ANALYTICAL VIEW (ROPE) — From a civilizational and universal perspective, the domain partition reading represents a coherent functional solution to the problem of incorporating two distinct soteriologies (salvation theories) into a single religious ecosystem. The constraint enables coordination between kami specialists (handling material flourishing) and buddha specialists (handling karmic liberation) without requiring either to claim dominion over the other's domain. This is pure coordination with no extractive overhead — the structure emerges from the logical partition of concerns, not from institutional power asymmetries.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(domain_partition_reading_tests).
:- end_tests(domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The domain partition reading models the constraint as pure coordination with minimal institutional overhead. Practitioners genuinely benefit from both domains; the clerical institutions capture modest maintenance fees but provide real ritual services that satisfy demand. No coercion required — practitioners choose to venerate both domains voluntarily. The extractiveness is close to zero because the benefit flow is bidirectional: practitioners get ritual coordination, institutions get maintenance support. Suppression (0.05): Minimal. Practitioners are mobile — they can cease veneration without material penalty (beyond foregone prayers for prosperity). No legal, economic, or physical barriers to exit. The domain partition is not enforced coercively; it emerges from the functional logic of the two domains. Theater ratio (0.12): Very low. The domain distinction is functionally transparent. Practitioners understand immediately why kami handle this-worldly concerns and buddhas handle karmic liberation — the separation requires no performative justification or ritual theater to maintain credibility. The low theater reflects that the constraint's legitimacy derives from logical coherence (partition of domains) rather than institutional performance.
 *
 * PERSPECTIVAL GAP:
 *   The domain partition reading produces a uniform rope classification across all perspectives because the constraint structure is genuinely coordinative from all positions. Individual practitioners experience rope (coordination solving the problem of accessing both this-worldly and afterlife benefits). The clerical establishment experiences rope (coordination of institutional roles without competitive threat). The analytical observer experiences rope (a logically coherent functional solution to complementary soteriology). The absence of perspectival gap (all perspectives converge on rope) is diagnostic: this reading has eliminated the contradictions that sibling readings (ontological_fusion_reading, pragmatic_incoherence_reading) would highlight. The stability of rope across all contexts indicates that the domain partition successfully resolves the structural tension between kami and buddha veneration.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for this reading: Both kami practitioners and buddha practitioners are beneficiaries with mobile exit options. No victims are declared because the partition reading eliminates asymmetric extraction — each domain has its own beneficiary set, and neither domain extracts from the other. The clerical institutions (temples/shrines) are institutional beneficiaries with arbitrage exit options — they profit from maintenance services but provide genuine coordination function in return. The derived directionality values produce low d (beneficiary status with mobile/arbitrage exit → d ≈ 0.15-0.25) across all agents, yielding low f(d) and minimal χ. The rope classification is stable because the coordination structure is genuine and the extraction is negligible.
 *
 * MANDATROPHY ANALYSIS:
 *   The domain partition reading avoids mandatrophy by eliminating the beneficiary-extraction contradiction: simultaneous veneration is legitimized as functional specialization rather than as incoherent compromise or institutional manipulation. The constraint is pure coordination (rope) because both communities (kami and buddha practitioners) genuinely require their respective domains for distinct soteriological purposes. The institutional beneficiaries (temples and shrines) capture fees but provide real services that satisfy demand — the extraction is minimal because the benefit flow is reciprocal. If this reading is correct, there is no mandatrophy: the constraint's coordination function is genuine, the extraction is minimal, and the institutional structure is stable because it solves a real coordination problem, not because it exploits confusion or coerces belief.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_alternative_ontological_fusion,
    'Are kami and buddhas ontologically fused entities operating across domains, or functionally distinct entities with separate domains?',
    'Doctrinal textual analysis of honjisuijaku (original essence, manifest traces) theory; survey of contemporary practitioner self-reports on whether they understand kami and buddhas as same or different entities; analysis of ritual invocations and their domain-specificity',
    'If ontologically fused: sibling constraint (ontological_fusion_reading) classifies differently, possibly as tangled_rope with beneficiaries extracting from unified domain control. If functionally distinct: domain_partition_reading (this reading) is accurate, rope classification stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_alternative_ontological_fusion, conceptual, 'Ontological identity of kami vs buddha entities').

omega_variable(
    kernel_alternative_pragmatic_incoherence,
    'Does simultaneous veneration of kami and buddhas constitute pragmatic incoherence (contradictory salvific claims) or complementary domain specialization?',
    'Logical analysis of salvific claims: do kami promises (prosperity in this life) and buddha promises (karmic liberation across lives) contradict or complement? Ethnographic observation of how practitioners navigate tension between material accumulation (kami) and renunciation (buddha); analysis of doctrinal texts addressing the relationship between worldly success and spiritual advancement',
    'If incoherent: sibling constraint (pragmatic_incoherence_reading) classifies as snare, with institutional beneficiaries (temples/shrines) extracting from practitioner confusion. If complementary: this reading (domain_partition) is correct, rope classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_alternative_pragmatic_incoherence, conceptual, 'Whether simultaneous veneration constitutes logical coherence or pragmatic incoherence').

omega_variable(
    extraction_via_ritual_specialization,
    'Do Buddhist temples and Shinto shrines extract economic benefit through artificial specialization (ritual monopolies that could be integrated), or do they coordinate genuine functional division?',
    'Economic analysis of ritual fee structures; comparison of maintenance costs for separated temples/shrines vs hypothetical integrated facilities; survey of whether practitioners perceive separate institutions as necessary or as profit-maximizing redundancy; historical analysis of institutional barriers to merger',
    'If genuine division: extraction is minimal, rope classification confirmed. If artificial: suppression and extractiveness rise, classification shifts toward tangled_rope or snare for institutional beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_via_ritual_specialization, empirical, 'Whether institutional specialization is functionally necessary or economically motivated').

omega_variable(
    reading_kernel_status,
    'Which reading of the simultaneous veneration kernel is instantiated by this constraint?',
    'This constraint is the domain_partition_reading: two parallel constraints (life-domain kami + death-domain buddha) with independent ε values, no extractive asymmetry, pure coordination rope structure. Sibling readings (ontological_fusion_reading, pragmatic_incoherence_reading) are separate constraint stories with different ε values and classification structures.',
    'This omega documents the committer frame: the kernel of simultaneous veneration has multiple defensible readings. This story instantiates the partition reading only. Do not fold alternative readings into this constraint''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_status, conceptual, 'This constraint is one reading of the simultaneous veneration kernel; sibling readings are other constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(domain_partition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doma_tr_t0, domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(doma_tr_t100, domain_partition_reading, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(doma_be_t0, domain_partition_reading, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(doma_be_t100, domain_partition_reading, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(domain_partition_reading, ontological_fusion_reading).
narrative_ontology:affects_constraint(domain_partition_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The simultaneous veneration kernel (kernel_id: simultaneous_veneration) has three structurally distinct readings modeled as separate constraint stories: (1) domain_partition_reading (this story, ε≈0.08, rope) — kami and buddhas partition domains, pure coordination; (2) ontological_fusion_reading (sibling, ε≈0.35, tangled_rope) — kami and buddhas are ontologically identical, competitive doctrine creates extractive institutional benefits; (3) pragmatic_incoherence_reading (sibling, ε≈0.58, snare) — kami and buddhas make contradictory promises, institutional beneficiaries extract from practitioner confusion. All three readings should be generated as separate constraint stories. Each reading is self-contained and ε-invariant. The network edges document that all three are readings of the same kernel and affect each other's plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
