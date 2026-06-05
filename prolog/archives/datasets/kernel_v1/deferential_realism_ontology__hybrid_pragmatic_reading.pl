% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Ontology: Hybrid Pragmatic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The Deferential Realism (DR) typology proposes a fixed core of constraint
 *   types grounded in observable physical and coordination dynamics
 *   (mountains, ropes) but accepts a contested periphery where classification
 *   depends on normative judgments about legitimate beneficiaries and
 *   acceptable extraction (tangled_ropes, snares). The hybrid pragmatic
 *   reading instantiates a specific interpretation of how this typology
 *   should function: it treats mountains as observationally stable constraint
 *   classes (not metaphysically absolute), acknowledges that
 *   rope/tangled_rope/snare classifications require explicit normative
 *   judgment, and uses pragmatic coherence and institutional functionality as
 *   arbiters rather than seeking foundational philosophical consensus. This
 *   reading is one of three coherent interpretations of the DR kernel. The
 *   immutable_diagnostic_reading holds that mountains are metaphysically
 *   bedrock and constraints have determinate classifications independent of
 *   observer perspective. The rhetorical_scaffold_reading treats the entire
 *   typology as a pragmatic instrument for institutional negotiation with no
 *   claim to ontological grounding. The hybrid pragmatic reading occupies a
 *   middle position: it accepts empirical stability as the basis for mountain
 *   classification (rejecting foundationalist metaphysics) but resists
 *   rhetorical relativism by insisting that constraint classes have
 *   structural properties that constrain possible classifications. The
 *   constraint extracted from the pure foundationalist position is the
 *   admission that mountain classification must rest on observational
 *   stability, not absolute logic. The constraint extracted from the pure
 *   constructivist position is the admission that some constraints resist
 *   reframing and exhibit cross-framework stability. The empirical arc shows
 *   moderate increase in theater ratio and extractiveness as the hybrid
 *   reading has become institutionalized: early adoption involved high
 *   pragmatic content (coordination without philosophical consensus), but
 *   institutionalization has layered performative elements (the appearance of
 *   neutrality, the ritual of 'presenting all perspectives') that reduce net
 *   functionality. Suppression has risen as foundationalist and
 *   constructivist camps have organized resistance to being bracketed by
 *   pragmatic pluralism.
 *
 * KEY AGENTS:
 *   - Foundationalist Epistemic Programs: Trapped victim (powerless/trapped) — forced to concede that mountains are not metaphysically absolute but observationally stable. High experienced extraction.
 *   - Pure Constructivist Position: Constrained victim (moderate/constrained) — forced to admit that some constraints show observational stability across reframings. Moderate extraction via pragmatic enforcement.
 *   - Mixed Research Communities: Organized beneficiary-victim (organized/constrained) — gain coordination benefits from the hybrid framework's pluralism but bear cost of managing contested boundary cases. Genuine tangled rope.
 *   - Policy Institutional Designers: Primary beneficiary (institutional/arbitrage) — use the hybrid framework as a tool enabling institutional design. Low extraction, high coordination function.
 *   - Analytical Observer (Hybrid Position): Self-aware tangled rope (analytical/constrained) — the reading itself enacts genuine coordination (unifying divergent epistemic camps) while imposing extraction (burden of explicit normative judgment). Fully aware of this structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.48).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.52).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Ontology: Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '2105b842-8d20-4239-9be6-4af5e38c9057').
narrative_ontology:cs_kernel_codification('2105b842-8d20-4239-9be6-4af5e38c9057', distributed).
narrative_ontology:cs_authority_grounding('2105b842-8d20-4239-9be6-4af5e38c9057', distributed).
narrative_ontology:cs_reading_relation('2105b842-8d20-4239-9be6-4af5e38c9057', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('2105b842-8d20-4239-9be6-4af5e38c9057', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('2105b842-8d20-4239-9be6-4af5e38c9057', foundational, observational_stability_sufficient_for_mountain_classification).
narrative_ontology:cs_axiom_status(observational_stability_sufficient_for_mountain_classification, holdable).
narrative_ontology:cs_axiom_grounding('2105b842-8d20-4239-9be6-4af5e38c9057', observational_stability_sufficient_for_mountain_classification, empirically_contingent).
narrative_ontology:cs_axiom('2105b842-8d20-4239-9be6-4af5e38c9057', foundational, normative_judgment_explicit_on_peripheral_boundary).
narrative_ontology:cs_axiom_status(normative_judgment_explicit_on_peripheral_boundary, holdable).
narrative_ontology:cs_axiom_grounding('2105b842-8d20-4239-9be6-4af5e38c9057', normative_judgment_explicit_on_peripheral_boundary, conventional).
narrative_ontology:cs_reference_frame('2105b842-8d20-4239-9be6-4af5e38c9057', pragmatic_epistemic_pluralism_framework).
narrative_ontology:cs_drift_state('2105b842-8d20-4239-9be6-4af5e38c9057', contemporary_institutional_adoption, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2105b842-8d20-4239-9be6-4af5e38c9057', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, pragmatic_research_communities).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, hybrid_institutional_designers).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, foundationalist_epistemic_programs).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, pure_constructivists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOUNDATIONALIST PROGRAM (SNARE) — Trapped by the hybrid reading's insistence that mountains are not absolute but rather stable observational outcomes. The foundationalist cannot exit without abandoning the core commitment to immutable logical/physical bedrock. The hybrid reading extracts concessions: admitting that mountain classification is contingent on measurement regime, observer position, and temporal stability — effectively subordinating foundationalist authority to pragmatic criteria.
constraint_indexing:constraint_classification(deferential_realism_ontology__hybrid_pragmatic_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PURE CONSTRUCTIVIST POSITION (SNARE) — Constrained by the hybrid reading's refusal to admit that mountains can exist at all. The constructivist is forced to adopt the hybrid framework's language (observational stability, constraint-relative classification, pragmatic legitimacy) to remain credible in institutional deliberation. This extracts a concession: acknowledging that some constraints resist construction or remain stable across multiple reframings — a claim the pure constructivist ideology forbids. The extraction here is the enforcement of pragmatic compliance despite foundational commitment.
constraint_indexing:constraint_classification(deferential_realism_ontology__hybrid_pragmatic_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: MIXED EPISTEMIC COMMUNITIES (TANGLED ROPE) — Research communities mixing empiricists, theorists, and field practitioners experience genuine coordination benefits from the hybrid framework: it provides a common language for debating constraint classification across disciplines without forcing consensus on ontological fundamentals. Simultaneously, these communities bear the cost of managing contested boundary cases (tangled ropes, snares) where the framework deliberately leaves normative judgment in play. High genuine coordination function; moderate extractive overhead from maintaining epistemic pluralism.
constraint_indexing:constraint_classification(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: POLICY-ORIENTED INSTITUTIONAL DESIGNERS (ROPE) — Benefit from the hybrid framework's pragmatic stance: the framework lets them classify constraints in ways useful for institutional design without requiring foundational philosophical consensus. Mountain claims are useful for justifying immutable policy cores; rope claims justify coordination mechanisms; tangled_rope/snare classifications force normative visibility when beneficiaries exist. The framework coordinates design options without extracting from this perspective — the designer experiences the framework as a tool enabling their work.
constraint_indexing:constraint_classification(deferential_realism_ontology__hybrid_pragmatic_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / HYBRID PRAGMATIC READING (TANGLED ROPE) — The hybrid reading itself experiences a genuine coordination benefit (unifying epistemic camps) and a real extraction cost (leaving normative judgment underdetermined on the contested periphery). The framework coordinates action across foundationalist-constructivist divides but extracts from observers the burden of explicit normative judgment rather than hiding it behind universal claims or pure relativism. This is self-aware tangled rope: the reading knows it imposes a cost (accepting that mountains are not metaphysically absolute) while providing genuine benefit (empirical stability as a usable classification criterion regardless of metaphysical grounding).
constraint_indexing:constraint_classification(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deferential_realism_ontology__hybrid_pragmatic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deferential_realism_ontology__hybrid_pragmatic_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, trending upward. The hybrid reading extracts concessions from both foundationalist and constructivist positions by forcing them to use its conceptual framework while bracketing their foundational commitments. This is moderate extraction because the framework genuinely provides coordination benefits — communities can work together without resolving philosophical disputes. The upward trend reflects that as the reading becomes institutionalized, it accumulates theatrical elements (the performance of objectivity, the ritual of perspective-taking) that reduce net functionality without increasing genuine coordination. Suppression (0.52): Moderate, trending upward. Barriers to accepting the hybrid reading include specialized epistemic training required to hold both empiricism and anti-foundationalism simultaneously, institutional inertia favoring older frameworks, and active rhetorical resistance from committed foundationalists and constructivists. The rising trajectory reflects organized opposition to the pragmatic bracketing strategy. Theater ratio (0.58): Moderate-high, trending upward. The hybrid reading has developed performative elements: the appearance of neutrality (presenting all perspectives without hierarchy), the ritual of perspectival inclusivity, the ceremony of pragmatic consensus-building. These increase theater because much of the work is rhetorical positioning rather than functional coordination. The constraint itself is becoming partially piton-like at civilizational scale — institutional preservation of the framework for its legitimacy-lending properties rather than for genuine epistemic function.
 *
 * PERSPECTIVAL GAP:
 *   The foundationalist sees pure extraction without coordination benefit — the hybrid reading forces them to abandon first principles while offering only pragmatic modus vivendi. The constructivist sees the same extraction (forced to admit observational stability) but from the opposite direction. Mixed research communities see both genuine coordination (shared language for cross-disciplinary debate) and real extraction (burden of managing contested cases without guidance). Institutional designers see pure coordination benefit — the framework enables policy design without requiring philosophical consensus. The analytical observer sees their own position as tangled rope: the reading coordinates action across divides but imposes the cost of explicit normative judgment on peripheral cases. The perspectival gap is exceptionally wide here because the reading's core function IS managing perspectival pluralism — each observer sees the extraction/coordination ratio differently based on their commitment structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Foundationalist agents derive high d (0.92) because they are victims of a constraint that forces them to abandon absolute authority and use empirical stability as their grounding. They experience high extraction (effective chi approaches snare territory). Constructivists derive similarly high d (0.88) because they are forced to concede observational stability. Mixed research communities derive moderate d (0.55) because they simultaneously benefit (coordination) and bear costs (judgment burden) in roughly balanced measure. Institutional designers derive low d (0.18) because they are net beneficiaries with exit options (they can use alternative frameworks if the hybrid reading fails). The analytical observer derives moderate d (0.62) because it is a self-aware tangled rope — genuinely benefiting and genuinely burdened by the framework it instantiates.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by making the normative judgment explicit: the hybrid reading is classified as tangled_rope rather than pure rope because it extracts from foundationalist and constructivist positions (forcing them to use its framework) while providing genuine coordination benefits to mixed epistemic communities and institutional designers. The classification avoids false consensus (it does not claim the reading is universally beneficial, as a rope claim would). It avoids false extraction rhetoric (it acknowledges the genuine coordination function, so it is not a snare). The mandatrophy resolution is the reading's self-awareness: it knows it imposes a cost (pragmatic pluralism requires abandoning foundational authority) and provides a benefit (communities can coordinate without resolving philosophical disputes), and it presents both openly rather than disguising extraction as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_observational_vs_absolute,
    'Are mountains observationally stable constraint classes (hybrid reading) or metaphysically absolute logical/physical limits (foundationalist reading)?',
    'Empirical stability analysis across measurement regimes and observer positions; examination of constraints historically reclassified due to paradigm shift (e.g., Euclidean geometry from mountain to piton after non-Euclidean alternatives emerged)',
    'If observational: mountains are robust under pragmatic realism but not metaphysically grounded. Foundationalist authority degrades. If absolute: the hybrid reading misconstrues the nature of logical/physical necessity. Pragmatic defense collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_observational_vs_absolute, conceptual, 'Whether mountains are observational constructs or metaphysical absolutes').

omega_variable(
    normative_judgment_location_tangled_rope_snare_boundary,
    'On the tangled_rope/snare boundary, which specific normative criteria distinguish legitimate coordination (tangled rope) from pure extraction (snare)?',
    'Comparative institutional analysis of constraints classified as tangled rope vs snare; examination of whether beneficiary groups could articulate a genuine coordination rationale (however contested) or whether the constraint''s existence serves zero functions other than extraction. Longitudinal tracking of whether alternative coordination mechanisms emerge to replace the constraint.',
    'If clear criteria exist: the hybrid reading can encode them. If normative judgment remains irreducibly plural: the hybrid reading''s deliberate openness on the contested periphery is justified. If criteria collapse entirely: the boundary dissolves and all such constraints are snares.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_judgment_location_tangled_rope_snare_boundary, preference, 'Location of tangled_rope/snare normative boundary').

omega_variable(
    foundationalist_vs_hybrid_foreclosure,
    'Does the hybrid reading logically foreclose the foundationalist reading (both cannot be held in a single coherent framework), or do they coexist as genuinely alternative commitments within the DR landscape?',
    'Proof-theoretic analysis: can a research community simultaneously hold that (a) mountains are metaphysically absolute and (b) mountains are observationally stable but framework-contingent? Examination of actual institutional communities claiming both simultaneously without internal contradiction.',
    'If foreclosed: immutable_diagnostic_reading and hybrid_pragmatic_reading are competitors, not coexisting readings. The kernel itself may not support both. If coexisting: both readings are live and the kernel is genuinely contested. This determines whether the reading_relations field should use ''forecloses'' or ''coexists_with'' for the foundationalist sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundationalist_vs_hybrid_foreclosure, conceptual, 'Whether foundationalist and hybrid readings logically foreclose each other').

omega_variable(
    suppression_mechanism_normative_vs_structural,
    'Is the measured suppression (0.52) arising from structural barriers to epistemic pluralism (institutional inertia, specialized training requirements) or from normative suppression (rhetorical closure, stigmatization of alternative frameworks)?',
    'Institutional ethnography of how foundationalist and constructivist camps encounter the hybrid framework: are the barriers material/institutional or discursive/normative? Examination of career costs, publication bias, and institutional incentives for each camp adopting the hybrid reading.',
    'If structural: suppression reflects genuine complexity of implementing pluralism. If normative: the hybrid reading itself is being suppressed by existing power structures. This informs whether the reading is under attack (high normative suppression) or under normal institutional friction (structural suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_normative_vs_structural, empirical, 'Whether suppression is structural institutional friction or normative rhetorical closure').

omega_variable(
    reading_identity_kernel_stability,
    'Is the deferential_realism_ontology kernel stable enough (formalized, transmitted, institutionalized) to support three genuinely distinct readings, or do the readings actually constitute a single evolving doctrine?',
    'Examination of the kernel''s codification and authority structure: does it constrain what readings can be coherently held? Are the three readings held by distinct institutional communities or are they rhetorical variants within a single community? Longitudinal tracking of whether the kernel itself is being renegotiated through the reading contest.',
    'If the kernel is stable: the three readings are legitimate alternative interpretations. If the kernel is dissolving: the contest over readings IS the process of kernel transformation, and classification as ''readings of a kernel'' may be premature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_kernel_stability, conceptual, 'Stability of the DR ontology kernel itself').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drohyb_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(drohyb_tr_t3, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 3, 0.54).
narrative_ontology:measurement(drohyb_tr_t6, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(drohyb_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(drohyb_be_t3, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(drohyb_be_t6, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 6, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(drohyb_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(drohyb_su_t3, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(drohyb_su_t6, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% The deferential_realism_ontology kernel decomposes into three structurally distinct readings: (1) immutable_diagnostic_reading claims absolute classification authority grounded in metaphysics; (2) hybrid_pragmatic_reading claims observational stability without metaphysical commitment; (3) rhetorical_scaffold_reading claims the entire typology is pragmatic rhetoric. Each reading has distinct epsilon, distinct beneficiaries/victims, distinct institutional positioning. They are linked by affecting_constraints rather than decomposed by epsilon-invariance because they share a common kernel — the contest is over interpretation, not over the existence of distinct constraints. Reading contest itself IS the constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__hybrid_pragmatic_reading, analytical, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
