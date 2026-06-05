% ============================================================================
% CONSTRAINT STORY: liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liturgical_continuity_reading, []).

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
 *   constraint_id: liturgical_continuity_reading
 *   human_readable: Hebrew as Living Language through Liturgical Continuity and Textual Study
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Hebrew language continuity across diaspora communities from late
 *   antiquity through modernity is sustained through unbroken chains of
 *   liturgical recitation and textual study. Synagogue prayers, daily
 *   blessings, Torah and Talmud study circles, and rabbinic commentary
 *   traditions preserve Hebrew's linguistic structure and cultural meaning
 *   across geographically dispersed, politically separated, and culturally
 *   heterogeneous Jewish communities spanning 2000+ years and five
 *   continents. This constraint describes ONE READING of the contested
 *   'hebrew_living_language' kernel: the reading that claims Hebrew remains
 *   'living' through continuous ritual practice and textual transmission,
 *   independent of whether native speakers exist or modern secular literature
 *   is produced in the language. This reading competes with two other
 *   readings: the native-generation reading (Hebrew is 'living' only when
 *   spoken natively by Israeli children as primary language) and the
 *   literary-revival reading (Hebrew is 'living' only through contemporary
 *   creative secular use). This story instantiates ONLY the liturgical
 *   continuity reading; the sibling readings are separate constraint stories
 *   with different ε values, different victim sets, and different
 *   institutional structures.
 *
 * KEY AGENTS:
 *   - Diaspora Jewish Communities: Primary beneficiary (moderate/mobile) — experience the constraint as coordination mechanism preserving cultural and religious identity across geographic separation. Voluntary participation; benefit from linguistic continuity and access to sacred texts.
 *   - Hebrew Textual and Liturgical Tradition: Institutional beneficiary (institutional/arbitrage) — constituted through the constraint; preservation of interpretive depth and textual accuracy depends on continuous recitation and study chains. No victim correlate.
 *   - Individual Liturgical Practitioners: Secondary beneficiary with constrained exit (organized/constrained) — gain religious function, cultural identity, intellectual engagement from participation in Hebrew study and prayer. Learning cost is real but not extractive because it provides access, not gatekeeping.
 *   - Institutional Religious Authority: Institutional actor observing partial degradation (institutional/arbitrage) — maintains liturgical requirements and educational standards; some enforcement has become theatrical as voluntary commitment has increased and linguistic necessity has decreased.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as canonical coordination: solving collective action problems of linguistic preservation, cultural transmission, and identity coordination across dispersed communities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liturgical_continuity_reading, 0.12).
domain_priors:suppression_score(liturgical_continuity_reading, 0.08).
domain_priors:theater_ratio(liturgical_continuity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liturgical_continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(liturgical_continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(liturgical_continuity_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liturgical_continuity_reading, rope).
narrative_ontology:human_readable(liturgical_continuity_reading, "Hebrew as Living Language through Liturgical Continuity and Textual Study").
narrative_ontology:topic_domain(liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liturgical_continuity_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(liturgical_continuity_reading, hebrew_textual_tradition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIASPORA JEWISH COMMUNITIES (ROPE) — Participants in liturgical recitation and textual study experience the constraint as pure coordination. Hebrew remains legible across centuries and continents through unbroken ritual practice and textual transmission. Communities benefit from linguistic and cultural continuity; the coordination function (shared language preserving group identity and religious practice) is genuine and voluntary. Exit options are mobile — individuals can choose to participate, learn, or disengage without total loss of community membership. Extraction is minimal.
constraint_indexing:constraint_classification(liturgical_continuity_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: HEBREW TEXTUAL AND LITURGICAL TRADITION (ROPE) — The tradition itself is constituted through the constraint. Continuous recitation and study preserve textual accuracy, interpretive depth, and symbolic resonance across diaspora. The tradition benefits from the maintenance mechanisms (daily prayers, weekly Torah readings, continuous commentary tradition); the tradition IS the coordination mechanism. No beneficiary/victim distinction applies at this perspective level — the tradition and its preservation mechanism are identical. Classification: Rope, maximum beneficiary position.
constraint_indexing:constraint_classification(liturgical_continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIVIDUAL LITURGICAL PRACTITIONERS (ROPE) — People who engage in regular Hebrew prayer and textual study face modest constraints (time commitment, learning requirements) but experience the constraint as coordination providing cultural identity, religious function, and intellectual engagement. Cost barriers are real but not extractive — learning Hebrew liturgically provides genuine access to meaning, not gatekeeping. Classification: Rope with constrained exit (high cost to abandon practiced skill and community role, but not impossible). Effective extraction remains low because the coordination benefit is substantial and voluntary.
constraint_indexing:constraint_classification(liturgical_continuity_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONAL RELIGIOUS AUTHORITY (PITON) — From the perspective of religious hierarchies and institutional bodies, the liturgical continuity mechanism is partly performative inertia. The institutions maintain Hebrew prayer requirements, textual standards, and educational frameworks through institutional authority rather than because participants would voluntarily choose these exact mechanisms absent tradition. Theater ratio (0.35) reflects that institutional enforcement of specific pronunciations, prayer structures, and interpretive boundaries has become increasingly theatrical as the original functional necessity (preventing linguistic drift) has been partially replaced by voluntary commitment and modern linguistic stability. The constraint persists through institutional maintenance, but its primary function has partially degraded from strict linguistic necessity to cultural symbol preservation. Classification: Piton (degraded from necessity to performance).
constraint_indexing:constraint_classification(liturgical_continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational and global analytical perspective, the constraint is pure coordination. Liturgical recitation and textual study solve genuine collective action problems: they preserve linguistic continuity, transmit cultural meaning, coordinate group identity across dispersed communities, and maintain textual accuracy through redundancy (multiple streams of recitation and commentary). The mechanism is entirely voluntary — communities participate because the coordination benefits exceed the costs, not because exit is impossible or extraction is high. No hidden victims exist; the constraint has no systematic asymmetry. Base extractiveness (0.12) reflects minimal overhead and no asymmetric capture. Classification: Rope (canonical coordination mechanism).
constraint_indexing:constraint_classification(liturgical_continuity_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liturgical_continuity_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(liturgical_continuity_reading, TR),
    TR >= 0.70.

:- end_tests(liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The liturgical continuity constraint has low extraction because participation is voluntary, beneficiaries are clearly identified (diaspora communities benefit from cultural continuity), no victim set exists (the cost of maintaining the tradition is internal to the benefit-seeking communities), and the coordination function is genuine and high-value. The constraint solves real coordination problems: how do geographically separated communities maintain linguistic intelligibility across centuries? How do sacred texts preserve meaning across translation boundaries? How do diasporic identities remain coherent without geographic or political unity? These are coordination problems, not extraction mechanisms. Suppression (0.08): Minimal. While learning Hebrew liturgically requires time and effort, and institutional religious authority enforces certain standards, suppression is low because: (1) alternatives exist (Hebrew can be learned secularly, through literature, through modern Israeli contexts); (2) exit from liturgical participation does not prevent participation in Jewish community; (3) communities generally want to maintain the tradition, so enforcement is rarely required. Theater ratio (0.35): Low-moderate and increasing. Historically (time=0, theater=0.15), liturgical recitation was functionally necessary to preserve Hebrew's linguistic structure in the absence of written standardization and institutional grammar. The recitation itself WAS the primary preservation mechanism. Over 2000 years (time=2000, theater=0.35), institutional enforcement of specific pronunciations, prayer sequences, and interpretive boundaries has become increasingly theatrical as: (1) written texts (printed prayer books, biblical commentaries) now provide standardization; (2) modern Hebrew exists as a living spoken language with its own normative grammar; (3) individual commitment to the tradition is now primarily cultural/religious rather than linguistically necessary. The constraint persists and functions, but some portion of its structural maintenance is now institutional inertia rather than functional necessity. The increase in theater ratio from 0.15 to 0.35 across the interval reflects this degradation of necessity into symbol preservation, but the rate is gradual because the coordination function remains genuine.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is MINIMAL across all perspectives except the Piton view. Perspectives 1-3 and 5 (Diaspora communities, Textual tradition, Practitioners, Analytical observer) all classify as Rope or Rope-adjacent. They differ in time horizon and power atom but not in fundamental classification. This minimal gap is diagnostic of a pure coordination mechanism. The Piton perspective (institutional religious authority) sees degradation: the constraint's original functional necessity (preventing linguistic drift without written standardization) has partially atrophied and been replaced by symbolic and institutional maintenance. This creates the only meaningful perspectival gap: has the constraint's primary function degraded (Piton) or does its coordination function remain sufficient to justify current maintenance (Rope)? The analytical response is that both are true: the constraint retains genuine coordination value (preventing diaspora linguistic isolation) while some institutional enforcement has become theatrical. This is not a contradiction but a lifecycle trajectory: from pure necessity, through genuine coordination, toward partial symbolic maintenance. The theater ratio trajectory (0.15→0.35) captures this progression.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality analysis: This constraint shows unusually uniform directionality across perspectives because it is a pure coordination mechanism with no systematic asymmetry. Communities that maintain the tradition benefit from linguistic continuity (low d, derived from beneficiary status + mobile exit → low effective extraction). Institutional actors maintain the tradition and benefit from its preservation (low d, beneficiary status + arbitrage exit → negative f(d), institutional subsidization). Analytical observers see no hidden extraction (d ≈ 0.5, neither beneficiary nor victim, → canonical f(d) ≈ 0.65 but scaled by low χ formula base). The liturgical continuity reading produces uniform Rope classification because no agent is being extracted from to subsidize others. This contrasts sharply with the native-generation reading (which would show high extractiveness from immigrant language communities forced into Hebrew dominance hierarchy) and the literary-revival reading (which would show moderate extractiveness from religious/rabbinic interpretive communities displaced by secular-literary dominance). The uniform directionality is diagnostic: it suggests this reading captures a genuine coordination mechanism, while the sibling readings would show perspectival gaps indicating extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING CASE STUDY: The mandatrophy here is located in the definition of 'living language' at the hebrew_living_language kernel. This liturgical_continuity_reading resolves the mandatrophy by claiming that linguistic continuity through unbroken ritual practice and textual study constitutes 'living' status. The constraint is Rope: the mechanism preserves language, coordinates diaspora identity, and maintains textual meaning. No mandatrophy resolution is required for this reading because the coordination function is unambiguous and the constraint type (Rope) is stable across all perspectives. The mandatrophy exists at the KERNEL level, not at the constraint level: readers of the kernel disagree about which observable (liturgical continuity, native generation, literary productivity) counts as evidence that Hebrew is 'living.' This story's task is to model the liturgical reading precisely and completely, allowing the comparative analysis across readings to reveal where the kernel disagreement is located. The three readings have different ε values (this reading: ε=0.12, low; native-generation reading: likely ε>0.4, moderate-high due to coercive language hierarchy; literary-revival reading: likely ε≈0.3, moderate due to hierarchical privilege given to secular use). The difference in ε values is NOT measurement uncertainty — it is structural difference. Each reading instantiates a different constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_necessity_vs_voluntary_commitment,
    'Is the unbroken chain of Hebrew liturgical recitation sustained by structural necessity (preventing linguistic extinction) or by voluntary cultural commitment now that the language has other means of survival (modern Hebrew, literature, education)?',
    'Counterfactual analysis: if institutional enforcement of liturgical Hebrew were removed, would the practice persist at similar scale? Comparative analysis with other liturgical languages (Latin, Classical Arabic, Sanskrit) to identify threshold points where institutional enforcement becomes optional.',
    'If necessity-driven: constraint is Rope (high-value coordination). If voluntary-commitment-driven: constraint is Rope (lower extractiveness because the binding is cultural preference, not institutional requirement). If mixed: clarifies the piton perspective''s claim that some institutional enforcement has become theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_necessity_vs_voluntary_commitment, empirical, 'Whether liturgical continuity is structurally necessary or culturally voluntary').

omega_variable(
    textual_accuracy_preservation_mechanism,
    'Does the constraint actually prevent significant linguistic or textual drift compared to alternative preservation mechanisms (institutional written standardization, digital archiving, modern Hebrew grammar normalization)?',
    'Longitudinal textual analysis: comparison of manuscript variants, pronunciation shifts, and interpretive traditions across 1000-year periods under liturgical continuity vs. periods under institutional written standardization; measurement of drift rates.',
    'If liturgical recitation significantly outperforms alternatives: the coordination function is genuine and high-value (Rope confirmed). If drift rates are similar: the constraint''s primary function is symbolic rather than functional (moves toward Piton classification). If written/digital methods outperform: the constraint is partially obsolete and maintained through inertia (Piton confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_accuracy_preservation_mechanism, empirical, 'Effectiveness of liturgical recitation for preserving textual accuracy').

omega_variable(
    hebrew_living_language_kernel_reading_specification,
    'This constraint instantiates the ''liturgical_continuity_reading'' of the contested ''hebrew_living_language'' kernel. What distinguishes this reading from the ''native_generation_reading'' (modern Hebrew as primary language spoken by native speakers) and the ''literary_revival_reading'' (Hebrew as vehicle for modern literature and secular discourse)?',
    'Each reading is a separate constraint story with its own ε value. The native-generation reading likely has high extractiveness (creation of obligatory school language hierarchy favoring Hebrew over immigrant languages). The literary reading has moderate extractiveness (privileging secular creative use over liturgical/rabbinic use). The liturgical reading (this story) has low extractiveness (voluntary participation, genuine coordination). The disagreement is located in the semantics of ''living language'' — which observable counts as evidence of linguistic vitality.',
    'The three readings are not contradictory perspectives on a single constraint — they are three structurally distinct constraints sharing a linguistic domain. Conflating them produces false mandatrophy (all three readings seeming simultaneously valid because they answer different questions). Proper analysis: declare each as a separate story, link via network.affects_constraints, document the kernel debate in omegas and commentary.kernel_context.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hebrew_living_language_kernel_reading_specification, conceptual, 'Kernel and reading specification for hebrew_living_language').

omega_variable(
    diaspora_participation_voluntariness,
    'In diaspora communities, is participation in Hebrew liturgical practice truly voluntary and mobile, or are social pressure, identity expectations, and community belonging sufficiently coercive that exit costs should reclassify ''mobile'' as ''constrained''?',
    'Ethnographic analysis of diaspora communities: measurement of exit costs (social ostracism, identity loss, community access restriction) for individuals who cease Hebrew liturgical participation while remaining community members. Comparison across different diaspora contexts (urban secular communities vs. Orthodox enclaves) to identify variance in actual exit costs.',
    'If exit costs are minimal: mobile classification confirmed, Rope type confirmed (low extraction). If exit costs are substantial: reclassify as constrained, move toward Tangled Rope (asymmetric extraction via social pressure). If exit costs vary dramatically by context: separate constraint stories per diaspora context with different exit_options per perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_participation_voluntariness, empirical, 'Whether Hebrew liturgical participation is truly voluntary in diaspora').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liturgical_continuity_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_ratio_t0_liturgical, liturgical_continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(theater_ratio_t1000_liturgical, liturgical_continuity_reading, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(theater_ratio_t2000_liturgical, liturgical_continuity_reading, theater_ratio, 2000, 0.35).

% Extraction over time
narrative_ontology:measurement(extractiveness_t0_liturgical, liturgical_continuity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(extractiveness_t1000_liturgical, liturgical_continuity_reading, base_extractiveness, 1000, 0.1).
narrative_ontology:measurement(extractiveness_t2000_liturgical, liturgical_continuity_reading, base_extractiveness, 2000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liturgical_continuity_reading, information_standard).
narrative_ontology:affects_constraint(liturgical_continuity_reading, hebrew_native_generation_reading).
narrative_ontology:affects_constraint(liturgical_continuity_reading, hebrew_literary_revival_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel produces three structurally distinct constraints: liturgical_continuity_reading (ε=0.12, Rope, voluntary coordination), native_generation_reading (higher extractiveness due to coercive language hierarchy in Israeli education/state), and literary_revival_reading (moderate extractiveness from hierarchical privilege given to secular literature). These are not perspectives on a single constraint but three separate constraints sharing a linguistic domain and a common kernel. Each has its own ε value, beneficiary/victim structure, and institutional mechanism. Network links establish that claims from one reading affect the plausibility of the others — evidence that liturgical Hebrew successfully preserved language structure over millennia strengthens the liturgical reading; evidence that native speakers are essential for 'living' language status weakens it; evidence that literature is the primary modern use of Hebrew supports the literary reading. The three constraints are linked through both logical dependency (they contest the same kernel definition) and empirical dependency (evidence for one affects confidence in the others).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
