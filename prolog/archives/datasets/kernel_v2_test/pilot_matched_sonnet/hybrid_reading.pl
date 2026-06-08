% ============================================================================
% CONSTRAINT STORY: hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: hybrid_reading
 *   human_readable: Hybrid Reading: Correct Latin as Medieval Continuity with Textual Reform
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The hybrid reading of correct Latin emerged in the 15th-16th centuries as
 *   humanist philologists navigated the tension between medieval Latin's
 *   institutional entrenchment and their own claims to superior textual
 *   authority. This reading asserts that medieval Latin preserved the
 *   grammatical core of classical Latin (legitimating continuity and
 *   institutional investment in Latin education) while introducing
 *   corruptions in orthography, vocabulary, and style that textual evidence
 *   can correct (legitimating humanist reform). The constraint coordinates
 *   genuine philological work—manuscript comparison, textual criticism,
 *   linguistic reconstruction—while embedding asymmetric extraction: medieval
 *   scribal practice is retrospectively delegitimized, humanist interpretive
 *   authority is institutionalized, and the correction mechanism (textual
 *   evidence) is controlled by those who benefit from the reform. The
 *   reading's theater ratio (0.40) reflects that 'grammatical continuity'
 *   claims were partly performative: selective preservation of features that
 *   supported humanist legitimacy while discarding those that didn't. The
 *   constraint's extractiveness (0.35) and suppression (0.45) increased over
 *   the interval as the hybrid reading became institutionalized in university
 *   curricula and printing standards, hardening from a scholarly position
 *   into an enforcement mechanism.
 *
 * KEY AGENTS:
 *   - Humanist Philologists: Primary beneficiaries (institutional/arbitrage) — capture authority to define correctness via textual evidence; can move between medieval and classical sources as suits their arguments
 *   - University Latin Curriculum: Mixed position (moderate/constrained) — benefits from legitimation of grammatical continuity but bears costs of reform pressure and textual updating requirements
 *   - Medieval Scribal Tradition: Primary victim (powerless/trapped) — retrospectively delegitimized as corrupt deviation; no exit from historical judgment; centuries of transmission labor reframed as error
 *   - Textual Scholarship Tradition: Organized agents (organized/mobile) — building transitional framework with sunset logic: as classical manuscripts are recovered, medieval intermediaries become less necessary
 *   - Vernacular Linguistic Development: Secondary victim (moderate/constrained) — constrained by Latin continuity claims that position vernacular traditions as derivative rather than independent
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees tangled rope structure: genuine coordination (philological method) embedding extraction (retrospective delegitimization, authority concentration)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_reading, 0.35).
domain_priors:suppression_score(hybrid_reading, 0.45).
domain_priors:theater_ratio(hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hybrid_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_reading, "Hybrid Reading: Correct Latin as Medieval Continuity with Textual Reform").
narrative_ontology:topic_domain(hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_reading, '19252d28-8a4a-4b07-ba1c-29c695b70563').
narrative_ontology:cs_kernel_codification('19252d28-8a4a-4b07-ba1c-29c695b70563', fixed_text).
narrative_ontology:cs_authority_grounding('19252d28-8a4a-4b07-ba1c-29c695b70563', lineage).
narrative_ontology:cs_interpretation_layer_present('19252d28-8a4a-4b07-ba1c-29c695b70563').
narrative_ontology:cs_reading_relation('19252d28-8a4a-4b07-ba1c-29c695b70563', hybrid_reading__continuity_reading, influences).
narrative_ontology:cs_reading_relation('19252d28-8a4a-4b07-ba1c-29c695b70563', hybrid_reading__discontinuity_reading, influences).
narrative_ontology:cs_axiom('19252d28-8a4a-4b07-ba1c-29c695b70563', foundational, grammatical_core_preservation).
narrative_ontology:cs_axiom_status(grammatical_core_preservation, holdable).
narrative_ontology:cs_axiom_grounding('19252d28-8a4a-4b07-ba1c-29c695b70563', grammatical_core_preservation, empirically_contingent).
narrative_ontology:cs_axiom('19252d28-8a4a-4b07-ba1c-29c695b70563', foundational, textual_evidence_corrective_primacy).
narrative_ontology:cs_axiom_status(textual_evidence_corrective_primacy, holdable).
narrative_ontology:cs_axiom_grounding('19252d28-8a4a-4b07-ba1c-29c695b70563', textual_evidence_corrective_primacy, conventional).
narrative_ontology:cs_axiom('19252d28-8a4a-4b07-ba1c-29c695b70563', secondary, orthographic_corruption_thesis).
narrative_ontology:cs_axiom_status(orthographic_corruption_thesis, holdable).
narrative_ontology:cs_axiom_grounding('19252d28-8a4a-4b07-ba1c-29c695b70563', orthographic_corruption_thesis, empirically_contingent).
narrative_ontology:cs_reference_frame('19252d28-8a4a-4b07-ba1c-29c695b70563', classical_latin_grammatical_core).
narrative_ontology:cs_drift_state('19252d28-8a4a-4b07-ba1c-29c695b70563', late_medieval_transmission, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('19252d28-8a4a-4b07-ba1c-29c695b70563', '').
narrative_ontology:cs_kernel_id(hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(hybrid_reading, university_latin_curriculum).
narrative_ontology:constraint_beneficiary(hybrid_reading, textual_scholarship_tradition).
narrative_ontology:constraint_victim(hybrid_reading, medieval_scribal_tradition).
narrative_ontology:constraint_victim(hybrid_reading, vernacular_linguistic_development).
narrative_ontology:constraint_vindicates(hybrid_reading, grammatical_continuity_thesis).
narrative_ontology:constraint_vindicates(hybrid_reading, textual_evidence_primacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL SCRIBAL TRADITION (SNARE) — Trapped by retrospective delegitimization. The living practice of medieval Latin transmission is reclassified as corrupt deviation requiring correction. No exit: the scribal tradition cannot defend itself against textual evidence it did not preserve in the form humanists demand. Maximum extraction: centuries of linguistic labor reframed as error.
constraint_indexing:constraint_classification(hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UNIVERSITY LATIN CURRICULUM (TANGLED ROPE) — Constrained by institutional inertia and pedagogical requirements. Benefits from the hybrid reading's legitimation of grammatical continuity (can teach medieval texts as Latin, not as corruption) but also bears costs of reform pressure (must update orthography, vocabulary, and textual editions). Mixed coordination and extraction: the reading both enables and constrains curricular practice.
constraint_indexing:constraint_classification(hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HUMANIST PHILOLOGISTS (ROPE) — Primary beneficiaries. The hybrid reading legitimates their professional authority: they are not rejecting medieval Latin wholesale (which would undermine continuity claims) but correcting it via superior textual evidence. Arbitrage exit: can move between medieval and classical sources as evidence demands. Experiences the constraint as coordination: establishing shared standards for textual criticism.
constraint_indexing:constraint_classification(hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: TEXTUAL SCHOLARSHIP TRADITION (SCAFFOLD) — Organized agents building a transitional framework. The hybrid reading is explicitly corrective: it acknowledges medieval transmission as the vehicle but positions textual evidence as the ultimate authority. Sunset logic: as more classical manuscripts are recovered and edited, the need for medieval intermediaries diminishes. The constraint is temporary support for a transition from medieval practice to reconstructed classical norms.
constraint_indexing:constraint_classification(hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: VERNACULAR LINGUISTIC DEVELOPMENT (TANGLED ROPE) — Constrained by the hybrid reading's partial legitimation of Latin continuity. Benefits from reduced pressure to prove complete rupture (Romance languages can claim Latin ancestry without defending medieval 'corruption') but also bears costs: vernacular literary traditions are positioned as derivative rather than independent developments. Mixed experience: coordination on linguistic genealogy, extraction via hierarchical positioning.
constraint_indexing:constraint_classification(hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the hybrid reading coordinates genuine philological work (textual criticism, manuscript comparison, linguistic reconstruction) while embedding asymmetric extraction: medieval scribes are retrospectively delegitimized, humanist authority is institutionalized, and the reform mechanism (textual evidence) is controlled by those who benefit from it. The reading is structurally a tangled rope: real coordination function, real extraction, active enforcement required to maintain the legitimacy hierarchy.
constraint_indexing:constraint_classification(hybrid_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The hybrid reading extracts from medieval scribal tradition by retrospectively delegitimizing their practice while benefiting humanist philologists who control the correction mechanism. But extraction is not maximal: the reading does legitimate grammatical continuity, preserving institutional investment in Latin education and acknowledging medieval transmission as the vehicle (even if corrupt). The value reflects real asymmetry in who benefits from the reform (humanists) vs. who bears its costs (medieval practice, vernacular development) without claiming total extraction. Suppression (0.45): Moderate. Significant barriers to contesting the hybrid reading include: humanist control of textual evidence and its interpretation, institutional entrenchment in university curricula and printing standards, and the retrospective nature of the judgment (medieval scribes cannot defend their practice). But suppression is not total: alternative readings (full continuity, full discontinuity) remained live positions, and vernacular traditions developed despite Latin's claimed primacy. Theater ratio (0.40): Moderate. The 'grammatical continuity' thesis was partly performative: selective preservation of features supporting humanist legitimacy while discarding others. The claim that medieval Latin preserved the grammatical core is empirically contestable—modern historical linguistics shows more complex patterns of change and continuity than the hybrid reading acknowledged. But the theater is not total: genuine philological work (manuscript comparison, textual reconstruction) occurred alongside the performative legitimation.
 *
 * PERSPECTIVAL GAP:
 *   The medieval scribal tradition sees pure extraction (Snare)—their practice is delegitimized with no exit. The university curriculum sees mixed coordination and extraction (Tangled Rope)—benefits from continuity claims, constrained by reform pressure. Humanist philologists see coordination (Rope)—they are establishing shared standards for textual criticism, experiencing the constraint as enabling their work. The textual scholarship tradition sees temporary support (Scaffold)—the hybrid reading is explicitly transitional, with a sunset as classical sources are recovered. Vernacular development sees tangled rope—coordination on linguistic genealogy, extraction via hierarchical positioning. The analytical observer sees tangled rope at the civilizational level—genuine philological coordination embedding asymmetric extraction through retrospective delegitimization and authority concentration. The gap reveals how the same structural arrangement (partial legitimation + textual correction) appears as pure extraction to those judged, coordination to those judging, and mixed to those caught between.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists are primary beneficiaries with arbitrage exit options—they control the textual evidence mechanism and can move between medieval and classical sources as suits their arguments. The engine derives low d (beneficiary + arbitrage) → low or negative chi. Medieval scribal tradition is the primary victim with trapped exit—retrospectively judged with no capacity to contest. The engine derives high d (victim + trapped) → high chi. University curriculum occupies a mixed position: benefits from grammatical continuity legitimation but bears reform costs. The engine derives moderate d (mixed beneficiary/victim + constrained exit) → moderate chi. Textual scholarship tradition has organized power and mobile exit (building alternative verification pathways), yielding lower effective extraction despite being positioned as reformers. Vernacular development is constrained by the reading's partial legitimation of Latin continuity, experiencing moderate extraction. The analytical observer sees the full tangled rope structure: coordination and extraction intertwined, with humanist authority embedded in the correction mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resolves the mandatrophy by demonstrating that tangled rope classification can be stable and legitimate: the constraint genuinely coordinates philological work (manuscript comparison, textual reconstruction, establishment of critical editions) while genuinely extracting from medieval practice (retrospective delegitimization) and concentrating authority (humanists control the correction mechanism). This is not a misclassification of pure coordination as extraction, nor of pure extraction as coordination—it is a structural hybrid where both functions are real and intertwined. The reading's stability over 150+ years shows that tangled rope constraints can persist when the coordination function is valuable enough to justify the extraction, and when those who bear the extraction costs (medieval scribes, vernacular traditions) lack the power to exit or contest. The mandatrophy question 'is this really coordination or really extraction?' dissolves: it is both, and the perspectival gap between those who experience coordination (humanists) and those who experience extraction (medieval tradition) is the constraint's defining feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the hybrid reading''s partial legitimation of medieval forms a genuine structural compromise, or a rhetorical strategy that preserves humanist authority while appearing conciliatory?',
    'Historical analysis of which medieval forms were retained vs. corrected; correlation between retention and humanist institutional interests; examination of whether ''grammatical continuity'' claims were empirically grounded or strategically asserted.',
    'If genuine compromise: the reading is a tangled rope with balanced coordination and extraction. If rhetorical strategy: the reading is closer to a snare with coordination theater masking extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether partial legitimation is structural compromise or rhetorical strategy').

omega_variable(
    textual_evidence_authority,
    'Does ''textual evidence'' as the corrective mechanism represent neutral empirical grounding, or does it encode humanist interpretive authority as the arbiter of what counts as evidence?',
    'Analysis of disputed textual readings: who adjudicates conflicts between manuscript traditions? What counts as ''better'' evidence? Are medieval manuscripts treated as evidence or as corruption to be corrected?',
    'If neutral: the hybrid reading''s extraction is lower (correction is empirically grounded). If encoded authority: extraction is higher (humanists control both the standard and the correction mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_evidence_authority, conceptual, 'Whether textual evidence is neutral or encodes interpretive authority').

omega_variable(
    grammatical_continuity_empirical_status,
    'Is the ''grammatical continuity'' thesis empirically defensible, or does it selectively preserve features that support humanist legitimacy while discarding those that don''t?',
    'Linguistic analysis of which grammatical features were claimed as continuous vs. which were classified as medieval corruption; comparison with modern historical linguistics on actual continuity patterns.',
    'If empirically defensible: the hybrid reading''s coordination function is genuine (it accurately describes linguistic transmission). If selective: the coordination claim is partly theater (grammatical continuity is asserted to legitimate the reading, not discovered).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grammatical_continuity_empirical_status, empirical, 'Empirical status of grammatical continuity thesis').

omega_variable(
    sibling_reading_coexistence,
    'Do the three readings (continuity, hybrid, discontinuity) represent genuinely distinct structural positions, or are they points on a continuous spectrum that different actors occupy strategically?',
    'Historical analysis of whether individual philologists held stable positions across their careers or shifted readings based on institutional context; examination of whether the readings map to distinct scholarly communities or are used situationally.',
    'If genuinely distinct: the readings are separate constraints with different beneficiary structures. If continuous spectrum: the readings are strategic framings of a single constraint, and the kernel structure is itself a humanist construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether sibling readings are distinct positions or strategic framings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_lat_theater_1400, hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hybrid_lat_theater_1450, hybrid_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(hybrid_lat_theater_1500, hybrid_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement(hybrid_lat_theater_1550, hybrid_reading, theater_ratio, 150, 0.42).

% Extraction over time
narrative_ontology:measurement(hybrid_lat_extract_1400, hybrid_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hybrid_lat_extract_1450, hybrid_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(hybrid_lat_extract_1500, hybrid_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(hybrid_lat_extract_1550, hybrid_reading, base_extractiveness, 150, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_lat_suppress_1400, hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hybrid_lat_suppress_1450, hybrid_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(hybrid_lat_suppress_1500, hybrid_reading, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(hybrid_lat_suppress_1550, hybrid_reading, suppression_requirement, 150, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% The hybrid reading is one of three sibling readings of the 'correct Latin' kernel. Each reading has its own beneficiary structure and extraction profile. The hybrid reading's moderate extractiveness (0.35) reflects its middle position: less extractive than the discontinuity reading (which fully delegitimates medieval practice) but more extractive than the continuity reading (which legitimates medieval development). Network edges to sibling readings will be declared via reading_relations in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
