% ============================================================================
% CONSTRAINT STORY: mythic_scaffolding_vs_formal_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mythic_scaffolding_vs_formal_fragmentation, []).

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
 *   constraint_id: mythic_scaffolding_vs_formal_fragmentation
 *   human_readable: Mythic Scaffolding vs. Formal Fragmentation in The Waste Land
 *   domain: literary_criticism/modernist_poetry/cultural_theory
 *
 * SUMMARY:
 *   T.S. Eliot's The Waste Land (1922) presents a structural tension between
 *   radical formal fragmentation (shifting voices, multiple languages, dense
 *   allusions, narrative discontinuities) and imposed mythic unity (Grail
 *   quest framework, vegetation ritual patterns, Tiresias as unifying
 *   consciousness). The poem's notes, added after initial publication to
 *   expand the book to publishable length, provide interpretive scaffolding
 *   that claims to resolve fragmentation through mythic coherence. This
 *   constraint operates across a century of literary criticism, shaping how
 *   readers and scholars engage the poem. The mythic scaffolding functions as
 *   both coordination mechanism (provides shared interpretive vocabulary,
 *   enables cumulative scholarship) and extraction mechanism (suppresses
 *   alternative readings, appropriates reader interpretive autonomy,
 *   maintains institutional gatekeeping). The constraint's theater ratio has
 *   increased over time as ritual citation of the Grail framework has
 *   replaced genuine interpretive work, while extractiveness has remained
 *   moderate-high as the critical establishment continues to benefit from the
 *   framework's institutional authority.
 *
 * KEY AGENTS:
 *   - Non-Specialist Reader: Primary victim (powerless/trapped) — cannot access poem without accepting imposed mythic framework; interpretive autonomy appropriated by critical apparatus
 *   - Graduate Student: Secondary victim (moderate/constrained) — must master mythic scaffolding for professional legitimacy; benefits from coordination function but bears cost of mandatory framework acceptance
 *   - Modernist Critical Establishment: Primary beneficiary (institutional/arbitrage) — benefits from mythic scaffolding as stable interpretive ground enabling century of scholarship and institutional authority
 *   - Revisionist Critical School: Organized agents (organized/mobile) — feminist, postcolonial, reader-response critics who challenge mythic unity; mobile enough to propose alternatives but constrained by need to engage dominant framework
 *   - New Critical Apparatus: Institutional actor (institutional/arbitrage) — mid-century reading of mythic unity has degraded into ritual citation; maintains performative scholarly gesture through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination (shared vocabulary) and structural extraction (suppressed alternatives)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mythic_scaffolding_vs_formal_fragmentation, 0.48).
domain_priors:suppression_score(mythic_scaffolding_vs_formal_fragmentation, 0.52).
domain_priors:theater_ratio(mythic_scaffolding_vs_formal_fragmentation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mythic_scaffolding_vs_formal_fragmentation, extractiveness, 0.48).
narrative_ontology:constraint_metric(mythic_scaffolding_vs_formal_fragmentation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(mythic_scaffolding_vs_formal_fragmentation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mythic_scaffolding_vs_formal_fragmentation, tangled_rope).
narrative_ontology:human_readable(mythic_scaffolding_vs_formal_fragmentation, "Mythic Scaffolding vs. Formal Fragmentation in The Waste Land").
narrative_ontology:topic_domain(mythic_scaffolding_vs_formal_fragmentation, "literary_criticism/modernist_poetry/cultural_theory").

domain_priors:requires_active_enforcement(mythic_scaffolding_vs_formal_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mythic_scaffolding_vs_formal_fragmentation, modernist_critical_establishment).
narrative_ontology:constraint_beneficiary(mythic_scaffolding_vs_formal_fragmentation, academic_gatekeepers).
narrative_ontology:constraint_beneficiary(mythic_scaffolding_vs_formal_fragmentation, eliot_scholarly_industry).
narrative_ontology:constraint_victim(mythic_scaffolding_vs_formal_fragmentation, reader_interpretive_autonomy).
narrative_ontology:constraint_victim(mythic_scaffolding_vs_formal_fragmentation, non_specialist_readers).
narrative_ontology:constraint_victim(mythic_scaffolding_vs_formal_fragmentation, alternative_critical_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-SPECIALIST READER (SNARE) — Trapped between the poem's radical fragmentation and the critical apparatus that claims mythic unity resolves it. Cannot access the poem without accepting the interpretive framework imposed by Eliot's notes and critical tradition. The mythic scaffolding functions as mandatory mediation — the reader must either accept the Grail/fertility ritual framework or remain locked out of interpretive legitimacy. Maximum extraction: the constraint appropriates interpretive labor while denying autonomous meaning-making.
constraint_indexing:constraint_classification(mythic_scaffolding_vs_formal_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GRADUATE STUDENT (TANGLED ROPE) — Constrained by disciplinary expectations to master the mythic framework (Grail quest, vegetation cycles, Tiresias as unifying consciousness) as prerequisite for professional legitimacy, but also benefits from the constraint's coordination function: the shared interpretive vocabulary enables scholarly conversation and career advancement. Can exit by leaving the field, but at significant professional cost. Experiences both genuine coordination (common critical language) and extraction (mandatory acceptance of contested framework).
constraint_indexing:constraint_classification(mythic_scaffolding_vs_formal_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MODERNIST CRITICAL ESTABLISHMENT (ROPE) — Benefits from the mythic scaffolding as coordination mechanism: the Grail/fertility framework provides stable interpretive ground for a century of scholarship, enabling cumulative critical work, academic careers, and institutional authority. Experiences minimal extraction — the constraint runs toward this agent. Can arbitrage between competing frameworks (New Criticism, structuralism, deconstruction) while maintaining the mythic scaffolding as foundational. The notes and their interpretive tradition function as professional infrastructure.
constraint_indexing:constraint_classification(mythic_scaffolding_vs_formal_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REVISIONIST CRITICAL SCHOOL (TANGLED ROPE) — Organized agents (feminist critics, postcolonial readers, reader-response theorists) who challenge the mythic unity claim and foreground the poem's fragmentation as its primary meaning. Mobile enough to propose alternative frameworks, but constrained by the need to engage the dominant mythic interpretation to gain hearing. Benefits from the constraint's existence (provides target for critique, generates scholarly productivity) while also bearing cost of having to argue against entrenched interpretive authority. Mixed coordination and extraction.
constraint_indexing:constraint_classification(mythic_scaffolding_vs_formal_fragmentation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: NEW CRITICAL APPARATUS (PITON) — The mid-century New Critical reading of The Waste Land as resolved through mythic unity has degraded into ritual citation. Contemporary scholars reference the Grail framework and Tiresias unification not because these interpretive moves still generate insight, but because the apparatus persists through institutional inertia. The notes function as performative scholarly gesture — cited to demonstrate mastery, not to advance understanding. High theater ratio: the critical machinery continues operating despite diminished functional output.
constraint_indexing:constraint_classification(mythic_scaffolding_vs_formal_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the mythic scaffolding represents both genuine coordination (provides shared interpretive vocabulary across generations of readers) and structural extraction (suppresses alternative readings, particularly those that embrace fragmentation as irreducible). The constraint's dual nature is visible: Eliot's notes genuinely help readers navigate allusive density, but also impose a specific interpretive framework (Grail quest, fertility ritual, Tiresias unification) that forecloses other meaning-making paths. The ratio of mythic framework elements to fragmentary techniques is the measurable site of this tension.
constraint_indexing:constraint_classification(mythic_scaffolding_vs_formal_fragmentation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mythic_scaffolding_vs_formal_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mythic_scaffolding_vs_formal_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mythic_scaffolding_vs_formal_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mythic_scaffolding_vs_formal_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mythic_scaffolding_vs_formal_fragmentation, TR),
    TR >= 0.70.

:- end_tests(mythic_scaffolding_vs_formal_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The mythic scaffolding appropriates reader interpretive labor by imposing a specific framework (Grail quest, fertility ritual, Tiresias unification) as prerequisite for legitimate engagement. Non-specialist readers are locked out of autonomous meaning-making; graduate students must accept the framework for professional advancement; alternative critical approaches remain marginalized. However, extraction is not maximal — the framework does provide genuine coordination benefits (shared vocabulary, cumulative scholarship), and organized critics can propose alternatives. Suppression (0.52): Moderate-high. Significant barriers to alternative interpretation include institutional authority of Eliot scholarship, disciplinary expectations for graduate training, citation networks that reinforce mythic framework, and the notes' paratextual authority. But suppression is not total — revisionist critics have gained some institutional legitimacy, and reader-response approaches have created space for non-mythic readings. Theater ratio (0.58): Moderate-high. Much contemporary citation of the Grail framework is performative — scholars reference Tiresias unification and vegetation cycles to demonstrate mastery rather than to generate new insight. The New Critical apparatus persists through inertia. However, some genuine interpretive work continues, and the framework still enables productive scholarship, so theater is not maximal.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates significant perspectival divergence. The modernist critical establishment sees coordination (Rope) — the mythic framework enables cumulative scholarship and professional infrastructure. The non-specialist reader sees pure extraction (Snare) — the framework appropriates interpretive autonomy and locks out alternative meaning-making. Graduate students and revisionist critics see mixed coordination and extraction (Tangled Rope) — the framework both enables and constrains their work. The New Critical apparatus sees its own degraded ritual (Piton) — mythic unity citations persist through inertia rather than function. The analytical observer sees the constraint's dual nature: genuine coordination benefits coexist with structural extraction. The gap reveals how the same interpretive apparatus functions differently depending on the agent's structural position — beneficiaries experience coordination, victims experience extraction, and organized agents experience both.
 *
 * DIRECTIONALITY LOGIC:
 *   The modernist critical establishment experiences low directionality (d ≈ 0.15) as primary beneficiary with arbitrage exit options — the mythic scaffolding runs toward them, providing stable interpretive ground and institutional authority. Non-specialist readers experience high directionality (d ≈ 0.92) as victims with trapped exit options — they bear maximum extraction, unable to access the poem without accepting the imposed framework. Graduate students experience moderate-high directionality (d ≈ 0.62) as victims with constrained exit options — they can exit the field but at significant professional cost, and they also benefit from the coordination function. Revisionist critics experience moderate directionality (d ≈ 0.48) as organized agents with mobile exit options — they can propose alternatives and have some institutional legitimacy, experiencing mixed coordination and extraction. The New Critical apparatus experiences low directionality (d ≈ 0.20) as institutional beneficiary with arbitrage options, though their perspective is piton (degraded function) rather than rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that mythic scaffolding is neither pure coordination nor pure extraction — it is genuinely both, and the classification depends on the observer's structural position. The modernist critical establishment's rope classification is their authentic experience: the framework does provide stable interpretive ground. The non-specialist reader's snare classification is equally authentic: the framework does appropriate interpretive autonomy. The tangled rope classification at the analytical level captures the constraint's dual nature: the mythic scaffolding coordinates scholarly conversation while extracting from reader autonomy. The omega variables identify the irreducible uncertainties (authorial intent authority, fragmentation resolution necessity) that prevent collapsing the constraint to a single type. The constraint is not mislabeled coordination masking extraction, nor is it mislabeled extraction masking coordination — it is structurally both, and the perspectival gap is the diagnostic signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorial_intent_authority,
    'Do Eliot''s notes (added after initial publication) constitute authoritative interpretive guidance or post-hoc rationalization that should be bracketed?',
    'Historical analysis of composition process; comparison of pre-notes and post-notes reception; examination of Eliot''s later statements about the notes as ''bogus scholarship''',
    'If notes are authoritative: mythic scaffolding is legitimate coordination. If notes are post-hoc: scaffolding is extractive imposition that appropriates reader interpretive labor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorial_intent_authority, conceptual, 'Whether Eliot''s notes constitute legitimate interpretive authority').

omega_variable(
    fragmentation_resolution_necessity,
    'Does the poem''s formal fragmentation require mythic unity for coherence, or is fragmentation itself the poem''s primary meaning?',
    'Reader response studies comparing interpretations with and without mythic framework; analysis of which formal elements (voice shifts, language switches, allusive density) are genuinely unified by Grail/fertility patterns vs. which remain irreducibly fragmentary',
    'If unity is necessary: constraint is coordination (Rope from more perspectives). If fragmentation is irreducible: constraint is extraction (Snare from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fragmentation_resolution_necessity, preference, 'Whether mythic unity is necessary for poem coherence').

omega_variable(
    alternative_framework_viability,
    'Can alternative interpretive frameworks (feminist, postcolonial, reader-response) generate equally productive scholarly work without engaging the mythic scaffolding?',
    'Citation analysis of scholarship that bypasses Grail/fertility framework; assessment of whether alternative readings achieve institutional legitimacy or remain marginalized',
    'If alternatives are viable: suppression is lower, extraction is lower, constraint weakens toward Rope. If alternatives remain marginalized: suppression is higher, extraction is higher, constraint strengthens toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_framework_viability, empirical, 'Whether alternative frameworks can bypass mythic scaffolding').

omega_variable(
    notes_as_paratextual_contamination,
    'Do the notes function as paratextual contamination that prevents readers from experiencing the poem''s fragmentation directly?',
    'Comparative reader studies: responses to poem with notes vs. without notes; analysis of whether notes pre-structure interpretation or merely provide helpful context',
    'If notes contaminate: theater_ratio is higher, extraction is higher. If notes merely contextualize: theater_ratio is lower, coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notes_as_paratextual_contamination, empirical, 'Whether notes contaminate direct reader experience').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mythic_scaffolding_vs_formal_fragmentation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(myth_frag_theater_1922, mythic_scaffolding_vs_formal_fragmentation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(myth_frag_theater_1947, mythic_scaffolding_vs_formal_fragmentation, theater_ratio, 25, 0.48).
narrative_ontology:measurement(myth_frag_theater_1972, mythic_scaffolding_vs_formal_fragmentation, theater_ratio, 50, 0.58).
narrative_ontology:measurement(myth_frag_theater_1997, mythic_scaffolding_vs_formal_fragmentation, theater_ratio, 75, 0.62).
narrative_ontology:measurement(myth_frag_theater_2022, mythic_scaffolding_vs_formal_fragmentation, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(myth_frag_extract_1922, mythic_scaffolding_vs_formal_fragmentation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(myth_frag_extract_1947, mythic_scaffolding_vs_formal_fragmentation, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(myth_frag_extract_1972, mythic_scaffolding_vs_formal_fragmentation, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(myth_frag_extract_1997, mythic_scaffolding_vs_formal_fragmentation, base_extractiveness, 75, 0.5).
narrative_ontology:measurement(myth_frag_extract_2022, mythic_scaffolding_vs_formal_fragmentation, base_extractiveness, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mythic_scaffolding_vs_formal_fragmentation, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is part of a broader family of modernist interpretive frameworks that impose unity onto fragmentation. Related constraints include: Pound's Cantos and the ideogrammic method, Joyce's Ulysses and Homeric parallels, Woolf's The Waves and six-voice structure. Each exhibits similar tension between formal fragmentation and imposed coherence, with similar beneficiary/victim patterns across the modernist critical establishment and reader interpretive autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
