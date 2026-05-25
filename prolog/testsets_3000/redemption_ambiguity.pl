% ============================================================================
% CONSTRAINT STORY: redemption_ambiguity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_redemption_ambiguity, []).

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
 *   constraint_id: redemption_ambiguity
 *   human_readable: Redemption Ambiguity in The Waste Land
 *   domain: literary_criticism/modernist_poetry/cultural_theory
 *
 * SUMMARY:
 *   The Waste Land's ending presents an irreducible interpretive ambiguity:
 *   the thunder's teachings ('Datta. Dayadhvam. Damyata.') and the Sanskrit
 *   benediction ('Shantih shantih shantih') suggest spiritual resolution,
 *   while the preceding line ('These fragments I have shored against my
 *   ruins') frames the entire poem as defensive gesture rather than achieved
 *   redemption. This structural oscillation creates differential extraction
 *   based on reader's interpretive framework. Theological readers seeking
 *   doctrinal confirmation experience the ambiguity as extraction — the text
 *   promises spiritual meaning through its redemptive imagery but withholds
 *   definitive resolution. Pluralist readers experience coordination — the
 *   ambiguity vindicates interpretive multiplicity and generates ongoing
 *   critical discourse. The constraint is downstream of
 *   mythic_scaffolding_vs_formal_fragmentation: the formal fragmentation that
 *   prevents mythic unity also prevents redemptive closure. The ambiguity is
 *   not a bug but a load-bearing structural feature — it enables the poem to
 *   function simultaneously as modernist artifact (fragmentation as formal
 *   principle) and spiritual document (redemption as thematic possibility).
 *
 * KEY AGENTS:
 *   - Doctrinal Certainty / Theological Readers: Primary victim (powerless/trapped) — framework requires definitive spiritual meaning; text structurally withholds it
 *   - Interpretive Pluralism / Critical Industry: Primary beneficiary (institutional/arbitrage) — ambiguity generates renewable scholarly production across generations
 *   - Graduate Students: Secondary victim (moderate/constrained) — must produce novel readings from exhausted ambiguity; also benefit from thesis material
 *   - Pluralist Coalition: Secondary beneficiary (organized/mobile) — New Critics, deconstructionists see ambiguity as vindication of their frameworks
 *   - Undergraduate Teachers: Mixed position (moderate/constrained) — pedagogical clarity extracted; engagement with difficulty coordinated
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees hybrid structure: genuine coordination function (pluralist discourse) with asymmetric extraction (doctrinal readers)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(redemption_ambiguity, 0.42).
domain_priors:suppression_score(redemption_ambiguity, 0.48).
domain_priors:theater_ratio(redemption_ambiguity, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(redemption_ambiguity, extractiveness, 0.42).
narrative_ontology:constraint_metric(redemption_ambiguity, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(redemption_ambiguity, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(redemption_ambiguity, tangled_rope).
narrative_ontology:human_readable(redemption_ambiguity, "Redemption Ambiguity in The Waste Land").
narrative_ontology:topic_domain(redemption_ambiguity, "literary_criticism/modernist_poetry/cultural_theory").

domain_priors:requires_active_enforcement(redemption_ambiguity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(redemption_ambiguity, interpretive_pluralism).
narrative_ontology:constraint_beneficiary(redemption_ambiguity, critical_industry).
narrative_ontology:constraint_beneficiary(redemption_ambiguity, secular_readers).
narrative_ontology:constraint_victim(redemption_ambiguity, doctrinal_certainty).
narrative_ontology:constraint_victim(redemption_ambiguity, theological_readers).
narrative_ontology:constraint_victim(redemption_ambiguity, pedagogical_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOCTRINAL READER (SNARE) — Reader seeking definitive spiritual meaning is trapped by the poem's structural refusal of resolution. Cannot exit the interpretive bind: the text provides enough redemptive imagery to promise meaning but withholds confirmation. Maximum extraction — the ambiguity extracts interpretive labor without delivering the doctrinal payoff the reader's framework requires.
constraint_indexing:constraint_classification(redemption_ambiguity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GRADUATE STUDENT (TANGLED ROPE) — Constrained by disciplinary expectations to produce novel readings, but also benefits from the ambiguity's generative capacity for thesis material. The constraint coordinates access to critical conversation while extracting interpretive labor. Mixed experience: the ambiguity enables scholarly production but demands continuous engagement with unresolvable tension.
constraint_indexing:constraint_classification(redemption_ambiguity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CRITICAL INDUSTRY (ROPE) — Academic publishers, conference organizers, and journal editors benefit from the ambiguity's inexhaustibility. Each generation produces new readings because the text refuses closure. Net beneficiary — the constraint coordinates ongoing scholarly production with minimal extraction from this position. The ambiguity is a renewable resource for critical output.
constraint_indexing:constraint_classification(redemption_ambiguity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLURALIST COALITION (ROPE) — New Critics, deconstructionists, and reader-response theorists see the ambiguity as vindication of interpretive multiplicity. Organized agents with theoretical frameworks that accommodate indeterminacy experience the constraint as coordination: the text demonstrates that meaning is constructed, not discovered. Low extraction because their frameworks expect and value ambiguity.
constraint_indexing:constraint_classification(redemption_ambiguity, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNDERGRADUATE TEACHER (TANGLED ROPE) — Faces pedagogical pressure to provide clear interpretive guidance while the text resists summary. The ambiguity coordinates engagement with modernist difficulty but extracts labor in managing student frustration. Cannot fully exit (curriculum requirements) but has some agency in framing the ambiguity as productive rather than obstructive.
constraint_indexing:constraint_classification(redemption_ambiguity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the ambiguity serves a genuine coordination function (enabling multiple interpretive communities to engage the text) while extracting from those requiring closure. The constraint is structurally hybrid: it coordinates pluralist discourse while suppressing doctrinal readings. Not a mountain — the ambiguity is a formal choice, not an inherent property of language. Not pure rope — the extraction from certainty-seeking readers is real and asymmetric.
constraint_indexing:constraint_classification(redemption_ambiguity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(redemption_ambiguity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(redemption_ambiguity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(redemption_ambiguity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(redemption_ambiguity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate. The ambiguity extracts interpretive labor from readers requiring closure — theological readers invest significant effort seeking definitive spiritual meaning the text structurally withholds. But extraction is not maximal: pluralist readers experience the ambiguity as generative rather than obstructive, and the critical industry benefits from ongoing interpretive production. The value reflects asymmetric extraction concentrated on certainty-seeking readers. Suppression (0.48): Moderate. The text's formal structure suppresses doctrinal readings by refusing to confirm redemptive imagery. The Upanishadic references and 'Shantih' provide enough evidence to sustain theological interpretation but not enough to close the question. Suppression is not total — some readers do achieve satisfying redemptive readings by privileging certain textual evidence. Theater ratio (0.35): Moderate-low. The ambiguity is not primarily performative — it is a genuine structural feature of the text's formal organization. Some theater exists in critical discourse (ritualized acknowledgment of 'productive ambiguity' without substantive engagement), but the core constraint is functional: the oscillation between redemption and nihilism is load-bearing for the poem's dual identity as modernist and spiritual text.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates indexical classification across reader positions. Doctrinal readers see a snare — the text promises spiritual meaning but structurally withholds it, extracting interpretive labor without payoff. Graduate students and teachers see tangled rope — the ambiguity both enables (thesis material, pedagogical engagement) and constrains (exhausted topic, student frustration). The critical industry and pluralist coalition see rope — the ambiguity coordinates ongoing scholarly production and vindicates interpretive multiplicity with minimal extraction from their positions. The analytical observer sees tangled rope at the civilizational scale — genuine coordination function (enabling multiple interpretive communities) with asymmetric extraction (suppressing doctrinal certainty). The perspectival gap is not 'which reading is correct?' but 'which structural position are you occupying?' The poem's ambiguity is simultaneously extractive and coordinative depending on the reader's framework and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to the ambiguity's extraction flow. Doctrinal readers are victims with trapped exit — they cannot abandon their framework's requirement for closure without ceasing to be doctrinal readers. High d, high chi. The critical industry is a beneficiary with arbitrage exit — they can engage or disengage from any particular reading without cost. Low d, low/negative chi. Graduate students are victims (must produce readings) but with constrained rather than trapped exit (can choose less ambiguous texts for future work). Moderate d, moderate chi. Pluralist coalition members are beneficiaries with mobile exit — their frameworks accommodate ambiguity, so they experience coordination rather than extraction. Low d, low chi. The analytical observer sees the hybrid structure: coordination for pluralists, extraction for doctrinalists.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the ambiguity is genuinely hybrid — it coordinates pluralist discourse while extracting from certainty-seeking readers. This is not mislabeled coordination (the extraction from doctrinal readers is real and asymmetric) nor mislabeled extraction (the coordination function for pluralist readers is genuine and load-bearing). The tangled_rope classification captures the structural duality: the same textual feature (oscillation between redemptive and nihilistic imagery) serves as coordination mechanism for some agents and extraction mechanism for others. The classification prevents both false negatives (ignoring extraction from theological readers) and false positives (ignoring coordination function for critical discourse). The ambiguity is not a natural law (mountain) — it is a formal choice Eliot made. It is not pure coordination (rope) — the suppression of doctrinal readings is structural, not incidental. It is not pure extraction (snare) — pluralist readers genuinely benefit. The hybrid classification is the structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorial_intent_vs_textual_autonomy,
    'Does Eliot''s documented spiritual trajectory (conversion to Anglo-Catholicism in 1927, four years post-publication) retrospectively resolve the ambiguity, or does the text''s formal structure override authorial biography?',
    'Comparative analysis of pre- and post-conversion critical reception; examination of Eliot''s own later commentary on the poem; theoretical adjudication between intentionalist and autonomist interpretive frameworks',
    'If intent resolves: the ambiguity is a historical artifact of incomplete information, reducing extractiveness. If text autonomous: the ambiguity is structural, confirming tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorial_intent_vs_textual_autonomy, conceptual, 'Whether authorial intent can resolve textual ambiguity').

omega_variable(
    redemptive_imagery_sufficiency,
    'Do the Upanishadic references (''Datta. Dayadhvam. Damyata.'') and the closing ''Shantih shantih shantih'' constitute sufficient textual evidence for a redemptive reading, or are they undermined by the surrounding fragmentation?',
    'Close reading consensus among scholars with expertise in both Sanskrit literature and modernist poetics; quantitative analysis of redemptive vs nihilistic imagery distribution across the poem''s five sections',
    'If sufficient: doctrinal readers'' extraction is self-imposed (they ignore available evidence), reducing measured extractiveness. If insufficient: the text structurally withholds resolution, confirming high extraction from certainty-seeking readers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(redemptive_imagery_sufficiency, empirical, 'Whether textual evidence supports redemptive reading').

omega_variable(
    reader_framework_determinism,
    'Is interpretive outcome deterministically predicted by reader''s prior spiritual framework (theological readers see redemption, secular readers see nihilism), or does the text exert independent constraint on interpretation?',
    'Empirical study of reading outcomes across reader populations with documented spiritual commitments; identification of readers whose interpretations contradict their prior frameworks',
    'If deterministic: the ambiguity is a mirror (low inherent extractiveness, high theater). If text constrains: the ambiguity is a structural feature that extracts differentially based on framework mismatch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_framework_determinism, empirical, 'Whether reader framework determines interpretive outcome').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(redemption_ambiguity, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(redemp_theater_1922, redemption_ambiguity, theater_ratio, 0, 0.25).
narrative_ontology:measurement(redemp_theater_1947, redemption_ambiguity, theater_ratio, 25, 0.3).
narrative_ontology:measurement(redemp_theater_1972, redemption_ambiguity, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(redemp_extract_1922, redemption_ambiguity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(redemp_extract_1947, redemption_ambiguity, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(redemp_extract_1972, redemption_ambiguity, base_extractiveness, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(redemption_ambiguity, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of mythic_scaffolding_vs_formal_fragmentation. The formal fragmentation that prevents mythic unity also prevents redemptive closure. The two constraints share structural DNA but have different epsilon values: mythic_scaffolding addresses the poem's overall organizational principle (ε ≈ 0.38), while redemption_ambiguity addresses the specific interpretive bind at the ending (ε = 0.42). The redemption ambiguity is a localized intensification of the broader fragmentation constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
