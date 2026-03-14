% ============================================================================
% CONSTRAINT STORY: stephen_aesthetic_capture_1904
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stephen_aesthetic_capture_1904, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: stephen_aesthetic_capture_1904
 *   human_readable: Stephen Aesthetic Capture in Literary Canon Formation (1904)
 *   domain: literary_studies/cultural_authority
 *
 * SUMMARY:
 *   In 1904, Sir Leslie Stephen's influence on English literary canon
 *   formation had become nearly total within institutional contexts
 *   (universities, literary reviews, publishing judgments, educational
 *   curricula). Stephen's biographical method, emphasis on moral character
 *   over aesthetic innovation, and focus on English national literature as
 *   the measure of literary value became the framework through which all
 *   literary judgment was legitimated. This constraint captures how a
 *   specific aesthetic framework — which solved real problems of literary
 *   coordination in the 1870s-1880s — became an extractive institutional lock
 *   by 1904, suppressing alternative aesthetic traditions (working-class
 *   serialized fiction, continental decadence, aesthetic modernism) while
 *   performing coordination work it no longer genuinely accomplished. The
 *   constraint exhibits the full spectrum of DR classification: the Stephen
 *   establishment experiences pure coordination (Rope), emerging modernist
 *   movements see a temporary problem with sunset (Scaffold), provincial
 *   institutions face mixed coordination-extraction (Tangled Rope),
 *   working-class aesthetic traditions are trapped (Snare), the critical
 *   apparatus itself has become performative ritual (Piton), and the natural
 *   law perspective naturalizes what is actually contingent institutional
 *   choice (Mountain). The theater ratio rising from 0.42 to 0.68 reflects
 *   the mechanism: as new aesthetic forms emerged that the Stephen framework
 *   could not accommodate, defenders of the framework increasingly performed
 *   tradition-maintenance rather than practicing genuine aesthetic
 *   adjudication.
 *
 * KEY AGENTS:
 *   - Stephen Critical Establishment: Institutional beneficiary (institutional/arbitrage) — universities, literary reviews, publishing gatekeepers operating within Stephen framework; experiences constraint as coordinating mechanism enabling shared judgment
 *   - Working-Class Aesthetic Traditions: Primary victim (powerless/trapped) — serialized fiction, music hall culture, penny dreadfuls; unable to exit or achieve cultural legitimacy within Stephen framework; aesthetic authority completely extracted
 *   - Provincial Literary Societies: Secondary victim (moderate/constrained) — regional editors, local critics, provincial academies; constrained by prestige economy requiring adoption of metropolitan Stephens standards; participate in alternative aesthetics but at cost of national legitimacy
 *   - Emerging Modernist Movements: Organized opposition (organized/constrained) — decadents, naturalists, aesthetic modernists, little magazine networks; building alternative authority structures (continental aestheticism, coterie publishing); perceive Stephens canon as temporary obstacle with sunset
 *   - Victorian Critical Apparatus: Institutional maintainer (institutional/arbitrage) — university positions, reviewing infrastructure, curricular authority; maintains Stephen framework through institutional inertia despite declining explanatory power
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing as inevitable what is contingent institutional arrangement of canon formation and aesthetic authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stephen_aesthetic_capture_1904, 0.58).
domain_priors:suppression_score(stephen_aesthetic_capture_1904, 0.65).
domain_priors:theater_ratio(stephen_aesthetic_capture_1904, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stephen_aesthetic_capture_1904, extractiveness, 0.58).
narrative_ontology:constraint_metric(stephen_aesthetic_capture_1904, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(stephen_aesthetic_capture_1904, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stephen_aesthetic_capture_1904, tangled_rope).
narrative_ontology:human_readable(stephen_aesthetic_capture_1904, "Stephen Aesthetic Capture in Literary Canon Formation (1904)").
narrative_ontology:topic_domain(stephen_aesthetic_capture_1904, "literary_studies/cultural_authority").

domain_priors:requires_active_enforcement(stephen_aesthetic_capture_1904).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stephen_aesthetic_capture_1904, stephen_critical_establishment).
narrative_ontology:constraint_victim(stephen_aesthetic_capture_1904, alternative_aesthetic_frameworks).
narrative_ontology:constraint_victim(stephen_aesthetic_capture_1904, working_class_literary_culture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING-CLASS AND ALTERNATIVE AESTHETIC FRAMEWORKS (SNARE) — Those outside the Stephen critical establishment cannot escape the constraint once literary canon becomes institutionalized. Their aesthetic traditions (folk ballads, music hall culture, serialized fiction) are excluded from legitimate cultural discourse. No exit mechanism available; full extraction of cultural authority. Theater-bound: suppression operates through canonical definition, not coercion.
constraint_indexing:constraint_classification(stephen_aesthetic_capture_1904, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROVINCIAL LITERARY SOCIETIES (TANGLED ROPE) — Constrained by the prestige economy: adoption of Stephen standards gains access to national literary networks, but at cost of suppressing local aesthetic preferences. Genuine coordination function exists (shared literary culture), but coordination requires asymmetric extraction (acceptance of metropolitan aesthetic authority). High suppression reflects that alternative judgments are treated as provincial rather than legitimately different.
constraint_indexing:constraint_classification(stephen_aesthetic_capture_1904, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STEPHEN CRITICAL ESTABLISHMENT (ROPE) — Institutions (universities, literary reviews, publishing houses) benefit from the constraint as pure coordination: shared aesthetic standards enable collective judgment and reduce uncertainty in canon formation. Extractive element is minimized from this perspective — the framework is experienced as solving a genuine coordination problem (how to adjudicate aesthetic value). Arbitrage available: institutions can defect to alternative aesthetic schemes if coordination advantage disappears.
constraint_indexing:constraint_classification(stephen_aesthetic_capture_1904, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMERGING MODERNIST MOVEMENTS (SCAFFOLD) — Literary movements emerging 1890-1920 (decadence, aestheticism, naturalism) perceive Stephen-based canon as temporary obstacle with sunset: new aesthetic frameworks are building alternative authority structures (little magazines, coterie publishing, continental aesthetic theory) that bypass Victorian institutional validation. Organized opposition creates exit pathway, reducing effective extraction. Theater ratio rises as Stephen canon becomes defensive, performing tradition rather than discovering value.
constraint_indexing:constraint_classification(stephen_aesthetic_capture_1904, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VICTORIAN LITERARY CRITICISM APPARATUS (PITON) — By 1904, the critical apparatus built around Stephen (biographical method, moral evaluation, English national literature framing) is becoming increasingly performative. Critics maintain the apparatus through institutional habit — university chairs, reviewing posts, canon curricula — despite emerging awareness that the framework cannot accommodate new literary forms. Theater ratio (0.68) reflects sustained performative activity around a framework whose explanatory power is declining. The apparatus persists through institutional inertia rather than genuine coordinating function.
constraint_indexing:constraint_classification(stephen_aesthetic_capture_1904, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, aesthetic authority concentration may appear as an inevitable feature of cultural organization: all societies must have some canon-formation mechanism, and that mechanism requires centralized judgment. From this view, Stephen capture is a natural law of literary culture. However, the structural data reveals this as false naturalization — the constraint's extractiveness (0.58) and suppression (0.65) reflect specific institutional choices (university curricula, reviewing authority, publishing gatekeeping) rather than inevitable features of aesthetic coordination. The mountain classification masks contingency as necessity.
constraint_indexing:constraint_classification(stephen_aesthetic_capture_1904, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stephen_aesthetic_capture_1904_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stephen_aesthetic_capture_1904, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stephen_aesthetic_capture_1904, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(stephen_aesthetic_capture_1904, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(stephen_aesthetic_capture_1904, TR),
    TR >= 0.70.

:- end_tests(stephen_aesthetic_capture_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Stephen framework provides genuine benefit to institutions adopting it (coordination of judgment, reduction of aesthetic uncertainty), but extraction of authority from alternative traditions is substantial. The value increased from 0.35 to 0.58 over the interval as framework became more defensively entrenched and less capable of accommodating new forms. The constraint extracts aesthetic legitimacy from those outside the framework and transfers it to institutional interpreters operating within Stephen methodology. Suppression (0.65): Moderate-high. Significant barriers to alternative aesthetic recognition include institutional monopoly on literary reviewing, university curricular authority, publishing gatekeeping, and cultural prestige concentration. Working-class aesthetics face structural suppression — exclusion from legitimate discourse not through explicit prohibition but through systematic non-recognition. Some exit pathways exist (alternative publishing, coterie networks) but require abandoning claims to national legitimacy. Theater ratio (0.68): High and increasing. By 1904, critical apparatus was performing tradition-maintenance. Stephen biographical method was treated as objective methodology despite emergence of alternative critical frameworks (formalism, aestheticism, naturalism) that could not be accommodated within the biographical/moral paradigm. The increase from 0.42 to 0.68 reflects the constraint's defensive posturing as it lost capacity to explain new literary phenomena.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental structural difference between institutional beneficiaries and cultural victims. The Stephen establishment experiences pure coordination (Rope) — they genuinely need shared aesthetic standards to function as a national literary community. Their experience of the constraint is functional and reversible (arbitrage available). Working-class aesthetic traditions experience extraction (Snare) — their cultural voice is systematically delegitimized with no voice in the frameworks determining legitimacy. Their experience is zero-agency and irreversible (trapped). Provincial societies experience the constraint as mixed (Tangled Rope) — they benefit from coordinated national literary culture but at cost of suppressing local aesthetic preferences. Modernist movements experience it as temporary (Scaffold) — they see emerging alternative frameworks (continental aestheticism, coterie magazines, new critical methods) providing exit pathways within a generation. The analytical observer risks the false naturalization: seeing aesthetic canon formation as inevitably requiring centralized authority rather than recognizing the Stephens framework as one contingent institutional choice among alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the constraint. Stephen institutional establishment (institutional/arbitrage) has low d: they benefit, they can defect to alternative frameworks if coordination advantage disappears, experienced extraction runs toward them. Provincial societies (moderate/constrained) have moderate d: they face real costs to exit (loss of national prestige) but retain some agency and some benefit from shared standards. Working-class aesthetic traditions (powerless/trapped) have high d: they cannot organize exit or defect without total abandonment of cultural voice, experienced extraction is maximal. Modernist movements (organized/constrained) have moderate-to-low d: they are organizing alternative authority structures and can exit toward new frameworks, though at cost of institutional prestige during transition period. The perspectival gap between beneficiary (rope) and victim (snare) reflects that the same constraint solves real problems for those with institutional power while systematically suppressing those without it.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE CAPTURE IN CULTURAL AUTHORITY: The Stephen constraint demonstrates how a genuine coordination mandate (establishing shared literary culture) transforms into extractive authority capture. 1870-1880: Stephen biographical method solves real problem — enables critics and educators to make shared judgments about literary value and train new readers using consistent framework. 1885-1895: Framework becomes institutionalized — university positions, reviewing authority, publishing networks consolidate around Stephen methodology. 1895-1904: Framework becomes extractive — increasingly applied not to coordinate legitimate differences but to delegitimize alternative aesthetics; working-class culture, continental influences, and emerging modernism are excluded not through academic debate but through institutional non-recognition. Theater ratio rising to 0.68 signals the mandatrophy: critical apparatus maintains framework through performative tradition-maintenance rather than genuine aesthetic adjudication. The constraint is resolves mandatrophy by showing that coordinate mandate (national literary culture) and extractive capture (Stephens monopoly) are not separate phenomena but a single institutional dynamic: coordination always requires some agents to accept authority of others, and that acceptance, once institutionalized, becomes difficult to withdraw. The emergence of alternative authority structures (modernist aesthetics, continental theory, little magazines) begins the sunset process by rebuilding coordination around competing frameworks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stephen_influence_scope_boundary,
    'How far beyond metropolitan literary institutions did Stephen aesthetic capture actually extend in 1904? Was suppression of alternatives structural (enforced across national institutions) or performative (limited to cultural prestige)?',
    'Analysis of provincial publishing, regional literary societies, and working-class reading practices 1895-1910. Measurement of non-Stephens aesthetic authority in parallel cultural channels (serialized fiction, music hall, penny dreadfuls).',
    'If truly national: constraint is high-suppression snare (extraction enforced across channels). If limited to prestige institutions: constraint is lower-suppression tangled rope (alternative pathways available to those outside canon).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stephen_influence_scope_boundary, empirical, 'Geographic scope of Stephen aesthetic capture''s suppression mechanism').

omega_variable(
    coordination_benefit_reality,
    'Did Stephen-based canon actually solve a genuine coordination problem (enabling shared literary discourse), or was coordination framing a cover story for prestige consolidation?',
    'Historical analysis of literary discourse before and after Stephen canonization. Measurement of whether shared Stephen reference actually enabled broader participation in literary culture or restricted it to those with institutional access.',
    'If coordination was genuine: tangled rope classification confirmed (hybrid coordination-extraction). If purely cover story: snare classification more accurate (extraction disguised as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_benefit_reality, conceptual, 'Whether Stephen canon served genuine coordination function or only prestige consolidation').

omega_variable(
    modernist_alternative_scalability,
    'Could the emerging aesthetic alternatives (decadence, naturalism, continental aestheticism) have scaled to provide national-level canon formation, or were they inherently coterie-based?',
    'Comparative institutional analysis: Did alternative movements develop educational institutions, reviewing infrastructure, publishing networks sufficient for national coordination? Timeline of when alternative aesthetics gained university curricular presence.',
    'If scalable: scaffold sunset is structural (real exit pathway). If not: modernist movements were local resistance, not systematic alternative, and extractive constraint persisted longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernist_alternative_scalability, empirical, 'Scalability of alternative aesthetic frameworks to national-level canon formation').

omega_variable(
    suppression_internalization_depth,
    'To what extent did working-class and regional agents internalize Stephen aesthetic standards as legitimate versus resisting them as imposed?',
    'Literary analysis of provincial and working-class publications, personal correspondence, and cultural production. Evidence of aesthetic judgment made on Stephens-derived criteria versus alternative criteria.',
    'If deeply internalized: suppression operates as identity-locked constraint (agents see Stephen standards as natural). If resisted: suppression is structural (external enforcement), and constraint is more explicitly extractive from victim perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Degree of internalization of Stephen aesthetic standards across class boundaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stephen_aesthetic_capture_1904, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stephenaes_tr_t0, stephen_aesthetic_capture_1904, theater_ratio, 0, 0.42).
narrative_ontology:measurement(stephenaes_tr_t4, stephen_aesthetic_capture_1904, theater_ratio, 4, 0.55).
narrative_ontology:measurement(stephenaes_tr_t8, stephen_aesthetic_capture_1904, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(stephenaes_be_t0, stephen_aesthetic_capture_1904, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stephenaes_be_t4, stephen_aesthetic_capture_1904, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(stephenaes_be_t8, stephen_aesthetic_capture_1904, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stephen_aesthetic_capture_1904, identity_coordination).
narrative_ontology:affects_constraint(stephen_aesthetic_capture_1904, victorian_literary_curriculum_formation).
narrative_ontology:affects_constraint(stephen_aesthetic_capture_1904, working_class_cultural_exclusion_mechanisms).

% DUAL FORMULATION NOTE:
% Stephen aesthetic capture is decomposed into two aspects: (1) institutional canon formation (coordinate authentication mechanism with extractive overlay), and (2) class-based cultural delegitimization (systematic suppression of working-class aesthetic traditions). These are structurally distinct — canon coordination could theoretically persist without class extraction if alternative aesthetic frameworks were afforded institutional legitimacy. Separate stories model the domain-specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stephen_aesthetic_capture_1904, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
