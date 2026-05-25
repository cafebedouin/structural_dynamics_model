% ============================================================================
% CONSTRAINT STORY: ulysses_chp02
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_school_1904, []).

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
 *   constraint_id: ulysses_chp02
 *   human_readable: The Nightmare of History (Dalkey School)
 *   domain: economic/social/political
 *
 * SUMMARY:
 *   Stephen Dedalus arrives at the Dalkey School in 1904 to teach history and
 *   English. The school is operated under Church authority within the
 *   framework of British colonial rule. Stephen is tasked with teaching Irish
 *   history through an imperial lens—a curriculum that presents the
 *   colonization and subjugation of Ireland as historical inevitability,
 *   often framed as redemptive through Christian conversion and British
 *   civilization. For Stephen, this is the 'nightmare of history': he must
 *   teach a narrative that legitimizes the very oppression he suffers, to
 *   students who are being conditioned to accept their own subjugation as
 *   natural and divinely ordained. The constraint is structural: he cannot
 *   quit without severe economic and social consequences, the curriculum is
 *   imposed by higher authority (Church and colonial administration),
 *   alternative narratives are unavailable within official channels, and the
 *   entire system is designed to suppress the intellectual autonomy and
 *   cultural identity of Irish youth. Stephen's teaching is an act of
 *   complicity he cannot escape—a perfect Snare.
 *
 * KEY AGENTS:
 *   - Stephen Dedalus: Primary victim (powerless/trapped) — forced to teach imperial history while intellectually aware of its falsity and oppressiveness; no viable exit from employment; subject to enforced complicity
 *   - Student Consciousness: Collective victim (moderate/constrained) — young minds systematically shaped by imperial narratives with no access to counter-narratives; constrained agency through family and social pressure
 *   - The Colonial Church: Primary beneficiary (institutional/arbitrage) — uses school as mechanism for spiritual and ideological control; maintains institutional authority over conscience and curriculum
 *   - British Imperial Authority: Co-beneficiary (institutional/arbitrage) — school serves as mechanism for disseminating imperial ideology and training compliant colonial subjects
 *   - Irish Nationalist Movement: Organized observer (organized/constrained) — recognizes the snare but lacks sufficient institutional power to dismantle it; constrained exit options
 *   - School Administration: Institutional performer (institutional/arbitrage) — maintains the curriculum through inertia and performative ritual; sees own system as degraded but continues it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp02, 0.68).
domain_priors:suppression_score(ulysses_chp02, 0.72).
domain_priors:theater_ratio(ulysses_chp02, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp02, extractiveness, 0.68).
narrative_ontology:constraint_metric(ulysses_chp02, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ulysses_chp02, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp02, snare).
narrative_ontology:human_readable(ulysses_chp02, "The Nightmare of History (Dalkey School)").
narrative_ontology:topic_domain(ulysses_chp02, "economic/social/political").

domain_priors:requires_active_enforcement(ulysses_chp02).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp02, colonial_church).
narrative_ontology:constraint_beneficiary(ulysses_chp02, british_imperial_authority).
narrative_ontology:constraint_victim(ulysses_chp02, stephen_dedalus).
narrative_ontology:constraint_victim(ulysses_chp02, student_consciousness).
narrative_ontology:constraint_victim(ulysses_chp02, intellectual_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STEPHEN DEDALUS (SNARE) — Cannot exit the educational apparatus without severe social and economic consequences. Forced to internalize historical narratives of colonial subjugation and religious authority. No genuine alternatives exist within his social class. Maximum extraction: his labor (teaching history), his intellectual development (shaped by imperial curriculum), and his spiritual autonomy (Catholic doctrine) are all subordinated to institutional goals. The 'nightmare of history' is something he must teach while being trapped within it.
constraint_indexing:constraint_classification(ulysses_chp02, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STUDENT CONSCIOUSNESS (SNARE) — Young minds are systematically conditioned to accept imperial history as inevitable and redemptive. Suppression includes: no alternative historical narratives available, religious authority reinforces historical submission, family pressure maintains compliance. Exit options are constrained but not entirely eliminated—some students may later question, but the formative damage is done. Moderate power because students form a collective that sustains the system through compliance; yet individually they are powerless.
constraint_indexing:constraint_classification(ulysses_chp02, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COLONIAL CHURCH & BRITISH IMPERIAL AUTHORITY (ROPE) — From this institutional perspective, the school functions as pure coordination: disseminating imperial ideology, training compliant subjects, legitimizing historical subjugation as divine will. The beneficiaries experience the constraint as a coordination mechanism (how to maintain control over a colonized population). They have complete exit options (arbitrage)—they can redirect resources, change curricula, or withdraw institutional support. They see the structure as functional, not extractive from their position.
constraint_indexing:constraint_classification(ulysses_chp02, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IRISH NATIONALIST MOVEMENT (TANGLED ROPE) — Organized agents recognize the school as both coordinating imperial control AND extracting Irish cultural autonomy. They benefit from coordination of educational access (schooling exists) but are victimized by its content and direction. Constrained exit options: cannot easily build parallel schools without resources or political recognition. Must work within and against the system simultaneously. This perspective sees the snare clearly but lacks sufficient power to demolish it unilaterally.
constraint_indexing:constraint_classification(ulysses_chp02, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SCHOOL ADMINISTRATION (PITON) — The administrators see the curriculum as degraded—they perform the ritual of teaching imperial history knowing it no longer commands genuine belief among intellectuals. The apparatus persists through institutional inertia: it is easier to continue the old curriculum than to redesign. Theater ratio is high (0.65) because much of the pedagogical activity is performative—delivering prescribed narratives, enforcing compliance through ritual rather than genuine intellectual engagement. The primary function (control) is maintained; the secondary function (actual education) has atrophied.
constraint_indexing:constraint_classification(ulysses_chp02, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE MOUNTAIN VIEW (MOUNTAIN) — From a civilizational perspective, one might argue that the transmission of cultural narratives to young people is an immutable feature of all societies—every culture must teach history to survive. But this naturalizes the snare: it confuses the structural necessity (some transmission of cultural knowledge) with the specific contingent implementation (imperial history as the only available narrative, with no counter-narratives permitted). The constraint is NOT that history must be taught; it is that THIS history, in THIS way, with THIS suppression of alternatives, is enforced. The mountain classification is a false summit—the analytical observer risks naturalizing institutional oppression as natural law.
constraint_indexing:constraint_classification(ulysses_chp02, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp02_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp02, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp02, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp02, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp02, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp02_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from Stephen multiple forms of value: his intellectual labor (teaching), his temporal resources (employment), his spiritual autonomy (forced participation in religiously-sanctioned narratives), and his moral agency (complicity in oppressing students). The measurement trajectory shows increasing extractiveness from 0.55 to 0.68 as Stephen internalizes the contradiction between his intellectual awareness and his enforced role—as the constraint deepens its grip through accumulated complicity. Suppression (0.72): High. Multiple mechanisms suppress exit and alternatives: (1) Economic dependence—Stephen needs the salary; (2) Social pressure—Irish Catholic families depend on Church-approved education and employment; (3) Narrative monopoly—imperial history is the only official curriculum available; (4) Authority enforcement—the Church and colonial administration enforce compliance; (5) No institutional recognition of counter-narratives—Irish nationalist history is not taught in official schools. Theater ratio (0.65): Moderate-high. Much of the pedagogical activity is performative ritual: the schoolmaster recites prescribed narratives, students perform belief, examinations test memorization rather than understanding. The theater increases over the interval as Stephen becomes more aware that the entire apparatus is maintained through performative compliance rather than genuine conviction. Claimed type (Snare): The engine gates verify: extractiveness (0.68) > 0.46 ✓, suppression (0.72) > 0.60 ✓, victims declared (stephen_dedalus, student_consciousness, intellectual_autonomy) ✓.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer risks committing a false natural law by framing the 'nightmare of history' as an immutable feature of any civilization that transmits historical knowledge. This naturalizes the snare. The actual constraint is not 'history must be taught' (true and universal) but 'THIS oppressive history, with THIS monopoly on narrative, enforced through THIS institutional apparatus, with NO counter-narratives permitted' (contingent and institutional). The perspectival gap between Stephen's Snare and the Church's Rope reveals that 'the nightmare' is not nightmare because history is taught, but because history is weaponized—because the only available narrative is one that legitimizes Stephen's own subjugation and requires him to replicate that subjugation in students.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) captures each agent's structural position relative to the constraint's extraction mechanism. Stephen: victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extractiveness. Student consciousness: victim status (indirect—they are being shaped) + constrained exit → d ≈ 0.85 → f(d) ≈ 1.15 → high experienced extractiveness. Church/Imperial Authority: beneficiary status + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative experienced extractiveness (they are subsidized). Nationalist movement: victim status (culture is being suppressed) + constrained exit → d ≈ 0.75 → f(d) ≈ 1.05 → moderate-high experienced extractiveness, but the organized power atom modulates this—they have some collective agency. School administration: beneficiary status (maintains institutional resources) + arbitrage exit → d ≈ 0.15 → f(d) ≈ -0.01 → near-zero experienced extractiveness, but the piton classification overrides this because the theater_ratio gate fires (0.65 > 0.70 threshold is not met; however, the inertial maintenance of degraded ritual is still captured).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The snare classification is robust across all perspectives except the false Mountain and the Rope (from the beneficiaries' view). The mandatrophy is avoided by recognizing that extractive and coordinative functions can coexist in the same institution—the school DOES coordinate something (ideological uniformity, social control), but it DOES extract something more valuable (intellectual autonomy, cultural identity, moral agency). The Snare gate fires because: (1) extractiveness > 0.46, (2) suppression > 0.60, (3) victims are clearly identified, (4) the existence of the constraint depends on suppressing alternatives (without suppression, students could choose different narratives). The classification would collapse into pure Rope only if: (a) alternative narratives were freely available, AND (b) students could choose to teach them without penalty, AND (c) the main function was not extraction but genuine coordination. None of these conditions hold in 1904 Dublin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuineness_of_student_internalization,
    'Do students genuinely internalize the imperial historical narrative, or do they perform belief while maintaining private skepticism?',
    'Private letters, diaries, post-school testimony from students; comparison of public utterances vs private correspondence; longitudinal tracking of whether students maintain or abandon the taught narratives after leaving school',
    'If genuine internalization: suppression is near-total (0.85+), snare classification is robust. If performative: suppression is lower (0.55-0.65), and the constraint transitions toward scaffold (temporary theater maintained until exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuineness_of_student_internalization, empirical, 'Whether imperial history narrative is genuinely internalized or performatively maintained').

omega_variable(
    alternative_narrative_availability,
    'Are alternative Irish historical narratives actually unavailable to students, or do they exist in suppressed form that motivated students could access?',
    'Inventory of books available in Ireland 1904; survey of what nationalist families taught children; examination of underground printing and distribution networks; oral history of which students knew which counter-narratives',
    'If truly unavailable: suppression ≥ 0.70, snare is robust. If available but forbidden: suppression is lower (0.55-0.65), constraint appears as more of a tangled rope (some agency present).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_narrative_availability, empirical, 'Real availability of counter-narratives to students').

omega_variable(
    stephen_exit_feasibility,
    'What were Stephen''s actual exit options from teaching at Dalkey beyond quitting the school—could he emigrate, pursue other work, maintain social position?',
    'Analysis of Stephen''s economic resources, family circumstances, and social opportunities in 1904 Dublin; comparison with other Irish intellectuals'' actual choices and career paths; examination of what Stephen believed his options to be vs what they actually were',
    'If exit is genuinely impossible: exit_options = trapped, snare classification is maximal. If exit is possible but costly: exit_options = constrained, effective extractiveness diminishes somewhat (d = 0.80 rather than 0.95).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stephen_exit_feasibility, empirical, 'Stephen''s actual exit options from school employment').

omega_variable(
    institutional_collapse_timeline,
    'How long did the imperial curriculum maintain this suppressive power—when did alternative narratives become institutionally recognized rather than forbidden?',
    'Curriculum histories; timeline of Irish educational reform; comparison with neighboring colonized education systems (India, Egypt) for parallel timelines',
    'If suppression persists for 40+ years: this is a stable snare. If it collapses within 15 years: this is a scaffold with a real sunset (1904-1919). Classification shifts based on temporal horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_collapse_timeline, empirical, 'Duration of suppressive curriculum before institutional collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp02, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_theater_t0, ulysses_chp02, theater_ratio, 0, 0.5).
narrative_ontology:measurement(ulys_theater_t5, ulysses_chp02, theater_ratio, 5, 0.58).
narrative_ontology:measurement(ulys_theater_t10, ulysses_chp02, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(ulys_extractiveness_t0, ulysses_chp02, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ulys_extractiveness_t5, ulysses_chp02, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(ulys_extractiveness_t10, ulysses_chp02, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp02, information_standard).
narrative_ontology:affects_constraint(ulysses_chp02, colonial_education_systems).
narrative_ontology:affects_constraint(ulysses_chp02, irish_cultural_suppression).
narrative_ontology:affects_constraint(ulysses_chp02, church_institutional_authority).

% DUAL FORMULATION NOTE:
% The Dalkey School constraint is downstream of broader colonial and ecclesiastical authority structures. It represents a specific institutional implementation of cultural suppression through educational monopoly. The constraint family includes: (1) the general colonial education system (broader scope, lower epsilon because it includes some coordination), (2) Irish cultural suppression (broader scope, higher epsilon because it is pure extraction with no coordination), (3) Church institutional authority (broader scope, focuses on clerical control). This story focuses on the specific pedagogical mechanism through which imperial history is weaponized against students and through which complicity is extracted from teachers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
