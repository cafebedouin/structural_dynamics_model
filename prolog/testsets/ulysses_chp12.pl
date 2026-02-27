% ============================================================================
% CONSTRAINT STORY: ulysses_chp12
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_cyclops_1904, []).

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
 *   constraint_id: ulysses_chp12
 *   human_readable: The Cyclopean Snare (Barney Kiernan's Pub)
 *   domain: social/political/nationalist
 *
 * SUMMARY:
 *   Barney Kiernan's pub in Dublin, 1904, functions as a structural site
 *   where nationalist fervor operates simultaneously as pure coordination for
 *   the 'true men' and as a snare of xenophobic extraction targeting Leopold
 *   Bloom and other outsiders. The Citizen and his coalition experience the
 *   constraint as Rope — a voluntary coordination mechanism for nationalist
 *   sentiment and identity affirmation. Bloom, an outsider by virtue of his
 *   Jewish heritage and cosmopolitan disposition, experiences the identical
 *   social space as a Snare — he is verbally assaulted, mocked, socially
 *   excluded, and ultimately attacked with a biscuit tin. The same structural
 *   phenomenon (nationalist pub culture) classifies as Rope from the
 *   beneficiary perspective, Snare from the victim perspective, Tangled Rope
 *   from the perspective of moderate participants caught between coordination
 *   benefits and coercive pressure, and Piton from the
 *   institutional/civilizational view (nationalist fervor as performative
 *   ritual disconnected from material political outcome). This constraint is
 *   an exemplar of how indexical classification reveals the perspectival
 *   reality of social violence: the perpetrators experience justice and
 *   coordination; the victims experience pure extraction.
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Primary victim (powerless/trapped) — bears full extraction; cannot exit without abandonment of social participation; experiences verbal assault and physical violence
 *   - The Citizen: Primary beneficiary (institutional/arbitrage) — leads nationalist coalition; benefits from coordination function; enforces group boundaries; has full exit option
 *   - Nationalist Coalition ('True Men'): Beneficiary collective (organized/arbitrage) — experiences pure coordination; voluntary participation; shared identity affirmation; no extraction experienced
 *   - Foreign Residents and Outsiders: Secondary victims (powerless/trapped) — systematically excluded and extracted from through social and economic marginalization
 *   - Casual Pub Attendees: Ambiguous participants (moderate/constrained) — experience both coordination benefits (social belonging) and coercive pressure (forced conformity); constrained exit
 *   - Barney Kiernan's Pub: Institutional theater (institutional/arbitrage) — site of nationalist performance; coordination mechanism for in-group; enforcement mechanism against outsiders
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp12, 0.68).
domain_priors:suppression_score(ulysses_chp12, 0.72).
domain_priors:theater_ratio(ulysses_chp12, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp12, extractiveness, 0.68).
narrative_ontology:constraint_metric(ulysses_chp12, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ulysses_chp12, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp12, snare).
narrative_ontology:human_readable(ulysses_chp12, "The Cyclopean Snare (Barney Kiernan's Pub)").
narrative_ontology:topic_domain(ulysses_chp12, "social/political/nationalist").

domain_priors:requires_active_enforcement(ulysses_chp12).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp12, nationalist_coalition).
narrative_ontology:constraint_beneficiary(ulysses_chp12, the_citizen).
narrative_ontology:constraint_victim(ulysses_chp12, leopold_bloom).
narrative_ontology:constraint_victim(ulysses_chp12, foreign_resident_outsiders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEOPOLD BLOOM (SNARE) — Physically present in the pub but socially trapped. Cannot exit without abandonment of his daily routine and social participation. Experiences maximum extraction: verbal assault, mockery, social exclusion, culminating in physical violence. No alternatives presented; trapped exit option; powerless relative to the mobilized nationalist coalition.
constraint_indexing:constraint_classification(ulysses_chp12, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: FOREIGN RESIDENTS / OUTSIDERS (SNARE) — Systematically extracted from by nationalist enforcement mechanisms. Cannot exit Dublin society without abandonment of livelihood and social ties. Experience suppression through social exclusion, economic marginalization, and threat of violence. Institutional exclusion from 'true men' coalition creates sustained extraction mechanism. Powerless collective; trapped by economic and social dependency.
constraint_indexing:constraint_classification(ulysses_chp12, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE NATIONALIST COALITION / 'TRUE MEN' (ROPE) — Pure coordination mechanism for in-group members. The pub functions as a node for mobilizing nationalist sentiment, sharing grievances, and reinforcing collective identity. No extraction experienced by coalition members — participation is voluntary, benefits are clear (solidarity, identity affirmation, social status), and exit is available (one can choose not to attend). Sees the constraint as a coordination solution to nationalist political demands.
constraint_indexing:constraint_classification(ulysses_chp12, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: THE CITIZEN (ROPE) — Institutional leader who coordinates nationalist fervor and enforces in-group boundaries. Benefits from coalition solidarity and his recognized authority. Experiences the pub as a functional coordination mechanism for nationalist ideology and action. Has full arbitrage options — can exit or redefine the coalition at will. No extraction experienced; pure beneficiary from the coordination function.
constraint_indexing:constraint_classification(ulysses_chp12, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: CASUAL PUB ATTENDEES (TANGLED ROPE) — Regular patrons who participate in nationalist discourse but lack full agency in enforcement. Experience both coordination (social belonging, shared political identity) and extraction (pressure to conform, mockery if insufficiently nationalist, implicit coercion to participate in or tolerate violence). Constrained exit — abandoning the pub means loss of social ties but also escape from coercion. Moderate power relative to nationalist leadership; experience asymmetric extraction wrapped in coordination benefits.
constraint_indexing:constraint_classification(ulysses_chp12, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: THE PUB AS INSTITUTIONAL THEATER (PITON) — Barney Kiernan's pub has become a performative site of nationalist fervor. The rituals of drinking, conversation, and invocation of nationalist heroes are largely theatrical — the actual political function is minimal, but the performance persists through institutional inertia and nostalgia. Theater ratio high (0.58): much of the nationalist assertion is performative speech unconnected to material political outcome. Yet the theater produces real violence (biscuit tin assault), revealing the piton's degradation: the institution persists as ritual even as its original coordinating function has atrophied relative to the extraction it enables.
constraint_indexing:constraint_classification(ulysses_chp12, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/anthropological perspective, tribal in-group/out-group boundaries are inherent to human social organization. Exclusion of foreigners, enforcement of group identity, and social violence toward outsiders appear as natural properties of nationalist sentiment. The constraint looks like an immutable feature of ethnic and political identity. However, the structural data contradicts this: the extraction (0.68) and suppression (0.72) are contingent on institutional choices (pub as enforcement site, nationalism as mobilizing ideology), not laws of nature. Engine false summit detector identifies naturalization.
constraint_indexing:constraint_classification(ulysses_chp12, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp12_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp12, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp12, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp12, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp12, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp12_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Bloom's experience of the pub is dominated by extraction — he is verbally attacked, mockery targets his outsider status, and his attempt to counter the nationalist narrative with rational argument is met with dismissal and escalating hostility culminating in physical violence. The trajectory from 0.52 to 0.68 reflects intensification as the evening progresses and nationalist fervor peaks. The extraction is not total (0.85+) because Bloom retains some social presence and some participants (Crofton) show ambivalence; but the dominant experience is extraction from a trapped position. Suppression (0.72): High. Multiple suppression mechanisms operate: social mockery forces conformity to nationalist sentiment; economic dependency on pub-frequenting networks (trade, social capital) makes exit costly; implicit threat of violence enforces exclusion; nationalist ideology delegitimizes outsider perspective. Bloom's rational counter-arguments are suppressed not through debate but through institutional dismissal. Theater ratio (0.58): Moderate-high. The nationalist assertions in the pub are substantially performative — invocations of nationalist heroes, grandiose rhetoric about Irish destiny, ritualistic drinking toasts. However, this theater is not purely hollow: it produces real violence (biscuit tin) and real social exclusion. The theater ratio reflects that much of the nationalist assertion is performative speech unconnected to material political outcome, yet the performance generates material harm.
 *
 * PERSPECTIVAL GAP:
 *   This is one of the sharpest perspectival gaps in the constraint corpus. The Citizen experiences pure coordination (Rope) — the pub is a functional mechanism for mobilizing nationalist identity and political sentiment among willing participants. Bloom experiences pure extraction (Snare) — he is trapped in a social context designed to exclude and mock him, with escalating hostility and ultimate violence. Casual attendees experience Tangled Rope — they benefit from social belonging and nationalist identity affirmation but are also coerced into conformity and implicitly pressured to participate in or tolerate mockery of outsiders. The pub-as-institution (Piton) perspective reveals the underlying degradation: nationalism has atrophied from political action into performative ritual, yet the ritual produces real violence. The analytical observer risks naturalizing the constraint as 'inherent tribalism' (Mountain), but the structural data reveals it as contingent on institutional choices (pub culture, nationalist ideology, economic power asymmetry). The perspectival gap is not merely cognitive — it reflects the real structural reality that insiders and outsiders occupy mutually incommensurable positions within the same social space.
 *
 * DIRECTIONALITY LOGIC:
 *   Bloom's directionality (d) is high (~0.85-0.95) because he is a trapped victim with no exit options from the constraint: powerless + trapped exit → d ≈ 0.85-0.95 → f(d) ≈ 1.25-1.42. His experienced extractiveness (chi) is computed as ε × f(d) × σ(S) = 0.68 × 1.35 × 0.8 (local scope) ≈ 0.73. The Citizen's directionality (d) is very low (~0.05) because he is a beneficiary with full arbitrage options: institutional + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12. His experienced extractiveness is 0.68 × (-0.12) × 0.8 ≈ -0.07 (negative chi: constraint subsidizes his position). Casual attendees occupy intermediate positions (d ≈ 0.50-0.60), experiencing both coordination benefits and coercive pressure. The perspectival gap is maximal: the same constraint produces wildly different experienced extractiveness depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint demonstrates why the six-type classification system is necessary. A one-dimensional framework (extraction scale: 0-1) would either classify Barney Kiernan's pub as Rope (emphasizing the genuine coordination function for nationalist members) or as Snare (emphasizing the genuine extraction targeting outsiders), but not both. The mandatrophy would be irresolvable: 'Is this Rope or Snare?' The answer is: both, depending on the observer. The constraint resolves the mandatrophy by showing that the classification is not a property of the constraint in isolation but of the constraint-observer pair. From the Citizen's position (institutional, arbitrage), it is genuinely Rope. From Bloom's position (powerless, trapped), it is genuinely Snare. The Piton classification for the pub-as-institution reveals that the constraint has begun to degrade — the nationalism has become increasingly theatrical relative to its political function, yet the theater continues to produce real violence. This degradation pattern (increasing theater_ratio while extractiveness remains high) is diagnostic of institutional rot: the constraint persists not because it solves a coordination problem but because it has achieved self-perpetuation through ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_participation_ambiguity,
    'Is Bloom''s presence in the pub genuinely voluntary or does social/economic pressure constitute a de facto trap?',
    'Historical analysis of Bloom''s alternative sites of social participation; examination of economic dependency on pub-frequenting networks; comparative study of other outsider responses to nationalist pressure',
    'If genuinely voluntary: classification shifts toward Tangled Rope for Bloom (some benefit from pub participation). If de facto trapped: Snare classification holds; suppression mechanism is implicit rather than explicit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_participation_ambiguity, conceptual, 'Ambiguity in whether Bloom''s participation is voluntary').

omega_variable(
    coordination_vs_extraction_boundary,
    'Where is the boundary between the pub''s function as nationalist coordination and its function as extraction mechanism against outsiders?',
    'Textual analysis of pub conversation: which utterances serve coordination (in-group solidarity) and which serve extraction (outsider mockery/exclusion); historical patterns of nationalist pubs with and without outsider assault',
    'If coordination is primary function: classification shifts to Tangled Rope for coalition (hybrid coordination-extraction). If extraction is equally primary: Snare classification confirmed for outsiders, Rope classification for insiders stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Boundary between coordination and extraction functions').

omega_variable(
    violence_as_constraint_or_outcome,
    'Is the biscuit tin assault the constraint itself, or the outcome of a pre-existing extraction mechanism?',
    'Examination of whether violence was predictable from nationalist rhetoric; study of other Dublin pub incidents; analysis of whether suppression (0.72) requires explicit violence or operates through social coercion alone',
    'If violence is outcome: suppression metric may be overstated (social exclusion alone is 0.50-0.60). If violence is structural feature: suppression (0.72) is appropriate and the snare classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(violence_as_constraint_or_outcome, empirical, 'Whether violence is intrinsic to the constraint or its outcome').

omega_variable(
    outsider_coalition_possibility,
    'Could Bloom and other outsiders form a counter-coalition to escape the snare, or is the power disparity inherently blocking collective action?',
    'Historical analysis of outsider collective responses to nationalist snares; examination of literary treatment of Bloom''s isolation (whether it reflects social reality or narrative choice); study of diaspora/outsider coalition formation in nationalist contexts',
    'If counter-coalition is structurally possible: Dynamic Coalition extension applies; powerless agents with critical mass can upgrade to organized. If power disparity is inherent: Snare classification holds without coalition modification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outsider_coalition_possibility, conceptual, 'Possibility of outsider coalition formation to escape snare').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp12, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp12, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ulys_tr_t5, ulysses_chp12, theater_ratio, 5, 0.5).
narrative_ontology:measurement(ulys_tr_t10, ulysses_chp12, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp12, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ulys_be_t5, ulysses_chp12, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(ulys_be_t10, ulysses_chp12, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp12, enforcement_mechanism).
narrative_ontology:affects_constraint(ulysses_chp12, irish_nationalist_movement_1900s).
narrative_ontology:affects_constraint(ulysses_chp12, jewish_diaspora_dublin_exclusion).

% DUAL FORMULATION NOTE:
% The Cyclopean Snare is downstream of the broader Irish nationalist movement (which drives ideology) but represents a distinct structural constraint (the pub as enforcement site). The upstream constraint has its own extractiveness reflecting nationalist political mobilization; the Cyclopean Snare reflects the local enforcement mechanism and its targeting of outsiders. These are linked by causality (nationalist ideology enables pub enforcement) but have distinct metrics and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp12, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
