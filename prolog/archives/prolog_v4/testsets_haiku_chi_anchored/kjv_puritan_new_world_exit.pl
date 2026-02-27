% ============================================================================
% CONSTRAINT STORY: kjv_puritan_new_world_exit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_puritan_new_world_exit, []).

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
 *   constraint_id: kjv_puritan_new_world_exit
 *   human_readable: The Puritan Textual Re-Indexing (KJV in the New World)
 *   domain: political/religious
 *
 * SUMMARY:
 *   The King James Version of the Bible functioned as a primary tool of
 *   cultural and religious control across the Atlantic world from its
 *   publication in 1611 through the late 18th century. In England, the KJV
 *   was mandated by the Crown as the sole legitimate English Bible,
 *   suppressing rival translations (especially the Geneva Bible, which
 *   contained Puritan marginal notes) and fragmenting dissenting Protestant
 *   identity. For Puritan separatists and dissenters in England, the KJV
 *   represented a snare: a constraint imposed through state power that had no
 *   legitimate domestic exit. The suppression was enforced through book
 *   burning, imprisonment, fines, and threat of execution. However, the
 *   geographic expansion of English colonialism created a spatial exit:
 *   Puritans who emigrated to New England experienced the KJV differently.
 *   The same text that suppressed them in England became a coordination
 *   mechanism for their colonial community — a shared scripture that unified
 *   their covenant theology, enabled congregational governance, and provided
 *   doctrinal authority. Yet even in the colonies, the KJV carried embedded
 *   Anglican and royal assumptions that continued to constrain their
 *   theology. Meanwhile, as the KJV spread throughout colonial territories,
 *   it became a primary vector for indigenous cultural suppression and
 *   territorial extraction, used to justify missionary conversion, language
 *   replacement, and land claims. The constraint is thus a tangled rope in
 *   the colonies (mixing coordination function with residual extraction) and
 *   a snare in both metropolitan England (for dissenters) and indigenous
 *   territories (for Native Americans). Over time, as dissenter emigration
 *   succeeded and the Puritan schism became geographically decoupled, the
 *   KJV's suppression function atrophied in England, replaced by
 *   institutional inertia — the text persisted through ceremonial and
 *   establishment custom rather than active enforcement, degrading into a
 *   piton. The analytical observer risks naturalizing this entire arrangement
 *   as a natural law of textual stability, when it was actually a contingent
 *   product of political power, military expansion, and linguistic monopoly.
 *
 * KEY AGENTS:
 *   - English Crown and Established Church: Primary beneficiary (institutional/arbitrage) — maintains scriptural monopoly, suppresses dissent, coordinates realm through linguistic control
 *   - English Puritan Dissenters: Primary victim in England (powerless/trapped) — subjected to book bans, imprisonment, fines, exile; no legitimate domestic exit
 *   - Colonial Puritan Communities: Mixed agent (organized/mobile) — benefit from scriptural coordination for community building yet constrained by embedded Anglican doctrinal assumptions; exodus provides geographic exit
 *   - Indigenous Peoples of the New World: Primary victim in colonies (powerless/trapped) — experience KJV as vector for linguistic suppression, cultural erasure, and territorial extraction; no exit option
 *   - Colonial Merchants and Crown-Licensed Traders: Secondary beneficiary (organized/arbitrage) — use KJV as coordination mechanism for property claims, contract enforcement, and colonial legitimacy
 *   - Established Church Ritual Apparatus: Institutional actor (institutional/arbitrage) — maintains KJV through ceremony and tradition; suppression function atrophies over time, replaced by piton inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing political arrangement as linguistic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_puritan_new_world_exit, 0.52).
domain_priors:suppression_score(kjv_puritan_new_world_exit, 0.68).
domain_priors:theater_ratio(kjv_puritan_new_world_exit, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, extractiveness, 0.52).
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(kjv_puritan_new_world_exit, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_puritan_new_world_exit, tangled_rope).
narrative_ontology:human_readable(kjv_puritan_new_world_exit, "The Puritan Textual Re-Indexing (KJV in the New World)").
narrative_ontology:topic_domain(kjv_puritan_new_world_exit, "political/religious").

domain_priors:requires_active_enforcement(kjv_puritan_new_world_exit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_puritan_new_world_exit, english_crown).
narrative_ontology:constraint_beneficiary(kjv_puritan_new_world_exit, colonial_puritan_communities).
narrative_ontology:constraint_victim(kjv_puritan_new_world_exit, puritan_dissenters_in_england).
narrative_ontology:constraint_victim(kjv_puritan_new_world_exit, indigenous_peoples_of_new_world).
narrative_ontology:constraint_victim(kjv_puritan_new_world_exit, radical_protestant_sects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENGLISH PURITAN DISSENTER (SNARE) — Trapped in England, the Puritan dissenter experiences the KJV mandate as pure extraction. The Crown imposes the KJV as the sole legitimate English Bible, criminalizing possession of alternative translations (Geneva Bible, Coverdale). No domestic exit available; imprisonment, fines, and exile are the suppression mechanisms. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENGLISH CROWN & ESTABLISHED CHURCH (ROPE) — For the Crown, the KJV is a coordination mechanism: unifying the realm under a single, state-controlled scriptural text. Eliminates competitive scriptural fragmentation and subordinates dissent to a standardized doctrinal frame. The Crown sees this as solving the problem of religious faction through linguistic monopoly. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.04. Net beneficiary through monopoly coordination.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: COLONIAL PURITAN COMMUNITIES (TANGLED ROPE) — Puritans who emigrate to New England experience the KJV differently. The same text becomes a coordination mechanism for their own community formation (shared scriptural authority for covenant theology and congregational governance). Yet the KJV remains a Crown artifact — its translation philosophy embeds Anglican episcopal assumptions. The community benefits from the uniform text (coordination function) while experiencing residual extraction through embedded doctrinal constraints. They have mobile options (exit to colonies) that differentiate them from trapped English dissenters. d≈0.48, f(d)≈0.58, σ=1.2 → χ≈0.36.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIGENOUS PEOPLES OF THE NEW WORLD (SNARE) — For Native Americans, the KJV becomes a weapon of cultural erasure, arriving with colonial expansion and missionary activity. The text functions as a vector for linguistic suppression (replacing indigenous languages), cultural substitution (replacing indigenous cosmologies), and territorial extraction (missionaries and settlers claim land justified by scriptural mandate). No exit option; the constraint is imposed through colonial violence. d≈0.98, f(d)≈1.45, σ=1.2 → χ≈0.79.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: COLONIAL MERCHANTS & CROWN-LICENSED TRADERS (ROPE) — For mercantile actors operating in the colonies, the KJV serves as a coordination mechanism for colonial governance and legitimacy. A shared Bible enables shared moral framework for commerce, reduces disputes over scriptural interpretation (which might disrupt trade networks), and provides Crown-backed authority for property claims and contract enforcement. d≈0.15, f(d)≈0.05, σ=1.2 → χ≈0.06.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ESTABLISHED CHURCH RITUAL APPARATUS (PITON) — By the late 17th century, the KJV's original suppression function atrophies as Puritan dissenters successfully emigrate to New England and the Separatist schism becomes geographically decoupled from the Church of England. The KJV persists in English worship through institutional inertia — the text is now maintained not because it suppresses dissenters (its original function) but because it is the establishment artifact. Theater ratio rises as the liturgical use of the KJV becomes ceremonial rather than functionally coercive. theater_ratio≥0.70 satisfies piton gate. d≈0.12, f(d)≈-0.02, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LINGUISTIC FIXATION VIEW (MOUNTAIN) — From a civilizational perspective, the KJV's dominance reflects a natural law of textual stability: large populations cannot maintain coherence with fragmentary or variable scriptural texts. The translation becomes 'fixed' by size and institutional backing, making its existence appear inevitable rather than contingent. The analytical observer risks naturalizing what is actually a political and military arrangement (Crown power, colonial expansion, exile as an enforcement mechanism). However, the structural data (ε=0.52, suppression=0.68, theater=0.58) contradicts the mountain classification. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_puritan_new_world_exit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kjv_puritan_new_world_exit, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_puritan_new_world_exit, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kjv_puritan_new_world_exit, TR),
    TR >= 0.70.

:- end_tests(kjv_puritan_new_world_exit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The KJV begins as a highly coercive constraint (ε ≈ 0.68 in 1611 England) but its extraction function declines as geographic diversification allows exit (emigration to colonies) and as dissenter movements succeed in establishing alternatives. By the mid-18th century, ε has declined to 0.52 as the constraint's primary mechanism shifts from suppression to cultural dominance. The residual extraction reflects: (1) for English dissenters who cannot emigrate, the ongoing book-burning and prosecution regime; (2) for colonial Puritans, the embedded doctrinal assumptions that constrain theological innovation; (3) for indigenous peoples, the linguistic suppression and cultural erasure weaponized through missionary activity. Suppression (0.68): High. The Crown's enforcement of the KJV includes prohibition of rival translations, book confiscation, imprisonment of dissenting clergy and printers, fines, and threat of execution. Suppression is enforced through state apparatus (courts, bishops, Star Chamber) and private violence (mob attacks on dissenters). For indigenous peoples, suppression operates through colonial violence, forced conversion, and linguistic displacement. The suppression mechanism is structural and legally codified. Theater ratio (0.58): Moderate. The KJV's original function is genuinely coercive and extractive — the text is not maintained through performance but through active state enforcement. However, as time passes and the constraint becomes routine, the theater increases: by the late 17th and 18th centuries, the KJV's presence in English churches becomes ceremonial (liturgical reading, inherited authority) rather than constantly re-enforced through persecution. The increasing theater reflects the piton degradation of the constraint in England while suppression remains acute in colonial territories.
 *
 * PERSPECTIVAL GAP:
 *   The KJV is experienced as a snare by English dissenters (trapped, no exit) and indigenous peoples (trapped, total suppression). It is experienced as a rope by the English Crown and established church (coordination mechanism for realm unity). It is experienced as a tangled rope by colonial Puritans (coordination benefit for community, but residual doctrinal extraction from embedded Anglicanism, yet mobile enough to create relative autonomy). It degrades into a piton by the 18th century in England (maintained through institutional inertia rather than active suppression). The analytical observer risks seeing it as a mountain (natural law of textual stability). The perspectival gap reveals how geographic mobility and institutional positioning fundamentally reshape the classification: the same text is simultaneously a tool of oppression, a coordination mechanism, and an institutional artifact, depending on where the observer stands relative to exit options and power structures.
 *
 * DIRECTIONALITY LOGIC:
 *   English Crown & Established Church: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Clear net beneficiary. English Puritan Dissenters: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction — no domestic exit, state-imposed penalty. Colonial Puritan Communities: Beneficiary (coordination) + Victim (doctrinal constraint) + mobile → d≈0.48, f(d)≈0.58. Mixed directionality reflects genuine coordination benefit (shared scripture for congregational governance) balanced against residual extraction (doctrinal assumptions embedded in the translation). Their mobility (emigration option) prevents full trap classification. Indigenous Peoples: Victim (total suppression) + trapped → d≈0.98, f(d)≈1.45. Near-maximum extraction — forced conversion, linguistic erasure, territorial displacement, no exit option. Colonial Merchants: Beneficiary (coordination for commerce) + arbitrage → d≈0.15, f(d)≈0.05. Net beneficiary; KJV enables legitimate property claims. Established Church apparatus: Institutional beneficiary + arbitrage → d≈0.12, f(d)≈-0.02. Piton classification derived from theater_ratio≥0.70, not from high chi; the net position is weakly beneficial but increasingly performative.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that geographic mobility and institutional positioning are orthogonal to nominal power levels. The English Crown is institutional/arbitrage (beneficiary, mobile options). Colonial Puritans are organized/mobile (mixed agent, some benefits and costs). English dissenters are powerless/trapped (victims, no exit). Indigenous peoples are powerless/trapped (victims, no exit despite having a different structural relationship to the text). The crucial distinction is that colonial Puritans' mobile options (exit to New England) allow them to reframe the same constraint as a coordination mechanism, while English dissenters' trapped status forces a snare classification. This is NOT a disagreement about the underlying constraint — it is a structural fact that mobility reshapes the constraint's function. The tangled rope classification for colonial communities is the mandatrophy resolution: the constraint is genuinely hybrid (coordination + extraction) rather than pure snare, because the coordination function is real for community building while the extraction function persists in doctrinal constraints. The alternative would be to misclassify colonists as snare (flattening the distinction between active suppression in England and residual extraction in colonies), which would erase the genuine gain from geographic exit. The piton degradation in England by the 18th century reflects that suppression's attenuation as the Puritan schism becomes geographically successful — the constraint persists through institutional inertia after its primary function (suppression of English dissenters) has largely been achieved through forced emigration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_adoption_vs_coercion,
    'Was colonial Puritan adoption of the KJV a genuine choice (coordination benefit) or a continuation of Crown suppression under the guise of exile autonomy?',
    'Historical analysis of Puritan theological critiques of the KJV; examination of alternative Bible versions used in New England congregations; records of disputes over translation choices in colonial churches',
    'If voluntary: tangled_rope classification is correct — colonial communities genuinely benefit from coordination. If coerced: classification upgrades to snare even in colonies — the exit to America is merely displacement of extraction, not escape from it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_adoption_vs_coercion, empirical, 'Whether colonial Puritan adoption of KJV was genuine or coerced').

omega_variable(
    indigenous_textual_agency,
    'Did indigenous peoples have any capacity to reinterpret, resist, or locally control the KJV''s meaning once it arrived, or was suppression total?',
    'Historical records of indigenous responses to KJV missionary activity; analysis of syncretistic practices; documentation of resistance movements; examination of indigenous-language biblical translations and their departures from KJV theology',
    'If indigenous reinterpretation occurred: classification downgrades from pure snare to tangled_rope (some local coordination function). If suppression was total: snare classification confirmed at maximum extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_textual_agency, empirical, 'Whether indigenous peoples had any textual agency or reinterpretation capacity').

omega_variable(
    doctrinal_embedding_persistence,
    'How much of the KJV''s doctrinal content (episcopal, Calvinist, English-cultural assumptions) persisted as extraction in New England Puritan theology, versus how much was genuinely recontextualized into covenant theology and congregational polity?',
    'Comparative theology: analysis of New England Puritan divines'' hermeneutical choices; examination of which KJV passages were emphasized vs. reinterpreted; study of doctrinal departures from English Anglicanism in colonial congregations',
    'If doctrinal embedding persisted: tangled_rope classification remains robust. If thoroughly recontextualized: classification upgrades toward rope (more pure coordination, less residual extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_embedding_persistence, conceptual, 'How much KJV doctrinal content persisted versus was recontextualized').

omega_variable(
    piton_degradation_timeline,
    'At what point did the KJV''s suppression function atrophy and the piton mechanism (theatrical inertia) become primary in English religious life?',
    'Historical timeline of dissenter emigration, separatist schism success, and rise of Methodism/evangelical movements; analysis of shifts in church discipline and dissent prosecution rates; examination of when the KJV transitioned from enforcement tool to heritage artifact',
    'If transition occurred by 1700: piton classification is correct. If suppression remained active into the 18th century: snare persists longer than the story assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_degradation_timeline, empirical, 'Timeline of KJV suppression function atrophy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_puritan_new_world_exit, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv_tr_t0, kjv_puritan_new_world_exit, theater_ratio, 0, 0.32).
narrative_ontology:measurement(kjv_tr_t25, kjv_puritan_new_world_exit, theater_ratio, 25, 0.45).
narrative_ontology:measurement(kjv_tr_t50, kjv_puritan_new_world_exit, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(kjv_be_t0, kjv_puritan_new_world_exit, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(kjv_be_t25, kjv_puritan_new_world_exit, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(kjv_be_t50, kjv_puritan_new_world_exit, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_puritan_new_world_exit, information_standard).
narrative_ontology:affects_constraint(kjv_puritan_new_world_exit, calvinist_predestination_uniformity).
narrative_ontology:affects_constraint(kjv_puritan_new_world_exit, colonial_linguistic_substitution).

% DUAL FORMULATION NOTE:
% The KJV as a textual constraint decomposes into three structurally distinct stories: (1) The KJV as suppression mechanism in England (snare, ε≈0.68); (2) The KJV as coordination mechanism in New England colonies (tangled_rope, ε≈0.52); (3) The KJV as cultural erasure vector for indigenous peoples (snare, ε≈0.79). This story integrates all three perspectives but focuses on the constraint as experienced across the geographic/institutional gradient. Upstream constraints (calvinist doctrinal disputes, Geneva Bible translation) drive the KJV's creation; downstream constraints (Methodist challenge to KJV monopoly, indigenous language preservation movements) challenge its dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_puritan_new_world_exit, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
