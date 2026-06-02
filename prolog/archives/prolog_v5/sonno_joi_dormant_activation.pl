% ============================================================================
% CONSTRAINT STORY: sonno_joi_dormant_activation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sonno_joi_dormant_activation, []).

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
 *   constraint_id: sonno_joi_dormant_activation
 *   human_readable: Sonnō Jōi Dormant-Container Activation (Meiji Restoration Legitimacy Frame)
 *   domain: japanese_history/restoration_movement/institutional_legitimacy
 *
 * SUMMARY:
 *   The sonnō jōi (revere the emperor, expel the barbarians) movement of the
 *   1860s employed a structural mechanism distinctive in institutional
 *   transformation: activation of a formally-preserved but
 *   operationally-dormant legitimacy container. Throughout the Tokugawa
 *   period (1603-1868), the Imperial Court retained formal authority on paper
 *   — the bakufu's rule was technically a delegation from the Emperor — but
 *   this authority was operationally suspended. The sonnō jōi movement
 *   inverted this relationship: as Western military pressure exposed the
 *   bakufu's incapacity to defend Japan, restoration factions (primarily
 *   Satsuma and Chōshū domains) activated the dormant imperial legitimacy to
 *   displace the operationally-active but now-delegitimized bakufu regime.
 *   The constraint demonstrates how dormancy can be weaponized: by invoking
 *   preserved-but-suspended authority, the restoration faction achieved
 *   regime transition without requiring new legitimacy construction, and
 *   without full military conquest (though military capacity was necessary to
 *   make the activation stick). The Meiji Restoration (1868) operationalized
 *   this activation, converting the dormant Emperor from a formal symbol into
 *   an operational locus of state authority. The constraint exhibits Tangled
 *   Rope characteristics: it coordinates the regime transition through shared
 *   legitimacy referent (the Emperor) while simultaneously enabling
 *   extraction of administrative positions and samurai repositioning by the
 *   restoration faction and complicit elites. The theater ratio increases
 *   over the interval (0.35 to 0.62 to 0.55) as the sonnō jōi framing shifts
 *   from descriptive (an actual expulsion of Western threats was initially
 *   the stated objective) to performative (the Meiji state became the primary
 *   agent of Westernization, and the sonnō jōi rhetoric was maintained as
 *   legitimacy fiction rather than governing objective).
 *
 * KEY AGENTS:
 *   - Restoration Faction (Satsuma, Chōshū, Tosa domains, allied samurai): Primary beneficiary (organized/arbitrage) — activates dormant container and captures regime-level restructuring and administrative positions
 *   - Bakufu Administrative Structure: Primary victim (institutional/trapped) — legitimacy retroactively invalidated through dormant-container activation; no exit from operationally-active but now-delegitimized regime
 *   - Samurai Elite (class-level): Secondary victim and partial beneficiary (powerful/mobile) — face displacement from bakufu positions but repositioning opportunities in new regime; extraction distributed asymmetrically by domain
 *   - Imperial Court: Dormant container (institutional/constrained) — authority mobilized from formal-but-dormant to operational; constrained by restoration faction's military capacity and strategic control
 *   - Tokugawa Elite (daimyo, bakufu counselors): Institutional victim (powerful/constrained) — face displacement; some negotiate transition terms but structural loss of authority
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies dormant-container activation as distinctive resolution pattern enabling regime transition through legitimacy preservation rather than conquest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sonno_joi_dormant_activation, 0.38).
domain_priors:suppression_score(sonno_joi_dormant_activation, 0.48).
domain_priors:theater_ratio(sonno_joi_dormant_activation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sonno_joi_dormant_activation, extractiveness, 0.38).
narrative_ontology:constraint_metric(sonno_joi_dormant_activation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sonno_joi_dormant_activation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sonno_joi_dormant_activation, tangled_rope).
narrative_ontology:human_readable(sonno_joi_dormant_activation, "Sonnō Jōi Dormant-Container Activation (Meiji Restoration Legitimacy Frame)").
narrative_ontology:topic_domain(sonno_joi_dormant_activation, "japanese_history/restoration_movement/institutional_legitimacy").

domain_priors:requires_active_enforcement(sonno_joi_dormant_activation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sonno_joi_dormant_activation, restoration_faction).
narrative_ontology:constraint_beneficiary(sonno_joi_dormant_activation, samurai_elite_repositioning).
narrative_ontology:constraint_victim(sonno_joi_dormant_activation, bakufu_administrative_structure).
narrative_ontology:constraint_victim(sonno_joi_dormant_activation, tokugawa_consolidation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BAKUFU FUNCTIONARIES (SNARE) — Once the dormant Imperial legitimacy was activated, bakufu officials faced structural collapse with no exit. The constraint emerged as pure extraction from their position: they were bound to an administrative structure whose legitimacy had been retroactively invalidated. The operationally-suspended authority of the Emperor was mobilized against their active but now-delegitimized regime. Suppression was severe — alternative administrative pathways were foreclosed, and resistance was framed as disloyalty to an authority structure (the Emperor) that had never formally revoked bakufu authority but was now operationally activated to do so.
constraint_indexing:constraint_classification(sonno_joi_dormant_activation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAMURAI ELITE (TANGLED ROPE) — The warrior class experienced the constraint as both coordination and extraction. The sonnō jōi framing coordinated samurai across domains through shared legitimacy referent (the Emperor). But the activation also enabled repositioning: samurai who switched to the restoration faction gained access to new administrative roles, while those who remained bakufu loyalists faced structural displacement. The constraint's enforcement was active — domains that supported restoration gained institutional resources; bakufu-loyal samurai lost position. Mobile exit options (some could switch allegiance, some could retreat to rural estates, some could pursue commerce) meant that the elite experienced extraction as significant but not total.
constraint_indexing:constraint_classification(sonno_joi_dormant_activation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: RESTORATION FACTION (ROPE) — From the restoration coalition's perspective (Satsuma, Chōshū, Tosa domains and their samurai networks), the constraint was pure coordination without extraction. They activated a formally-available but operationally-suspended authority structure (the Emperor's legitimacy, preserved throughout Tokugawa) to displace an operationally-active but now-delegitimized regime (the bakufu). The sonnō jōi framing solved a pure coordination problem: how to displace the bakufu without constructing new legitimacy from scratch. The restoration faction captured no extraction rent from this coordination — they captured the entire regime-level restructuring. Arbitrage exit options (they could switch allegiance, join bakufu, or negotiate terms) were available but irrelevant because the coordination was inherently aligned with their interest in administrative access.
constraint_indexing:constraint_classification(sonno_joi_dormant_activation, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IMPERIAL COURT (TANGLED ROPE) — The Emperor's legitimacy was formally preserved but operationally dormant throughout Tokugawa — the court had authority on the books but no administrative function. The sonnō jōi movement activated this dormancy without requiring the court to construct new institutions. The court benefited from restoration (renewed administrative relevance, resource flows from the new Meiji regime), but the coordination required the court to accept subordination to the restoration faction's strategic objectives. The court's exit options were constrained — it could not return to full Tokugawa dormancy after activation, and it could not reject the restoration coalition without losing the legitimacy benefit. Activation was enforced by the restoration faction's military capacity.
constraint_indexing:constraint_classification(sonno_joi_dormant_activation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TOKUGAWA ADMINISTRATIVE STRUCTURE (SCAFFOLD) — From the perspective of the operational bureaucracy (not the daimyo elite, but the functional administrators maintaining Tokugawa institutions), the constraint was a temporary coordination failure with a sunset: the bakufu system was being systematically dismantled and replaced with Meiji institutions. Some administrators transitioned to the new regime; some were displaced; some took roles in prefectures. The theater ratio was moderate (0.55) because the transition involved both genuine institutional reconstruction and performative legitimacy ritual — the sonnō jōi framing was partly substantive coordination and partly ceremonial restoration of the Emperor's authority that had been symbolic rather than operational.
constraint_indexing:constraint_classification(sonno_joi_dormant_activation, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SONNŌ JŌI RITUAL FRAME (PITON) — From a civilizational perspective, the sonnō jōi movement is now seen as partially performative: the Meiji state used the Emperor's restored authority to legitimize centralization that was fundamentally about modernization, military capacity, and geopolitical competition with Western powers. The sonnō jōi rhetoric invoked preservation of Japanese tradition and imperial authority, but the actual function was enabling regime restructuring and displacement of the samurai class. The constraint persists through institutional inertia — the fiction that the Meiji Restoration was a restoration rather than a revolution has become part of the legitimacy narrative of the Japanese state itself. Theater ratio reflects this degradation: the Emperor's role as source of sovereign legitimacy is maintained, but the actual coordinate of power has shifted continuously through the Meiji, Taishō, and Shōwa periods.
constraint_indexing:constraint_classification(sonno_joi_dormant_activation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the analytical perspective, the sonnō jōi constraint demonstrates a structural resolution pattern: dormant-container activation. The constraint coordinates regime transition (genuine coordination function) while enabling elite repositioning and structural extraction (asymmetric distribution of new administrative positions). The pattern is generalizable: when an active outer container faces legitimacy crisis, a formally-preserved but operationally-dormant inner container can be activated if inner actors have sufficient independent operational capacity. This is a hybrid mechanism — coordination of the regime transition itself, extraction of position in the new regime by actors who executed the activation. The analytical observer sees this as neither pure coordination nor pure extraction, but as a distinctive institutional pattern where dormancy is weaponized as legitimacy preservation.
constraint_indexing:constraint_classification(sonno_joi_dormant_activation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sonno_joi_dormant_activation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sonno_joi_dormant_activation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sonno_joi_dormant_activation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sonno_joi_dormant_activation, TR),
    TR >= 0.70.

:- end_tests(sonno_joi_dormant_activation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The sonnō jōi constraint exhibits substantial extraction from the bakufu elite and samurai class, but the extraction is not maximal because the activation also coordinates a genuine collective problem (Japan's exposure to Western military pressure required regime restructuring, and the restoration faction delivered a functional solution). The extractiveness increases over the interval (0.22 in 1840s, 0.42 by 1868) as the coordination function diminishes and the extraction function dominates — once the bakufu is eliminated, the extraction becomes repositioning of elites and concentration of power rather than collective defense. The post-restoration drop (0.38) reflects stabilization of the new regime. Suppression (0.48): Moderate-high. Bakufu resistance to dormant-container activation was suppressed through military capacity (the Boshin War, 1868-1869), but suppression was not total — bakufu officials were not systematically eliminated, and some achieved positions in the new regime. Samurai suppression of alternative pathways (remaining bakufu-loyal, attempting to preserve samurai privilege) was severe, enforced through military defeat and administrative exclusion. Theater ratio (0.55): Moderate-high. The sonnō jōi framing was initially functionally descriptive (expulsion of Western threats was a stated objective) but became increasingly performative as the Meiji state became the primary agent of Westernization while maintaining sonnō jōi rhetoric as state legitimacy fiction. Theater increases sharply (0.35 to 0.62) during the critical period (1850-1868) when the framing was doing maximal legitimacy work, then moderates slightly (0.55) post-restoration as the fiction stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The sonnō jōi constraint produces a distinctive perspectival structure where the same dormant-container activation mechanism is experienced as coordination (from restoration faction perspective), extraction (from bakufu perspective), mixed coordination-extraction (from samurai elite perspective), and mobilization of dormancy (from imperial court perspective). The gap reveals that dormant-container activation is a specific coordination mechanism with built-in asymmetry: the actors who activate the dormant container capture the regime-level restructuring benefits. This is not pure extraction (genuine coordination of Japan's military vulnerability response) but not pure coordination (extraction of position from those displaced). The analytical observer's classification (Tangled Rope) integrates all perspectives: the constraint coordinates regime transition while enabling extraction of new administrative structure. The Piton perspective (examining the sonnō jōi ritual itself in civilizational perspective) reveals that the mechanism's legitimacy power has persisted through institutional inertia — the Emperor's authority restored in 1868 became the actual locus of state sovereignty in Japanese legal and constitutional tradition, so the activation was not performative. However, the specific sonnō jōi objective (expulsion of barbarians) was not achieved — instead Westernization was embraced — so the framing became theatrical while the authority was operationalized.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for each perspective is determined by the agent's structural relationship to the dormant-container activation. The restoration faction (organized/arbitrage) activates the dormant container and has exit options (they could negotiate with bakufu, remain as regional daimyo, or switch allegiance) but chooses activation because it captures regime-level benefit — low or negative d, experiencing the constraint as coordination rather than extraction. The bakufu (institutional/trapped) is the target of activation with no exit once the dormant container is operationally mobilized — high d, experiencing maximum extraction. The samurai elite (powerful/mobile) face mixed extraction (displacement from bakufu positions) and repositioning (access to new administrative roles) — moderate d, experiencing Tangled Rope with asymmetric distribution of new positions. The imperial court (institutional/constrained) is mobilized but remains operationally constrained by the restoration faction's military capacity — moderate-high d, experiencing Tangled Rope because coordination is genuine but constrained by dependence on restoration faction. The bakufu bureaucracy (moderate/constrained) experiences temporary displacement with some transition to new regime — moderate-high d, experiencing Scaffold because the transition is genuine but the trajectory is sunset (administrative continuity in new regime).
 *
 * MANDATROPHY ANALYSIS:
 *   The sonnō jōi constraint resolves the mandatrophy by demonstrating that dormant-container activation is a legitimate coordination mechanism with built-in extraction asymmetry. The mandatrophy resolution is: 'How can regime transition be coordination-and-extraction simultaneously?' Answer: Dormant-container activation coordinates the regime transition (solves the collective problem of Japan's military vulnerability and bakufu incapacity) while extracting position by operationalizing the coordination mechanism. The actors who activate the dormant container gain access to the restructured regime's administrative positions. This is neither pure coordination (Rope) nor pure extraction (Snare), but a hybrid where coordination is the mechanism and extraction is the asymmetric distribution of restructuring benefits. The pattern generalizes: dormant-container activation enables regime transition through legitimacy preservation (using formally-available but operationally-suspended authority to displace operationally-active but delegitimized regimes). The pattern requires two conditions: (1) a formally-preserved dormant authority structure (the Emperor's unrevoked but suspended authority in Tokugawa), and (2) inner actors with sufficient independent operational capacity to execute the activation (Satsuma-Chōshū military and administrative capacity). Under these conditions, the dormant-container mechanism produces Tangled Rope rather than military conquest (Snare) or simple coordination (Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bakufu_legitimacy_retroactive_invalidation,
    'Was the bakufu''s legitimacy invalidated by Western pressure exposure (empirical loss of regime capacity to protect Japan) or by deliberate sonnō jōi framing that retroactively reinterpreted bakufu authority as illegitimate delegation from the Emperor?',
    'Historical analysis of contemporaneous documents: Did bakufu legitimacy crisis precede or follow restoration faction''s sonnō jōi narrative activation? Compare bakufu self-justification 1850-1860 with restoration faction''s explicit articulation of dormant-container logic.',
    'If empirical loss of capacity: dormant-container activation is epiphenomenal to material power shift. If deliberate reframing: dormant-container activation is the primary mechanism enabling regime displacement without military conquest (coordination through legitimacy rather than force). Distinction affects whether pattern generalizes to other contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bakufu_legitimacy_retroactive_invalidation, empirical, 'Whether bakufu legitimacy crisis was material or discursive').

omega_variable(
    dormant_container_preservation_mechanism,
    'How did the Imperial Court''s authority remain formally legitimate throughout Tokugawa despite 260 years of operational dormancy? What institutional mechanism sustained the fiction that bakufu authority was delegated from the Emperor rather than replacing imperial authority?',
    'Analysis of Tokugawa court protocols, daimyo oath structures, legal codices (buke shohatto). Did bakufu courts explicitly cite imperial authority, or did imperial authority persist as background assumption? Comparison with historical containers that lost formal legitimacy (e.g., Yuan Dynasty under Ming restoration).',
    'If bakufu explicitly cited imperial delegation: dormancy is a thin institutional fiction, fragile. If imperial authority was background assumption: dormancy is structurally robust. Affects predictability of pattern''s applicability to other regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormant_container_preservation_mechanism, empirical, 'Institutional mechanism preserving dormant legitimacy through Tokugawa period').

omega_variable(
    restoration_faction_independent_capacity_threshold,
    'What minimum level of independent operational capacity (military, administrative, resource control) did restoration domains require to activate the dormant Imperial container? Would sonnō jōi framing alone have succeeded without Satsuma-Chōshū military capacity?',
    'Counterfactual analysis: comparison with earlier sonnō jōi movements (1840s, early 1850s) that lacked military capacity and failed. Modeling of bakufu response if restoration factions had invoked Emperor without military backup.',
    'If capacity threshold is high: pattern requires material power base. If framing alone suffices: pattern enables displacement through legitimacy alone. Affects generalizability to contexts with weaker would-be challengers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_faction_independent_capacity_threshold, conceptual, 'Minimum capacity threshold for dormant-container activation').

omega_variable(
    samurai_elite_extraction_distribution,
    'Was the extraction experienced by samurai elite (those displaced from bakufu) shared equitably among restoration faction, or did leadership concentrate new administrative positions in Satsuma and Chōshū domains, excluding other samurai?',
    'Quantitative analysis of Meiji administrative positions: proportion filled by ex-bakufu samurai vs. restoration faction samurai by domain origin. Analysis of samurai stipend commutation (chitsurokusha) distribution and access to new commercial opportunities.',
    'If distributed: Tangled Rope extraction is moderate and broad. If concentrated: extraction is severe and directed toward specific domains — Snare for excluded samurai, even more favorable Rope for concentrated beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(samurai_elite_extraction_distribution, empirical, 'Distribution of extraction from samurai repositioning among restoration factions').

omega_variable(
    modernization_misdirection_via_restoration_frame,
    'To what extent was the sonnō jōi framing a deliberate misdirection: the Meiji state used restoration rhetoric to obscure that it was actually centralizing power, eliminating samurai privilege, and implementing wholesale Westernization?',
    'Comparison of modernization rate and content under Meiji with stated sonnō jōi objectives. Analysis of Meiji leaders'' private correspondence (e.g., Ōkubo, Iwakura) vs. public sonnō jōi rhetoric. Evaluation of whether modernization occurred despite or because of restoration framing.',
    'If deliberate misdirection: sonnō jōi is pure theater (Piton classification confirmed at Meiji state level). If genuinely believed framework: restoration was earnest attempt to modernize while preserving tradition — Rope classification from leadership perspective. Affects whether Piton perspective is structural or interpretive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_misdirection_via_restoration_frame, conceptual, 'Whether sonnō jōi framing was deliberate misdirection or genuine framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sonno_joi_dormant_activation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sonno_joi_theater_1840s, sonno_joi_dormant_activation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sonno_joi_theater_1855_1860, sonno_joi_dormant_activation, theater_ratio, 10, 0.48).
narrative_ontology:measurement(sonno_joi_theater_1865_1868, sonno_joi_dormant_activation, theater_ratio, 20, 0.62).
narrative_ontology:measurement(sonno_joi_theater_post_restoration, sonno_joi_dormant_activation, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(sonno_joi_extractiveness_1840s, sonno_joi_dormant_activation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sonno_joi_extractiveness_1855_1860, sonno_joi_dormant_activation, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(sonno_joi_extractiveness_1865_1868, sonno_joi_dormant_activation, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(sonno_joi_extractiveness_post_restoration, sonno_joi_dormant_activation, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sonno_joi_dormant_activation, identity_coordination).
narrative_ontology:affects_constraint(sonno_joi_dormant_activation, bakufu_legitimacy_suspension).
narrative_ontology:affects_constraint(sonno_joi_dormant_activation, samurai_class_repositioning).
narrative_ontology:affects_constraint(sonno_joi_dormant_activation, meiji_modernization_framing).

% DUAL FORMULATION NOTE:
% The sonnō jōi constraint decomposes into three structurally distinct stories: (1) bakufu_legitimacy_suspension (ε=0.15, Mountain — the formal preservation of imperial authority throughout Tokugawa, a natural outcome of delegation theory), (2) samurai_class_repositioning (ε=0.52, Snare — the structured extraction of samurai privilege through military defeat and administrative displacement), and (3) meiji_modernization_framing (ε=0.48, Piton — the theatrical maintenance of restoration ideology as the Meiji state pursued Westernization). The sonnō_joi_dormant_activation constraint (ε=0.38, Tangled Rope) is the coordinating mechanism linking all three: dormant-container activation coordinates the regime transition (solving bakufu legitimacy crisis) while enabling samurai repositioning extraction (asymmetrically distributed by domain) and justifying modernization through restoration rhetoric.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sonno_joi_dormant_activation, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
