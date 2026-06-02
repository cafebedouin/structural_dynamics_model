% ============================================================================
% CONSTRAINT STORY: foreign_pressure_catalyst
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_foreign_pressure_catalyst, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: foreign_pressure_catalyst
 *   human_readable: Foreign Pressure as Constitutional Crisis Catalyst in Japan, 1850-1868
 *   domain: political_philosophy/constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   The arrival of Western military pressure (Perry's expedition 1853)
 *   coincided with and accelerated a pre-existing contest over the legitimacy
 *   basis of Tokugawa rule. The bakufu (shogunate) claimed delegated imperial
 *   authority — the emperor granted the shogun military command while
 *   remaining ceremonial sovereign. Imperial loyalists (shishi) invoked an
 *   alternative reading of the same imperial lineage: direct divine mandate
 *   requiring active imperial sovereignty, with the Tokugawa delegation as a
 *   usurpation. Foreign pressure did not create this reading contest, but it
 *   delegitimized the Tokugawa's ability to defend the realm under the
 *   delegation paradigm, providing the loyalists a structural opening to
 *   claim that restoration of direct imperial rule was necessary for national
 *   survival. This constraint exhibits the structure of a tangled rope:
 *   foreign pressure created a genuine coordination problem (how to respond
 *   to Western military technology) that required centralized action, while
 *   simultaneously enabling extraction by the restoration faction through
 *   weaponization of foreign threat narrative to delegitimize the bakufu and
 *   justify consolidation of authority under imperial symbolism. The theater
 *   ratio increased from 0.42 to 0.64 as the crisis deepened — performative
 *   compliance with isolation policy (sakoku) persisted even as it became
 *   functionally irrelevant, and both bakufu and restoration factions
 *   increasingly deployed foreign threat rhetoric to justify internal power
 *   consolidation.
 *
 * KEY AGENTS:
 *   - Tokugawa Shogunate: Institutional beneficiary/victim (institutional/constrained) — initially benefits from security function, but trapped by inability to respond to foreign technology without delegitimizing their own authority structure
 *   - Imperial Loyalist Faction (shishi): Secondary beneficiary (moderate-to-organized/constrained) — benefits from foreign pressure narrative as delegitimizing bakufu, gain through restoration of direct imperial authority
 *   - Samurai Elite (rōnin, lower-ranking samurai): Mixed victim and beneficiary (moderate/constrained) — bear costs of foreign threat and bakufu repression, gain through restoration promises of status recovery
 *   - Satsuma and Choshu Domains: Organized beneficiaries (organized/mobile) — mobilize restoration coalition, mobilize around anti-bakufu sentiment, position themselves as alternatives to central Tokugawa authority
 *   - Imperial Court: Institutional beneficiary (institutional/arbitrage) — passive under Tokugawa, foreign pressure enables shift to active sovereignty; can arbitage between factions
 *   - Western Powers: External pressure source (institutional/arbitrage) — not parties to the domestic legitimacy contest but their military presence functions as delegitimization force for Tokugawa delegation paradigm
 *   - Japanese Population/Common Folk: Victim (powerless/trapped) — bear costs of foreign threat and internal instability without capacity to shape outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(foreign_pressure_catalyst, 0.52).
domain_priors:suppression_score(foreign_pressure_catalyst, 0.58).
domain_priors:theater_ratio(foreign_pressure_catalyst, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(foreign_pressure_catalyst, extractiveness, 0.52).
narrative_ontology:constraint_metric(foreign_pressure_catalyst, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(foreign_pressure_catalyst, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(foreign_pressure_catalyst, tangled_rope).
narrative_ontology:human_readable(foreign_pressure_catalyst, "Foreign Pressure as Constitutional Crisis Catalyst in Japan, 1850-1868").
narrative_ontology:topic_domain(foreign_pressure_catalyst, "political_philosophy/constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(foreign_pressure_catalyst).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(foreign_pressure_catalyst, 'fpc-20260226-001').
narrative_ontology:cs_kernel_codification('fpc-20260226-001', fixed_text).
narrative_ontology:cs_authority_grounding('fpc-20260226-001', lineage).
narrative_ontology:cs_interpretation_layer_present('fpc-20260226-001').
narrative_ontology:cs_reading_relation('fpc-20260226-001', tokugawa_delegation_reading, forecloses).
narrative_ontology:cs_reading_relation('fpc-20260226-001', bakufu_legitimacy_erasure, influences).
narrative_ontology:cs_axiom('fpc-20260226-001', foundational, emperor_holds_direct_sovereignty).
narrative_ontology:cs_axiom_status(emperor_holds_direct_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('fpc-20260226-001', emperor_holds_direct_sovereignty, theological).
narrative_ontology:cs_axiom('fpc-20260226-001', foundational, delegation_is_conditional_not_permanent).
narrative_ontology:cs_axiom_status(delegation_is_conditional_not_permanent, holdable).
narrative_ontology:cs_axiom_grounding('fpc-20260226-001', delegation_is_conditional_not_permanent, deontological).
narrative_ontology:cs_axiom('fpc-20260226-001', secondary, foreign_threat_justifies_sovereignty_restoration).
narrative_ontology:cs_axiom_status(foreign_threat_justifies_sovereignty_restoration, holdable).
narrative_ontology:cs_axiom_grounding('fpc-20260226-001', foreign_threat_justifies_sovereignty_restoration, instrumental).
narrative_ontology:cs_reference_frame('fpc-20260226-001', imperial_direct_mandate_framework).
narrative_ontology:cs_drift_state('fpc-20260226-001', meiji_restoration_1868, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fpc-20260226-001', '2026-02-26T00:00:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(foreign_pressure_catalyst, imperial_loyalists).
narrative_ontology:constraint_beneficiary(foreign_pressure_catalyst, samurai_elite).
narrative_ontology:constraint_victim(foreign_pressure_catalyst, tokugawa_legitimacy).
narrative_ontology:constraint_victim(foreign_pressure_catalyst, institutional_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TOKUGAWA INSTITUTIONAL LEGITIMACY (SNARE) — The bakufu system cannot exit the foreign pressure crisis without dissolving. Trapped by the structural fact that Western military technology invalidates the shogunate's security function. The entire edifice of delegated imperial authority collapses once external threats expose the institutional deficit. Maximum extraction — the system bears all costs of foreign pressure without the capacity to respond.
constraint_indexing:constraint_classification(foreign_pressure_catalyst, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BAKUFU BUREAUCRACY (TANGLED ROPE) — Constrained by institutional path-dependency and career investment, but also benefits from foreign pressure as justification for technical modernization and consolidation of power. The crisis provides cover for centralizing measures. High extraction with genuine coordination function — foreign threats require coordinated defense infrastructure.
constraint_indexing:constraint_classification(foreign_pressure_catalyst, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IMPERIAL LOYALIST FACTION (ROPE) — Benefits from foreign pressure as delegitimizing the bakufu delegation paradigm. Foreign threat narrative supports restoration of direct imperial sovereignty. Experiences the constraint as pure coordination: foreign pressure solves their problem (creating opening for restoration) without requiring coercion. Arbitrage exit — can shift allegiance between bakufu and emperor based on which offers better institutional positioning.
constraint_indexing:constraint_classification(foreign_pressure_catalyst, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMAIN REFORMERS (SCAFFOLD) — Organized groups within individual han seeking to modernize military and administrative capacity. Foreign pressure provides temporary justification for reforms (sunset: once Western threat is managed or domains consolidate under central authority, ad-hoc reform coalitions lose coherence). The reformers experience extraction through the theater of foreign danger (must justify every modernization effort as response to threat) but also mobilization (coordinated activity becomes possible under crisis cover).
constraint_indexing:constraint_classification(foreign_pressure_catalyst, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SAKOKU CLOSURE FRAMEWORK (PITON) — The Tokugawa foreign policy (sakoku — closed country) persists through institutional inertia despite its function being destroyed by foreign pressure. The theater of isolation (maintaining the fiction that Japan's borders are closed) becomes detached from the actual security challenge. High theater ratio: the policy is maintained performatively even as it becomes structurally irrelevant. The framework itself becomes a trap — defending sakoku orthodoxy prevents adaptive response.
constraint_indexing:constraint_classification(foreign_pressure_catalyst, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational/universal perspective, the constraint appears to be an immutable feature of historical process: states with superior military technology inevitably transform states with inferior technology through pressure. This reading naturalizes the foreign pressure dynamic as law-like, rather than as a contingent institutional arrangement that beneficiaries (imperial loyalists, samurai factions) exploit. The engine will flag this as a false summit — the 'inevitability' of restoration conceals the political contest between readings of imperial legitimacy.
constraint_indexing:constraint_classification(foreign_pressure_catalyst, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(foreign_pressure_catalyst_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(foreign_pressure_catalyst, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(foreign_pressure_catalyst, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(foreign_pressure_catalyst, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(foreign_pressure_catalyst, TR),
    TR >= 0.70.

:- end_tests(foreign_pressure_catalyst_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits genuine extraction by the restoration coalition through weaponization of foreign threat narrative — legitimacy claims deployed strategically to displace Tokugawa authority and consolidate power under imperial symbolism. However, extraction is not maximal because a real coordination problem exists (response to Western military technology) that requires centralized action, and restoration does provide institutional mechanism for addressing it. The moderate level reflects that extraction and coordination are genuinely intertwined. Suppression (0.58): Moderate-high, rising over interval. Early suppression (0.35 in 1853) reflects bakufu's established control mechanisms. Rising suppression (0.48 by 1858, 0.58 by 1868) reflects increasing repression of loyalist organizing and increasing need to enforce Tokugawa legitimacy as it erodes. The rise is driven not by increasing capacity to suppress, but by increasing difficulty of maintaining order as the bakufu's institutional basis loses credibility. Theater ratio (0.64): Moderate-high, rising from 0.42. The sakoku (closed country) framework persists as performative policy even as foreign pressure makes closure impossible. By 1868, defense of sakoku orthodoxy has become purely theatrical — the bakufu maintains the fiction of closure while negotiating with Western powers. Restoration coalition also performs foreign threat narrative (exaggerating external danger to justify internal mobilization) alongside genuine security concerns. Theater increases as both factions deploy crisis rhetoric while pursuing factional advantage.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The Tokugawa see foreign pressure as an external threat requiring coordinated response (tangled rope — genuine coordination with embedded extraction as they consolidate power). The restoration faction sees it as delegitimizing bakufu authority and creating opportunity for restoration (rope — pure coordination toward restoration goal). The bakufu bureaucracy sees it as justification for technical modernization and centralization (tangled rope — extraction disguised as necessity). The loyalist intellectuals see it as natural law determining that restoration is inevitable (mountain — false summit that naturalizes political contest). The analytical observer must avoid the false summit: foreign pressure is not an autonomous force determining institutional change, but a structural feature that different factions deploy rhetorically. The Tokugawa institutional order cannot escape the constraint (snare) because foreign pressure exposes the institutional deficit inherent in delegation — security function is the core legitimacy of the bakufu, and military inferiority invalidates that function. The sakoku policy persists as pure theater (piton) because defending isolation orthodoxy becomes detached from actual security strategy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows from beneficiary/victim declarations and exit options. The restoration faction (institutional/arbitrage) experiences low d (~0.15) — they are beneficiaries with clear exit options (shift allegiance between bakufu and emperor based on institutional advantage). The Tokugawa institutional order (institutional/constrained) experiences high d (~0.70) — nominally powerful but constrained by path-dependency and trapped by the structural fact that foreign pressure exposes their institutional deficit. They cannot exit by remaining Tokugawa — they must either modernize (preserving institutional form but losing delegation basis) or be displaced (institutional dissolution). The bakufu bureaucracy experiences moderate d (~0.55) — they benefit from crisis justification for power consolidation while bearing costs of increasing instability. The analytical observer (analytical/analytical) experiences moderate d (~0.72) under the canonical fallback, but the false summit detection identifies the mountain classification as naturalizing what is actually a political contest, suggesting observer-level identity lock (the mountain reading naturalizes the restoration as inevitable, preventing recognition of contingency and factional contestation).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE FOR CHANGE TRAP — The constraint resolves the mandatrophy by clarifying the relationship between foreign pressure (exogenous force) and institutional reading (endogenous contest). Foreign pressure itself does not determine the restoration — it enables the loyalist reading to displace the Tokugawa reading by revealing the institutional failure of delegation. The mandatrophy is the trap between (a) treating foreign pressure as an autonomous force (mountain reading) that determines restoration inevitability, versus (b) treating the restoration as purely factional contest disconnected from structural vulnerability (rope reading). The truth is tangled: foreign pressure creates genuine structural vulnerability (Tokugawa cannot defend the realm under delegation paradigm) while simultaneously enabling factional extraction (restoration coalition weaponizes threat to delegitimize bakufu). The constraint is a tangled rope precisely because genuine coordination and factional extraction are inseparable — responding to foreign pressure requires centralization of authority, which creates opening for restoration coalition to claim that centralization requires restoration of direct imperial rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_direction_ambiguity,
    'Did foreign pressure cause the Meiji Restoration, or did the restoration coalition weaponize foreign pressure rhetoric to delegitimize the Tokugawa?',
    'Counterfactual analysis: would restoration have occurred without foreign pressure? Temporal analysis of royalist organizing predating Perry''s arrival; examination of loyalist strategic claims vs. independent historical causation.',
    'If foreign pressure is exogenous and causal: constraint is snare (bakufu trapped by external force). If foreign pressure is rhetorical weaponization: constraint is tangled rope (restoration coalition extracts from foreign threat narrative). Classification depends on separating genuine external pressure from strategic deployment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_direction_ambiguity, empirical, 'Whether foreign pressure is exogenous cause or rhetorical resource for factional contest').

omega_variable(
    reading_contest_kernel_identity,
    'Is the ''imperial mandate'' a unified kernel that different parties read differently, or are the Tokugawa delegation reading and the loyalist sovereignty reading describing genuinely different institutional commitments?',
    'Textual analysis of imperial edicts and bakufu legitimation documents; examination of whether both parties claimed the same legitimacy source or invoked different kernels entirely; historical evidence of whether the contest was over interpretation or over which kernel governed.',
    'If unified kernel: constraint is CS reading contest (commitment system with multiple readings). If different kernels: constraint is institutional competition (no unified kernel to contest). Changes cs_structure characterization and omega resolution strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'Whether imperial mandate is unified contested kernel or genuinely separate legitimacy claims').

omega_variable(
    suppression_mechanism_militarization,
    'Was suppression of restoration sentiment primarily enforced through military surveillance and repression, or through ideological capture (bakufu legitimacy internalized by population)?',
    'Historical analysis of resistance patterns: were suppressed movements active/organized (suggesting external suppression) or dormant/internalized (suggesting ideological capture)? Examination of post-restoration speed of loyalty transfer — rapid transfer suggests suppression was structural not internalized.',
    'If militarization: suppression value justified at 0.58 (external barriers). If ideological: actual suppression lower, but identity_locked mechanisms active in loyalist perspective. Changes exit_options characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_militarization, empirical, 'Whether suppression was military enforcement or internalized legitimacy').

omega_variable(
    meiji_continuity_contested,
    'Did the Meiji Restoration represent genuine restoration of imperial sovereignty, or repackaging of elite power under imperial symbolism?',
    'Structural analysis of decision-making authority post-1868: did the emperor retain actual power, or did the oligarchs (Satsuma, Choshu) exercise control via imperial mandate? Examination of Taisho Democracy and Showa periods for evidence of whether imperial sovereignty was real or theatrical.',
    'If genuine restoration: foreign pressure catalyst led to legitimate constitutional realignment. If theatrical: foreign pressure was weaponized to produce illusion of restoration while maintaining elite extraction through different institutional form. Changes mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_continuity_contested, empirical, 'Whether Meiji restoration represented genuine imperial sovereignty or repackaged elite rule').

omega_variable(
    false_summit_natural_law,
    'Is the foreign pressure catalyst an immutable law of geopolitical competition (states with superior technology dominate) or a contingent institutional dynamic that beneficiaries exploit?',
    'Comparative historical analysis: did other non-Western societies undergo similar institutional transformations under similar foreign pressure? Examination of alternative pathways available to Tokugawa (technical modernization without regime change, military adaptation without restoration). Test whether foreign pressure determined outcome or merely enabled it.',
    'If law-like: mountain classification from analytical perspective is justified. If contingent: mountain is false summit, and analytical observer''s naturalization conceals political contest. Affects how engine flags restoration as inevitable vs. constructed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, empirical, 'Whether foreign pressure is geopolitical law or weaponized institutional narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(foreign_pressure_catalyst, 1853, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fpc_theater_1853, foreign_pressure_catalyst, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fpc_theater_1858, foreign_pressure_catalyst, theater_ratio, 5, 0.58).
narrative_ontology:measurement(fpc_theater_1868, foreign_pressure_catalyst, theater_ratio, 15, 0.64).

% Extraction over time
narrative_ontology:measurement(fpc_extractiveness_1853, foreign_pressure_catalyst, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fpc_extractiveness_1858, foreign_pressure_catalyst, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(fpc_extractiveness_1868, foreign_pressure_catalyst, base_extractiveness, 15, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fpc_suppression_1853, foreign_pressure_catalyst, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fpc_suppression_1858, foreign_pressure_catalyst, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(fpc_suppression_1868, foreign_pressure_catalyst, suppression_requirement, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(foreign_pressure_catalyst, enforcement_mechanism).
narrative_ontology:affects_constraint(foreign_pressure_catalyst, loyal_samurai_identity_lock).
narrative_ontology:affects_constraint(foreign_pressure_catalyst, bakufu_legitimacy_erasure).
narrative_ontology:affects_constraint(foreign_pressure_catalyst, meiji_oligarchy_restoration_theater).

% DUAL FORMULATION NOTE:
% Foreign pressure functions as structural catalyst for the imperial legitimacy reading contest. Separate stories decompose the different mechanisms: (1) foreign_pressure_catalyst (this story) — the exogenous force and its role in enabling reading displacement; (2) loyal_samurai_identity_lock — the psychological mechanisms binding samurai personnel to restoration ideology; (3) bakufu_legitimacy_erasure — the institutional dissolution dynamics as delegation paradigm loses authority; (4) meiji_oligarchy_restoration_theater — the post-restoration continuation of extraction under imperial symbolism, revealing that restoration was partly rhetorical theater. These four stories form a constraint family linked by the common kernel (imperial mandate) and the reading contest dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(foreign_pressure_catalyst, institutional, 0.7).
constraint_indexing:directionality_override(foreign_pressure_catalyst, analytical, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
