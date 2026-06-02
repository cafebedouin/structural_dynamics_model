% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Imperial Mandate Through Institutional Delegation (Bakufu Reading)
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   The bakufu delegation reading instantiates one interpretation of how
 *   imperial legitimacy operates through institutional delegation in East
 *   Asian constitutional systems. In this reading, the Emperor retains a
 *   supreme ritual-legitimacy function — the power to grant authority to
 *   govern — while the Shogun (and through him, the samurai class) exercises
 *   actual political authority. This bifurcation solves a coordination
 *   problem: legitimacy is decoupled from administrative capacity, allowing
 *   stable political authority even when regimes collapse and are replaced.
 *   The same emperor can legitimize multiple shogunal regimes across
 *   centuries, creating institutional continuity without requiring the
 *   emperor to govern. However, this reading presents a competed kernel: the
 *   loyalist restoration reading claims that unified imperial sovereignty is
 *   legitimate and delegation is a usurpation. This story instantiates only
 *   the bakufu delegation interpretation.
 *
 * KEY AGENTS:
 *   - Imperial Court: Ritual-authority holder (trapped) — holds legitimacy-granting function but suppressed from political involvement
 *   - Shogunal Administration: Administrative-authority holder (organized/constrained) — receives delegated authority from emperor; benefits from legitimacy transfer but constrained by imperial approval requirement
 *   - Samurai Governing Class: Institutional beneficiary (organized/constrained) — legitimized as governing stratum across regime changes; benefits from institutional stability
 *   - Peasantry and Commoners: Excluded orders (powerless/trapped) — experience extraction without political voice or legitimacy claim
 *   - Bakufu Institutional Structure: System-level entity (institutional/arbitrage) — coordinates authority distribution and enables regime change without legitimacy crisis
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees bifurcated sovereignty as solution to coordination problem with unavoidable extraction asymmetries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.48).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.62).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate Through Institutional Delegation (Bakufu Reading)").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, 'kernel_reading_bakufu_delegation_2026').
narrative_ontology:cs_kernel_codification('kernel_reading_bakufu_delegation_2026', formalized).
narrative_ontology:cs_authority_grounding('kernel_reading_bakufu_delegation_2026', lineage).
narrative_ontology:cs_interpretation_layer_present('kernel_reading_bakufu_delegation_2026').
narrative_ontology:cs_reading_relation('kernel_reading_bakufu_delegation_2026', imperial_mandate__loyalist_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('kernel_reading_bakufu_delegation_2026', foundational, imperial_legitimacy_separable_from_governance).
narrative_ontology:cs_axiom_status(imperial_legitimacy_separable_from_governance, holdable).
narrative_ontology:cs_axiom_grounding('kernel_reading_bakufu_delegation_2026', imperial_legitimacy_separable_from_governance, conventional).
narrative_ontology:cs_axiom('kernel_reading_bakufu_delegation_2026', foundational, samurai_class_governance_legitimate_through_delegation).
narrative_ontology:cs_axiom_status(samurai_class_governance_legitimate_through_delegation, overridden).
narrative_ontology:cs_axiom_grounding('kernel_reading_bakufu_delegation_2026', samurai_class_governance_legitimate_through_delegation, conventional).
narrative_ontology:cs_reference_frame('kernel_reading_bakufu_delegation_2026', bifurcated_sovereign_authority).
narrative_ontology:cs_drift_state('kernel_reading_bakufu_delegation_2026', meiji_restoration_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('kernel_reading_bakufu_delegation_2026', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, shogunal_administrative_stratum).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_governing_class).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_political_authority).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, peasantry_commoner_orders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPERIAL COURT (SNARE) — The emperor retains the spiritual-legitimacy function but is structurally barred from political decision-making. Exit is impossible: abdication is ceremonial only; the imperial institution is hereditary and inescapable. The emperor experiences the constraint as pure extraction: legitimacy is extracted from the imperial office and granted to the shogun, while political authority is withheld. The court bears costs (ceremonial obligations, resource constraints, loss of governing agency) without corresponding benefits in actual power.
constraint_indexing:constraint_classification(imperial_mandate__bakufu_delegation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SHOGUNAL ADMINISTRATION (TANGLED ROPE) — The shogun receives legitimacy delegation from the emperor (genuine coordination function: imperial sanction enables the shogun to rule without constant legitimacy contestation). But the relationship is also extractive: the shogun extracts political authority from the imperial office while constraining the emperor's political involvement. The shogun has some exit options (regime can be overthrown, as occurred multiple times in Japanese history) but faces high costs. The constraint coordinates a functional division of labor while maintaining asymmetric extraction of authority.
constraint_indexing:constraint_classification(imperial_mandate__bakufu_delegation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SAMURAI GOVERNING STRATUM (ROPE) — The bakufu delegation reading legitimizes the samurai as the governing class, creating institutional stability for samurai authority across regime changes. The constraint coordinates the samurai's continued political dominance through institutional continuity: even when regimes collapse, the delegated-authority framework preserves samurai-class governance. This is primarily a coordination mechanism (enables stable samurai rule) with minimal extraction — the samurai benefit from the institutional framework without bearing costs of legitimacy contestation.
constraint_indexing:constraint_classification(imperial_mandate__bakufu_delegation_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PEASANTRY AND COMMONER ORDERS (SNARE) — Excluded from the delegated-authority framework entirely. The constraint extracts labor, taxes, and obedience without granting any political voice or legitimacy claim. The peasantry and commoners experience no coordination function — only extraction. Suppression is total: alternatives (merchant governance, peasant councils) are foreclosed by the samurai monopoly on political legitimacy derived from imperial delegation. Exit is impossible (caste-like hereditary status).
constraint_indexing:constraint_classification(imperial_mandate__bakufu_delegation_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: BAKUFU INSTITUTIONAL STRUCTURE (TANGLED ROPE) — Viewed as a system-level entity, the bakufu coordinates the distribution of political authority across regimes and maintains samurai dominance (coordination function). Simultaneously, it extracts legitimacy from the imperial office and legitimacy-based authority from all lower social orders (extraction function). The institutional structure has some agency (can be reformed or overthrown) but faces high path-dependency costs from regime change. The constraint is both functionally necessary (enables stable governance) and extractive (concentrates authority).
constraint_indexing:constraint_classification(imperial_mandate__bakufu_delegation_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the bakufu delegation reading represents a specific institutional solution to a universal coordination problem: how to maintain legitimacy continuity across regime changes without centralizing legitimacy in a single actor (risking that actor's collapse bringing the whole system down). The reading bifurcates sovereignty (ritual authority vs administrative authority) to solve this problem. The system is both genuinely coordinative (legitimacy transfer mechanism prevents legitimacy crises) and genuinely extractive (upward concentration of authority from peasantry to samurai to shogun). The structural data supports tangled_rope classification — both coordination and extraction are present and irreducible.
constraint_indexing:constraint_classification(imperial_mandate__bakufu_delegation_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(imperial_mandate__bakufu_delegation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(imperial_mandate__bakufu_delegation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The bakufu delegation reading bifurcates sovereignty, creating extraction of political authority from the imperial office and allocation to the shogun. The extraction is substantial (peasantry and commoners are completely excluded; the emperor is suppressed from politics) but not total (the constraint does coordinate legitimate authority transfer and enables regime change without legitimacy collapse). The moderate-high value reflects both coordination and extraction functions. Suppression (0.62): High. Suppression operates at multiple levels: (1) The emperor is institutionally barred from political involvement through delegated-authority framework; (2) Samurai monopoly on legitimacy excludes peasantry and commoners from any legitimacy claim; (3) Peasant alternatives (merchant governance, commoner councils) are foreclosed by the monopoly structure. The suppression is enforced through institutional design (bakufu legitimacy is grounded in imperial delegation, so deviation requires rejecting both imperial authority and bakufu authority simultaneously). Theater ratio (0.58): Moderate-high. The imperial delegation ceremony has substantial performative content — the ritual transfer of authority is the functional mechanism, but the theater of the ritual (ceremonial language, symbolic gestures, religious elements) comprises a significant portion. Over the interval, theater increases as the bakufu becomes more established and formal ceremonies more elaborate, suggesting increasing performative content as the functional coordination stabilizes. The trajectory reflects institutional maturation: early bakufu (Kamakura) were more directly administrative; later bakufu (Tokugawa) more ceremonial and formal.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives diverge sharply on whether the constraint is coordination, extraction, or both. The Imperial Court sees pure extraction (snare) — legitimacy is taken, authority is withheld, no coordination benefit. The Shogunal Administration sees mixed coordination and extraction (tangled_rope) — receives necessary legitimacy transfer but pays the cost of imperial constraint. The Samurai Class sees pure coordination (rope) — the constraint enables their stable governance across regimes. The Peasantry sees pure extraction (snare) — excluded entirely from both legitimacy and authority. The Bakufu System sees itself as coordinative and extractive (tangled_rope) — solves a real problem while maintaining authority concentration. The Analytical Observer sees the constraint as genuinely tangled_rope — both functions are irreducibly present. The perspectival gap between the Court (snare), Shogun (tangled_rope), and Samurai (rope) reveals the structural ambiguity: is bifurcated sovereignty a constraint on imperial authority, a coordination mechanism for the shogun, or a legitimacy framework for the samurai? The answer is: all three, from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values d derive from each agent's structural position relative to the authority-extraction flow. The emperor is beneficiary-in-name (ritual authority is recognized) but victim-in-fact (political authority is withheld); the trapped exit option produces high d (~0.92), yielding high f(d) (~1.32), resulting in snare classification. The shogun is beneficiary (receives authority) and partially victim (constrained by imperial approval); constrained exit produces moderate d (~0.55), yielding moderate f(d) (~0.75), resulting in tangled_rope. The samurai class benefits from institutional legitimization; constrained exit produces moderate-low d (~0.42), yielding f(d) (~0.48), resulting in rope classification. The peasantry are victims (excluded from legitimacy claim); trapped exit produces maximum d (~0.98), yielding high f(d) (~1.42), resulting in snare classification. The system-level perspective (analytical) sees both extracted authority flowing upward and coordinated legitimacy flowing downward, producing moderate d (~0.50), yielding balanced f(d) (~0.65), resulting in tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint presents a genuine mandatrophy because the bakufu delegation reading contains both irreducible coordination and irreducible extraction, and these functions cannot be separated without destroying the institutional solution. The coordination function (legitimacy transfer across regime changes) is real — it solves a genuine problem. The extraction function (upward concentration of political authority) is also real. Neither is eliminable. The mandatrophy is resolved by recognizing the tangled_rope classification as diagnostic: the constraint MUST have both functions because the problem it solves (how to maintain legitimacy through regime change without centralizing legitimacy dangerously) requires bifurcated sovereignty. The cost of solving the coordination problem is unavoidable extraction. The bakufu reading does not attempt to hide the extraction — it explicitly claims that samurai governance is legitimate and peasant exclusion is proper. The mandatrophy is not resolved by denying extraction, but by showing that the extraction is the structural cost of the coordination solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bakufu_legitimacy_source_ambiguity,
    'Does the bakufu''s legitimacy derive from the imperial delegation, from samurai class dominance, or from effective administrative performance?',
    'Historical analysis of bakufu stability: (a) Do regimes collapse when imperial approval is withdrawn? (b) Do bakufu with poor administrative performance persist if imperial support continues? (c) Can a samurai faction without imperial delegation establish a new bakufu? Cross-case comparison across Kamakura, Ashikaga, and Tokugawa periods.',
    'If legitimacy is primarily from imperial delegation: this reading''s core premise is sustained (emperor''s ritual authority is functionally necessary). If legitimacy is primarily from samurai class power or administrative performance: the imperial delegation is ornamental (piton reading emerges as stronger). If multi-source: the interaction model (delegation + class power + performance) replaces single-source models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bakufu_legitimacy_source_ambiguity, empirical, 'Source of bakufu legitimacy: imperial delegation, samurai power, or administrative performance').

omega_variable(
    emperor_suppression_mechanism_type,
    'Is imperial political suppression structural (legal/institutional barriers prevent emperor action) or internalized (emperor accepts ritual role as normatively correct)?',
    'Historical documentation: (a) How many emperors attempted political involvement and were forcibly prevented? (b) How many emperors voluntarily accepted ritual-only roles? (c) Are there documented ideological justifications for imperial withdrawal? (d) Comparison of cases where emperors maintained vs relinquished political authority.',
    'If suppression is primarily structural: the emperor is a trapped agent experiencing snare extraction (perspective 1 confirmed). If suppression is primarily internalized: the emperor is identity_locked (accepts the ritual role as proper to imperial status), potentially changing exit_options and the experienced constraint type. Mixed suppression requires documentation of ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emperor_suppression_mechanism_type, empirical, 'Whether imperial suppression is structural or internalized').

omega_variable(
    committer_kernel_ambiguity_bakufu_vs_loyalist,
    'Is the bifurcated sovereignty reading (bakufu delegation = functional institutional solution) genuinely competing with the loyalist restoration reading (emperor''s full sovereignty is rightful and delegated authority is illegitimate), or do these readings serve different historical actors with different interests?',
    'Documentary history: (a) Do contemporary bakufu legitimation texts explicitly argue FOR bifurcated sovereignty as superior to unified sovereignty? (b) Do loyalist texts explicitly argue AGAINST delegation as a legitimate mechanism? (c) Are these readings present in scholarly interpretation, in contemporary political theory, or only in modern retrospective analysis? (d) Do the readings coexist within single political actors or belong to opposing factions?',
    'If readings genuinely compete within unified frameworks: they may exhibit foreclosure relation (only one can be institutionally valid). If readings reflect different actors'' commitments: they coexist_with each other (both remain live in the polity). The distinction changes how the network constraint works and what policy divergences follow from reading choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_ambiguity_bakufu_vs_loyalist, conceptual, 'Whether bakufu and loyalist readings genuinely compete or reflect different actors'' institutional interests').

omega_variable(
    institutional_continuity_vs_legitimacy_cycling,
    'Does the bakufu delegation reading''s claim about institutional continuity across regime changes rely on the same institution persisting, or on the institutional FORM (bifurcated sovereignty structure) persisting through different occupants?',
    'Structural analysis: (a) When a bakufu regime collapses (e.g., Ashikaga to Tokugawa transition), what institutional features transfer? (b) Is it the specific shogunal family/organization, or the delegation framework itself? (c) Does a new shogun need imperial re-legitimation, or does delegation automatically transfer to the new regime? (d) Analysis of regime-change ceremonies: what is reaffirmed, what is new?',
    'If continuity means institutional form persists through occupant change: the coordination function is real (bakufu structure enables regime change without legitimacy crisis). If continuity requires re-legitimation from emperor for each regime: the emperor retains more political leverage than the reading suggests (hidden coordination cost). If emperor cannot prevent delegation-to-new-regime: emperor is locked into automatic legitimacy transfer (even more constrained suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_continuity_vs_legitimacy_cycling, empirical, 'What persists across bakufu regime changes: institutional occupancy or institutional form').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imand_tr_t0, imperial_mandate__bakufu_delegation_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(imand_tr_t150, imperial_mandate__bakufu_delegation_reading, theater_ratio, 150, 0.55).
narrative_ontology:measurement(imand_tr_t300, imperial_mandate__bakufu_delegation_reading, theater_ratio, 300, 0.58).

% Extraction over time
narrative_ontology:measurement(imand_be_t0, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(imand_be_t150, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 150, 0.45).
narrative_ontology:measurement(imand_be_t300, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 300, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(imand_su_t0, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(imand_su_t150, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 150, 0.58).
narrative_ontology:measurement(imand_su_t300, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 300, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, imperial_mandate__loyalist_restoration_reading).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, samurai_legitimacy_monopoly).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, peasant_exclusion_from_political_voice).

% DUAL FORMULATION NOTE:
% The imperial mandate constraint family contains two sibling constraint stories: bakufu_delegation_reading (this story) and loyalist_restoration_reading. The readings have different ε values because they describe different institutional structures: the bakufu reading (ε=0.48, tangled_rope) describes a system with bifurcated sovereignty; the loyalist reading (ε=estimated higher, snare or mountain depending on restoration claims) describes a system with unified sovereignty. These are structurally distinct claims about legitimacy flow. Do NOT attempt to force both readings into one story. Each reading instantiates a different constraint with different ε, different beneficiaries/victims, and different perspective patterns. The readings are linked through the network because the bakufu reading's institutional stability depends partly on foreclosing the loyalist claim (or at least making it historical/obsolete). The loyalist reading's institutional viability depends on delegitimizing the bakufu claim. See imperial_mandate kernel documentation for the kernel structure and reading relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_mandate__bakufu_delegation_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
