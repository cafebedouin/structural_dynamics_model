% ============================================================================
% CONSTRAINT STORY: algeria_france_colonial_legacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algeria_france_colonial_legacy, []).

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
 *   constraint_id: algeria_france_colonial_legacy
 *   human_readable: The persistent structural legacy of French colonization in Algeria
 *   domain: geopolitical/economic/postcolonial
 *
 * SUMMARY:
 *   The structural legacy of French colonization in Algeria persists 62 years
 *   after formal independence (1962-2024) as a hybrid coordination-extraction
 *   mechanism embedded in institutions perceived as neutral rather than
 *   coercive. France occupied and colonized Algeria for 132 years
 *   (1830-1962), establishing French as the administrative, educational, and
 *   economic lingua franca while extracting resources and labor through
 *   systematic institutional structures. Formal decolonization transferred
 *   political sovereignty but left intact institutional templates (legal
 *   codes, educational curricula, language regimes, administrative
 *   procedures, currency mechanisms via trade dependence) that reproduce
 *   French cultural and economic dominance. The constraint operates through
 *   this institutional embedding rather than through direct military coercion
 *   — making it more durable than conventional extraction and harder to
 *   recognize as extraction at all. The working class experiences this as a
 *   trap: educational mobility requires French language fluency; labor
 *   markets reward French credentials; emigration requires negotiating French
 *   visa regimes; capital flows are denominated in relationship to French
 *   markets. Algerian state institutions are constrained by path dependency
 *   on inherited French templates while coordinating genuine functions
 *   (infrastructure, trade, technical cooperation). Francophone elites
 *   benefit from cultural capital asymmetries but are also locked into the
 *   extraction mechanism. French capital interests experience the arrangement
 *   as pure coordination — market access, linguistic homogeneity,
 *   institutional predictability — with no perceived enforcement overhead
 *   because the enforcement is built into institutions. The theater ratio
 *   reflects that the colonial legacy is increasingly maintained through
 *   institutional inertia and linguistic embedding rather than active
 *   political enforcement. Over the 64-year postcolonial interval,
 *   extractiveness has declined from 0.72 to 0.58 as Algerian institutional
 *   autonomy has expanded (central bank independence, energy sector
 *   diversification, arabicization of education expansion), but the
 *   constraint remains severe because path dependency is structural.
 *
 * KEY AGENTS:
 *   - Algerian Working Class: Primary victim (powerless/trapped) — trapped in French language dependency, colonial education curricula, and institutional templates; experiences maximum extraction with minimal coordination benefit
 *   - Algerian State Institutions: Secondary victim (moderate/constrained) — constrained by inherited French legal codes, bureaucratic structures, and institutional lock-in; some agency through diversification efforts
 *   - French Capital and Political Elite: Primary beneficiary (institutional/arbitrage) — experiences the constraint as pure coordination mechanism with privileged market access and labor force trained in compatible cultural norms
 *   - Francophone Algerian Elites: Ambivalent actor (organized/mobile) — benefits from French language cultural capital but also constrained by dependence on French validation of credentials and elite network participation
 *   - Algerian Resource Sectors: Victim (moderate/constrained) — subject to asymmetric terms of trade with French markets, currency mechanism pressures, and energy export dependency
 *   - Residual Colonial Administrative Apparatus: Structural persistence mechanism (institutional/arbitrage) — maintains constraint through language policy, educational curricula, legal codes, and visa regimes operating as neutral institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algeria_france_colonial_legacy, 0.58).
domain_priors:suppression_score(algeria_france_colonial_legacy, 0.68).
domain_priors:theater_ratio(algeria_france_colonial_legacy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algeria_france_colonial_legacy, tangled_rope).
narrative_ontology:human_readable(algeria_france_colonial_legacy, "The persistent structural legacy of French colonization in Algeria").
narrative_ontology:topic_domain(algeria_france_colonial_legacy, "geopolitical/economic/postcolonial").

domain_priors:requires_active_enforcement(algeria_france_colonial_legacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algeria_france_colonial_legacy, french_capital_interests).
narrative_ontology:constraint_beneficiary(algeria_france_colonial_legacy, francophone_elites_algeria).
narrative_ontology:constraint_victim(algeria_france_colonial_legacy, algerian_working_class).
narrative_ontology:constraint_victim(algeria_france_colonial_legacy, resource_sovereignty).
narrative_ontology:constraint_victim(algeria_france_colonial_legacy, educational_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGERIAN WORKING CLASS (SNARE) — Trapped in structural dependency on French language, capital flows, and institutional templates. Education system reproduces French cultural dominance; labor markets value French credentials; emigration requires French visa regimes. No plausible exit without decades of institutional reorientation. Experiences maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ALGERIAN STATE INSTITUTIONS (TANGLED ROPE) — Constrained by inherited French institutional templates, legal codes, and bureaucratic structures. Coordinating function: French infrastructure investments, technical cooperation, market access. Extraction function: currency mechanisms (CFA-equivalent pressures through trade dependence), professional licensing requirements favoring francophone credentials, terms-of-trade asymmetries. Some agency through diversification efforts but decades of path dependency create lock-in.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FRENCH CAPITAL AND POLITICAL ELITE (ROPE) — Experiences the legacy constraint as pure coordination mechanism: privileged market access, labor force trained in French language and cultural norms, institutional alignment reducing transaction costs. Benefits from Algerian energy exports, agricultural production, and remittances without enforcement overhead. Effective extraction is subsidized by asymmetric exit options — France can arbitrage toward other markets; Algeria cannot escape institutional embedding.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FRANCOPHONE ALGERIAN ELITES (TANGLED ROPE) — Organized actors benefiting from cultural capital asymmetry: French language fluency as rare skill in job markets, access to French networks, educational prestige. But also constrained by dependence on French validation of credentials and periodic pressure to assimilate French political positions. Ambivalent relationship: benefits from extraction mechanism but also embedded in it. Some agency through elite networking but vulnerable to shifts in French policy.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RESIDUAL COLONIAL APPARATUS (PITON) — Forms like language policy, educational curriculum, administrative codes, and visa regimes persist through institutional inertia despite formal independence. Theater ratio: high. These mechanisms perform legitimacy (described as mutual cooperation, shared civilization, bilateral partnership) while maintaining extraction. The apparatus has lost direct military/political enforcement capacity but retains behavioral lock-in through path dependency and lack of institutional alternatives. Maintainable only through constant theatrical reinforcement.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the constraint as a hybrid coordination-extraction mechanism operating via institutional embedding rather than direct coercion. Coordination elements: investment, trade, technical cooperation. Extraction elements: asymmetric dependency, cultural dominance, terms-of-trade disadvantage. The constraint persists because it is embedded in institutions perceived as neutral (language, law, education, currency) rather than coercive. Recognition of both functions is required to avoid false naturalness.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algeria_france_colonial_legacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algeria_france_colonial_legacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algeria_france_colonial_legacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algeria_france_colonial_legacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algeria_france_colonial_legacy, TR),
    TR >= 0.70.

:- end_tests(algeria_france_colonial_legacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The colonial legacy extracts through institutional embedding rather than direct coercion: asymmetric language dependency creates labor market barriers; educational curricula privilege French cultural content; legal codes reproduce French institutional logic; currency mechanisms create trade asymmetries. The extraction is real but lower than during direct colonial period (0.72) because Algerian state has acquired some institutional autonomy (central bank, energy sector management, educational arabicization expansion). Suppression (0.68): High. Barriers to exit include: decades of path dependency in institutional templates; absence of plausible alternative institutional anchors (Moroccan/Tunisian models are not clearly superior; Berber/Arabic institutional capacity is underdeveloped); high transition costs for language shift; demographic lock-in (600,000+ French settlers left but cultural/linguistic legacy persists in institutions, not population). Suppression is not total because some Algerian agency exists — arabicization programs are expanding, regional trade cooperation is developing, French language is declining in education. Theater ratio (0.55): Moderate. The constraint is increasingly maintained through institutional inertia and linguistic embedding rather than active enforcement. Language policy is presented as neutral educational choice, not colonial legacy; trade relationships are framed as mutual benefit; visa regimes are administrative routine, not extraction mechanism. But theater has increased over the interval because the constraint has lost its overt enforcement apparatus — persistence depends entirely on perception of institutions as natural rather than coercive.
 *
 * PERSPECTIVAL GAP:
 *   Extreme. The beneficiary (French capital) sees pure coordination and mutual benefit. The organized elite (Francophone Algerians) sees mixed benefit and constraint. The state institutions see constrained coordination. The working class sees pure extraction. The residual apparatus sees neutral administrative routine. The analytical observer sees tangled rope — genuine coordination functions embedded in extraction mechanisms. The perspectival gap arises because the constraint operates through institutional embedding: the same language regime is experienced as educational infrastructure by some, as cultural domination by others, as market access facilitator by France, as labor market barrier by workers. No single perspective is wrong — the presheaf over institutional positions IS the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's structural position determines their d value and experienced extractiveness. French capital interests derive d ≈ 0.10 (beneficiary with arbitrage exit) → f(d) ≈ -0.08 → negative χ (they are subsidized by the constraint). Algerian working class derives d ≈ 0.92 (victim with trapped exit) → f(d) ≈ 1.38 → high χ (they bear maximum extraction). Algerian state institutions derive d ≈ 0.58 (mixed, some victim status, constrained exit) → f(d) ≈ 0.72 → moderate χ. Francophone elites derive d ≈ 0.35 (partial beneficiary, but constrained by French network dependence) → f(d) ≈ 0.25 → low-moderate χ. The constraint is tangled rope at the state/institutional level because genuine coordination functions (trade, technical cooperation, investment) are bundled with extraction mechanisms (asymmetric dependency, terms-of-trade disadvantage, cultural dominance). Neither can be cleanly separated.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint cannot be classified as pure Rope because the extraction is structurally embedded — asymmetric language dependency, terms-of-trade disadvantage, and cultural dominance are not artifacts of measurement but structural features. It cannot be classified as pure Snare because genuine coordination benefits exist (infrastructure investment, trade access, technical cooperation) and France experiences no enforcement overhead. The Tangled Rope classification resolves the mandatrophy by acknowledging both functions: the constraint solves real coordination problems (how to maintain institutional compatibility, facilitate trade, enable technical cooperation) AND extracts asymmetrically (through language dominance, institutional lock-in, terms-of-trade imbalance). The critical distinction is that the extraction persists not through force but through institutional embedding — the machinery of constraint is perceived as neutral (education, law, administration, language) rather than as coercive apparatus. Mandatrophy would occur if we forced this into either pure coordination (Rope) or pure extraction (Snare). The Tangled Rope classification reveals that the constraint's durability derives precisely from this hybrid nature — beneficiaries experience only coordination, victims experience only extraction, and the institutional apparatus maintains separation of concerns that obscures the asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_path_dependency_or_deliberate_maintenance,
    'Does the colonial legacy persist primarily through institutional lock-in and path dependency, or through deliberate maintenance by French elites?',
    'Comparative institutional analysis: Do French policy shifts favor Algerian institutional autonomy? Do remittance patterns and capital flows reflect structural embedding or active steering? Analysis of French diplomatic pressure vs. Algerian policy choice autonomy.',
    'If path dependency dominates: constraint classification shifts toward Piton (degraded, theater-driven). If deliberate maintenance dominates: classification remains Tangled Rope or shifts toward Snare (active extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_path_dependency_or_deliberate_maintenance, conceptual, 'Whether the colonial legacy is maintained through path dependency or deliberate French policy').

omega_variable(
    francophone_elite_beneficiary_status,
    'Are Francophone Algerian elites genuine beneficiaries of the colonial legacy or trapped participants whose collaboration is extracted by French interests?',
    'Longitudinal analysis of elite economic mobility: Do francophone credentials improve material outcomes relative to arabophone peers? Do elites have genuine exit options or are they locked into dependency relationships? Analysis of elite political autonomy from French pressure.',
    'If genuine beneficiaries: extraction is less severe, regime stability is higher, transition difficulties increase. If trapped collaborators: the constraint is more purely extractive, regime vulnerability is higher, coalition potential among elites increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(francophone_elite_beneficiary_status, empirical, 'Whether Francophone elites are beneficiaries or trapped collaborators').

omega_variable(
    language_sovereignty_reversibility,
    'How reversible is the institutional embedding of French language in Algerian education, law, and administration? What would transition costs be?',
    'Feasibility studies from comparable postcolonial transitions (Indonesia''s language shift, India''s education pluralism). Cost-benefit analysis of Arabicization or Berber/Tamazight elevation vs. maintaining French. Measurement of public demand for language shift vs. elite resistance.',
    'If reversible at moderate cost: institutional lock-in is weaker than assumed, exit options improve, extraction classification downgrades. If highly costly: path dependency is structural, exit is genuinely trapped, extraction is amplified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(language_sovereignty_reversibility, empirical, 'Reversibility of French language institutional embedding').

omega_variable(
    alternative_coordination_pathway_viability,
    'Could North African (Moroccan, Tunisian, Berber-centered) institutional templates provide functional alternatives to French ones without massive disruption?',
    'Comparative institutional design: Are Moroccan/Tunisian legal codes functionally superior or just different? Could Berber/Arabic education systems provide equivalent human capital? Analysis of regional cooperation frameworks (AU, Arab League) as alternative institutional anchors.',
    'If viable alternatives exist: the constraint is more extractive than inevitable, transition is plausible, classification remains Tangled Rope or shifts toward Snare. If no alternatives credibly exist: the constraint approaches necessity, extractiveness interpretation shifts toward Rope or even Mountain (coordination necessity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_pathway_viability, conceptual, 'Viability of alternative non-French institutional templates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algeria_france_colonial_legacy, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algfr_tr_t0, algeria_france_colonial_legacy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(algfr_tr_t32, algeria_france_colonial_legacy, theater_ratio, 32, 0.48).
narrative_ontology:measurement(algfr_tr_t64, algeria_france_colonial_legacy, theater_ratio, 64, 0.55).

% Extraction over time
narrative_ontology:measurement(algfr_be_t0, algeria_france_colonial_legacy, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(algfr_be_t32, algeria_france_colonial_legacy, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(algfr_be_t64, algeria_france_colonial_legacy, base_extractiveness, 64, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algeria_france_colonial_legacy, global_infrastructure).
narrative_ontology:affects_constraint(algeria_france_colonial_legacy, maghreb_economic_integration).
narrative_ontology:affects_constraint(algeria_france_colonial_legacy, francophone_education_systems).
narrative_ontology:affects_constraint(algeria_france_colonial_legacy, north_african_currency_regimes).

% DUAL FORMULATION NOTE:
% The colonial legacy constraint decomposes into three structural components: (1) Language/education embedding (ε ≈ 0.48, Tangled Rope) — institutional lock-in through French language and curriculum dominance. (2) Trade/currency dependency (ε ≈ 0.65, Snare at working-class perspective) — asymmetric market access and capital flow mechanisms. (3) Administrative/legal templates (ε ≈ 0.42, Rope at state level) — inherited institutional structures that coordinate but also embed dependency. These are linked via network.affects_constraints but operate with distinct ε values reflecting their different structural mechanisms. The aggregate constraint story (ε ≈ 0.58) represents their combined effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algeria_france_colonial_legacy, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
