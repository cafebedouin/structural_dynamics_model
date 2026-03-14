% ============================================================================
% CONSTRAINT STORY: developing_economy_capital_flight
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_developing_economy_capital_flight, []).

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
 *   constraint_id: developing_economy_capital_flight
 *   human_readable: Capital Flight Constraint in Developing Economies
 *   domain: economic/political
 *
 * SUMMARY:
 *   Capital flight from developing economies represents a structural
 *   constraint where wealthy residents and multinational corporations
 *   systematically move capital to developed-economy financial centers,
 *   offshore tax havens, and third-party jurisdictions. The mechanism
 *   combines asymmetric incentive structures (capital earns returns in
 *   stable, liquid markets), weak enforcement capacity (developing economy
 *   states lack tools to prevent outflows), elite coordination (wealthy
 *   elites share understanding that capital should move offshore for
 *   preservation), and institutional design (offshore financial centers and
 *   tax havens provide legal structures for opacity). The constraint exhibits
 *   the full spectrum of DR classifications depending on observer position:
 *   the domestic poor experience it as an extractive snare (trapped with no
 *   exit, bearing full cost of reduced public services); the state apparatus
 *   maintains performative control mechanisms that are functionally degraded
 *   (piton); the developed banking sector benefits from coordination of
 *   global flows (rope); and the international regulatory coalition is
 *   building alternative pathways through transparency and automatic
 *   information exchange (scaffold). The constraint's extractiveness has
 *   increased over the 20-year measurement interval (0.35 → 0.58) as
 *   technological capacity for capital movement has improved and as
 *   developing economies have faced repeated currency crises and political
 *   instability that motivate flight. Theater ratio has remained moderate to
 *   moderately high, reflecting that while genuine capital flows exist, they
 *   are partly obscured by transfer pricing, shell company layering, and
 *   beneficial ownership opacity.
 *
 * KEY AGENTS:
 *   - Domestic poor populations: Primary victim (powerless/trapped) — bear cost of reduced public services and investment; have no exit option
 *   - Domestic middle-class workers: Secondary victim (moderate/constrained) — face job and investment loss as capital flees; constrained by visa and skill barriers
 *   - Multinational corporations: Beneficiary (organized/arbitrage) — repatriate profits; coordinate global tax strategies; have mobile exit
 *   - Developed-economy banking sector: Primary institutional beneficiary (institutional/arbitrage) — receive capital flows; manage offshore accounts; structure tax-efficient vehicles
 *   - Domestic elites and capital holders: Mixed agent (powerful/mobile) — simultaneously extract from economy and experience it as coordination mechanism for wealth preservation
 *   - Developing economy state apparatus: Inert institutional actor (institutional/constrained) — maintains control theater but lacks enforcement capacity or political will
 *   - International regulatory coalition: Organized coalition (organized/constrained) — building transparency infrastructure; represents scaffold sunset logic
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing constructed financial architecture as economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(developing_economy_capital_flight, 0.58).
domain_priors:suppression_score(developing_economy_capital_flight, 0.68).
domain_priors:theater_ratio(developing_economy_capital_flight, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(developing_economy_capital_flight, extractiveness, 0.58).
narrative_ontology:constraint_metric(developing_economy_capital_flight, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(developing_economy_capital_flight, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(developing_economy_capital_flight, tangled_rope).
narrative_ontology:human_readable(developing_economy_capital_flight, "Capital Flight Constraint in Developing Economies").
narrative_ontology:topic_domain(developing_economy_capital_flight, "economic/political").

domain_priors:requires_active_enforcement(developing_economy_capital_flight).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(developing_economy_capital_flight, capital_holders).
narrative_ontology:constraint_beneficiary(developing_economy_capital_flight, offshore_financial_centers).
narrative_ontology:constraint_beneficiary(developing_economy_capital_flight, developed_economy_banking_sector).
narrative_ontology:constraint_victim(developing_economy_capital_flight, domestic_poor_populations).
narrative_ontology:constraint_victim(developing_economy_capital_flight, productive_investment_base).
narrative_ontology:constraint_victim(developing_economy_capital_flight, state_tax_revenue).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC POOR (SNARE) — Trapped within territorial jurisdiction. No ability to move capital offshore or access tax havens. Bears full cost of degraded public services, higher taxes, and reduced productive investment as capital flees. Extraction is maximal — they cannot exit the constraint while experiencing its full burden.
constraint_indexing:constraint_classification(developing_economy_capital_flight, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC MIDDLE-CLASS WORKERS (TANGLED ROPE) — Constrained by skill requirements, visa barriers, and family ties. Genuine coordination exists: the capital system provides some employment and investment opportunities within the domestic economy. But extraction is asymmetric — their opportunities shrink as capital flees, and they cannot access the offshore vehicles that wealthy elites use. Mixed experience of coordination benefit and extraction cost.
constraint_indexing:constraint_classification(developing_economy_capital_flight, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTINATIONAL CORPORATIONS (ROPE) — Exit options are mobile (can shift operations) and arbitrage (can access offshore structures). Experience the constraint as coordination of global capital flows — profit repatriation, transfer pricing, and dividend routing are legitimate functions of international business. Beneficiaries with genuine coordination rationale: the system enables profit-taking while minimizing tax exposure.
constraint_indexing:constraint_classification(developing_economy_capital_flight, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEVELOPED-ECONOMY BANKING SECTOR (ROPE) — Primary institutional beneficiary. Receives and invests capital flows from developing economies. Coordinates international financial flows through correspondent banking and wealth management. Extraction runs toward this agent — capital flight benefits their asset bases and fee structures. Zero suppression from their position — they have full agency and exit options.
constraint_indexing:constraint_classification(developing_economy_capital_flight, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL REGULATORY COALITION (SCAFFOLD) — Organized agents (FATCA, Common Reporting Standard, base erosion and profit shifting initiatives) are building alternative transparency and enforcement pathways. These represent sunset logic for the traditional capital flight constraint: as automatic information exchange matures and beneficial ownership registries proliferate, the ability to hide capital offshore declines. The constraint is temporary — exit is possible through regulatory maturation. Theater is moderate because actual compliance varies by jurisdiction.
constraint_indexing:constraint_classification(developing_economy_capital_flight, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DEVELOPING ECONOMY STATE APPARATUS (PITON) — Maintains the appearance of capital controls and tax enforcement while lacking capacity, will, or institutional interest to enforce them effectively. Theater ratio is high: regulations exist, reporting structures are in place, penalties are prescribed — but enforcement is selective, corrupted, or absent. The state sees its own control mechanisms as degraded (inertial) rather than functional. Many capital controls persist only because dismantling them would require political capital; their enforcement has atrophied.
constraint_indexing:constraint_classification(developing_economy_capital_flight, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: DOMESTIC ELITES AND CAPITAL HOLDERS (TANGLED ROPE) — Experience mixed coordination and extraction. The offshore system coordinates their ability to preserve wealth across borders and hedge against domestic political/currency risk — genuine coordination function. But extraction operates within this: they extract from the domestic economy by moving capital out, reducing investment and tax base. They have mobile exit options and structural power. The constraint serves them: it is the mechanism through which they extract from the trapped majority.
constraint_indexing:constraint_classification(developing_economy_capital_flight, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, capital flight appears as a natural law of economics: capital moves to high-return environments and away from unstable ones. Interest rate differentials, currency volatility, and political risk create inexorable pressure for capital movement. This perspective risks naturalizing what is actually a contingent institutional arrangement: the particular architecture of offshore finance, tax havens, and weak information exchange is not a law of nature but a constructed system with deliberately designed opacity.
constraint_indexing:constraint_classification(developing_economy_capital_flight, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(developing_economy_capital_flight_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(developing_economy_capital_flight, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(developing_economy_capital_flight, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(developing_economy_capital_flight, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(developing_economy_capital_flight, TR),
    TR >= 0.70.

:- end_tests(developing_economy_capital_flight_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Capital flight represents genuine extraction from the domestic productive economy: capital that could finance investment in developing economy infrastructure, manufacturing, and services instead flows to developed-economy assets. The extraction is not maximal (0.70+) because some capital flight represents legitimate hedging against currency/political risk and serves real coordination functions for international investment. However, the 20-year trend shows extractiveness rising from 0.35 to 0.58, reflecting that opacity mechanisms have improved, elite coordination has intensified, and institutional capacity for resistance has weakened. Suppression (0.68): High. Multiple barriers prevent the domestic economy from capturing and retaining capital: (a) capital account restrictions are unenforced or corrupted; (b) domestic investment opportunities are perceived as riskier than foreign alternatives; (c) elites control both the capital AND the regulatory apparatus, creating structural misalignment; (d) ordinary citizens cannot access offshore vehicles even if capital could legally be moved; (e) once capital has fled, repatriation faces tax barriers and political barriers (elites resist domestic capital call). Theater ratio (0.55): Moderate. The constraint combines genuine capital flows (non-theatrical) with substantial performative regulation (capital controls that are unenforced, disclosure requirements that are corrupted, tax codes that are selectively applied). The ratio has increased over the interval as developing economies have adopted international standards (FATCA, CRS) partly to signal competence while maintaining actual lax enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals how the same constraint appears as pure extraction (snare) to the trapped majority but as pure coordination (rope) to the beneficiary institutions. This is the diagnostic signature of asymmetric extraction masquerading as coordination: the beneficiary institutions genuinely do coordinate capital flows (their rope classification is structurally justified), but the coordination function serves extraction (capital is extracted from the domestic economy to benefit offshore actors). The scaffold perspective shows an exit path (international transparency) that the piton perspective denies (state control mechanisms are inert). The mountain perspective naturalizes the constraint; the tangled rope perspective names it as constructed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from beneficiary/victim declarations and exit options. Capital holders and offshore financial centers are beneficiaries with arbitrage exit options (d ≈ 0.05-0.15, low effective extraction). Domestic poor are victims with trapped exit (d ≈ 0.95, maximum effective extraction). Multinational corporations are beneficiaries with mobile exit (d ≈ 0.25, low-moderate effective extraction). Developed banking sector is beneficiary with arbitrage (d ≈ 0.10, low extraction). The state apparatus is inert (constrained exit despite institutional power), making its directionality ambiguous — it appears to tolerate the constraint despite having formal regulatory authority, suggesting that state elites are themselves beneficiaries, raising d for state-level analysis. Domestic middle-class workers are secondary victims with constrained exit (d ≈ 0.70), experiencing mixed extraction and coordination benefit (hence tangled rope classification). Each perspective's χ value is computed from base extractiveness (0.58) × f(d) × σ(S), with scope ranging from national (0.8) to global (1.2). A powerless trapped agent at national scope experiences χ ≈ 0.58 × 1.42 × 0.8 ≈ 0.66 (snare threshold); an institutional arbitrage agent experiences χ ≈ 0.58 × (-0.12) × 1.2 ≈ -0.08 (negative extraction, pure coordination).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the classification varies structurally with observer position rather than being an artifact of measurement ambiguity. The snare classification for domestic poor is NOT contradicted by the rope classification for the beneficiary institutions — they are legitimate perspectival readings of different structural positions. The mandatrophy surfaces at the state level: the state apparatus claims to maintain control (piton perspective, theater ratio high) but lacks enforcement capacity or will (actual functional degradation). The resolution is that the state is not a neutral arbiter but a captured actor — state elites are themselves beneficiaries of capital flight, making them part of the tangled rope classification rather than independent enforcers. Once the state is recognized as a mixed agent (both victim of extraction via tax base degradation AND beneficiary of capital flight access), the perspectival landscape becomes coherent: the system coordinates benefits for elites (beneficiary institutions + state apparatus + domestic capital holders) while extracting from the trapped majority. The scaffold perspective provides the final resolution: international regulatory mechanisms are building real constraints on capital flight, suggesting the traditional constraint has a sunset horizon. The piton classification for the state apparatus is justified — the state's control theater is maintained by inertia because dismantling it would require admitting complicity in capital flight, while maintaining it allows deniability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_vs_political_will,
    'Is the persistence of capital flight due to technical capacity constraints or deliberate political choice by state elites who benefit from the system?',
    'Comparative analysis: states with strong institutional capacity but high capital flight (Singapore, UAE, Monaco) vs states with weak capacity but low flight (Belarus, North Korea). Correlation between enforcement spending and actual compliance rates.',
    'If capacity-constrained: technology transfer and institutional building could reduce flight. If political choice: elites have internalized the offshore system and will resist enforcement regardless of capacity. Changes fundamental remedial strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_political_will, empirical, 'Whether capital flight is capacity-constrained or politically chosen').

omega_variable(
    legitimate_hedging_vs_extraction,
    'How much capital flight represents legitimate hedging against currency risk and political instability vs pure extraction of rents and tax evasion?',
    'Flow analysis: correlation between capital flight timing and: (a) currency volatility spikes, (b) political unrest, (c) tax law changes, (d) corporate earnings announcements. Proportional attribution of flows to each driver.',
    'If primarily hedging: reducing extraction may require stabilization of political/currency regime, not just enforcement. If primarily extraction: enforcement and transparency become primary levers. Shifts locus of problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_hedging_vs_extraction, empirical, 'Proportion of capital flight that is legitimate hedging vs extraction').

omega_variable(
    regulatory_coalition_effectiveness,
    'Will automatic information exchange (FATCA, CRS) and beneficial ownership registries actually reduce capital flight, or will they merely shift capital to remaining non-reporting jurisdictions?',
    'Post-CRS implementation tracking: capital flow changes in nations with strong automatic exchange agreements vs those without. Whether flows shift to remaining non-reporters or actually reverse.',
    'If effective: scaffold sunset is real and the constraint will self-limit. If ineffective: regulatory coalition is performing (theater ratio) without functional constraint reduction. Determines whether scaffold classification is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_coalition_effectiveness, empirical, 'Whether international transparency initiatives reduce capital flight or merely redistribute it').

omega_variable(
    elite_coordination_mechanism,
    'How much does capital flight depend on elite coordination (shared understanding that capital should move offshore) vs structural incentives that operate regardless of collective intent?',
    'Elite interview analysis; text analysis of financial media and elite discourse; network analysis of capital flow patterns to identify clustering vs dispersal.',
    'If coordination-dependent: changing elite narratives could reduce flight. If incentive-driven: narrative change alone is insufficient — structural barriers must change. Affects whether the tangled rope classification captures a real coordination function or is aspirational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_coordination_mechanism, conceptual, 'Whether capital flight is sustained by elite coordination or structural incentives').

omega_variable(
    state_complicity_degree,
    'What proportion of capital flight involves direct state actor participation (kleptocratic extraction by officials) vs passive toleration of private capital outflows?',
    'Sanctions investigation data; leaked financial records (Panama Papers, etc.); comparison of state officials'' disclosed vs actual assets across time.',
    'If state-led: piton classification is insufficient — should be snare (the state IS the extractor). If passive toleration: piton classification is justified (the state apparatus has become inert). Affects victim declarations and institutional perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_complicity_degree, empirical, 'Degree of state official participation in capital flight').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(developing_economy_capital_flight, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decf_tr_t0, developing_economy_capital_flight, theater_ratio, 0, 0.4).
narrative_ontology:measurement(decf_tr_t10, developing_economy_capital_flight, theater_ratio, 10, 0.5).
narrative_ontology:measurement(decf_tr_t20, developing_economy_capital_flight, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(decf_be_t0, developing_economy_capital_flight, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(decf_be_t10, developing_economy_capital_flight, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(decf_be_t20, developing_economy_capital_flight, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(developing_economy_capital_flight, resource_allocation).
narrative_ontology:affects_constraint(developing_economy_capital_flight, currency_crisis_vulnerability).
narrative_ontology:affects_constraint(developing_economy_capital_flight, domestic_investment_deficit).
narrative_ontology:affects_constraint(developing_economy_capital_flight, tax_base_erosion).
narrative_ontology:affects_constraint(developing_economy_capital_flight, state_capacity_degradation).

% DUAL FORMULATION NOTE:
% Capital flight is upstream of multiple downstream constraints: it creates currency vulnerability (capital outflows trigger devaluation), investment deficits (capital that could finance domestic productive investment is unavailable), tax base erosion (capital flight reduces taxable income and asset bases), and state capacity degradation (reduced tax revenue forces cuts in public capacity). Each downstream constraint has its own extractiveness value and perspectives. The capital flight constraint coordinates these effects through a single mechanism: the structural incentive for capital to leave the developing economy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(developing_economy_capital_flight, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
