% ============================================================================
% CONSTRAINT STORY: swift_correspondent_banking_restrictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_swift_correspondent_banking_restrictions, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: swift_correspondent_banking_restrictions
 *   human_readable: SWIFT Correspondent Banking Restrictions and Financial Access Asymmetry
 *   domain: financial_systems/geopolitical
 *
 * SUMMARY:
 *   SWIFT correspondent banking restrictions represent a structural
 *   constraint where genuine coordination infrastructure (cross-border
 *   payment settlement) is weaponized as a geopolitical control mechanism.
 *   The constraint exhibits asymmetric extraction masked as prudent risk
 *   management. Sanctioned entities face capital controls and financial
 *   isolation; developing financial systems absorb de-risking compliance
 *   burdens; humanitarian organizations in restricted zones face operational
 *   paralysis; major correspondent banks capture net benefits through margin
 *   expansion and market consolidation; alternative infrastructure projects
 *   represent a genuine sunset mechanism; and compliance bureaucracy
 *   maintains increasingly performative sanctions screening. The constraint's
 *   extractiveness has increased from 0.38 to 0.62 over the measurement
 *   interval as secondary sanctions have widened and compliance conservatism
 *   has deepened, while theater ratio has risen from 0.32 to 0.45 as the gap
 *   between stated sanctions goals and actual effectiveness has widened.
 *
 * KEY AGENTS:
 *   - Sanctioned Entity: Primary victim (powerless/trapped) — subject to capital controls, SWIFT exclusion, asset freezes; no payment alternatives available
 *   - Humanitarian Organization: Secondary victim (powerless/trapped) — operating in restricted zones; faces de facto financial blockade despite exemption status
 *   - Developing Financial System: Secondary victim (organized/constrained) — bears disproportionate de-risking compliance burden relative to size; faces correspondent banking exclusion risk
 *   - Major Correspondent Bank: Primary beneficiary (institutional/arbitrage) — captures margin expansion through market consolidation; reduces counterparty risk exposure; maintains arbitrage to alternative systems
 *   - Sanctioning Jurisdiction: Beneficiary (institutional/arbitrage) — exercises geopolitical control through financial infrastructure; maintains institutional commitment to sanctions regime
 *   - Alternative Payment Infrastructure Coalition: Organized actor (organized/mobile) — CIPS, INSTEX, mBridge represent parallel settlement with explicit sunset logic
 *   - SWIFT Compliance Bureaucracy: Institutional actor (institutional/constrained) — maintains performative compliance theater; trapped by regulatory requirements despite degraded functional effectiveness
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees dual coordination-extraction function; resolves mandatrophy through indexical decomposition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(swift_correspondent_banking_restrictions, 0.62).
domain_priors:suppression_score(swift_correspondent_banking_restrictions, 0.68).
domain_priors:theater_ratio(swift_correspondent_banking_restrictions, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(swift_correspondent_banking_restrictions, extractiveness, 0.62).
narrative_ontology:constraint_metric(swift_correspondent_banking_restrictions, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(swift_correspondent_banking_restrictions, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(swift_correspondent_banking_restrictions, tangled_rope).
narrative_ontology:human_readable(swift_correspondent_banking_restrictions, "SWIFT Correspondent Banking Restrictions and Financial Access Asymmetry").
narrative_ontology:topic_domain(swift_correspondent_banking_restrictions, "financial_systems/geopolitical").

domain_priors:requires_active_enforcement(swift_correspondent_banking_restrictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(swift_correspondent_banking_restrictions, sanctioning_jurisdictions).
narrative_ontology:constraint_beneficiary(swift_correspondent_banking_restrictions, major_correspondent_banks).
narrative_ontology:constraint_victim(swift_correspondent_banking_restrictions, sanctioned_entities).
narrative_ontology:constraint_victim(swift_correspondent_banking_restrictions, developing_financial_systems).
narrative_ontology:constraint_victim(swift_correspondent_banking_restrictions, humanitarian_organizations_in_restricted_zones).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SANCTIONED ENTITY (SNARE) — Complete financial isolation via SWIFT exclusion and correspondent banking de-risking. No alternatives for international payment settlement. Maximum suppression: capital controls, asset freezes, and exclusion from dollar-denominated transactions are irreversible within the constraint's lifetime. Zero degrees of freedom.
constraint_indexing:constraint_classification(swift_correspondent_banking_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HUMANITARIAN ORGANIZATION (SNARE) — NGOs delivering medical aid, food, and emergency services in sanctioned regions face de facto financial blockade. Unable to process payroll, import supplies, or operate bank accounts. Trapped by association with geographic location, not by direct targeting. Full extraction: must choose between mission and solvency.
constraint_indexing:constraint_classification(swift_correspondent_banking_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPING FINANCIAL SYSTEM (TANGLED ROPE) — Correspondent banking access is essential coordination infrastructure for cross-border settlement, but de-risking creates asymmetric extraction. Smaller banks in developing economies face disproportionate compliance costs and exclusion risk relative to major global banks. Genuine coordination function (settlement) coexists with asymmetric extraction (compliance burden concentrated on weaker actors). Constrained exit: switching payment rails (e.g., CIPS, mBridge) requires geopolitical alignment and institutional investment.
constraint_indexing:constraint_classification(swift_correspondent_banking_restrictions, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR CORRESPONDENT BANK (ROPE) — Coordination function is primary: routing payments across borders, enabling settlement between banks without direct bilateral relationships. SWIFT restrictions enable profit extraction (de-risking reduces counterparty exposure, increasing net interest margins) alongside genuine coordination. Arbitrage option: can shift business to non-SWIFT systems or bilateral relationships if sanctions environment changes. Net beneficiary through risk reduction and market consolidation.
constraint_indexing:constraint_classification(swift_correspondent_banking_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE PAYMENT INFRASTRUCTURE (SCAFFOLD) — China's CIPS, EU's INSTEX, and multilateral initiatives (mBridge) represent parallel settlement pathways with explicit sunset logic: as alternative rails mature, the dominance of SWIFT-USD system becomes less binding. The constraint itself has built-in obsolescence: decentralization of payment settlement reduces the leverage of any single messaging system. Organized agents see the restriction as a temporary phase in the migration to multipolar financial infrastructure.
constraint_indexing:constraint_classification(swift_correspondent_banking_restrictions, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: SWIFT COMPLIANCE BUREAUCRACY (PITON) — The compliance infrastructure around SWIFT restrictions has become substantially performative: sanctions screening, sanctions-busting detection, and transaction monitoring consume massive resources but often fail to prevent determined actors (who use shell companies, cryptocurrency mixers, hawala networks). The ritual persists through institutional inertia despite degraded function. Banks maintain elaborate compliance theater because regulators demand it, not because it reliably achieves stated objectives. Theater ratio driven by compliance costs vastly exceeding effectiveness.
constraint_indexing:constraint_classification(swift_correspondent_banking_restrictions, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational scale, SWIFT restrictions serve dual functions: genuine coordination of cross-border settlement (enabling legitimate commerce) and asymmetric geopolitical extraction (weaponizing financial infrastructure). The constraint cannot be reduced to pure coordination (rope) because it explicitly targets and extracts from sanctioned entities and smaller financial systems. Cannot be reduced to pure extraction (snare) because it genuinely solves settlement coordination at scale. The analytical view resolves the mandatrophy: both functions are real, neither is hidden, and the indexical position determines which dominates the agent's experience.
constraint_indexing:constraint_classification(swift_correspondent_banking_restrictions, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(swift_correspondent_banking_restrictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(swift_correspondent_banking_restrictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(swift_correspondent_banking_restrictions, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(swift_correspondent_banking_restrictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(swift_correspondent_banking_restrictions, TR),
    TR >= 0.70.

:- end_tests(swift_correspondent_banking_restrictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The constraint extracts asymmetrically: sanctioned entities lose access entirely (near-total extraction), developing financial systems bear compliance costs disproportionate to size and benefit, humanitarian organizations face operational paralysis. However, extraction is not maximal (0.72+) because: (1) alternative systems (crypto, hawala, informal trade finance) provide partial workarounds at higher cost, (2) developed economies maintain SWIFT access for legitimate commerce, and (3) the constraint's sunset is structural (alternative infrastructure maturation reduces SWIFT dominance). Suppression (0.68): High. Correspondent banking de-risking creates severe barriers to payment access. Capital controls, asset freezes, and bank account closures remove mobility options. Suppression is high but not absolute (0.72+) because determined actors can use alternative channels, and humanitarian exemptions exist on paper (though ineffective in practice). Theater ratio (0.45): Moderate. The constraint maintains some performative compliance components (sanctions screening that often fails to prevent evasion), but the functional extraction mechanism is directly legible — SWIFT exclusion is a material barrier, not theatrical. The theater is lower than pure compliance bureaucracies because the restriction's enforcement is transparent.
 *
 * PERSPECTIVAL GAP:
 *   The sanctioned entity and humanitarian organization see a snare — irreversible financial isolation with maximum extraction and no perceived exit. The developing financial system sees tangled rope — must participate in correspondent banking (coordination function) but absorbs compliance burden asymmetry (extraction). The major correspondent bank sees rope — genuine coordination infrastructure enabling settlement, with net benefit through de-risking. The alternative infrastructure coalition sees scaffold — building parallel settlement pathways with explicit sunset for SWIFT dominance (civilizational time horizon). The SWIFT compliance bureaucracy sees piton — performative sanctions screening persisting through inertia despite degraded effectiveness. The analytical observer at civilizational scale sees the full tangled rope — coordination and extraction are both real, neither hidden, and the indexical position determines which dominates. The perspectival gap reveals that different agents experience the same constraint as ranging from pure extraction (snare) to pure coordination (rope) because they occupy fundamentally different structural positions relative to the restriction's enforcement mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position: power level, exit options, and relationship to the extraction flow. Sanctioned entities: d ≈ 0.95 (full target, trapped exit) → high f(d) ≈ 1.42 → high effective extraction χ despite moderate base ε. Humanitarian organizations: d ≈ 0.92 (full target, trapped exit) → high f(d) ≈ 1.35 → high χ. Developing financial systems: d ≈ 0.60 (mixed victim-participant, constrained exit) → moderate f(d) ≈ 0.75 → moderate χ below the maximum that trapped agents experience. Major correspondent banks: d ≈ 0.15 (beneficiary, arbitrage exit) → low f(d) ≈ -0.01 → negative effective extraction (constraint subsidizes this agent). Sanctioning jurisdiction: d ≈ 0.10 (beneficiary, arbitrage exit) → low f(d) ≈ -0.10 → strong institutional benefit through geopolitical leverage. Alternative infrastructure: d ≈ 0.50 (symmetric, mobile exit) → moderate f(d) ≈ 0.65 → moderate experienced extraction because the coalition has agency and sees an exit path. Compliance bureaucracy: d ≈ 0.45 (weak victim, constrained exit) → moderate f(d) ≈ 0.55 → moderate experienced extraction despite being institutional (trapped by regulatory requirement to maintain performative screening). Analytical observer: d ≈ 0.72 (neutral observer position) → high f(d) ≈ 1.15 → enables cross-perspectival analysis without privileging any single position.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by indexical decomposition. At immediate time horizons (major correspondent banks) and institutional power (compliance bureaucracy), the constraint appears as rope or scaffold — coordination mechanisms with side effects. At biographical time horizons (sanctioned entities, humanitarian organizations) and powerless power (trapped exit), the constraint is a snare — pure extraction without coordination benefit. At generational horizons (developing financial systems) and organized power (constrained exit), it appears as tangled rope — mixed coordination and extraction. At civilizational horizons (alternative infrastructure) and analytical power (mobile/analytical exit), it is a scaffold — temporary constraint being replaced by parallel infrastructure. No single type is 'correct' — the mandatrophy is resolved by recognizing that the constraint exhibits all types simultaneously across different observational positions. The apparent contradiction (coordination + extraction) is not a classification error but a structural reality: SWIFT correspondence banking genuinely solves cross-border settlement coordination while simultaneously weaponizing that infrastructure for geopolitical extraction. The analytical observer sees both functions as real, neither as hidden or secondary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secondary_sanctions_effectiveness,
    'Do SWIFT restrictions actually prevent sanctioned entities from accessing international financial services, or merely increase transaction costs and intermediation opacity?',
    'Empirical tracking of cross-border flows through alternative channels (crypto, hawala, informal trade finance); comparison of transaction costs and success rates pre- vs post-restriction',
    'If restrictions prevent access: classification remains snare (suppression high). If restrictions merely increase costs: reclassify target perspective as constrained (rather than trapped) and shift from snare to tangled_rope (extraction coexists with continued coordination, albeit at higher cost).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_sanctions_effectiveness, empirical, 'Whether SWIFT restrictions prevent access or merely increase costs').

omega_variable(
    compliance_burden_concentration,
    'Does de-risking concentration (large banks absorbing correspondent banking traffic from smaller banks) represent efficiency improvement or extraction mechanism leveraging compliance asymmetry?',
    'Analysis of spreads and margin changes for smaller banks pre- vs post-restriction; investigation of market concentration in correspondent relationships; cost allocation data',
    'If efficiency: tangled_rope classification is correct (coordination benefit exists despite extraction). If extraction: extractiveness increases toward snare as non-privileged financial systems face structural exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_burden_concentration, empirical, 'Whether compliance-driven concentration improves efficiency or concentrates extraction').

omega_variable(
    alternative_infrastructure_maturity,
    'Are CIPS, INSTEX, and mBridge genuinely viable alternatives to SWIFT-USD, or aspirational infrastructure projects that lack the institutional depth and scale to replace correspondent banking?',
    'Comparative transaction volume, latency, settlement finality, liquidity availability, institutional participation; longitudinal tracking of adoption rates and interoperability',
    'If viable: scaffold perspective confirmed — sunset is structural and real. If aspirational: scaffold is premature; restriction remains effectively permanent (snare dynamics intensify). Affects temporal classification and theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_infrastructure_maturity, empirical, 'Whether alternative payment infrastructure is functionally mature').

omega_variable(
    humanitarian_exemption_effectiveness,
    'Do humanitarian exemptions from sanctions actually enable NGO operations, or does compliance conservatism create de facto blockade despite formal exceptions?',
    'Tracking of actual disbursements through humanitarian channels vs stated capacity; documentation of bank rejections citing sanctions ambiguity despite exemption status; comparison of humanitarian organization operational capacity pre- vs post-restriction',
    'If effective: humanitarian victims perspective shifts from snare to constrained (high-cost exit exists). If ineffective: snare classification is understated; theater_ratio increases as exemptions become performative theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_exemption_effectiveness, empirical, 'Whether humanitarian exemptions enable actual operations or create performative theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(swift_correspondent_banking_restrictions, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swift_tr_t0, swift_correspondent_banking_restrictions, theater_ratio, 0, 0.32).
narrative_ontology:measurement(swift_tr_t3, swift_correspondent_banking_restrictions, theater_ratio, 3, 0.38).
narrative_ontology:measurement(swift_tr_t6, swift_correspondent_banking_restrictions, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(swift_be_t0, swift_correspondent_banking_restrictions, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(swift_be_t3, swift_correspondent_banking_restrictions, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(swift_be_t6, swift_correspondent_banking_restrictions, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(swift_correspondent_banking_restrictions, global_infrastructure).
narrative_ontology:boltzmann_floor_override(swift_correspondent_banking_restrictions, 0.18).
narrative_ontology:affects_constraint(swift_correspondent_banking_restrictions, currency_hegemony_dollar_system).
narrative_ontology:affects_constraint(swift_correspondent_banking_restrictions, international_sanctions_regime).
narrative_ontology:affects_constraint(swift_correspondent_banking_restrictions, developing_economy_financial_exclusion).

% DUAL FORMULATION NOTE:
% SWIFT restrictions are downstream of the international sanctions regime and currency hegemony (dollar dominance in global settlement). The sanctions regime constraint has its own ε reflecting the political-legal machinery that imposes restrictions; SWIFT restrictions represent the financial mechanism that enforces those political decisions. Alternative infrastructure maturation (CIPS, mBridge, INSTEX) represents a distinct constraint family member addressing whether parallel settlement systems can meaningfully reduce SWIFT's structural dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(swift_correspondent_banking_restrictions, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
