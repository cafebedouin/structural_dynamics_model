% ============================================================================
% CONSTRAINT STORY: canadian_equalization_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_canadian_equalization_regime, []).

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
 *   constraint_id: canadian_equalization_regime
 *   human_readable: Canadian Equalization Regime
 *   domain: fiscal_federalism/political_economy
 *
 * SUMMARY:
 *   The Canadian equalization regime is a federal fiscal transfer mechanism
 *   designed to ensure that recipient provinces can deliver public services
 *   comparable to national standards despite unequal tax-raising capacity.
 *   Established under Section 36 of the Constitution Act 1982, equalization
 *   transfers approximately $20-24 billion annually from federal revenues to
 *   recipient provinces (Quebec, Atlantic provinces, and occasionally
 *   Manitoba and Saskatchewan). The regime is simultaneously a coordination
 *   mechanism solving the collective action problem of maintaining
 *   pan-Canadian public service standards and an extraction mechanism
 *   concentrating fiscal capacity in resource-rich provinces while penalizing
 *   provincial governments for resource development. The constraint exhibits
 *   all six DR types depending on observer position: trapped donor provinces
 *   see snare, recipient provinces see mixed coordination-extraction (tangled
 *   rope), the federal system sees coordination (rope), the technical formula
 *   apparatus sees institutionalized theater (piton), reform coalitions see a
 *   solvable problem with indefinite sunset (scaffold), and the analytical
 *   observer risks naturalizing a contingent arrangement as inevitable (false
 *   mountain). The regime's theater ratio has increased from 0.32 in 1982 to
 *   0.58 in 2024, reflecting the gap between the formula's mathematical
 *   complexity (16+ iterations, multi-component calculations) and the
 *   functional problem it solves (straightforward interprovincial
 *   redistribution). Extractiveness has increased from 0.35 to 0.52 over the
 *   same period, driven by larger resource wealth disparities between donor
 *   and recipient provinces and increasing sensitivity of the formula to
 *   commodity price cycles.
 *
 * KEY AGENTS:
 *   - Donor Provinces (Alberta, Saskatchewan, British Columbia): Primary victims (powerless/trapped) — structurally extract through higher equalization obligations as resource revenues increase; have no constitutional exit option.
 *   - Recipient Provinces (Quebec, Atlantic Canada): Primary beneficiaries with constrained dependency (moderate/constrained) — receive equalization funds enabling public services but face fiscal dependency on federal formula; experience mixed coordination and extraction.
 *   - Federal Treasury: Institutional beneficiary (institutional/arbitrage) — controls formula calibration and recalibration; has discretion in implementation; benefits from federal-provincial fiscal coordination.
 *   - Federal Parliament & Cabinet: Organized agents (organized/arbitrage) — make periodic formula adjustments; have strategic discretion over revision timing and magnitude.
 *   - Provincial Finance Ministers: Organized agents (organized/constrained) — recognize extraction asymmetry but face constitutional and political constraints on reform; form intermittent coalitions for formula changes.
 *   - Intergovernmental Affairs Bureaucracies: Institutional actors (institutional/constrained) — administer the formula; perceive their function as theatrical calibration rather than substantive redistribution.
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing federalism's inherent redistribution need as justification for this specific regime's unchangeability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canadian_equalization_regime, 0.52).
domain_priors:suppression_score(canadian_equalization_regime, 0.48).
domain_priors:theater_ratio(canadian_equalization_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canadian_equalization_regime, extractiveness, 0.52).
narrative_ontology:constraint_metric(canadian_equalization_regime, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(canadian_equalization_regime, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(canadian_equalization_regime, tangled_rope).
narrative_ontology:human_readable(canadian_equalization_regime, "Canadian Equalization Regime").
narrative_ontology:topic_domain(canadian_equalization_regime, "fiscal_federalism/political_economy").

domain_priors:requires_active_enforcement(canadian_equalization_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(canadian_equalization_regime, recipient_provinces).
narrative_ontology:constraint_beneficiary(canadian_equalization_regime, federal_treasury_administrative_apparatus).
narrative_ontology:constraint_victim(canadian_equalization_regime, donor_provinces).
narrative_ontology:constraint_victim(canadian_equalization_regime, regional_economic_asymmetry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DONOR PROVINCE (SNARE) — Alberta, Saskatchewan, and British Columbia face structural fiscal extraction through equalization payments with no exit option. These provinces generate resource wealth but cannot retain proportional fiscal capacity. Increasing resource revenues trigger higher equalization obligations, creating perverse disincentive to maximize production. The trap is constitutional: equalization is entrenched in the Constitution Act 1982 and cannot be unilaterally exited. Suppression is high — the mechanism is embedded in federal-provincial fiscal architecture with no alternative path to resource development.
constraint_indexing:constraint_classification(canadian_equalization_regime, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RECIPIENT PROVINCE (TANGLED ROPE) — Quebec and Atlantic provinces experience the regime as both coordination mechanism and extraction. Equalization funds provincial public services (coordination benefit) while creating fiscal dependency on federal formulas and demographic assumptions (extraction dynamic). The provinces have constrained exit — they could theoretically reject transfers, but doing so would eliminate healthcare and education funding. They also benefit from the coordination function: equalization enables national standards for public services across disparate fiscal capacities. This is genuine mixed coordination and asymmetric extraction.
constraint_indexing:constraint_classification(canadian_equalization_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL TREASURY (ROPE) — From the federal institutional perspective, equalization is coordination: it solves the collective action problem of maintaining public service standards across provinces with unequal fiscal capacity. The federal system benefits from arbitrage — it can redefine the formula, adjust the denominator, or recalibrate the recipient pool depending on fiscal conditions. The federal government has discretion in implementation (e.g., whether to include particular tax bases) and can adjust the formula periodically. This is relatively symmetrical coordination from this perspective.
constraint_indexing:constraint_classification(canadian_equalization_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EQUALIZATION FORMULA APPARATUS (PITON) — The technical equalization formula has become substantially theatrical. The formula changes frequently (16+ iterations since 1982), each change generating political conflict and recalibration disputes. The formula's mathematical complexity (5-year rolling averages, 33-point standard, capability-to-raise-revenues assessment, net fiscal benefit calculations) far exceeds what is necessary to achieve the stated coordination goal. The formula persists through institutional inertia — changing it is politically costly, so adjustments are marginal rather than structural. Theater ratio (0.58) reflects the gap between the formula's complexity (performative calibration) and its actual equalization function (straightforward redistribution would achieve similar public service coordination with lower overhead).
constraint_indexing:constraint_classification(canadian_equalization_regime, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERGOVERNMENTAL REFORM COALITION (SCAFFOLD) — Provincial finance ministers, fiscal policy organizations, and parliamentary committees see equalization as a temporary coordination problem awaiting structural reform. Proposed alternatives (resource revenue-sharing, per-capita transfers, or decentralized tax-raising) represent genuine exit pathways that would reduce extraction asymmetry. The coalition is organized and has policy agency. However, the sunset is indefinite — reform requires constitutional amendment or federal-provincial consensus, both of which face supermajority requirements. The constraint persists not because the coalition lacks agency but because the institutional path to change is blocked. Treating this as scaffold reflects the recognition that alternatives exist but are institutionally constrained.
constraint_indexing:constraint_classification(canadian_equalization_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: RESOURCE-RICH PROVINCE (TANGLED ROPE) — Alberta and Saskatchewan have higher power than trapped provinces because of their resource wealth and economic leverage. They experience equalization as mixed extraction and coordination. The coordination function: equalization prevents interprovincial fiscal wars and maintains pan-Canadian public service standards. The extraction: resource-rich provinces subsidize the entire regime while facing formula penalties for revenue growth. These provinces have constrained arbitrage — they cannot exit (constitutional constraint) but have strategic negotiating position (federal dependence on resource tax revenue, electoral significance). Their extraction is high but not total because of their power asymmetry — powerful agents experience less extraction than trapped ones.
constraint_indexing:constraint_classification(canadian_equalization_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, some form of interprovincial fiscal redistribution appears inherent to federalism itself. Disparities in fiscal capacity across federated regions are a structural feature of federal systems. Maintaining common public service standards requires some transfer mechanism. This perspective risks naturalizing the equalization regime's specific form as inevitable when it is actually a contingent institutional arrangement. The mountain classification is a false summit — the analytical observer is conflating 'some redistribution is necessary' with 'this specific regime is unchangeable.' The structural data shows the regime is highly malleable (16+ formula changes, ongoing reform proposals), revealing the naturalization as ideological rather than analytical.
constraint_indexing:constraint_classification(canadian_equalization_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canadian_equalization_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(canadian_equalization_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canadian_equalization_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(canadian_equalization_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(canadian_equalization_regime, TR),
    TR >= 0.70.

:- end_tests(canadian_equalization_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The regime creates measurable fiscal asymmetry favoring recipient provinces while penalizing resource-rich provinces for revenue growth. However, extraction is not maximal because: (1) the coordination function is genuine — equalization does solve the public service parity problem, (2) recipient provinces face dependency that limits their benefit, and (3) periodic formula adjustments create uncertainty but also reform opportunity. The value reflects genuine mixed extraction-coordination. Suppression (0.48): Moderate. Donor provinces face constitutional suppression (no unilateral exit from equalization) but have federal negotiating leverage (electoral significance, resource tax revenue dependence). Recipient provinces face dependency-based suppression but also have federal political leverage (Quebec's electoral weight). Suppression is not totalizing because powerful agents can constrain it through negotiation. Theater ratio (0.58): Moderate-high. The equalization formula has become substantially performative. Sixteen formula iterations since 1982 generate political conflict and technical recalibration without substantively changing the redistribution function. The formula's complexity (5-year rolling averages, 33-point standard, capability-to-raise-revenues calculations, net fiscal benefit assessments) far exceeds what is necessary for straightforward provincial redistribution. The theater persists because formula changes are politically safer than structural reform (avoids constitutional amendment requirement). The increasing theater ratio over time reflects formula proliferation without functional change — the apparatus has grown more ornate while the underlying coordination goal remains constant.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The donor province sees snare (trapped extraction with constitutional suppression and no exit). The recipient province sees tangled rope (genuine public service coordination function combined with fiscal dependency and extraction). The federal system sees rope (coordinate public service parity across unequal provinces). The formula apparatus sees piton (theatrical recalibration divorced from functional reform). The reform coalition sees scaffold (solvable problem awaiting structural change). The analytical observer risks mountain (naturalizing federalism's inherent redistribution as unchangeable). The perspectival gap reflects genuine structural difference: donor provinces and recipients occupy asymmetric positions in the extraction flow, with different power levels and exit capacities. The gap is not perspectival bias but accurate measurement of different structural realities from different agent positions. Resolution requires recognizing that all perspectives are analytically valid from their respective structural positions — the regime is simultaneously snare, rope, and piton depending on which agent's experience one measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position in the extraction flow. Donor provinces (powerless/trapped) have d ≈ 0.95 — maximum targeting with no exit capacity. Recipient provinces with constrained exit have d ≈ 0.60-0.75 depending on revenue dependence magnitude. The federal treasury (institutional/arbitrage) has d ≈ 0.05 — full beneficiary with exit capacity (formula adjustment, transfer redirection). Resource-rich provinces with powerful status have d ≈ 0.45-0.55 — targeted but with negotiating leverage. The derived d values reflect that extraction concentration on powerless agents (donor provinces without exit) produces higher effective extractiveness than extraction on powerful agents (resource provinces with federal leverage). The beneficiary/victim declarations map to real structural roles: recipient provinces and federal apparatus are beneficiaries (they receive transfer flows and gain coordination benefits); donor provinces and the property of regional economic parity are victims (they bear fiscal costs and face disincentive to resource development).
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL COMPLEXITY WITHOUT MANDATROPHY: The Canadian equalization regime does not present a mandatrophy (ambiguity between coordination and pure extraction) because the tangled rope classification clearly captures the regime's hybrid nature. Both the coordination function (maintaining public service parity) and the extraction mechanism (redistributing wealth from donor to recipient provinces, penalizing resource development) are structurally present and measurable. The mandatrophy resolution would require showing that one function (coordination or extraction) is epiphenomenal — i.e., that the regime's stated coordination goal is pure cover for extraction, or that the extraction is incidental to genuine coordination. The empirical record contradicts both: (1) recipient provinces genuinely need equalization to maintain public services at comparable levels (coordination is real), and (2) donor provinces genuinely face fiscal penalties for resource development under the formula (extraction is real). The regime is not a snare disguised as coordination or coordination disguised as extraction — it is genuinely both. The mandatrophy is resolved by accepting the tangled rope classification as accurate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_threshold,
    'At what ratio of transfer magnitude to recipient province GDP does equalization transition from coordination to pure extraction?',
    'Comparative analysis of recipient provinces'' public service quality as function of equalization dependence; measurement of fiscal autonomy degradation relative to transfer magnitude',
    'Low threshold (<15% of provincial revenue): more provinces classified as snares. High threshold (>30%): more provinces retain rope classification. Current range for Atlantic Canada: 35-40% of provincial revenue from equalization, suggesting snare classification for some recipients despite the coordinated public service benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_threshold, empirical, 'Threshold ratio of equalization to provincial GDP at which regime transitions from coordination to extraction').

omega_variable(
    formula_reform_implementability,
    'Are proposed alternatives to the current formula (per-capita transfers, resource revenue-sharing, decentralized tax-raising) structurally feasible within the existing constitutional framework, or do they require constitutional amendment?',
    'Legal analysis of federal authority under Constitution Act 1867 Section 36; review of prior reform attempts and their constitutional barriers',
    'If feasible without amendment: scaffold sunset is structural, not indefinite. If amendment required: sunset is indefinite, and piton classification is more apt than scaffold for reform mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formula_reform_implementability, conceptual, 'Whether equalization formula reform requires constitutional amendment').

omega_variable(
    resource_revenue_correlation_mechanism,
    'Is the extraction mechanism in resource-rich provinces primarily driven by the formula''s direct penalization of resource revenues (capability-to-raise-revenues component), or by deeper incentive misalignment in federalism itself (federal-provincial conflict over resource development)?',
    'Counterfactual analysis: simulate equalization regime with capability-to-raise-revenues component removed; measure resulting provincial resource development decisions and federal-provincial conflict intensity',
    'If formula-driven: targeted formula reform could reduce extraction asymmetry. If incentive-driven: structural fiscal federalism redesign needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_revenue_correlation_mechanism, empirical, 'Whether resource revenue extraction is formula-specific or inherent to federalism incentives').

omega_variable(
    inter_provincial_solidarity_dependence,
    'How much of equalization''s persistence relies on inter-provincial solidarity norms (recognition that all provinces face fiscal shocks) versus federal enforcement mechanisms?',
    'Analysis of provincial opt-out requests and federal response; comparison of equalization to other federal transfers where provinces have stronger opt-out rights (e.g., healthcare transfers)',
    'If solidarity-dependent: constraint is more rope-like than snare-like; donor provinces perceive fairness in the mechanism despite extraction. If enforcement-dependent: constraint is more snare-like; extraction is maintained through federal power, not consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_provincial_solidarity_dependence, empirical, 'Role of inter-provincial solidarity norms versus federal enforcement in equalization persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(canadian_equalization_regime, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(can_eq_tr_t1982, canadian_equalization_regime, theater_ratio, 1982, 0.32).
narrative_ontology:measurement(can_eq_tr_t2000, canadian_equalization_regime, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(can_eq_tr_t2015, canadian_equalization_regime, theater_ratio, 2015, 0.58).
narrative_ontology:measurement(can_eq_tr_t2024, canadian_equalization_regime, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(can_eq_be_t1982, canadian_equalization_regime, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement(can_eq_be_t2000, canadian_equalization_regime, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(can_eq_be_t2015, canadian_equalization_regime, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(can_eq_be_t2024, canadian_equalization_regime, base_extractiveness, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(canadian_equalization_regime, resource_allocation).
narrative_ontology:affects_constraint(canadian_equalization_regime, canadian_provincial_debt_dynamics).
narrative_ontology:affects_constraint(canadian_equalization_regime, interprovincial_trade_regulatory_barriers).

% DUAL FORMULATION NOTE:
% Equalization is one component of Canadian federal fiscal architecture. The regime affects provincial fiscal capacity for other policies (debt management, trade regulation), which in turn generate their own constraints. Equalization is upstream to these downstream constraints because changes to the equalization formula directly affect recipient and donor provincial fiscal capacity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(canadian_equalization_regime, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
