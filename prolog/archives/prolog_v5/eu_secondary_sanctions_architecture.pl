% ============================================================================
% CONSTRAINT STORY: eu_secondary_sanctions_architecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_secondary_sanctions_architecture, []).

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
 *   constraint_id: eu_secondary_sanctions_architecture
 *   human_readable: EU Secondary Sanctions Architecture
 *   domain: geopolitical_economics/sanctions_regime
 *
 * SUMMARY:
 *   The EU secondary sanctions architecture represents an institutional
 *   mechanism designed to extend sanctions enforcement beyond the EU's direct
 *   territorial and regulatory scope to third-party financial institutions,
 *   intermediaries, and traders globally. The architecture coordinates
 *   multilateral pressure against target states (principally Russia following
 *   2022 Ukraine invasion, and Iran under longstanding regimes) by
 *   threatening asset-blocking, transaction prohibition, and regulatory
 *   penalties against any institution that facilitates sanctioned
 *   transactions. This creates a structural tension between the genuine
 *   coordination benefit (multilateral enforcement of EU geopolitical
 *   preferences) and the asymmetric extraction from multiple victim groups:
 *   target-state economies bear forced economic reorganization; EU commercial
 *   sectors lose export markets; third-party intermediaries face regulatory
 *   uncertainty and compliance costs; and the entire international financial
 *   system internalizes a new layer of political risk. The constraint
 *   exhibits genuine Tangled Rope characteristics: a real coordination
 *   function (multilateral deterrence, enforcement scale) layered atop
 *   extractive mechanisms (asymmetric cost distribution, unilateral
 *   rule-setting by the EU, no exit for target states). The theater ratio
 *   reflects the performative invocation of 'rules-based international order'
 *   to justify what is fundamentally a unilateral enforcement capability.
 *
 * KEY AGENTS:
 *   - European Union Institutional Framework: Primary beneficiary (institutional/arbitrage) — consolidates geopolitical deterrence capacity and enforces policy preferences at scale; controls rule-setting and enforcement discretion
 *   - Target State Economy: Primary victim (powerless/trapped) — bears forced economic reorganization, financial exclusion, supply chain disruption with no viable exit short of geopolitical capitulation
 *   - Third-Party Intermediaries: Secondary victim (powerful/constrained) — face regulatory threat and compliance costs; retain arbitrage capacity within legal bounds but cannot safely ignore sanctions
 *   - EU Commercial Sector: Secondary victim (moderate/constrained) — loses export markets and supply access; constrained by regime participation but can relocate or diversify partnerships
 *   - Compliant Financial Infrastructure: Beneficiary (organized/mobile) — gains coordination benefit through transparent rules; develops compliance infrastructure as profitable service line
 *   - International Rules-Based Order Narrative: Institutional theatre (institutional/arbitrage) — maintains performative justification for unilateral enforcement; theater persists through rhetorical maintenance
 *   - Analytical Observer: Geopolitical analyst (analytical/analytical) — observes hybrid coordination-extraction structure; notes contradiction between 'rules-based' rhetoric and unilateral enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_secondary_sanctions_architecture, 0.58).
domain_priors:suppression_score(eu_secondary_sanctions_architecture, 0.72).
domain_priors:theater_ratio(eu_secondary_sanctions_architecture, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_secondary_sanctions_architecture, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_secondary_sanctions_architecture, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(eu_secondary_sanctions_architecture, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_secondary_sanctions_architecture, tangled_rope).
narrative_ontology:human_readable(eu_secondary_sanctions_architecture, "EU Secondary Sanctions Architecture").
narrative_ontology:topic_domain(eu_secondary_sanctions_architecture, "geopolitical_economics/sanctions_regime").

domain_priors:requires_active_enforcement(eu_secondary_sanctions_architecture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_secondary_sanctions_architecture, european_union_institutional).
narrative_ontology:constraint_beneficiary(eu_secondary_sanctions_architecture, compliant_financial_institutions).
narrative_ontology:constraint_victim(eu_secondary_sanctions_architecture, target_state_economy).
narrative_ontology:constraint_victim(eu_secondary_sanctions_architecture, third_party_intermediaries).
narrative_ontology:constraint_victim(eu_secondary_sanctions_architecture, eu_commercial_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGET STATE ECONOMY (SNARE) — Trapped by extraterritorial enforcement; no exit from the sanctions regime short of geopolitical capitulation or structural economic transformation. Bears maximum extraction through financial exclusion, supply chain disruption, and forced economic reorganization. No alternatives exist within the constraint's operative time horizon.
constraint_indexing:constraint_classification(eu_secondary_sanctions_architecture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THIRD-PARTY INTERMEDIARIES (TANGLED ROPE) — Constrained by the threat of secondary sanctions and asset-blocking, but retain genuine coordination benefit through the legal compliance framework: knowing which transactions incur penalty enables selective arbitrage. High suppression (regulatory risk, asset freezes) but also real coordination function (transparent rules enable market participation within bounds). Asymmetric extraction flows toward those who violate the architecture; those who comply benefit from predictability.
constraint_indexing:constraint_classification(eu_secondary_sanctions_architecture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EU INSTITUTIONAL FRAMEWORK (ROPE) — Experiences secondary sanctions as pure coordination mechanism: multilateral enforcement through asset-blocking, transaction prohibition, and regulatory pressure. Benefits from the architecture by consolidating geopolitical power and deterrence capacity. Exit options available through diplomatic off-ramps or policy revision. Low experienced extraction — the mechanism serves the EU's coordination interests directly.
constraint_indexing:constraint_classification(eu_secondary_sanctions_architecture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EU COMMERCIAL SECTOR (TANGLED ROPE) — Bears significant extraction through export market foreclosure and supply chain reorientation, but also experiences coordination benefit through state backing, unified market access, and competitive advantage against non-EU firms (who face secondary sanctions). Constrained exit: firms cannot opt out of the sanctions regime, but can relocate production or partnerships. Mixed experience: extraction from lost sales, coordination from state protection.
constraint_indexing:constraint_classification(eu_secondary_sanctions_architecture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPLIANT FINANCIAL INFRASTRUCTURE (ROPE) — Experiences secondary sanctions as pure coordination: transparent enforcement rules enable institutions to price regulatory risk, conduct sanctions screening, and participate in arbitrage opportunities (premium pricing for sanctioned-adjacent transactions). Organized actors (major banks, compliance vendors) have mobile exit options (operate in different markets, adjust compliance posture). Net beneficiary from the architecture's scale — compliance becomes a competitive advantage.
constraint_indexing:constraint_classification(eu_secondary_sanctions_architecture, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL RULES-BASED ORDER NARRATIVE (PITON) — Secondary sanctions are justified within a framing of multilateral law, humanitarian protection, and defense of democratic norms. This narrative has high theater_ratio: the performative invocation of 'rules-based order' masks the unilateral enforcement capacity that enables the secondary sanctions architecture. The mechanism persists through institutional inertia and rhetorical maintenance despite the functional contradiction between 'rules-based' (negotiated, consensual) and secondary sanctions (unilateral, extraterritorial). Theater ratio elevated by the gap between stated justification and structural enforcement.
constraint_indexing:constraint_classification(eu_secondary_sanctions_architecture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational realist perspective, secondary sanctions coordinate enforcement against target states while extracting compliance rent (financial institution fees, compliance infrastructure costs, market distortion premiums) from all participants. The architecture has genuine coordination function (multilateral pressure) AND asymmetric extraction (concentrated power, externalized costs). The beneficiary (EU institutional capacity, Western geopolitical dominance) and victim (target state, intermediate traders, excluded commercial sectors) are structurally asymmetric. Tan tangled rope classification reflects the hybrid nature and the constraint's genuine dual function.
constraint_indexing:constraint_classification(eu_secondary_sanctions_architecture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_secondary_sanctions_architecture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_secondary_sanctions_architecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_secondary_sanctions_architecture, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_secondary_sanctions_architecture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_secondary_sanctions_architecture, TR),
    TR >= 0.70.

:- end_tests(eu_secondary_sanctions_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the asymmetric cost distribution across victim groups. The EU captures deterrence benefit without bearing proportional costs; target states bear maximum costs (financial exclusion, supply disruption); third parties bear compliance costs with no offsetting benefits; EU commercial sectors lose markets. The value increased over the 9-year measurement interval (0.35 → 0.58) as secondary sanctions scope expanded (from narrow Russian oligarch sanctions to broad Russian financial system isolation to sectoral trade restrictions) and enforcement intensity increased (asset freezes, transaction blocking, regulatory fines). The trajectory reflects escalation of the extractive mechanism without corresponding expansion of coordination benefit proportionality. Suppression (0.72): High. Target states cannot exit through normal market mechanisms; third-party intermediaries face regulatory threat for any engagement; EU commercial actors are bound by regime rules they did not negotiate. The suppression operates through multiple channels: legal prohibition, financial infrastructure choking (transaction blocking), regulatory threat (fines against intermediaries), and reputational damage. Theater ratio (0.58): Moderate-high. The 'rules-based international order' framing of secondary sanctions invokes multilateral law and institutional legitimacy, but the structural reality is unilateral enforcement by the EU using financial system dominance. The performative content increased over time (0.42 → 0.58) as the gap between 'rules-based' rhetoric and the actual unilateral enforcement became more apparent to non-Western observers. The theater reflects the narrative work required to maintain legitimacy for a fundamentally hegemonic mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark and instructive. The EU institutional perspective sees Rope (pure coordination of multilateral deterrence). The compliant financial infrastructure sees Rope (transparent rules enabling profitable compliance services). The target state sees Snare (pure extraction with no exit). Third-party intermediaries see Tangled Rope (mixed extraction through regulatory threat and coordination benefit through transparent rules). EU commercial sectors see Tangled Rope (mixed extraction from market loss and coordination benefit from state backing). The piton perspective reveals the performative 'rules-based order' narrative serving to legitimize unilateral enforcement. The analytical observer sees the full Tangled Rope structure: genuine coordination function (multilateral enforcement) layered with asymmetric extraction (concentrated benefit, distributed costs, unilateral rule-setting). The gap reflects how the beneficiaries experience the constraint as coordination while the victims experience it as extraction — the same structural mechanism produces opposite experienced effects depending on position.
 *
 * DIRECTIONALITY LOGIC:
 *   The EU institutional framework experiences the secondary sanctions architecture as coordination because it controls rule-setting and benefits from enforcement at scale (d ≈ 0.05, strong beneficiary). Target states experience it as maximum extraction because they have no exit and bear forced economic costs (d ≈ 0.95, trapped victim). Third-party intermediaries navigate suppression (regulatory threat of asset-blocking) but retain arbitrage options within legal bounds (d ≈ 0.70, constrained victim with some agency). EU commercial sectors are bound by regime participation but can relocate or seek subsidies (d ≈ 0.60, moderate victim with constrained exit). Compliant financial institutions profit from compliance infrastructure and pricing power (d ≈ 0.15, beneficiary with organized exit). These directionality values feed the sigmoid f(d) function: EU institutional actors get low f(d) reflecting their beneficiary position; target states get high f(d) reflecting trapped victimhood; moderate and constrained actors get intermediate f(d) values. The analytical observer spans the range, seeing how the same architectural mechanism produces different experienced extractiveness depending on agent position.
 *
 * MANDATROPHY ANALYSIS:
 *   The secondary sanctions architecture resolves the mandatrophy by demonstrating that Tangled Rope is the correct classification at the analytical level, and the single-type perspectives (Rope from beneficiaries, Snare from powerless targets) represent partial views from positions within the structure. The beneficiary (EU) sees Rope because they benefit from and control the coordination mechanism. The powerless target sees Snare because they experience maximum extraction with no exit. The analytical observer, seeing the full structure — genuine multilateral coordination layered with asymmetric extraction, unilateral rule-setting by the beneficiary, no exit for the victim — correctly classifies as Tangled Rope. The mandatrophy is resolved by recognizing that beneficiary and victim perspectives are not 'wrong' but rather position-dependent readings of a hybrid constraint. The danger of single-type classification: calling it pure Rope (rules-based order) naturalizes the unilateral enforcement as legitimate coordination; calling it pure Snare (hegemon extraction) misses the genuine deterrence coordination benefit. The Tangled Rope classification captures the structural reality: the architecture genuinely coordinates multilateral pressure while distributing extraction asymmetrically. This is not a pathology — it is the intended design. The analytical edge is in refusing to naturalize the coordination or invisibilize the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secondary_sanctions_effectiveness_threshold,
    'At what extraction level do secondary sanctions become counterproductive (triggering counter-sanctions, dedollarization, alliance dissolution)?',
    'Comparative analysis of historical sanctions regimes; measurement of target state economic resilience, counter-sanctions declarations, and alternative payment infrastructure adoption rates',
    'If threshold < current extraction: secondary sanctions are driving target-state isolation deeper into alternative networks (CIPS, SPFS, barter). If threshold > current: mechanism remains sustainable. Affects classification timeline — if unsustainable, the constraint devolves from Tangled Rope to Snare (pure extraction with no coordination survival value).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_sanctions_effectiveness_threshold, empirical, 'Threshold at which secondary sanctions trigger counterproductive systemic responses').

omega_variable(
    rules_based_order_legitimacy_gap,
    'Is the ''rules-based international order'' framing for secondary sanctions credible, or does the unilateral enforcement capacity reveal it as hegemonic theater?',
    'Analysis of non-Western state responses; examination of BRICS++ infrastructure development, dedollarization declarations, and alternative dispute-resolution institution formation as proxy for perception of legitimacy',
    'If legitimacy is credible: piton perspective is wrong, and the narrative serves real coordination function (Rope upgrade for the ''rules-based order''). If theater: piton classification confirmed, and the secondary sanctions architecture is maintained by enforcement threat rather than normative consensus. Affects long-term sustainability and non-Western bloc defection probability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rules_based_order_legitimacy_gap, conceptual, 'Whether rules-based order framing reflects genuine legitimacy or hegemonic performance').

omega_variable(
    eu_commercial_sector_resilience_asymmetry,
    'Do EU commercial sectors most dependent on target-state markets (energy services, component manufacturing, luxury goods) actually bear proportionally more extraction than less-dependent sectors, or is the extraction distributed by firm size/lobbying power rather than market exposure?',
    'Sectoral economic impact analysis; cross-reference export loss data, subsidy distribution, and government support packages by firm size and industry. Measure whether small/medium enterprises in dependent sectors receive proportional support vs. large exporters in unaffected sectors.',
    'If distributed by market exposure: EU commercial sector perspective is accurate (high extraction for import-dependent firms, low for others). If distributed by firm size/political power: the constraint is more regressive than the moderate classification suggests, and vulnerable SMEs experience Snare-level extraction while large firms experience Rope-level benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eu_commercial_sector_resilience_asymmetry, empirical, 'Whether sanctions extraction is distributed by sectoral exposure or political/economic power').

omega_variable(
    third_party_actor_compliance_sustainability,
    'Can third-party intermediaries (Asian banks, Middle Eastern traders, African commodity brokers) sustain a stable arbitrage equilibrium within secondary sanctions threat, or does the enforcement threat accumulate such that compliance becomes involuntary (trapped rather than constrained)?',
    'Tracking of secondary sanctions enforcement actions against third parties; measurement of compliance cost trajectory (regulatory fines, reputational damage, capital requirements); interviews and banking data revealing whether actors feel they have viable exit options',
    'If equilibrium is stable: third-party perspective remains Tangled Rope (mixed extraction and coordination). If compliance becomes forced (threatened institutions abandon sanctioned-trade activity entirely): classification degrades to Snare for that agent cohort, suppression rises, and the ''secondary'' sanctions become primary for non-EU enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_actor_compliance_sustainability, empirical, 'Whether third-party actors maintain stable arbitrage or face forced compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_secondary_sanctions_architecture, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(euss_tr_t0, eu_secondary_sanctions_architecture, theater_ratio, 0, 0.42).
narrative_ontology:measurement(euss_tr_t3, eu_secondary_sanctions_architecture, theater_ratio, 3, 0.48).
narrative_ontology:measurement(euss_tr_t6, eu_secondary_sanctions_architecture, theater_ratio, 6, 0.55).
narrative_ontology:measurement(euss_tr_t9, eu_secondary_sanctions_architecture, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(euss_be_t0, eu_secondary_sanctions_architecture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(euss_be_t3, eu_secondary_sanctions_architecture, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(euss_be_t6, eu_secondary_sanctions_architecture, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(euss_be_t9, eu_secondary_sanctions_architecture, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_secondary_sanctions_architecture, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_secondary_sanctions_architecture, 0.18).
narrative_ontology:affects_constraint(eu_secondary_sanctions_architecture, dollar_hegemony_infrastructure).
narrative_ontology:affects_constraint(eu_secondary_sanctions_architecture, swift_network_centralization).
narrative_ontology:affects_constraint(eu_secondary_sanctions_architecture, dedollarization_alternative_clearing).

% DUAL FORMULATION NOTE:
% Secondary sanctions architecture is downstream of broader dollar-hegemony and SWIFT centralization constraints. The secondary sanctions mechanism leverages the structural properties of the global financial infrastructure (dollar dominance, SWIFT routing, US-EU enforcement coordination) to enforce geopolitical preferences. If the upstream constraints (dollar_hegemony, SWIFT_centralization) degrade due to dedollarization or alternative clearing systems, the secondary sanctions architecture's extractive capacity declines — the architecture depends on structural financial dominance that is not unconditional. Each upstream constraint has its own ε value reflecting its own stability; downstream sanctions mechanism's extractiveness is contingent on upstream infrastructure durability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_secondary_sanctions_architecture, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
