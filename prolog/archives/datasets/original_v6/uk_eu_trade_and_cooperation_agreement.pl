% ============================================================================
% CONSTRAINT STORY: uk_eu_trade_and_cooperation_agreement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_eu_trade_and_cooperation_agreement, []).

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
 *   constraint_id: uk_eu_trade_and_cooperation_agreement
 *   human_readable: UK-EU Trade and Cooperation Agreement: Coordination with Asymmetric Extraction
 *   domain: international_trade/political_economy
 *
 * SUMMARY:
 *   The UK-EU Trade and Cooperation Agreement (TCA) negotiated in December
 *   2020 and implemented January 2021 represents a structural attempt to
 *   preserve market access after UK withdrawal from the EU political union.
 *   The constraint exhibits hybrid coordination and extraction
 *   characteristics because it simultaneously solves a genuine collective
 *   action problem (how to maintain trade access without full institutional
 *   integration) and distributes asymmetric costs across different actor
 *   populations. Different actors experience the TCA through entirely
 *   different structural lenses: UK manufacturing faces new friction costs
 *   they did not incur as EU members; EU agricultural exporters gain
 *   tariff-free access but incur certification costs; UK financial services
 *   retained equivalence frameworks; Northern Ireland border communities
 *   experience the constraint as geopolitical entrapment; the Joint Committee
 *   machinery represents temporary governance scaffolding. The TCA's
 *   extractiveness has increased from implementation (0.38) to current state
 *   (0.52) as regulatory divergence accumulates and the optimistic
 *   assumptions of the transition period proved unrealistic. Theater has
 *   risen in parallel as formal dispute-resolution processes substitute for
 *   operational friction reduction.
 *
 * KEY AGENTS:
 *   - UK Financial Services Sector: Primary beneficiary (institutional/arbitrage) — retained equivalence frameworks and regulatory autonomy; net positive position from TCA
 *   - EU Agricultural Exporters: Secondary beneficiary (powerful/mobile) — secured tariff-free access but face new certification burdens; moderate benefit extraction
 *   - UK Manufacturing Supply Chains: Primary victim (moderate/constrained) — face non-tariff barriers, rules-of-origin friction, compliance costs with limited alternative markets; bears asymmetric extraction
 *   - Northern Ireland Border Communities: Secondary victim (powerless/trapped) — structurally confined between two regulatory regimes; bears political and economic costs of protocol implementation with no exit option
 *   - EU Regulatory Institutions: Secondary enforcer (institutional/constrained) — maintain equivalence surveillance and dispute mechanisms; carry burden of active enforcement
 *   - UK-EU Joint Committee: Temporary governance structure (organized/constrained) — manages dispute resolution and regulatory coordination; sunset logic embedded in original design
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees TCA as geopolitically contingent hybrid that naturalizes regulatory asymmetries as technical necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_eu_trade_and_cooperation_agreement, 0.52).
domain_priors:suppression_score(uk_eu_trade_and_cooperation_agreement, 0.48).
domain_priors:theater_ratio(uk_eu_trade_and_cooperation_agreement, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_eu_trade_and_cooperation_agreement, extractiveness, 0.52).
narrative_ontology:constraint_metric(uk_eu_trade_and_cooperation_agreement, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(uk_eu_trade_and_cooperation_agreement, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_eu_trade_and_cooperation_agreement, tangled_rope).
narrative_ontology:human_readable(uk_eu_trade_and_cooperation_agreement, "UK-EU Trade and Cooperation Agreement: Coordination with Asymmetric Extraction").
narrative_ontology:topic_domain(uk_eu_trade_and_cooperation_agreement, "international_trade/political_economy").

domain_priors:requires_active_enforcement(uk_eu_trade_and_cooperation_agreement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_eu_trade_and_cooperation_agreement, uk_financial_services_sector).
narrative_ontology:constraint_beneficiary(uk_eu_trade_and_cooperation_agreement, eu_agricultural_exporters).
narrative_ontology:constraint_victim(uk_eu_trade_and_cooperation_agreement, uk_manufacturing_supply_chains).
narrative_ontology:constraint_victim(uk_eu_trade_and_cooperation_agreement, eu_regulatory_harmonization_burden).
narrative_ontology:constraint_victim(uk_eu_trade_and_cooperation_agreement, northern_ireland_border_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NORTHERN IRELAND BORDER COMMUNITIES (SNARE) — Structurally trapped between two regulatory regimes with no meaningful exit. Faces customs friction, regulatory divergence, and identity-political weaponization of border status. Cannot exit the constraint without relocation or wholesale community dissolution. Bears costs of protocol implementation (friction, uncertainty) with minimal coordination benefits.
constraint_indexing:constraint_classification(uk_eu_trade_and_cooperation_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UK MANUFACTURING SUPPLY CHAINS (TANGLED ROPE) — Constrained by non-tariff barriers, rules-of-origin requirements, and customs procedures; also benefit from tariff-free goods access and regulatory alignment with EU standards. High costs of regulatory divergence (retooling, compliance infrastructure) but genuine coordination function through harmonized standards reduces total friction vs. full regulatory decoupling. Extraction is asymmetric — compliance costs fall primarily on UK firms, benefit distribution skews toward EU.
constraint_indexing:constraint_classification(uk_eu_trade_and_cooperation_agreement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UK FINANCIAL SERVICES SECTOR (ROPE) — Primary beneficiary of TCA. Preserved equivalence frameworks and regulatory arbitrage opportunities. Experiences the constraint as pure coordination: the agreement enables cross-border financial services without full integration. Net beneficiary position — the agreement protects UK financial sector autonomy while enabling EU market access. Arbitrage options (Asian markets, domestic consolidation) keep this agent mobile.
constraint_indexing:constraint_classification(uk_eu_trade_and_cooperation_agreement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EU AGRICULTURAL EXPORTERS (TANGLED ROPE) — Secured tariff-free access to UK market (coordination benefit) but face new sanitary/phytosanitary certification requirements and customs procedures (extraction cost). Extraction is moderate because mobile — can redirect exports to other markets if barriers rise. But genuine coordination function preserved: zero tariffs on agricultural goods is substantive benefit balancing new compliance burden.
constraint_indexing:constraint_classification(uk_eu_trade_and_cooperation_agreement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: EU REGULATORY HARMONIZATION INFRASTRUCTURE (TANGLED_ROPE) — EU institutions bear ongoing cost of maintaining dual-track regulatory surveillance (monitoring UK divergence, managing equivalence reviews). TCA requires active enforcement: reciprocal market access conditioned on regulatory proximity. Benefits from this burden fall to EU exporters (tariff-free access preserved); costs distributed to EU regulatory bodies. Constrained because EU cannot simply abandon the framework without sacrificing trade gains for member states.
constraint_indexing:constraint_classification(uk_eu_trade_and_cooperation_agreement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: UK-EU JOINT COMMITTEE AND DISPUTE RESOLUTION (SCAFFOLD) — Temporary governance structures built to manage divergence and resolve disputes. Functions as coordination mechanism during adjustment period to post-Brexit regulatory distance. Theater moderate (formal procedures, regular meetings) but genuine dispute-resolution function exists. Implied sunset: as both parties develop stable regulatory postures and businesses adapt to new friction, the need for intensive committee management should decline. Current intensity reflects temporary post-transition coordination demand.
constraint_indexing:constraint_classification(uk_eu_trade_and_cooperation_agreement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: EU REGULATORY EQUIVALENCE FRAMEWORK (PITON) — The framework for declaring UK financial regulation 'equivalent' persists largely through institutional inertia and path dependence. Its functional role is minimal because equivalence decisions can be revoked unilaterally by EU; trust eroded by divergence risk. Theater high: annual reviews, formal assessments, procedural maintenance — but the mechanism's actual constraint on either party's regulatory choices is weak. Maintained because alternatives haven't fully replaced it (services trade otherwise in limbo) but both sides recognize its limitations.
constraint_indexing:constraint_classification(uk_eu_trade_and_cooperation_agreement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED_ROPE) — From a civilizational perspective, the TCA represents a hybrid coordination mechanism: genuine benefits from tariff elimination and services access preserved (coordination function), combined with substantial extraction through regulatory friction and asymmetric compliance burdens (extraction function). The constraint is not resolvable into pure coordination (would require full regulatory alignment) or pure extraction (would require complete market closure). Both parties extract from lower-power actors (businesses, border communities) to sustain the mixed arrangement.
constraint_indexing:constraint_classification(uk_eu_trade_and_cooperation_agreement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_eu_trade_and_cooperation_agreement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_eu_trade_and_cooperation_agreement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_eu_trade_and_cooperation_agreement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_eu_trade_and_cooperation_agreement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_eu_trade_and_cooperation_agreement, TR),
    TR >= 0.70.

:- end_tests(uk_eu_trade_and_cooperation_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The TCA began at 0.38 (optimistic — tariff-free trade, preserved services access, manageable adjustment) and has risen to 0.52 as non-tariff barriers proved more onerous than anticipated, regulatory divergence accelerated, and the political consensus enabling compromise eroded. The extraction is neither uniform nor simple: UK manufacturers extract real costs (supply chain reorganization, compliance infrastructure, customs friction); EU gets tariff-free agricultural access (benefit); UK financial sector keeps regulatory autonomy (benefit); Northern Ireland loses certainty and gains protocol friction (cost). The 0.52 reflects that benefits are concentrated (financial services, some trade flows) while costs are distributed (SMEs, border communities, regulatory bodies). Suppression (0.48): Moderate. Firms have exit options (reshoring, market redirection, hedging through diversification) but they are costly. Northern Ireland has no exit. The overall suppression reflects that the TCA is not coercive in the criminal sense — it is a voluntary agreement both parties formally accepted — but its implementation has created friction barriers higher than parties expected, reducing practical exit. Theater (0.58): Moderate. The Joint Committee and dispute mechanisms are genuine coordination structures, but they increasingly function as theater: formal processes that manage political differences without resolving underlying friction. The equivalence frameworks for financial services are highly performative — they lack substantive governance power and can be revoked unilaterally. Border protocols are formal procedures that substitute for real infrastructure investment. Claimed type tangled_rope reflects that genuine coordination function (tariff elimination, services access preservation) coexists with substantial asymmetric extraction (compliance costs, regulatory uncertainty, political weaponization of protocol).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is dramatic. The beneficiary sector (UK financial) sees coordination (Rope); the maximally-trapped sector (Northern Ireland) sees extraction (Snare); the moderately-constrained sector (UK manufacturing) sees mixed (Tangled Rope); the temporary governance machinery sees itself as Scaffold. These classifications are not errors or disagreements about facts — they are structural observations of genuine asymmetry in the constraint's operation. The engine's task is not to adjudicate which is 'correct' but to measure the gap's magnitude and understand what it reveals about distributional politics. The large gap indicates that the TCA has not achieved genuine consensus coordination: it sustains itself through concentrated benefit to UK services and EU agriculture, distributed cost across manufacturing and border communities, and active enforcement by regulatory bodies. This is the signature of extraction disguised as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is not uniform across agents. Beneficiaries (UK financial services, EU agricultural exporters) experience low or negative directionality (d ≈ 0.10-0.35) because extraction runs toward them. Victims (UK manufacturing, Northern Ireland) experience high directionality (d ≈ 0.70-0.95) because extraction runs away from them. Institutional enforcers (EU regulatory bodies) experience intermediate directionality (d ≈ 0.55) because they are both beneficiaries of preserved trade flows and victims of enforcement burden. The scope multiplier (1.0 for national/continental) does not reduce experienced extraction for any party — the constraint operates at meaningful scale for all actors. Suppression (0.48) is a structural property reflecting that agents have imperfect exit: firms can reshoring or market-redirect, but at substantial cost; Northern Ireland has no exit. This asymmetric suppression — perfect for some (financial services, EU exporters), severe for others (border communities) — is the mechanism sustaining the hybrid extraction/coordination structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The TCA resolves the mandatrophy by revealing that both 'pure coordination' and 'pure extraction' frames are partially correct, and they map to different agent classes. From the beneficiary perspective (UK financial services), the constraint IS pure coordination — it solves the problem of maintaining market access without political integration. From the victim perspective (Northern Ireland, UK manufacturing), it IS extraction — costs are imposed with minimal voice in decisions and minimal exit options. The tangled_rope classification bridges these truths: the constraint exhibits genuine coordination function (tariff elimination, services access) AND asymmetric extraction (costs distributed to manufacturing and border communities, benefits concentrated in financial and agricultural sectors). The mandatrophy is not resolvable into a single type because the constraint is not uniform — it solves a genuine collective action problem (market access without political union) while creating new asymmetries (regulatory divergence costs, protocol friction). The classification scheme must accommodate this hybridity without collapsing into an undifferentiated 'it's both coordination and extraction.' Tangled Rope is precisely the type that does this: coordination function genuinely present, asymmetric extraction genuinely present, both structurally required. No single type would capture this architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_divergence_trajectory,
    'Will UK and EU regulatory divergence accelerate or stabilize over the next 5-10 years?',
    'Longitudinal tracking of regulatory announcements, rules changes, and equivalence review outcomes; empirical measurement of regulatory distance between UK and EU frameworks in key sectors (financial services, data protection, environmental standards)',
    'If divergence accelerates: TCA transforms from tangled_rope (mixed coordination-extraction) toward snare (extraction dominates). If stabilizes: TCA remains tangled_rope with predictable extraction costs that businesses can price in. Classification depends on divergence rate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_divergence_trajectory, empirical, 'Whether UK-EU regulatory divergence will accelerate or stabilize').

omega_variable(
    northern_ireland_protocol_endurance,
    'Will the Northern Ireland Protocol (Windsor Framework successor) remain functional and stable, or will it become a source of renewed political crisis requiring renegotiation?',
    'Monitoring of protocol operation metrics (customs friction data, border delay averages, compliance costs); political stability indicators (unionist consent measures, power-sharing stability); US/Irish government diplomatic pressure; feasibility studies of alternative arrangements',
    'If stable: Northern Ireland communities experience constrained/mobile exit (trapped → constrained). If unstable: trapped classification persists or intensifies. The protocol is the structural mechanism confining extraction to this region — instability means extraction mechanisms themselves become contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(northern_ireland_protocol_endurance, empirical, 'Whether Northern Ireland Protocol remains stable or requires renegotiation').

omega_variable(
    services_market_access_bifurcation,
    'Will UK financial services maintain effective EU market access through equivalence frameworks, or will equivalence declarations erode, fragmenting the services market?',
    'Tracking of EU equivalence review outcomes; measuring share of UK financial services revenue dependent on EU market access; monitoring regulatory divergence in data protection, payments, derivatives that affect equivalence eligibility',
    'If equivalence preserved: UK financial sector remains beneficiary (rope classification). If eroded: UK financial sector experiences extraction (moves toward tangled_rope or snare). This shift would reverse the beneficiary designation for institutional actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(services_market_access_bifurcation, empirical, 'Whether UK financial services equivalence will be preserved or eroded').

omega_variable(
    supply_chain_adaptation_burden_distribution,
    'Will the costs of non-tariff barriers and rules-of-origin compliance be borne primarily by UK firms (as current data suggests) or will burden-sharing emerge through EU infrastructure investment or alternative channels?',
    'Comparative analysis of compliance cost burdens; measurement of supply chain reshoring vs. EU-side logistics consolidation; tracking of border infrastructure investment (ports, customs facilities); monitoring of SME exit rates from UK-EU trade',
    'If burden remains asymmetric: UK manufacturing remains victim in tangled_rope (extraction sustained). If burden-sharing emerges: classification shifts toward more symmetric tangled_rope or even rope. Current suppression (0.48) reflects asymmetric burden — if symmetry increases, suppression should decline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_adaptation_burden_distribution, empirical, 'Distribution of non-tariff barrier compliance costs between UK and EU firms').

omega_variable(
    geopolitical_realignment_effect,
    'If UK-EU geopolitical alignment deteriorates (via NATO divergence, China policy, or other great-power competition), will the TCA''s dispute resolution mechanisms remain effective, or will political leverage override contractual obligations?',
    'Monitoring of geopolitical friction indicators (trade dispute escalation, sanctions coordination, technology standards conflicts); examining historical precedent of trade agreements under geopolitical stress; tracking political rhetoric from UK and EU leaders regarding treaty enforcement',
    'If alignment deteriorates: TCA transforms from legalist coordination framework to theater — formal structures persist but actual constraint-enforcement power erodes. Both beneficiary and victim roles become unstable; extraction becomes weaponized. Classification toward piton or snare depending on which party uses trade leverage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_realignment_effect, empirical, 'Effect of geopolitical realignment on TCA enforcement credibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_eu_trade_and_cooperation_agreement, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uktca_tr_t0, uk_eu_trade_and_cooperation_agreement, theater_ratio, 0, 0.42).
narrative_ontology:measurement(uktca_tr_t2, uk_eu_trade_and_cooperation_agreement, theater_ratio, 2, 0.5).
narrative_ontology:measurement(uktca_tr_t4, uk_eu_trade_and_cooperation_agreement, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(uktca_be_t0, uk_eu_trade_and_cooperation_agreement, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(uktca_be_t2, uk_eu_trade_and_cooperation_agreement, base_extractiveness, 2, 0.46).
narrative_ontology:measurement(uktca_be_t4, uk_eu_trade_and_cooperation_agreement, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_eu_trade_and_cooperation_agreement, resource_allocation).
narrative_ontology:affects_constraint(uk_eu_trade_and_cooperation_agreement, northern_ireland_protocol).
narrative_ontology:affects_constraint(uk_eu_trade_and_cooperation_agreement, uk_financial_services_equivalence).
narrative_ontology:affects_constraint(uk_eu_trade_and_cooperation_agreement, eu_agricultural_market_access).
narrative_ontology:affects_constraint(uk_eu_trade_and_cooperation_agreement, supply_chain_regulatory_compliance).

% DUAL FORMULATION NOTE:
% The TCA is an umbrella constraint under which multiple domain-specific constraints operate. The protocol (Northern Ireland border terms) has its own extractiveness profile. UK financial services equivalence is downstream of the TCA but has distinct evaluation metrics (equivalence review frequency, regulatory divergence rates). EU agricultural market access reflects a specific TCA benefit with its own measurement. Supply chain compliance burden is a specific extraction mechanism within the broader constraint. All are linked through the TCA framework but warrant separate analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_eu_trade_and_cooperation_agreement, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
