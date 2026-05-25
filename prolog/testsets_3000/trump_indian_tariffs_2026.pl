% ============================================================================
% CONSTRAINT STORY: trump_indian_tariffs_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trump_indian_tariffs_2026, []).

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
 *   constraint_id: trump_indian_tariffs_2026
 *   human_readable: Trump's Tariff Reduction on India (2026)
 *   domain: economic/geopolitical/trade
 *
 * SUMMARY:
 *   In 2026, a Trump administration reduces tariffs on Indian goods from
 *   approximately 22% to 18% following an agreement with the Modi government
 *   to reduce oil purchases from Russia and redirect strategic alignment
 *   toward the US-India partnership. This constraint exhibits a hybrid
 *   coordination-extraction structure: it solves a geopolitical problem
 *   (reducing Russian leverage over India's energy independence) while
 *   extracting costs from trapped domestic actors on both sides (Indian
 *   manufacturers, US protected-sector workers). The theater ratio reflects
 *   that the tariff reduction is publicly framed as 'strategic partnership'
 *   and 'free trade alignment,' obscuring its conditional nature (contingent
 *   on India's Russia pivot) and its extraction mechanics (creating losers
 *   among domestic producers). The constraint is stabilized by asymmetric
 *   information (public framing vs. private quid pro quo), institutional
 *   power (executive tariff authority), and geopolitical urgency. Base
 *   extractiveness (0.52) reflects moderate but real coercive extraction:
 *   Indian manufacturers face capital loss; US protected sectors face import
 *   competition; Russian energy sector faces demand reduction; yet
 *   coordination benefits exist for US consumers and Indian exporters. The
 *   suppression score (0.48) reflects moderate barriers: exit options exist
 *   (alternative suppliers, relocation, diversification) but are costly and
 *   slow; political voice is suppressed for losing constituencies relative to
 *   beneficiary lobbies.
 *
 * KEY AGENTS:
 *   - Trump Administration: Primary beneficiary (institutional/arbitrage) — achieves geopolitical objective of India-Russia decoupling; captures narrative control and strategic positioning
 *   - Modi Government: Secondary beneficiary (institutional/arbitrage) — gains preferential trade access in exchange for managed Russia relationship; strategic arbitrage between US and Russia
 *   - Indian Exporters: Primary beneficiary (organized/mobile) — gain preferential 18% tariff access to US market; expand export capacity and capture market share
 *   - US Consumers: Secondary beneficiary (moderate/mobile) — access cheaper Indian goods (pharmaceuticals, textiles, components); experience constraint as coordination benefit
 *   - Indian Domestic Manufacturers: Primary victim (powerless/trapped) — face import competition with tariff disadvantage; cannot exit without business closure; high extraction
 *   - US Protected-Sector Workers: Primary victim (powerless/trapped) — face job losses and wage pressure from increased Indian imports; locked into geographic/sectoral position; high extraction
 *   - Russian Energy Sector: Secondary victim (organized/constrained) — forced demand reduction; pivots to China/OPEC but at cost; extraction via geopolitical reorientation
 *   - WTO Dispute Mechanism: Institutional observer (institutional/arbitrage) — nominally constrained by MFN rules but enforcement is performative; framework largely ignored by major powers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trump_indian_tariffs_2026, 0.52).
domain_priors:suppression_score(trump_indian_tariffs_2026, 0.48).
domain_priors:theater_ratio(trump_indian_tariffs_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trump_indian_tariffs_2026, tangled_rope).
narrative_ontology:human_readable(trump_indian_tariffs_2026, "Trump's Tariff Reduction on India (2026)").
narrative_ontology:topic_domain(trump_indian_tariffs_2026, "economic/geopolitical/trade").

domain_priors:requires_active_enforcement(trump_indian_tariffs_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trump_indian_tariffs_2026, indian_exporters).
narrative_ontology:constraint_beneficiary(trump_indian_tariffs_2026, us_consumers).
narrative_ontology:constraint_beneficiary(trump_indian_tariffs_2026, trump_administration_geopolitical_objectives).
narrative_ontology:constraint_victim(trump_indian_tariffs_2026, russian_energy_sector).
narrative_ontology:constraint_victim(trump_indian_tariffs_2026, us_domestic_manufacturers).
narrative_ontology:constraint_victim(trump_indian_tariffs_2026, indian_domestic_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIAN DOMESTIC MANUFACTURERS (SNARE) — Trapped by tariff reduction that floods market with cheaper imports. Have no escape option; cannot compete with 18% tariff advantage. Face business closure or consolidation. Maximum extraction from this structural position.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: US DOMESTIC MANUFACTURERS / PROTECTED SECTORS (SNARE) — Trapped in sectors facing Indian import competition. Cannot exit tariff regime; bear extraction costs from preferential treatment given to India. No compensating benefit; high suppression of political voice relative to agricultural lobby.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: INDIAN EXPORTERS & MODI GOVERNMENT (ROPE) — Primary beneficiaries. Experience constraint as pure coordination: tariff reduction is reward for geopolitical alignment (reducing Russian oil purchases). Net benefit from preferential trade access. Exit option via continued Russia alignment, but Modi chooses strategic arbitrage toward US alignment. Low experienced extraction.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US CONSUMERS & RETAIL (ROPE) — Secondary beneficiaries. Access to cheaper Indian goods (textiles, pharmaceuticals, electronics components). Experience constraint as coordination mechanism enabling consumption at lower cost. Can arbitrage to alternative suppliers but benefit from this route. Low experienced extraction.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RUSSIAN ENERGY SECTOR (TANGLED ROPE) — Victim of geopolitical extraction. Constrained exit (sanctions environment already limits options). Also experiences benefit from sustained energy sales to India during reduction period (Modi buys Russian oil at discounts). Hybrid: extraction through forced market loss, benefit through pricing leverage. Suppression very high but not absolute — can pivot to China/OPEC alternatives.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRUMP ADMINISTRATION POLITICAL OBJECTIVES (TANGLED ROPE) — Uses tariff reduction as tool for geopolitical coordination (reducing Russian leverage over India, strengthening US-India axis) while also extracting compliance from India (Modi must reduce Russia engagement). Has multiple exit options but commits to this path for strategic gain. Experiences constraint as both coordination device and coercive lever. Moderate extraction achieved through asymmetric information and structural power.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: GLOBAL SUPPLY CHAIN ACTORS (SCAFFOLD) — Tariff reduction is temporary coordination tool for reshaping supply chains away from China/Russia toward India-US alignment. Benefits from lower tariffs enabling relocation of manufacturing. Theater: framed as permanent 'strategic partnership' but actually contingent on geopolitical alignment. Has sunset: if India-Russia relations normalize or US-China relations reset, tariff benefits evaporate. Organized actors can exploit window to relocate capacity.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 8: WTO DISPUTE RESOLUTION FRAMEWORK (PITON) — Tariff reduction nominally complies with MFN principles if applied multilaterally, but bilateral nature suggests potential violation. WTO enforcement is substantially performative: disputes take years, remedies are slow, and major powers ignore unfavorable rulings. Framework persists through institutional inertia despite low functional constraint on US tariff policy. Theater ratio high.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / COMPARATIVE ADVANTAGE VIEW (MOUNTAIN) — Risks naturalizing contingent policy as economic law: 'tariff reduction reflects comparative advantage discovery; inevitable realignment of trade flows.' This perspective sees the constraint as emerging naturally from cost differentials. However, base extraction (0.52) contradicts the mountain gate. The 'natural law' framing obscures the geopolitical coercion (Modi forced to reduce Russian purchases) and institutional extraction (domestic manufacturers trapped without exit).
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_indian_tariffs_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trump_indian_tariffs_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_indian_tariffs_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trump_indian_tariffs_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trump_indian_tariffs_2026, TR),
    TR >= 0.70.

:- end_tests(trump_indian_tariffs_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The tariff reduction creates winners (Indian exporters, US consumers, Trump administration geopolitical position) and losers (Indian domestic manufacturers, US protected sectors, Russian energy sector). The asymmetry is real but not absolute — victims have non-zero exit options (relocation, productivity gains, diversification). The constraint's extractiveness is driven by the conditional nature of the tariff benefit: Modi is effectively trading Russia relations for market access, creating a coercive quid pro quo that structurally benefits the US administration. Suppression (0.48): Moderate. Barriers to exit include: (a) for Indian manufacturers, capital sunk in existing capacity and competitive disadvantage from 18% tariff; (b) for US workers, geographic and skill lock-in; (c) for Russia, sanctions environment limiting alternative partnerships. However, these barriers are not absolute — factories can relocate (costly), workers can retrain (costly), and Russia can pivot to China. The suppression reflects the structural pain of adjustment, not a complete elimination of alternatives. Theater (0.58): Moderate-high. Public framing emphasizes 'strategic partnership' and mutual benefit, downplaying the extraction mechanics and the conditionality on India's Russia pivot. Media coverage reflects this theatrical framing — the bilateral nature and geopolitical quid pro quo are less prominent than 'US-India economic integration' narratives. The theater has risen over the interval as the agreement has been operationalized and adjusted to manage political backlash from losing constituencies.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries and victims is extreme. Indian exporters see a coordination mechanism enabling market access and competitive advantage — the constraint solves their export problem. The Modi government sees strategic arbitrage opportunity — geopolitical alignment converts into market access. US consumers see pure coordination benefit — cheaper goods without loss. But Indian domestic manufacturers see a snare — they bear full extraction costs with no offsetting benefit and no exit option. US protected-sector workers also see a snare — they face job losses and wage pressure with limited recourse. The Russian energy sector sees tangled rope — forced demand loss (extraction) paired with pricing leverage during the remaining purchase window (benefit). The theater ratio explains why these gaps persist: the constraint is framed publicly as mutual benefit and free trade alignment, obscuring the geopolitical quid pro quo and the extraction mechanics. The WTO framework's piton status reflects that the nominally binding dispute resolution mechanism cannot effectively constrain US executive tariff authority — the framework persists as performative ritual despite non-compliance by major powers.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from its structural position relative to the tariff reduction. The Modi government experiences low d (beneficiary status + arbitrage exit) → negative χ = they gain from this constraint. Indian exporters experience low d (beneficiary + mobile exit) → negative χ. Trump administration experiences low d (beneficiary + arbitrage capacity) → negative χ. US consumers experience low d (beneficiary + mobile substitution options) → negative χ. Indian domestic manufacturers experience high d (victim + trapped exit) → high χ (strong experienced extraction). US protected-sector workers experience high d (victim + trapped geographic/sectoral position) → high χ (strong experienced extraction). Russian energy sector experiences moderate-high d (victim + partially constrained exit via China pivots) → moderate-high χ. The WTO framework is constrained by major-power non-compliance but has arbitrage options (case filing, counter-cases), placing it at moderate d. The tangled rope classification emerges from the presence of both beneficiaries (exporters, consumers, Trump admin) and victims (Indian manufacturers, US workers, Russia), combined with active enforcement (executive tariff authority) and asymmetric extraction (benefits flow to aligned parties, costs to unaligned or powerless actors).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint satisfies all three mandatrophy gates for tangled rope classification: (1) Beneficiaries clearly identified (Indian exporters, US consumers, Trump admin, Modi government) with coordination function (tariff reduction enables trade expansion and geopolitical alignment); (2) Victims clearly identified (Indian manufacturers, US protected sectors, Russian energy) with asymmetric extraction (tariff regime benefits some actors while imposing concentrated costs on others); (3) Active enforcement is present (executive tariff authority, bilateral negotiation enforcement, WTO dispute-process suppression). The falsity of the 'free trade' narrative is captured by the suppression and theater metrics. A false mountain (comparative advantage naturally reordering trade) is prevented by the explicit base extraction (0.52) and the identification of coercive political conditioning. The snare perspective (from Indian manufacturers or US protected workers) is structurally legitimate but not the whole story — the constraint genuinely provides coordination benefit to US-India trade and geopolitical alignment, preventing pure snare classification. The scaffold perspective is rejected (no sunset clause evident in the tariff agreement; India-Russia pivot appears intended to be durable) but could reemerge if future administrations abandon the tariff regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    india_russia_pivot_permanence,
    'Will India''s reduction in Russian oil purchases persist if US-India relations cool or if geopolitical conditions shift back toward Russia alignment?',
    'Longitudinal tracking of India-Russia energy flows; analysis of strategic communications from Modi government regarding energy security autonomy; monitoring of US-India friction points post-2026',
    'If permanent: constraint is sustainable tangled rope. If reversible: constraint becomes temporary extraction mechanism (scaffold with shorter sunset). High geopolitical instability makes this empirically uncertain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(india_russia_pivot_permanence, empirical, 'Whether India''s Russia pivot is durable or contingent').

omega_variable(
    domestic_manufacturing_viability,
    'Can Indian domestic manufacturers survive tariff reduction through productivity gains, or will consolidation/closure be the primary outcome?',
    'Industry-level analysis of Indian manufacturing margins, capex investment, labor productivity trends; comparison with historical tariff reduction impacts (2000s India-China trade); tracking of firm-level exits vs upgrades in import-competing sectors',
    'If productive survival: snare classification softens to tangled rope (victims gain partial exit through innovation). If consolidation dominates: snare classification confirmed and extraction is severe and durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_manufacturing_viability, empirical, 'Domestic manufacturer adaptive capacity under tariff reduction').

omega_variable(
    us_manufacturing_political_exit,
    'Will US domestic manufacturers successfully lobby for sectoral exceptions or reciprocal tariffs, creating political exit from the snare?',
    'Tracking of lobbying expenditure and congressional interest by affected sectors; analysis of Trump administration''s responsiveness to domestic manufacturer pressure; monitoring of sectoral tariff modifications post-2026',
    'If successful exit: US domestic manufacturers move from snare to tangled rope (partial extraction offset by political access). If unsuccessful: snare persists and suppression remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_manufacturing_political_exit, empirical, 'US manufacturers'' political ability to modify tariff regime').

omega_variable(
    geopolitical_offset_durability,
    'Does the geopolitical benefit to the US (reduced Russian leverage over India) persist long enough to justify the economic extraction costs to domestic manufacturers and Indian domestic producers?',
    'Assessment of India''s actual pivot away from Russia in energy/defense/tech sectors; measurement of reduced Russian influence in Indian foreign policy; comparison of geopolitical gains to economic losses over 5-10 year horizon',
    'If geopolitical gains are durable and substantial: constraint is justified as strategic sacrifice (tangled rope with acceptable asymmetry). If gains evaporate quickly: constraint becomes unjustified extraction (snare classification strengthened).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_offset_durability, conceptual, 'Durability of geopolitical offsetting against economic costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trump_indian_tariffs_2026, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tit_tr_t0, trump_indian_tariffs_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tit_tr_t2, trump_indian_tariffs_2026, theater_ratio, 2, 0.52).
narrative_ontology:measurement(tit_tr_t5, trump_indian_tariffs_2026, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(tit_be_t0, trump_indian_tariffs_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tit_be_t2, trump_indian_tariffs_2026, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(tit_be_t5, trump_indian_tariffs_2026, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trump_indian_tariffs_2026, resource_allocation).
narrative_ontology:affects_constraint(trump_indian_tariffs_2026, us_manufacturing_tariff_regime).
narrative_ontology:affects_constraint(trump_indian_tariffs_2026, india_russia_energy_partnership).
narrative_ontology:affects_constraint(trump_indian_tariffs_2026, global_supply_chain_restructuring_2020s).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the broader US-India geopolitical partnership and upstream of specific sectoral tariff disputes and supply chain relocation dynamics. The tariff reduction mechanism is distinct from the underlying geopolitical alignment (which would be a separate constraint with different ε) but uses tariff policy as its enforcement tool.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trump_indian_tariffs_2026, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
