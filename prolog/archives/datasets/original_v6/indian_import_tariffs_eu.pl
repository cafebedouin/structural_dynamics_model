% ============================================================================
% CONSTRAINT STORY: indian_import_tariffs_eu
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_import_tariffs_eu, []).

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
 *   constraint_id: indian_import_tariffs_eu
 *   human_readable: Indian Protective Tariffs on European Union Imports (Autos & Spirits)
 *   domain: economic/political
 *
 * SUMMARY:
 *   India's protective tariffs on European Union imports — reaching 150% on
 *   automobiles and alcoholic beverages — constitute a structural constraint
 *   on trade that combines genuine coordination benefits (domestic industry
 *   protection, infant-industry development, revenue generation) with
 *   asymmetric extraction (prices inflated for consumers, market access
 *   blocked for EU exporters, supply chain fragmentation). The constraint
 *   exhibits a sharp perspectival gap: Indian manufacturers and the state see
 *   coordination and legitimate development support; consumers and EU
 *   exporters see extraction and trade suppression. The tariff regime is not
 *   a natural law of economics but a contingent policy choice justified under
 *   WTO infant-industry and developing-nation exemptions. Its future depends
 *   on whether India's automotive sector achieves genuine competitiveness,
 *   whether the EU can credibly retaliate, whether consumer coalitions emerge
 *   domestically, and whether India's economic rise erodes its legal
 *   justification for protection. The theater ratio reflects that the tariff
 *   operates through stated infant-industry development logic (partially
 *   true) but increasingly functions as pure rents to protected manufacturers
 *   as the sector matures. The constraint is actively enforced through
 *   customs mechanisms and regulatory structures, not maintained through
 *   cultural inertia (ruling out Piton as the primary classification), and
 *   negotiations are gradually reducing rates (showing Scaffold
 *   characteristics for some sectors).
 *
 * KEY AGENTS:
 *   - Indian Automotive Manufacturers: Primary beneficiary (institutional/arbitrage) — receive protected domestic market and time to build export capacity; extraction flows toward them
 *   - Indian Spirits Producers: Primary beneficiary (institutional/arbitrage) — protected from price competition; extract domestic consumer surplus
 *   - Indian Government: Active enforcer (organized/constrained) — receives tariff revenue and political constituency support but bears WTO dispute costs and bilateral pressure; both benefits and victims
 *   - Indian Consumers: Primary victim (powerless/trapped) — face 150% price markup with no exit option; trapped by national regulation and geography
 *   - EU Automotive Exporters: Organized victim (organized/constrained) — face tariff barrier that overrides competitive advantage; constrained by both tariff and legal exceptions that make WTO remedies slow
 *   - EU Spirits Exporters: Organized victim (organized/constrained) — face tariff barriers with limited ability to relocate supply chains or lobby effectively
 *   - India-EU Trade Negotiators: Organized agent (organized/mobile) — able to negotiate sector-specific reductions; show sunset logic through gradual tariff phase-downs in wine and spirits agreements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_import_tariffs_eu, 0.58).
domain_priors:suppression_score(indian_import_tariffs_eu, 0.72).
domain_priors:theater_ratio(indian_import_tariffs_eu, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_import_tariffs_eu, extractiveness, 0.58).
narrative_ontology:constraint_metric(indian_import_tariffs_eu, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(indian_import_tariffs_eu, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_import_tariffs_eu, tangled_rope).
narrative_ontology:human_readable(indian_import_tariffs_eu, "Indian Protective Tariffs on European Union Imports (Autos & Spirits)").
narrative_ontology:topic_domain(indian_import_tariffs_eu, "economic/political").

domain_priors:requires_active_enforcement(indian_import_tariffs_eu).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_import_tariffs_eu, indian_automotive_manufacturers).
narrative_ontology:constraint_beneficiary(indian_import_tariffs_eu, indian_spirits_producers).
narrative_ontology:constraint_beneficiary(indian_import_tariffs_eu, indian_government_revenue).
narrative_ontology:constraint_victim(indian_import_tariffs_eu, eu_exporters).
narrative_ontology:constraint_victim(indian_import_tariffs_eu, indian_consumers).
narrative_ontology:constraint_victim(indian_import_tariffs_eu, india_eu_trade_relationship).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIAN CONSUMER (SNARE) — Faces 150% tariffs on imported vehicles and spirits with limited domestic substitutes of equivalent quality. Exit is constrained by geography and national regulation. Bears full cost of tariff as price markup with no meaningful choice or alternative. Maximum extraction from trapped position.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIAN AUTOMOTIVE SECTOR (ROPE) — Primary beneficiary with substantial arbitrage options (domestic market protection, export capacity building, FDI attraction for local manufacturing). Experiences tariff as pure coordination mechanism enabling industry growth. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: EUROPEAN UNION EXPORTERS (SNARE) — Organized actors facing 150% effective tariff barriers with limited ability to compete domestically or restructure supply chains. WTO dispute options are constrained (India has legal justifications via developing nation exemptions). Extraction is severe despite organizational capacity because the tariff mechanism overrides market competition.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIAN GOVERNMENT (TANGLED ROPE) — Enforces tariff structure; receives substantial revenue and supports domestic industry coordination. Also bears legitimacy costs (consumer complaints, bilateral pressure from EU, potential retaliation). Actively enforces the constraint but experiences both extraction (from EU pressure, WTO disputes) and benefit (tax revenue, constituency support). Cannot fully exit due to domestic political economy.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: WTO FRAMEWORK (PITON) — Nominally governs tariff behavior but the constraint operates within it (India's tariff use is legal under developing nation flexibilities and infant-industry provisions). The rules framework is performative here — it theoretically constrains but functionally permits the tariff because legal exceptions are available. Theater ratio elevated because WTO dispute mechanisms are slow and often unresolved. The framework persists through institutional inertia despite limited functional constraint on this particular tariff regime.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL NEGOTIATION ACTORS (SCAFFOLD) — India-EU trade negotiations are gradually reducing tariff rates through bilateral agreements and sector-specific commitments. Some sectors (e.g., wine) have seen tariff reductions from 150% toward 50-70%. The sunset logic is real: as India's auto sector matures and achieves export competitiveness, the infant-industry justification weakens and tariff reduction becomes politically feasible. Negotiated pathways show low effective extraction because there is visible sunset and agency.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ECONOMIC LAW VIEW (MOUNTAIN) — From a pure comparative advantage analytical lens, the tariff is a violation of natural economic law: it reduces aggregate welfare, creates deadweight loss, and violates Ricardian principles of trade. This perspective sees the tariff as an immutable constraint that cannot be overcome without economic loss. However, the structural data contradicts the mountain classification — the tariff is a fully contingent policy choice, not a law of nature. The engine's false summit detector will identify this naturalization.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_import_tariffs_eu_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_import_tariffs_eu, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_import_tariffs_eu, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indian_import_tariffs_eu, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indian_import_tariffs_eu, TR),
    TR >= 0.70.

:- end_tests(indian_import_tariffs_eu_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The tariff extracts approximately 150% price markup from consumers on protected goods, representing substantial wealth transfer to protected manufacturers. However, this is not maximum extraction because (a) some genuine coordination benefit exists (infant-industry development has real effects on output and employment), (b) legal frameworks and negotiation pathways exist (not a pure coercive snare), and (c) India's stated justification (development support) is partially credible rather than purely theatrical. The value reflects genuine extraction but bounded by the presence of real coordination functions. Suppression (0.72): High. Multiple mechanisms constrain alternatives: tariff barriers raise prices above competitive levels, substitution from domestic producers is limited (quality gaps exist), consumer exit is blocked by geography and regulation, retaliation from EU is constrained by slow WTO processes, and developing-nation legal exemptions prevent simple dispute resolution. But suppression is not total — some substitution occurs, some consumers smuggle or wait for tariff reductions, and negotiation pathways exist. Theater ratio (0.48): Low-moderate. The tariff operates with substantial functional content (it actually protects the industry and generates revenue), but as the industry matures and extraction mechanisms become less justified, performative elements increase. The stated infant-industry logic is partially true but increasingly used to justify what is becoming pure rent extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Indian manufacturers see Rope (coordination for industry development). EU exporters see Snare (pure extraction blocking market access). The Indian government sees Tangled Rope (coordination benefit + enforcement cost + bilateral pressure). Consumers see Snare (pure extraction via price markup). Trade negotiators see Scaffold (tariff reductions negotiable, sunset mechanism real). The WTO framework sees Piton (formally governs but functionally permits through legal exemptions, increasingly performative as India's economy grows). The economic-law observer risks seeing Mountain (tariff violates comparative advantage, appears inevitable) but this is a false summit — the tariff is fully contingent and reversible through negotiation. The perspectival gap reflects that the same tariff rate appears protective, extractive, or developmental depending on the agent's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by beneficiary/victim status and exit capacity. Indian manufacturers as beneficiaries with full arbitrage options (they can lobby, exit into export markets, adjust production) derive low d values and experience the tariff as beneficial coordination. Indian consumers as victims with zero exit options (trapped by tariff and geography) derive high d values and experience maximum extraction chi. EU exporters as organized victims with constrained exit (they can negotiate but not bypass the tariff) derive high-moderate d values, experiencing high extraction despite their organization. The Indian government as an active enforcer derives moderate d values — it benefits from revenue and constituency support but bears legitimacy costs and bilateral pressure, placing it at a mixed position. Trade negotiators with mobile exit options (they can propose reductions) derive low-moderate d values, enabling their Scaffold perspective. The analytical observer with analytical exit options (they can observe from outside the constraint) derives d ≈ 0.72, producing the false-summit Mountain classification that the engine flags as naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through recognizing that the tariff exhibits genuine coordination (infant-industry support, domestic employment, revenue) AND genuine extraction (consumer price markup, market access suppression, wealth transfer). The Tangled Rope classification captures this hybrid: the tariff is not pure extraction (which would make it a Snare) because the coordination function is real and measurable (output growth, employment, technological capability building). But it is not pure coordination (which would make it Rope) because asymmetric extraction is also real and measurable (consumers bear 150% markup while manufacturers capture rents, trade suppression benefits some while harming others). The Scaffold perspective acknowledges that the constraint is NOT permanent — India's economic maturation will erode the infant-industry justification, and negotiated reductions are already visible in specific sectors. This prevents misclassification as a natural economic law (false Mountain) or as an irreversible institutional inertia (Piton). The true structure is: tangled hybrid (coordination + extraction) with a visible sunset mechanism (scaffold overlay) as the economy matures. The perspectival gaps (Rope from beneficiary view, Snare from victim view, Scaffold from negotiator view) are all structurally accurate; they reflect real differences in how the constraint functions depending on position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infant_industry_success_threshold,
    'At what production volume and cost parity does the Indian automotive sector achieve genuine international competitiveness without tariff protection?',
    'Longitudinal tracking of Indian auto manufacturer cost structures, production efficiency, export volumes, and competitive positioning in third-country markets (East Africa, Southeast Asia)',
    'If achieved within 5-10 years: tariff becomes negotiable and scaffold sunset logic strengthens. If not achieved: tariff may persist indefinitely, reclassifying from tangled_rope/scaffold toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infant_industry_success_threshold, empirical, 'Timeline and cost metrics for Indian auto sector competitive maturity').

omega_variable(
    eu_retaliation_credibility,
    'Will the EU impose substantive counter-tariffs or trade restrictions if the current tariff regime persists or escalates?',
    'Analysis of EU trade defense mechanisms, past retaliatory actions, and political economy constraints on EU action (internal divisions among member states, WTO dispute outcome timelines)',
    'If retaliation is credible and impactful: constrains India''s tariff escalation and enables negotiation pathways (scaffold perspective strengthens). If EU response is weak: India has higher capacity to maintain or increase tariffs (snare perspective for EU exporters strengthens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eu_retaliation_credibility, empirical, 'Whether EU trade retaliation is credible and materially consequential').

omega_variable(
    domestic_consumer_coalition_formation,
    'Will Indian domestic consumers and import-dependent industries (aviation, tourism) form a coalition to lobby against tariffs, and can they overcome rural/automotive constituencies?',
    'Political coalition analysis; tracking of consumer advocacy groups, business associations opposing tariffs, election cycle impact on consumer sentiment',
    'If coalition emerges: tariff becomes politically vulnerable and reclassifies toward scaffold (sunset mechanism). If industrial coalitions remain fragmented: tariff persists as institutional equilibrium (tangled_rope stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_consumer_coalition_formation, empirical, 'Whether domestic consumer opposition can overcome protectionist constituencies').

omega_variable(
    developing_nation_status_erosion,
    'As India''s per-capita income rises and it approaches emerging-market thresholds, will it lose legal WTO justification for high tariffs under infant-industry and developing-nation exemptions?',
    'WTO classification changes; analysis of India''s GDP per capita trajectory relative to threshold definitions; precedent from other countries (South Korea, Taiwan) graduating from protection',
    'If legal justification erodes: tariff becomes politically unsustainable and must be negotiated downward (scaffold strengthens). If India maintains developing status longer: tariff persists as legally defensible (tangled_rope stable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(developing_nation_status_erosion, empirical, 'Whether India''s economic status will erode its WTO infant-industry exemption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_import_tariffs_eu, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tariff_tr_t0, indian_import_tariffs_eu, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tariff_tr_t5, indian_import_tariffs_eu, theater_ratio, 5, 0.42).
narrative_ontology:measurement(tariff_tr_t10, indian_import_tariffs_eu, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(tariff_be_t0, indian_import_tariffs_eu, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tariff_be_t5, indian_import_tariffs_eu, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(tariff_be_t10, indian_import_tariffs_eu, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_import_tariffs_eu, resource_allocation).
narrative_ontology:affects_constraint(indian_import_tariffs_eu, eu_india_trade_balance).
narrative_ontology:affects_constraint(indian_import_tariffs_eu, indian_export_competitiveness).
narrative_ontology:affects_constraint(indian_import_tariffs_eu, spirits_market_access_india).

% DUAL FORMULATION NOTE:
% The tariff regime can be decomposed into two structurally distinct constraints: (1) the genuine infant-industry protection mechanism (lower ε, more Rope-like, justified by development logic) and (2) the rent-extraction overlay that emerges as the protected sector matures (higher ε, more Snare-like, justified by incumbent interest). The present story captures the hybrid; a full decomposition would separate these temporal phases into distinct constraint stories linked by the development arc.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indian_import_tariffs_eu, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
