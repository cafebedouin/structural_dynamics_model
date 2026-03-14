% ============================================================================
% CONSTRAINT STORY: maritime_dispute_resolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maritime_dispute_resolution, []).

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
 *   constraint_id: maritime_dispute_resolution
 *   human_readable: Maritime Dispute Resolution Framework
 *   domain: international_law/ocean_governance
 *
 * SUMMARY:
 *   The maritime dispute resolution framework, institutionalized primarily
 *   through UNCLOS (1982) and mechanisms like the International Court of
 *   Justice and International Tribunal for the Law of the Sea, coordinates
 *   ocean governance among 193 states while simultaneously extracting from
 *   those with limited capacity to participate. The constraint exhibits
 *   tangled rope characteristics: genuine coordination function (preventing
 *   military escalation over fishing rights, shipping lanes, resource zones)
 *   combined with asymmetric extraction (developing states pay proportionally
 *   higher costs for arbitration; indigenous communities are excluded;
 *   established maritime powers can ignore adverse rulings through de facto
 *   enforcement capacity). The theater ratio (0.61) reflects degradation:
 *   states treat formal arbitration as legitimacy ritual while maintaining
 *   informal power networks; enforcement is selective; and the framework
 *   persists through institutional inertia rather than functional necessity.
 *
 * KEY AGENTS:
 *   - Indigenous Fishing Communities: Primary victim (powerless/trapped) — lack legal standing, cannot afford international representation, dependent on ocean access, excluded from dispute mechanisms
 *   - Developing Coastal States: Secondary victim (moderate/constrained) — high arbitration costs, reliance on external expertise, enforcement mechanisms favor established powers, limited naval capacity
 *   - Established Maritime Powers: Primary beneficiary (powerful/mobile) — benefit from codified shipping lanes, can project naval force to enforce advantage, have exit options through military capacity, experience coordination benefit
 *   - International Arbitration Bodies: Institutional beneficiary (institutional/arbitrage) — generate prestige and revenue, maintain institutional position, genuine dispute resolution function
 *   - Ocean Governance Reform Coalition: Organized agents (organized/constrained) — working toward sunset through expanded protocols, alternative mechanisms, subsidized access
 *   - Legacy UNCLOS Structure: Institutional actor (institutional/arbitrage) — persists through inertia despite degraded function, maintains theater of legitimacy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable ocean governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maritime_dispute_resolution, 0.52).
domain_priors:suppression_score(maritime_dispute_resolution, 0.48).
domain_priors:theater_ratio(maritime_dispute_resolution, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maritime_dispute_resolution, extractiveness, 0.52).
narrative_ontology:constraint_metric(maritime_dispute_resolution, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(maritime_dispute_resolution, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maritime_dispute_resolution, tangled_rope).
narrative_ontology:human_readable(maritime_dispute_resolution, "Maritime Dispute Resolution Framework").
narrative_ontology:topic_domain(maritime_dispute_resolution, "international_law/ocean_governance").

domain_priors:requires_active_enforcement(maritime_dispute_resolution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maritime_dispute_resolution, established_maritime_powers).
narrative_ontology:constraint_beneficiary(maritime_dispute_resolution, institutional_arbitration_bodies).
narrative_ontology:constraint_victim(maritime_dispute_resolution, developing_coastal_states).
narrative_ontology:constraint_victim(maritime_dispute_resolution, indigenous_fishing_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS FISHING COMMUNITIES (SNARE) — Cannot exit maritime dispute resolution; dependent on ocean access for subsistence. Trapped by lack of legal standing in formal arbitration, language barriers, and inability to afford international legal representation. Bears full cost of exclusion from dispute mechanisms that determine ocean access rights.
constraint_indexing:constraint_classification(maritime_dispute_resolution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING COASTAL STATES (TANGLED ROPE) — Constrained by limited financial and technical capacity for international arbitration. Benefit from the existence of rules-based dispute framework (coordinates maritime order, provides alternative to military conflict) but experience asymmetric costs: expensive legal processes, reliance on external expertise, and enforcement mechanisms that favor established powers.
constraint_indexing:constraint_classification(maritime_dispute_resolution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL ARBITRATION BODIES (ROPE) — Institutional coordination mechanism. Arbitrators experience the constraint as legitimate dispute resolution coordination. High institutional prestige and fee-based revenue. Genuine coordination function: providing neutral forum prevents military escalation and provides rules for ocean access.
constraint_indexing:constraint_classification(maritime_dispute_resolution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ESTABLISHED MARITIME POWERS (ROPE) — Benefit from the rules-based framework that codifies existing advantage (established shipping routes, naval capacity, technological capability). Have exit options (can ignore adverse rulings through naval force projection) but find formal dispute resolution less costly than military conflict. Experience coordination benefit: predictable legal framework enables commerce.
constraint_indexing:constraint_classification(maritime_dispute_resolution, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OCEAN GOVERNANCE REFORM COALITION (SCAFFOLD) — Organized states and NGOs advocating for expanded legal standing for indigenous communities and subsidized arbitration access. See the current framework as temporary; working toward sunset through expanded UNCLOS protocols and alternative dispute mechanisms. Suppression declines as alternative pathways mature.
constraint_indexing:constraint_classification(maritime_dispute_resolution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY UNCLOS INSTITUTIONAL STRUCTURE (PITON) — The 1982 UN Convention on the Law of the Sea established dispute resolution mechanisms that served functional purposes for the 1980s-2000s. Now substantially theatrical: the framework persists through institutional inertia, states treat arbitration as legitimacy ritual rather than binding, and enforcement mechanisms are performative (ICJ rulings ignored by major powers). Theater ratio (0.61) reflects this degradation.
constraint_indexing:constraint_classification(maritime_dispute_resolution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, ocean governance may appear as an immutable constraint: the high seas are ungoverned space; sovereignty prevents supranational enforcement; physical geography determines resource distribution. However, structural data contradicts this — the framework is contingent on institutional arrangements (UNCLOS protocols, arbitration body funding, state cooperation) that vary over time and differ across regions. False summit: naturalizes what is institutional.
constraint_indexing:constraint_classification(maritime_dispute_resolution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maritime_dispute_resolution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maritime_dispute_resolution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maritime_dispute_resolution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(maritime_dispute_resolution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(maritime_dispute_resolution, TR),
    TR >= 0.70.

:- end_tests(maritime_dispute_resolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The framework generates asymmetric costs: developing states pay 5-10x per-capita more for arbitration than established powers; indigenous communities face prohibitive barriers; but the framework does provide alternatives to military conflict, which has net positive externality value. The extractiveness has increased over the 40-year interval (0.35→0.52) as the framework's functional capacity has degraded and its institutional capture has strengthened. Suppression (0.48): Moderate. Barriers to participation include arbitration costs ($5-50M), language requirements, need for specialized legal expertise, and exclusion of non-state actors. But developing states can participate and have (South China Sea cases, maritime boundary disputes), and some cost-sharing mechanisms exist. Suppression is not total. Theater ratio (0.61): Moderate-high. The framework exhibits increasing performativity: states file cases partly for legitimacy/signal value; arbitration rituals (written pleadings, oral hearings, deliberation) are extensive; but enforcement depends on power dynamics rather than legal force. Major powers ignore adverse rulings (Philippines v. China arbitration); smaller states comply more reliably. Theater has increased over the interval as enforcement credibility has declined.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates high perspectival variance from the same base properties. Established maritime powers see a successful coordination mechanism (Rope) — the framework enables commerce and prevents escalation. Developing states see mixed coordination and extraction (Tangled Rope) — rules exist but access is costly. Indigenous communities see pure extraction (Snare) — they are excluded from a mechanism that determines ocean access. Reform coalitions see a temporary system with a sunset (Scaffold) — emerging alternatives will replace the UNCLOS framework. The legacy institutional structure sees itself as degraded theater (Piton) — the framework persists through ritual rather than function. The analytical observer risks seeing immutable ocean governance (Mountain) — sovereignty prevents supranational enforcement — but the structural data reveals contingent institutional choices. The gap reflects real power asymmetries embedded in the framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and power-exit combinations. Indigenous communities (powerless/trapped) bear maximum d→high f(d)→high experienced extraction. Developing states (moderate/constrained) face moderate-high d from victim status but constrained exit moderates chi. Established powers (powerful/mobile) benefit structurally; arbitrage exit makes them net beneficiaries with low d. Arbitration bodies (institutional/arbitrage) benefit from institutional position with very low d. Reform coalition (organized/constrained) has agency and exit pathways (alternative mechanisms), so constrained exit limits experienced extraction. Legacy institutional structure (institutional/arbitrage) benefits from inertia but shows signs of capture, potentially justifying a directionality override upward to reflect that the structure increasingly serves established power interests rather than neutral coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES THROUGH PERSPECTIVE: This constraint avoids mandatrophy by recognizing that the six types represent different structural positions relative to the same framework. Established powers legitimately see Rope — the coordination function is real for them. Powerless communities legitimately see Snare — they experience extraction without alternative. The tangled_rope classification (claimed type) is appropriate from the macro-institutional perspective: the framework is both coordination and extraction, functioning as each for different agents. The manifest function (coordination) is genuine; the latent function (power codification) is equally real. No single type explains the constraint; the perspectival structure IS the explanation. The mandatrophy resolves by accepting that maritime dispute resolution simultaneously IS coordination and extraction from different vantage points.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_reality,
    'Are international maritime arbitration decisions actually enforced, or do states comply only when convenient?',
    'Longitudinal analysis of compliance with ICJ/ITLOS rulings; correlation between ruling outcome and state behavior; comparison with internal enforcement costs vs external pressure',
    'If genuinely enforced: rope classification confirmed across perspectives. If mainly performative: snare and piton classifications dominate; framework is theater masking power asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_reality, empirical, 'Whether international maritime dispute decisions are actually enforced').

omega_variable(
    indigenous_participation_mechanisms,
    'Are emerging mechanisms for indigenous legal standing (amicus curiae briefs, observer status, specialized tribunals) sufficient to move indigenous communities from trapped to constrained exit options?',
    'Case analysis of indigenous participation in recent maritime disputes; measurement of barriers (cost, legal accessibility, language); comparison with developing state participation metrics',
    'If barriers remain prohibitive: snare classification persists. If mechanisms materially reduce barriers: classification shifts toward tangled rope for indigenous communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_participation_mechanisms, empirical, 'Effectiveness of indigenous legal participation mechanisms').

omega_variable(
    institutional_capture_extent,
    'To what degree has maritime arbitration been captured by established maritime powers through institutional mechanisms (arbitrator selection, precedent interpretation, funding control)?',
    'Comparative institutional analysis: win rates by state power level; arbitrator origin/career patterns; funding source analysis for arbitration bodies; discourse analysis of rulings for power-consistent bias',
    'If high capture: extraction increases, moving constraint toward snare. If low capture: rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_extent, empirical, 'Degree of institutional capture by established maritime powers').

omega_variable(
    alternative_dispute_pathways_viability,
    'Are emerging alternatives (bilateral agreements, regional tribunals, blockchain-based maritime contracts) viable enough to constitute a genuine sunset for the UNCLOS framework?',
    'Growth trajectory analysis of alternative mechanisms; adoption rates by developing states; cost and accessibility comparison with traditional arbitration',
    'If alternatives are viable: scaffold perspective is realistic, sunset is real. If alternatives remain marginal: scaffold is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_dispute_pathways_viability, empirical, 'Viability of alternative maritime dispute mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maritime_dispute_resolution, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marit_tr_t0, maritime_dispute_resolution, theater_ratio, 0, 0.38).
narrative_ontology:measurement(marit_tr_t20, maritime_dispute_resolution, theater_ratio, 20, 0.5).
narrative_ontology:measurement(marit_tr_t40, maritime_dispute_resolution, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(marit_be_t0, maritime_dispute_resolution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(marit_be_t20, maritime_dispute_resolution, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(marit_be_t40, maritime_dispute_resolution, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maritime_dispute_resolution, enforcement_mechanism).
narrative_ontology:affects_constraint(maritime_dispute_resolution, fishing_rights_allocation).
narrative_ontology:affects_constraint(maritime_dispute_resolution, territorial_sovereignty_claims).
narrative_ontology:affects_constraint(maritime_dispute_resolution, deep_sea_mineral_extraction).

% DUAL FORMULATION NOTE:
% Maritime dispute resolution can be decomposed into three structurally distinct constraints: (1) the formal arbitration framework (this story, ε=0.52), (2) the underlying sovereignty claim enforcement (ε variable depending on state power), and (3) the actual fishing/resource allocation outcomes (ε dependent on regime type). This story models the mechanism's institutional structure. Downstream constraints inherit this structure as context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maritime_dispute_resolution, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
