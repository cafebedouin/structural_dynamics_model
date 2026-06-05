% ============================================================================
% CONSTRAINT STORY: south_china_sea_maritime_claims
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_south_china_sea_maritime_claims, []).

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
 *   constraint_id: south_china_sea_maritime_claims
 *   human_readable: South China Sea Maritime Claims Constraint
 *   domain: geopolitical/maritime_law/economic
 *
 * SUMMARY:
 *   The South China Sea maritime claims constraint represents a hybrid
 *   coordination-extraction mechanism operating across multiple institutional
 *   levels: bilateral state negotiations, regional governance forums (ASEAN,
 *   CLCS), international arbitration bodies (UNCLOS machinery), and global
 *   shipping infrastructure. The constraint exhibits genuine coordination
 *   functions (establishing shipping safety rules, resource-sharing
 *   frameworks, dispute-resolution mechanisms) while simultaneously
 *   extracting value through claim assertion, military enforcement, and
 *   geopolitical leverage. The extractiveness has increased over the measured
 *   interval (0.35 → 0.58) as disputes have intensified and militarization
 *   has expanded. The theater ratio (0.48) reflects a moderate balance
 *   between functional governance (actual enforcement of fishing regulations,
 *   resource extraction) and performative assertion (legislative claims,
 *   diplomatic repetition, administrative theater). The constraint satisfies
 *   Tangled Rope criteria: genuine coordination function (navigation safety,
 *   resource allocation frameworks exist in bilateral agreements), active
 *   enforcement (military, coast guard, coast guard-adjacent vessels),
 *   beneficiaries (major claimant states, resource-extracting companies,
 *   arbitrage actors), and victims (small island states, fishing communities,
 *   global shipping facing risk premia).
 *
 * KEY AGENTS:
 *   - Major Claimant Powers (China, Vietnam): Institutional/arbitrage beneficiaries — control resources, set enforcement priorities, extract geopolitical leverage without bearing suppression costs
 *   - Smaller Claimant States (Philippines, Malaysia, Brunei, Indonesia): Moderate/constrained participants — benefit from legitimacy of claims but constrained by power asymmetries in negotiation and enforcement
 *   - Small Island States and Fishing Communities: Powerless/trapped victims — face restricted access, licensing fees, military interdiction, no alternative routes or resources
 *   - International Maritime Governance Coalition: Organized/constrained actors — UNCLOS signatories, ASEAN, international arbitration bodies building alternative settlement pathways
 *   - Global Commercial Shipping: Organized/constrained actors — face risk premiums, insurance surcharges, rerouting costs; benefit if claims are settled
 *   - Analytical Observer: Civilizational/analytical perspective — risks naturalizing contingent institutional arrangements as immutable geopolitical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(south_china_sea_maritime_claims, 0.58).
domain_priors:suppression_score(south_china_sea_maritime_claims, 0.65).
domain_priors:theater_ratio(south_china_sea_maritime_claims, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(south_china_sea_maritime_claims, extractiveness, 0.58).
narrative_ontology:constraint_metric(south_china_sea_maritime_claims, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(south_china_sea_maritime_claims, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(south_china_sea_maritime_claims, tangled_rope).
narrative_ontology:human_readable(south_china_sea_maritime_claims, "South China Sea Maritime Claims Constraint").
narrative_ontology:topic_domain(south_china_sea_maritime_claims, "geopolitical/maritime_law/economic").

domain_priors:requires_active_enforcement(south_china_sea_maritime_claims).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(south_china_sea_maritime_claims, claimant_states).
narrative_ontology:constraint_beneficiary(south_china_sea_maritime_claims, resource_control_beneficiaries).
narrative_ontology:constraint_beneficiary(south_china_sea_maritime_claims, shipping_arbitrage_actors).
narrative_ontology:constraint_victim(south_china_sea_maritime_claims, constrained_maritime_users).
narrative_ontology:constraint_victim(south_china_sea_maritime_claims, regional_security_commons).
narrative_ontology:constraint_victim(south_china_sea_maritime_claims, international_law_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL ISLAND STATES / FISHING COMMUNITIES (SNARE) — Trapped within competing maritime claims with no alternative navigation routes or fishing zones. Experience maximum extraction through restricted access, licensing fees, military interdiction, and loss of traditional waters. No exit option; bears full cost of claim asymmetry. Suppression operates through military presence, administrative control, and legal proceedings initiated by claimant states.
constraint_indexing:constraint_classification(south_china_sea_maritime_claims, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALLER CLAIMANT STATES (TANGLED ROPE) — Constrained by power asymmetries and resource limitations but participate in claims coordination and access to disputed resources. Genuine coordination function exists (bilateral maritime agreements, resource-sharing frameworks), but extraction is asymmetric—larger claimants dominate negotiation outcomes and resource allocation. Benefits from the constraint structure through legitimacy claims and resource access; simultaneously bears enforcement costs and security risks.
constraint_indexing:constraint_classification(south_china_sea_maritime_claims, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MAJOR CLAIMANT POWERS (ROPE) — Experience the constraint as pure coordination of strategic interests and resource control. Benefits from the claim structure without bearing suppression costs; military capacity and diplomatic leverage enable arbitrage (threat credibility, exemptions for friendly vessels). Low experienced extraction—the constraint subsidizes their position through geopolitical advantage. Extraction runs toward these actors.
constraint_indexing:constraint_classification(south_china_sea_maritime_claims, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL MARITIME GOVERNANCE COALITION (SCAFFOLD) — Organized actors (UNCLOS signatories, international arbitration bodies, regional forums like ASEAN) work to establish alternative coordination pathways through adjudication, code-of-conduct frameworks, and multilateral agreements. See the overlapping claims as a temporary coordination failure resolvable through legal/diplomatic mechanisms. Low theater because the coordination function is genuine (navigation safety, resource sharing rules). Sunset logic: binding arbitration and agreed maritime demarcation would replace claim assertion with institutional settlement.
constraint_indexing:constraint_classification(south_china_sea_maritime_claims, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL TERRITORIAL DOCTRINE (PITON) — The 'nine-dash line' and comparable historical claims are largely performative—maintained through legislative assertion, diplomatic repetition, and administrative theater rather than through functional governance or resource extraction that depends on the specific claim boundaries. The doctrine persists due to institutional inertia (embedded in national identity narratives, military doctrines, education systems) despite weak international legal standing (2016 Hague tribunal invalidation). Theater ratio reflects the gap between claim assertion and actual capacity to govern the claimed space. The constraint is degraded—the original functional purpose (securing fisheries and resources) is now secondary to the political signaling function.
constraint_indexing:constraint_classification(south_china_sea_maritime_claims, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL COMMERCIAL SHIPPING (TANGLED ROPE) — Organized actors with high coordination incentives (predictable routes, stable regulations) but also extraction through uncertainty premiums, insurance surcharges, and rerouting costs. The constraint provides genuine coordination function (navigational clarity, reduced collision risk if claims were settled), but current overlapping-claim structure prevents this—instead extracting value through risk premia and rerouting inefficiencies. Benefits from the constraint only insofar as alternative routes exist; constrained by higher-cost pathways and security overhead.
constraint_indexing:constraint_classification(south_china_sea_maritime_claims, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REALIST GEOPOLITICS (MOUNTAIN) — From a civilizational realist perspective, overlapping maritime claims are an immutable feature of international anarchy: without world government, states pursue maximal territorial control, and overlapping claims are a structural inevitability. This perspective treats the constraint as a natural law of geopolitics—power distributions determine claim viability, not international law. However, the structural data reveals this as false naturalization: the constraint is institutional (enforceable through military, diplomatic, and economic mechanisms) and contingent (historical accidents of claim timing, technological change in resource extraction, evolution of international law). The engine will flag this as a false summit.
constraint_indexing:constraint_classification(south_china_sea_maritime_claims, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(south_china_sea_maritime_claims_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(south_china_sea_maritime_claims, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(south_china_sea_maritime_claims, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(south_china_sea_maritime_claims, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(south_china_sea_maritime_claims, TR),
    TR >= 0.70.

:- end_tests(south_china_sea_maritime_claims_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value through multiple mechanisms: major claimant states capture resource control and geopolitical leverage; smaller claimants and commercial actors pay enforcement costs and navigational uncertainty premiums; powerless agents (small island states, fishing communities) lose traditional access rights. The value is not maximal (0.70+) because legitimate coordination functions exist—bilateral agreements on fishing zones, resource-sharing frameworks, and shipping safety protocols do produce genuine coordination goods that offset some extraction. The increase over the interval reflects intensifying militarization and claim assertion. Suppression (0.65): High. Multiple mechanisms: military presence in disputed zones, coast guard enforcement, administrative control through licensing and permitting systems, legal proceedings initiated by claimant states, investment restrictions on resource projects. However, suppression is not total (0.80+) because some agents (major powers) can exit through military capacity, and alternative routes exist for global shipping. Theater ratio (0.48): Moderate. The constraint has genuine functional content (fishing regulations enforced, resource surveys conducted, shipping corridors maintained) but also significant performative content (legislative assertion of nine-dash line, diplomatic repetition, symbolic military patrols). The ratio has increased over the interval as political signaling has intensified relative to actual resource extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. Major claimant powers see coordination with subsidy (Rope)—solving the legitimate problem of resource allocation and security. The international governance coalition sees a temporary coordination failure with a clear sunset (Scaffold)—UNCLOS-based arbitration and code-of-conduct frameworks provide exit pathways. Smaller claimants see mixed coordination and extraction with constrained agency (Tangled Rope)—they benefit from claim legitimacy and resource access but are dominated in negotiation outcomes. Powerless agents (small island states, fishing communities) see pure extraction with no alternatives (Snare)—they lose traditional access rights with no exit option. The piton perspective recognizes that historical territorial claims are increasingly performative (legislative assertion, diplomatic repetition) rather than functionally justified. The realist analytical perspective risks treating this contingent institutional arrangement as an immutable law of geopolitics. The perspectival gap reveals that the constraint is not a 'natural' feature of international relations but a specific institutional arrangement that could be replaced by alternative coordination mechanisms (binding arbitration, code of conduct, demarcation agreements).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's position in the extraction flow. China and Vietnam as major military and economic powers occupy d ≈ 0.15-0.25 (net beneficiary with arbitrage capacity), producing low effective extraction. Smaller claimants (Philippines, Malaysia) occupy d ≈ 0.50-0.55 (symmetric position with moderate leverage), producing moderate effective extraction. Small island states and fishing communities occupy d ≈ 0.85+ (full target, trapped exit), producing high effective extraction. The constraint structure forces power asymmetries into economic and military channels, creating steep directionality gradients. International actors (shipping, arbitration bodies) occupy d ≈ 0.60-0.70 (moderate targets facing cost premium), producing moderate-high extraction. The engine derives these values from beneficiary/victim declarations plus power atom and exit options—the analytical leverage comes from measuring perspectival gaps as evidence of extraction asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing how the same structure produces coordination goods (shipping safety rules, resource allocation frameworks in bilateral agreements) while simultaneously extracting value through military enforcement and claim assertion. Major claimant powers see genuine coordination (Rope perspective) because they ARE the beneficiaries of claim structure. Smaller claimants and trapped agents see extraction (Tangled Rope and Snare) because they bear suppression costs without proportional benefit. The international governance coalition sees a resolvable coordination problem (Scaffold) because organized agents with legal authority can establish alternative frameworks. The historical claims doctrine perspective (Piton) shows how institutional inertia maintains political theater beyond functional purpose. The realist analytical perspective risks naturalizing the constraint as an immutable feature of international anarchy, but the structural data contradicts this—the constraint is institutionally enforced and could be displaced by UNCLOS-based mechanisms or regional agreements. The mandatrophy is resolved not by declaring a single 'true' type but by recognizing that the presheaf of perspectives (Rope → Tangled Rope → Snare → Scaffold → Piton → Mountain-false) accurately represents the structural reality: a hybrid coordination-extraction mechanism with asymmetric beneficiary distribution and viable alternative pathways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_resource_control_vs_claim_theater,
    'What proportion of extracted value derives from actual resource control (fisheries, mineral rights, energy reserves) versus geopolitical signaling and claim assertion theater?',
    'Economic accounting of resource extraction by zone, cross-reference with historical claim salience; measurement of enforcement intensity relative to resource density',
    'If resource control > 70%: constraint is primarily economic (Tangled Rope dominates). If claim theater > 50%: constraint is primarily political (Piton prevalence increases). Classification stability depends on this ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_resource_control_vs_claim_theater, empirical, 'Proportion of extraction from resource control versus geopolitical signaling').

omega_variable(
    enforcement_escalation_threshold,
    'At what point do overlapping claims trigger military escalation versus settlement through arbitration or bilateral negotiation?',
    'Incident analysis: correlation between claim overlap intensity and military intercept frequency; temporal analysis of escalation thresholds pre/post 2016 arbitration',
    'If threshold is low (escalates easily): suppression classification is correct (0.65+). If threshold is high (escalates rarely): suppression may be overstated; constraint functions more as Rope than Snare for most affected agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_escalation_threshold, empirical, 'Escalation threshold for military enforcement of maritime claims').

omega_variable(
    code_of_conduct_viability,
    'Can a code of conduct on conduct in South China Sea (CoC) actually replace overlapping claims as the coordination mechanism, or does it merely layer additional institutional theater on top of unresolved claims?',
    'Post-implementation monitoring of CoC compliance rates, incident reduction, settlement success; comparative analysis with similar regional frameworks (Arctic, Caribbean)',
    'If CoC functions: Scaffold perspective is correct—genuine sunset mechanism. If CoC becomes theater: constraint migrates toward Piton (institutional inertia layers theater onto unresolved structure). Classification chain shifts accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(code_of_conduct_viability, empirical, 'Whether code of conduct can replace overlapping claims as coordination mechanism').

omega_variable(
    claim_legitimacy_authority,
    'Is international maritime law (UNCLOS) or realpolitik (effective control and military capacity) the binding authority for claim legitimacy in this constraint structure?',
    'Outcome analysis: do settlements follow UNCLOS interpretations or de facto power distributions? Longitudinal tracking of claim enforcement success correlated with legal standing versus military capacity.',
    'If UNCLOS dominates: constraint is reclassifiable through law (Scaffold optimism). If realpolitik dominates: constraint is structurally anarchic (Mountain naturalization). Directionality derivation shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(claim_legitimacy_authority, conceptual, 'Whether international law or realpolitik determines claim legitimacy').

omega_variable(
    multilateral_vs_bilateral_preference,
    'Do claimant states have structural incentive to resolve claims multilaterally (through UNCLOS-based arbitration) or do power asymmetries push toward bilateral negotiations where major powers extract greater concessions?',
    'Strategic analysis of settlement offers and negotiation patterns; game-theoretic modeling of payoffs under multilateral vs bilateral resolution',
    'If multilateral preferred: Scaffold logic strengthens (organized coalition can enforce settlement). If bilateral preferred by major powers: extraction deepens (Snare from minor states'' perspective); constraint hardens toward Tangled Rope with embedded asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilateral_vs_bilateral_preference, preference, 'Whether claimant incentives favor multilateral or bilateral resolution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(south_china_sea_maritime_claims, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scs_tr_t0, south_china_sea_maritime_claims, theater_ratio, 0, 0.32).
narrative_ontology:measurement(scs_tr_t5, south_china_sea_maritime_claims, theater_ratio, 5, 0.4).
narrative_ontology:measurement(scs_tr_t10, south_china_sea_maritime_claims, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(scs_be_t0, south_china_sea_maritime_claims, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scs_be_t5, south_china_sea_maritime_claims, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(scs_be_t10, south_china_sea_maritime_claims, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(south_china_sea_maritime_claims, resource_allocation).
narrative_ontology:affects_constraint(south_china_sea_maritime_claims, freedom_of_navigation_dispute).
narrative_ontology:affects_constraint(south_china_sea_maritime_claims, east_china_sea_maritime_sovereignty).
narrative_ontology:affects_constraint(south_china_sea_maritime_claims, asean_regional_security_framework).

% DUAL FORMULATION NOTE:
% The South China Sea maritime claims decompose into multiple structurally distinct constraints: (1) overlapping territorial/EEZ claims (this story, ε=0.58, Tangled Rope), (2) freedom of navigation interpretation dispute (downstream, higher ε, primarily Snare), and (3) code of conduct negotiation (parallel, scaffolding structure). Each has distinct beneficiary/victim profiles and temporal trajectories. Network links indicate causal dependency and institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(south_china_sea_maritime_claims, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
