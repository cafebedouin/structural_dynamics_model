% ============================================================================
% CONSTRAINT STORY: roman_road_network
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_road_network, []).

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
 *   constraint_id: roman_road_network
 *   human_readable: The Roman Road Network as a Mechanism of Imperial Control and Economic Integration
 *   domain: technological/political/economic
 *
 * SUMMARY:
 *   The Roman road network, constructed between approximately 300 BCE and 300
 *   CE, represents a monumental infrastructure project that simultaneously
 *   enabled economic integration, military logistics, and imperial control.
 *   From one perspective, roads are a pure coordination mechanism solving the
 *   collective action problem of connecting dispersed populations to larger
 *   markets and reducing transaction costs. From another perspective, roads
 *   are a mechanism of extraction and suppression—enabling Roman armies to
 *   reach and control peripheral populations, facilitating tax collection,
 *   and subordinating local trade networks to imperial priorities. The
 *   constraint exhibits the full range of Deferential Realism types depending
 *   on the observer's structural position. The extractiveness increased over
 *   the interval as Rome's administrative capacity and financial demands
 *   grew, and as the roads' original purpose (military logistics) became
 *   increasingly entangled with extraction functions (taxation, resource
 *   requisition). Theater ratio remained relatively low (0.48) because roads
 *   served genuine functional purposes—transportation infrastructure always
 *   has concrete use-value—but increased as the performative aspects
 *   (monumental paving, tollgates, symbolic naming) became more prominent.
 *
 * KEY AGENTS:
 *   - Roman Military: Primary beneficiary (institutional/arbitrage) — roads enable logistical capability and rapid response to provincial threats
 *   - Imperial Treasury: Primary beneficiary (institutional/arbitrage) — roads enable efficient taxation and resource extraction
 *   - Provincial Farmers: Primary victim (powerless/trapped) — bear labor costs of road maintenance and vulnerability to military requisition
 *   - Provincial Merchants: Mixed agent (moderate/constrained) — benefit from trade access but constrained by tariffs and monopolies
 *   - Provincial Elite: Secondary actor (organized/constrained) — benefit from administrative integration but subordinated to Roman authority
 *   - Local Regional Networks: Victim (powerless/constrained) — autonomous pre-Roman trade routes suppressed by imperial standardization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent political choice as engineering necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_road_network, 0.58).
domain_priors:suppression_score(roman_road_network, 0.65).
domain_priors:theater_ratio(roman_road_network, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_road_network, extractiveness, 0.58).
narrative_ontology:constraint_metric(roman_road_network, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(roman_road_network, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_road_network, tangled_rope).
narrative_ontology:human_readable(roman_road_network, "The Roman Road Network as a Mechanism of Imperial Control and Economic Integration").
narrative_ontology:topic_domain(roman_road_network, "technological/political/economic").

domain_priors:requires_active_enforcement(roman_road_network).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_road_network, roman_military).
narrative_ontology:constraint_beneficiary(roman_road_network, central_imperial_administration).
narrative_ontology:constraint_beneficiary(roman_road_network, merchant_traders).
narrative_ontology:constraint_beneficiary(roman_road_network, roman_aristocracy).
narrative_ontology:constraint_victim(roman_road_network, provincial_populations).
narrative_ontology:constraint_victim(roman_road_network, local_autonomy).
narrative_ontology:constraint_victim(roman_road_network, regional_trade_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL FARMER (SNARE) — Trapped within the road network's logistical grid. Roads enable Roman tax collectors and military requisitions to reach remote territories. The farmer bears extraction costs (forced labor on road maintenance, resource appropriation for military supply) with minimal benefits. No exit option: the roads themselves eliminate isolation that might have provided protective distance. Maximum directionality toward victimhood.
constraint_indexing:constraint_classification(roman_road_network, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PROVINCIAL MERCHANT (TANGLED ROPE) — Benefits from road access to larger markets and reduced transport costs. Simultaneously constrained by Roman monopolies on certain goods (purple dye, salt), tariff collection at road stations, and the threat of military requisition. Mixed coordination and extraction: roads solve the collective action problem of market access, but asymmetric extraction flows toward Rome. Constrained exit — merchant can operate within the system but cannot easily escape it.
constraint_indexing:constraint_classification(roman_road_network, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ROMAN MILITARY LOGISTICS COMMAND (ROPE) — Pure coordination mechanism from this perspective. Roads solve the fundamental problem of moving legions and supplies across vast territories in predictable timeframes. The military experiences the roads as a coordination solution enabling expeditionary capability. Has arbitrage options: can redirect roads or logistics strategy without losing access to the fundamental resource (territorial control). Net beneficiary without significant extraction burden.
constraint_indexing:constraint_classification(roman_road_network, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IMPERIAL TREASURY (ROPE) — Roads enable tax collection efficiency and the monetization of regional differentiation through tariff stations. However, from Rome's central perspective, the roads are primarily a coordination mechanism enabling the extraction of value from the broader empire. Treasury experiences roads as infrastructure solving the logistical coordination problem of resource concentration. Arbitrage exit: taxation can be adjusted without losing road access.
constraint_indexing:constraint_classification(roman_road_network, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-ROMAN ROAD MAINTENANCE (PITON) — After Roman administrative collapse, roads persisted for centuries through local and regional maintenance efforts. Theater ratio high: maintaining Roman roads became a vestigial activity, often performed for local coordination reasons (connecting market towns, enabling pilgrimage) rather than for the original imperial logistics function. The roads' primary function atrophied, but the infrastructure remained due to path dependence. Medieval communities maintained roads not because the Roman extraction mechanism remained, but because roads were already there and useful for different purposes.
constraint_indexing:constraint_classification(roman_road_network, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PROVINCIAL ELITE (TANGLED ROPE) — Local aristocrats benefit from roads through trade access, military appointments, and administrative power delegated by Rome. However, they are simultaneously constrained by the requirement to maintain order and infrastructure, suppression of local autonomy, and subordination to Roman authority. Roads enable their enrichment but also eliminate the autonomy they would possess without Roman integration. Active enforcement required: Rome uses roads to project power against rebellious elites. Mixed coordination (enabling commerce and administration) and extraction (subordination and resource flow to Rome).
constraint_indexing:constraint_classification(roman_road_network, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, roads represent an engineering solution to the logistical problem of controlling large territories: any empire of Rome's geographic scale faces the irreducible constraint that information and supply movement are limited by transportation speed. Roads are presented as a natural law of imperial necessity. However, this naturalizes what is actually a contingent political choice: smaller empires, federated systems, and tribute networks solved territorial control problems differently. The classification as mountain is a false summit—roads are not a law of nature but a particular institutional solution.
constraint_indexing:constraint_classification(roman_road_network, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_road_network_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_road_network, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_road_network, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_road_network, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_road_network, TR),
    TR >= 0.70.

:- end_tests(roman_road_network_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Roads enabled Roman military mobility and tax collection efficiency, creating significant extraction mechanisms for peripheral populations. However, roads also generated genuine trade benefits and economic integration that reduced transaction costs. The value reflects that extraction was substantial but not absolute—roads served mixed coordination and extraction functions. The increase from 0.35 to 0.62 over the interval reflects Rome's expanding financial demands and the progressive entrenchment of road-based extraction systems (tariff stations, military supply requisitions). Suppression (0.65): High. Roads operated within a framework of military threat, legal coercion for labor and supply, and elimination of exit options for local autonomous systems. Provinces could not maintain autonomy if they lacked the infrastructure to resist Roman logistics. Suppression increased as Rome standardized road specifications and intensified enforcement. Theater ratio (0.48): Moderate. Roads served genuine functional purposes—transportation efficiency is real and measurable. However, an increasing portion became performative: monumental construction, symbolic naming, ceremonial processions. The theater ratio increased as roads became symbols of Roman power rather than primarily optimized logistics. By the late imperial period, some roads were maintained more for prestige than efficiency.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. The imperial military sees a pure coordination solution (Rope)—roads solve the transportation problem with minimal perceived extraction. The provincial farmer sees pure extraction (Snare)—bearing labor costs and vulnerability with no meaningful benefit. The provincial merchant experiences genuine mixed benefits and constraints (Tangled Rope)—roads enable trade but are exploited for taxation. The provincial elite experience subordination despite enrichment (Tangled Rope)—integration benefits them materially but suppresses autonomy. The post-Roman maintenance perspective (Piton) reveals that once the extraction mechanism (Roman military demand) disappeared, roads persisted through inertia rather than function, maintained for different coordination purposes. The analytical observer risks naturalizing roads as a law of nature ('any large empire needs roads') when they represent a particular political choice. This perspectival gap is not measurement error but genuine structural divergence—different agents truly experience the same infrastructure differently based on their relationship to extraction flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to extraction flows. The military and treasury see roads as enabling coordination (beneficiaries with arbitrage options—they can adjust strategy without losing road access). Directional value d ≈ 0.1-0.2, producing low/negative experienced extraction. Provincial farmers see roads as enabling their subjugation (victims trapped without exit options). Directional value d ≈ 0.85-0.95, producing maximum experienced extraction. Provincial merchants experience mixed positioning: benefiting from trade access but constrained by tariffs and monopolies. Directional value d ≈ 0.55-0.65, producing moderate extraction. Provincial elites experience subordination despite material benefit: constrained exit despite enrichment. Directional value d ≈ 0.50-0.60, producing moderate extraction. The piton perspective derives from theater ratio exceeding 0.70 in late periods: the original function (military logistics) attenuated while the constraint persisted through institutional inertia. The mountain perspective represents a false summit: the analytical view risks naturalizing what is a contingent institutional choice as a law of nature.
 *
 * MANDATROPHY ANALYSIS:
 *   The Roman road network resolves mandatrophy by demonstrating that roads are genuinely a Tangled Rope—they combine real coordination benefits (trade, economic integration, information flow) with genuine extraction mechanisms (military logistics, taxation, resource requisition, suppression of autonomy). The mandatrophy question 'is this coordination or extraction?' has the answer: both. The roads are not mislabeled as pure coordination (Rope)—they actively enforce asymmetric extraction. They are not mislabeled as pure extraction (Snare)—they generate real economic benefits. The Tangled Rope classification accurately captures the hybrid nature. The perspectival gap demonstrates that different agents experience the hybrid nature differently: beneficiaries (military, treasury) perceive primarily coordination; victims (provincial farmers) perceive primarily extraction. The analytical observer's false summit (mountain) is specifically a mandatrophy failure—naturalizing a particular institutional arrangement as inevitable. Resolution: the roads are a contingent political choice that happened to combine coordination and extraction functions, not a law of nature imposed by transport physics. The network would have evolved differently if Rome had chosen other administrative models (federated tribute, smaller strategic strongholds, local autonomy with trade treaties).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_balance,
    'Did the economic benefits of road-enabled trade outweigh the extraction costs of military logistics and taxation for provincial populations?',
    'Economic historians'' analysis of price convergence, wage data, and archaeological evidence of consumption patterns in provinces. Comparison of provincial prosperity metrics before and after major road construction.',
    'If benefits > costs: roads classify as genuine coordination (Rope from provincial perspective). If extraction > benefits: roads classify as extraction mechanism (Snare from provincial perspective). Current evidence suggests regional variation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_balance, empirical, 'Whether road network benefits to provinces exceeded extraction costs').

omega_variable(
    local_autonomy_tradeoff,
    'To what extent did regional road networks replace autonomous local trade routes versus extending beyond them?',
    'Archaeological evidence of pre-Roman trade patterns; comparative analysis of route efficiency and distance. Examination of whether Roman roads primarily followed existing trade corridors or imposed new patterns.',
    'If roads follow existing patterns: weaker suppression of local autonomy, more Rope classification. If roads impose new patterns: stronger suppression, more Snare/Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(local_autonomy_tradeoff, empirical, 'Whether Roman roads built on or replaced autonomous local trade networks').

omega_variable(
    provincial_elite_alignment,
    'Did provincial elites genuinely benefit from road-enabled integration or were they primarily instruments of Roman extraction?',
    'Prosopographic analysis of provincial elite families'' wealth accumulation, office tenure, and rebellion patterns. Comparison of elites with and without positions in the Roman administrative structure.',
    'If genuine benefit: provincial elite see Rope or Tangled Rope. If primarily instruments: provincial elite see Snare despite nominal privilege. This determines whether the roads'' integration was consensual or coercive at the elite level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provincial_elite_alignment, empirical, 'Whether provincial elite genuinely benefited from road-enabled integration').

omega_variable(
    voluntary_participation_threshold,
    'What proportion of road maintenance and logistics support was extracted through coercion versus incentivized through voluntary participation?',
    'Legal text analysis of road maintenance obligations and labor requisition laws. Archaeological evidence of forced versus market-driven supply chains. Comparison of voluntary participation rates across provinces with different administrative autonomy.',
    'If >70% coercive: roads are primarily Snare/Tangled Rope. If >50% voluntary: roads approach Rope. Current evidence suggests high regional variation and significant coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_threshold, empirical, 'Proportion of road system support that was coerced versus voluntary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_road_network, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rrn_tr_t0, roman_road_network, theater_ratio, 0, 0.25).
narrative_ontology:measurement(rrn_tr_t100, roman_road_network, theater_ratio, 100, 0.4).
narrative_ontology:measurement(rrn_tr_t200, roman_road_network, theater_ratio, 200, 0.48).

% Extraction over time
narrative_ontology:measurement(rrn_be_t0, roman_road_network, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rrn_be_t100, roman_road_network, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(rrn_be_t200, roman_road_network, base_extractiveness, 200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_road_network, resource_allocation).
narrative_ontology:affects_constraint(roman_road_network, provincial_autonomy_suppression).
narrative_ontology:affects_constraint(roman_road_network, imperial_tax_collection_mechanism).

% DUAL FORMULATION NOTE:
% The Roman road network should be analyzed as a constraint family with two distinct claims: (1) roads as logistics infrastructure (ε ≈ 0.15, coordination-primary), and (2) roads as extraction mechanism enabling military and fiscal control (ε ≈ 0.65, extraction-primary). This story captures both within the Tangled Rope classification. Decomposition into separate stories is not warranted because the extraction and coordination functions are structurally inseparable—one cannot build Roman roads for logistics without creating tools for control, and one cannot extract as efficiently without roads. The single story with multiple perspectives better models this unity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_road_network, institutional, 0.15).
constraint_indexing:directionality_override(roman_road_network, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
