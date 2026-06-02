% ============================================================================
% CONSTRAINT STORY: technology_export_controls
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_export_controls, []).

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
 *   constraint_id: technology_export_controls
 *   human_readable: Technology Export Controls and Strategic Decoupling
 *   domain: geopolitical/economic/technology
 *
 * SUMMARY:
 *   Technology export controls represent a multi-layered constraint operating
 *   simultaneously as security coordination, economic rent protection, and
 *   strategic dominance mechanism. The constraint classifies differently
 *   across all six DR types depending on the observer's structural position.
 *   For controlling nations, it functions as coordination (rope) — managing
 *   alliance security and preventing rival technology diffusion. For
 *   restricted nations, it appears as pure extraction (snare) — blocking
 *   technology access without opportunity for exit or compensation. For
 *   exporting firms, it is tangled rope — both constraining markets and
 *   coordinating supply chain stability. For cold-war legacy institutions, it
 *   persists as piton — performative bureaucracy maintained through inertia
 *   despite degraded security function. For organized restricted-tier
 *   coalitions, it presents as tangled rope shifting toward rope — genuine
 *   coordination within alternative supply networks alongside extraction from
 *   external controls. The analytical observer risks naturalizing this as
 *   immutable strategic necessity, but the structural data reveals contingent
 *   institutional arrangements that could be reorganized through
 *   international frameworks, technology commons, or alternative coordination
 *   mechanisms. The extractiveness has risen from 0.32 to 0.58 over the
 *   measurement interval, driven by geopolitical intensification and
 *   decoupling pressures. Theater ratio has similarly risen from 0.48 to
 *   0.64, indicating increasing performativity as controls drift from
 *   security-focused restrictions toward broad economic decoupling.
 *
 * KEY AGENTS:
 *   - Controlling National Security Apparatus: Primary beneficiary (institutional/arbitrage) — manages strategic advantage, alliance coordination, technology diffusion control; experiences constraint as functional capability
 *   - Technology Exporting Firms: Primary victim and secondary beneficiary (moderate/constrained) — face market restrictions and compliance costs but benefit from coordination of supply chains and IP protection within permitted tiers
 *   - Restricted-Tier Nations: Primary victims (powerless/trapped at biographical horizon, organized/constrained at generational horizon) — bear technology gap costs with no exit options at short timescales; building indigenous capacity at longer scales
 *   - Allied Tier-1 Nations: Secondary victims and beneficiaries (powerful/mobile) — benefit from preferred technology access but constrained by alliance compliance; can exit at generational scale but choose coordination at biographical scale
 *   - Cold War Legacy Institutions: Institutional actors (institutional/arbitrage) — maintain performative compliance frameworks; persist through bureaucratic inertia; experience own degradation
 *   - Restricted-Tier Organized Coalitions: Emerging counterpower (organized/constrained) — developing indigenous alternatives, coordinating mutual technology sharing, building countervailing supply networks
 *   - Analytical Observer: Civilizational viewpoint (analytical/analytical) — risks naturalizing contingent geopolitical arrangements as immutable strategic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_export_controls, 0.58).
domain_priors:suppression_score(technology_export_controls, 0.68).
domain_priors:theater_ratio(technology_export_controls, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_export_controls, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_export_controls, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(technology_export_controls, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_export_controls, tangled_rope).
narrative_ontology:human_readable(technology_export_controls, "Technology Export Controls and Strategic Decoupling").
narrative_ontology:topic_domain(technology_export_controls, "geopolitical/economic/technology").

domain_priors:requires_active_enforcement(technology_export_controls).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_export_controls, controlling_nation_security_apparatus).
narrative_ontology:constraint_beneficiary(technology_export_controls, domestic_technology_firms_protected_from_competition).
narrative_ontology:constraint_victim(technology_export_controls, technology_exporting_firms).
narrative_ontology:constraint_victim(technology_export_controls, technology_importing_nations).
narrative_ontology:constraint_victim(technology_export_controls, global_supply_chain_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESTRICTED FOREIGN MARKETS (SNARE) — Nations in restricted tiers cannot exit the export control regime; they bear full cost of technology gap and cannot invest capital to catch up. No alternative sources accessible; maximum experienced extraction through forced technological obsolescence.
constraint_indexing:constraint_classification(technology_export_controls, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TECHNOLOGY EXPORTING FIRMS (TANGLED ROPE) — Face significant restrictions limiting market access but also benefit from coordination of supply chains within permitted tiers and from protection of intellectual property norms. High compliance costs but some market preservation through controlled access. Exit constrained by license revocation risk and reputational damage; benefits include managed competition.
constraint_indexing:constraint_classification(technology_export_controls, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CONTROLLING NATIONAL SECURITY APPARATUS (ROPE) — Manages regime as pure coordination mechanism: controlling strategic technology diffusion, maintaining alliance cohesion, preventing rival proliferation. Experiences constraints as functional capability coordination. High exit optionality through regulatory discretion; net beneficiary of constraint architecture.
constraint_indexing:constraint_classification(technology_export_controls, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALLIED TIER-1 NATIONS (TANGLED ROPE) — Structurally mobile (can indigenize technology, form alternatives) but face real coordination benefits from controlled supply integration and technology-sharing agreements. Genuine multi-function: alliance coordination plus embedded extraction of favorable technology access terms. Can exit at generational scale but choose not to at biographical timescales.
constraint_indexing:constraint_classification(technology_export_controls, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR LEGACY INSTITUTION (PITON) — Original COCOM (Coordinating Committee for Multilateral Export Controls) and successor regimes persist through institutional inertia long after original bipolarity dissolved. Theater ratio (0.64) reflects performative compliance enforcement, lengthy bureaucratic review cycles, and classification systems updated slowly relative to technology obsolescence. Function degraded but maintained through path dependence and alliance reputation.
constraint_indexing:constraint_classification(technology_export_controls, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: RESTRICTED-TIER NATIONS (ORGANIZED) (TANGLED ROPE) — Organized coalitions (BRICS, SCO, competing standards bodies) developing indigenous alternatives, coordinating technology sharing, and building countervailing supply chains. Constrained by initial technological gap but gaining agency through coordinated domestic investment. See both extraction (technology denial) and genuine coordination (mutual learning networks). Classification shifts toward rope as capability accumulates.
constraint_indexing:constraint_classification(technology_export_controls, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, technology diffusion control appears as an immutable feature of strategic competition: asymmetric technological advantage creates security externalities, and managing diffusion is inherent to statecraft. This perspective naturalizes the regime as inevitable. However, the structural data reveals this as false naturalization: export controls are institutional constructs maintained through active enforcement, beneficiary capture, and suppressible through alternative institutions (multinational frameworks, technology commons).
constraint_indexing:constraint_classification(technology_export_controls, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_export_controls_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technology_export_controls, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technology_export_controls, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_export_controls, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(technology_export_controls, TR),
    TR >= 0.70.

:- end_tests(technology_export_controls_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value from restricted nations through forced technology lags, increased R&D costs for indigenous development, and supply chain dependency. However, extraction is not total — some technology transfer occurs through allied channels, and decoupling costs are emerging for controlling nations. The rise from 0.32 to 0.58 reflects geopolitical intensification and tightening of controls post-2022. Suppression (0.68): High. Substantial barriers to exit include legal prohibition (ITAR, EAR enforcement), reputational/commercial risks from sanction violation, technical dependency on restricted components, and international coordination of restrictions. However, suppression is not absolute — restricted nations are building indigenous capacity and alternative supply chains, and some technology leakage persists. Theater ratio (0.64): Moderate-high. The constraint exhibits significant performativity: lengthy export review bureaucracies, classification lags behind technology obsolescence, compliance theater from firms (formal procedures that don't prevent determined actors from acquiring tech), and Cold War–era list categories maintained for legacy reasons. The rise from 0.48 to 0.64 indicates theater increasing faster than genuine security function. This theater-to-function drift is characteristic of piton-class degradation, suggesting the legacy institutions' classification is accurate — constraints are maintained through institutional inertia more than security necessity.
 *
 * PERSPECTIVAL GAP:
 *   The gap between snare (restricted nations), tangled rope (exporting firms, restricted coalitions), rope (controlling institutions), and mountain (naturalized view) reveals the constraint operates as genuine multi-function coordination with built-in asymmetric extraction. The analytical observer's risk of false naturalization is particularly acute here: the framing 'technology diffusion control is inherent to statecraft' mirrors Cold War security discourse that has outlived its original justification. Today's controls persist partly through inertia (piton), partly through evolving geopolitical competition (rope for controllers, snare for targets), and partly through rent protection for domestic firms (hidden extraction). The perspectival method exposes this layering: no single type captures the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim declarations and power/exit combinations. Restricted-tier nations with powerless/trapped profiles show d ≈ 0.95 (full targets) → f(d) ≈ 1.42 → high experienced extraction. Allied tier-1 nations with powerful/mobile profiles but beneficiary status show d ≈ 0.25 (partial targets, partial beneficiaries) → f(d) ≈ 0.20 → low to moderate extraction. Technology exporting firms as moderate/constrained victims show d ≈ 0.70 → f(d) ≈ 0.95 → high extraction. Controlling institutions as institutional/arbitrage beneficiaries show d ≈ 0.08 (net beneficiaries) → f(d) ≈ -0.10 → negative extraction (they are extracting, not extracted from). The scope modifier σ(S) = 1.2 for global scope amplifies χ beyond the base ε × f(d) product, reflecting that export controls operate at planetary scale with significant visibility and coordination costs.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH MULTI-PERSPECTIVE MAPPING: The mandatrophy for technology export controls (whether they are security coordination or pure extraction) is resolved by showing that the answer is position-dependent. From the controlling nation's perspective, they are genuinely solving coordination problems (alliance management, diffusion prevention, strategic stability) — legitimate rope function. From the restricted nation's perspective, they face pure extraction with no coordination benefit — snare experience. From the exporting firm's perspective, it is tangled rope — both market restriction (extraction) and coordination benefit (supply chain stability, IP enforcement). The system's classificatory challenge is not which perspective is 'correct' but recognizing that the constraint simultaneously solves a coordination problem for some agents (controllers) while extracting from others (restricted nations). This is the definition of tangled rope at the systemic level. The false naturalization (mountain perspective) occurs when the controlling nation generalizes its rope experience to universal necessity — 'technology diffusion control is inherent to statecraft' — thereby erasing the snare experience of restricted nations. The mandatrophy resolves by rejecting the false summit and accepting the tangled rope classification at the systemic level, with perspectival variance across agent types.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_threshold_vs_rent_extraction,
    'What portion of export restrictions reflects legitimate security externality management versus rent extraction protecting domestic firm profitability?',
    'Comparative analysis of technology categories: (1) dual-use goods with measurable military applications, (2) advanced commercial tech with marginal military relevance, (3) obsolete technology maintained on lists due to bureaucratic inertia. Measure restriction intensity vs security justification for each category.',
    'If security dominates: constraint functions as rope (genuine coordination burden). If rent extraction dominates: constraint functions as snare for victims, piton for legacy enforcement. Split assessment would identify tangled rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_threshold_vs_rent_extraction, empirical, 'Proportion of restrictions driven by security vs economic rent protection').

omega_variable(
    indigenous_technology_breakthrough_threshold,
    'At what point does restricted-tier indigenous capability (chip fabrication, advanced materials, semiconductor design) reach parity with controlling-tier exports, collapsing the extraction mechanism?',
    'Technical benchmarking of restricted-tier capabilities against restricted-tier export standards; timeline projections for SMIC (China), TSMC (Taiwan alternative), indigenous EU fabs. Monitor for technology leakage, talent movement, and design-around innovation.',
    'If timeline < 10 years: scaffold sunset is approaching — restrictions lose extraction power as alternatives proliferate. If timeline > 20 years: extraction window persists across generational horizon; restricted-tier sees this as snare rather than temporary constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_technology_breakthrough_threshold, empirical, 'Timeline for restricted-tier technological parity with export controls').

omega_variable(
    alliance_coordination_vs_domination,
    'Do allied tier-1 nations experience export controls as alliance coordination mechanism or as asymmetric extraction of favorable terms by the controlling nation?',
    'Analysis of bilateral technology flows: asymmetry in allowed exports, IP protection enforcement differentials, licensing fee structures. Survey allied firm experience: licensing wait times, approval rates, technology age permitted for export vs controls maintained on newer generations.',
    'If coordination dominates: allied perspective correctly classifies as rope or balanced tangled rope. If domination: allied experience shifts toward snare, revealing false coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_coordination_vs_domination, empirical, 'Whether allied nation experience reflects coordination or extraction').

omega_variable(
    theater_ratio_collapse_mechanism,
    'As classified technologies become commercially obsolete faster than bureaucratic review cycles, does theater ratio approach 1.0 (pure performance with no security function)?',
    'Historical analysis of export control lists: identify technologies classified > 10 years after commercial commodification. Measure lag between technology market deployment and classification updates. Track compliance behavior: do firms compliance-theater or genuine security measures?',
    'If theater_ratio → 1.0: piton perspective dominates; constraint becomes pure institutional inertia with vanishing security function. Mandatrophy surfaces: are we defending security or defending bureaucracy?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_collapse_mechanism, empirical, 'Whether export control theater approaches total performativity').

omega_variable(
    decoupling_cost_asymmetry,
    'Who bears the larger structural cost of technology decoupling: controlling nations (through supply chain brittleness and lost markets) or restricted nations (through technology lag)?',
    'Economic modeling of decoupling scenarios: measure GDP impact, supply chain vulnerability, R&D cost multipliers for restricted-tier indigenous development vs controlling-tier forgone export revenue. Compare semiconductor dependency vs restricted-tier chip fabrication costs.',
    'If controlling-nation costs exceed restricted-nation costs: extraction is mutual or self-harming; constraint may degrade toward rope (mutual coordination) or invert toward snare for controller. If restricted-nation costs dominate: snare classification for restricted tier confirmed across longer timehorizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_cost_asymmetry, empirical, 'Asymmetry of decoupling costs between controlling and restricted nations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_export_controls, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(techexp_tr_t0, technology_export_controls, theater_ratio, 0, 0.48).
narrative_ontology:measurement(techexp_tr_t3, technology_export_controls, theater_ratio, 3, 0.56).
narrative_ontology:measurement(techexp_tr_t6, technology_export_controls, theater_ratio, 6, 0.64).
narrative_ontology:measurement(techexp_tr_t9, technology_export_controls, theater_ratio, 9, 0.72).

% Extraction over time
narrative_ontology:measurement(techexp_be_t0, technology_export_controls, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(techexp_be_t3, technology_export_controls, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(techexp_be_t6, technology_export_controls, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(techexp_be_t9, technology_export_controls, base_extractiveness, 9, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_export_controls, enforcement_mechanism).
narrative_ontology:affects_constraint(technology_export_controls, semiconductor_supply_chain_dependency).
narrative_ontology:affects_constraint(technology_export_controls, intellectual_property_regime).
narrative_ontology:affects_constraint(technology_export_controls, geopolitical_alliance_stability).

% DUAL FORMULATION NOTE:
% Technology export controls operate at the intersection of security coordination and economic extraction. The constraint family includes: (1) export_controls_security_tier (ε=0.28, Rope) — genuine alliance coordination for security; (2) export_controls_rent_protection (ε=0.45, Tangled Rope) — protection of domestic firm competitive advantage; (3) export_controls_technology_denial (ε=0.72, Snare) — from restricted-nation perspective. The integrated story (technology_export_controls, this file) presents the constraint across all three formulations simultaneously, as the same institutional mechanism serves all three functions for different beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_export_controls, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
