% ============================================================================
% CONSTRAINT STORY: fragile_middle_layer_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fragile_middle_layer_collapse, []).

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
 *   constraint_id: fragile_middle_layer_collapse
 *   human_readable: The Intermediary Decay
 *   domain: economic/technological/logistical
 *
 * SUMMARY:
 *   The intermediary decay describes a systemic transformation where the
 *   middle layer of economic and logistical systems — regional distributors,
 *   local maintenance contractors, human moderators, community-embedded
 *   service providers — is hollowed out by automation, platform scaling, and
 *   direct-to-end-user business models. This constraint demonstrates how a
 *   technologically-driven efficiency gain (direct-to-consumer platforms
 *   reduce transaction costs and improve service velocity) simultaneously
 *   extracts from multiple victim groups: the intermediaries themselves
 *   (career/capital destruction), end consumers (hidden precarity and
 *   lock-in), and abstract systemic resilience (loss of geographic redundancy
 *   and tacit knowledge). The constraint is not primarily coercive in the
 *   conventional sense — there are no legal barriers, no overt suppression,
 *   no threat of violence. Instead, it operates through economic obsolescence
 *   and structural substitution: intermediaries cannot compete with platform
 *   automation because capital requirements and scale economics favor
 *   centralized systems. The extraction is latent in the asymmetric
 *   distribution of gains (end consumers see lower prices; platform operators
 *   capture data and market control; intermediaries face unemployment). The
 *   theater ratio reflects the performative regulatory framework that
 *   persists despite the collapse: licensing schemes, labor classifications,
 *   and local business regulations designed for the intermediary era continue
 *   to exist but have lost functional relevance.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture data access, network effects, and market control; drive the technological substitution
 *   - End Consumers: Secondary beneficiary/victim (moderate/constrained) — gain lower prices and faster service but face hidden precarity costs and lock-in; cannot easily exit
 *   - Regional Intermediaries: Primary victim (powerless/trapped) — structurally displaced by technological substitution; lack capital and skills to pivot; face biographical-timescale unemployment
 *   - Last-Mile Workers: Secondary victim (powerless/trapped) — absorbed into precarious platform labor (gig economy); wages suppressed relative to intermediary-era baselines; lack scheduling predictability and benefits
 *   - System Resilience & Redundancy: Tertiary victim (powerless/trapped) — abstract collective good; loses geographic redundancy, tacit knowledge distribution, local problem-solving capacity; cannot organize or exit
 *   - Regulatory Framework: Institutional actor (institutional/constrained) — maintains performative licensing and labor protections despite loss of functional substrate; sees own authority degraded (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fragile_middle_layer_collapse, 0.58).
domain_priors:suppression_score(fragile_middle_layer_collapse, 0.68).
domain_priors:theater_ratio(fragile_middle_layer_collapse, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fragile_middle_layer_collapse, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fragile_middle_layer_collapse, snare).
narrative_ontology:human_readable(fragile_middle_layer_collapse, "The Intermediary Decay").
narrative_ontology:topic_domain(fragile_middle_layer_collapse, "economic/technological/logistical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fragile_middle_layer_collapse, end_consumers).
narrative_ontology:constraint_beneficiary(fragile_middle_layer_collapse, platform_operators).
narrative_ontology:constraint_victim(fragile_middle_layer_collapse, regional_intermediaries).
narrative_ontology:constraint_victim(fragile_middle_layer_collapse, local_service_providers).
narrative_ontology:constraint_victim(fragile_middle_layer_collapse, system_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED REGIONAL INTERMEDIARY (SNARE) — Structurally trapped by technological obsolescence and capital requirements; cannot pivot to new business models without massive retraining and investment. Bears full extraction cost as market access evaporates. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PLATFORM OPERATOR (ROPE) — Genuine coordination benefit: direct-to-consumer scaling reduces transaction costs and improves service velocity. Extracts via platform control and data access, but solves the coordination problem of matching supply to demand at scale. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: END CONSUMER (TANGLED ROPE) — Coordination benefit: direct access, lower prices, faster delivery. Extraction cost: reduced local resilience, hidden labor precarity, supply chain fragility, data extraction via behavioral monitoring. Cannot easily exit the platform ecosystem; constrained by network effects. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SYSTEM RESILIENCE (SNARE) — Abstract collective good. Intermediaries provided geographic redundancy, tacit knowledge retention, local problem-solving capacity, and distributed slack. As they collapse, systemic fragility increases: supply shocks cascade faster, local knowledge is lost, single points of failure multiply. System has no exit and no voice. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Licensing, labor protections, and local business regulations were designed for the intermediary layer. As the layer collapses, the regulatory framework becomes performative theater: gig economy classification schemes, 'partner' vs 'employee' designations, and local licensing regimes all persist despite fundamental changes to the economic substrate. theater_ratio=0.62. Regulations lack enforcement capacity and real function in the new ecosystem.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT) — The observer might naturalize middle-layer collapse as an inherent feature of network scaling: 'Intermediaries are always eliminated by direct platforms — this is a law of information economics.' However, base properties (ε=0.58, suppression=0.68, theater=0.62) do not support a mountain. The collapse is contingent on specific technological/regulatory conditions, not immutable. This perspective risks false summitry.
constraint_indexing:constraint_classification(fragile_middle_layer_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fragile_middle_layer_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fragile_middle_layer_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fragile_middle_layer_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fragile_middle_layer_collapse, TR),
    TR >= 0.70.

:- end_tests(fragile_middle_layer_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, high): The constraint extracts from intermediaries and system resilience. The extraction mechanism is technological substitution (platform automation and scale economies) rather than overt coercion. The initial extractiveness (0.22) reflects the early era of platform emergence when intermediaries and platforms coexisted in complementary roles. As platforms achieved scale dominance (time=7-14), extractiveness rose to 0.58 as intermediaries faced structural obsolescence. Suppression (0.68, high): Significant barriers prevent escape: intermediaries cannot access capital to automate, cannot compete on scale, cannot pivot skills without retraining, lack leverage to demand regulatory protection. Last-mile workers face algorithmic management and rating systems that suppress wage negotiation. End consumers face high switching costs (network effects, data lock-in). The suppression is not state-imposed but structural (capital requirements, information asymmetries, coordination failures). Theater ratio (0.62, moderate-high): Regulatory frameworks for business licensing, labor classification ('partner' vs 'employee'), and local commerce continue to exist and maintain ceremonial authority, but lack enforcement capacity and functional relevance in the platform-direct ecosystem. Licensing regimes for regional distribution still exist in many jurisdictions but have minimal impact on platform operations. Labor classification schemes debate the status of gig workers without changing fundamental precarity.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gap separates the platform operator (rope) from the displaced intermediary (snare). The platform operator solves a genuine coordination problem — matching supply and demand at scale with lower transaction costs — and experiences the transformation as a solution. The intermediary experiences the same transformation as structural displacement with no path forward (high suppression, trap exit option). The end consumer perspective is more complex (tangled rope): they benefit from lower prices and faster service (coordination function), but this benefit is partially subsidized by hidden extraction elsewhere (precarious labor, system fragility, data access) and reinforced by lock-in effects (constrained exit). The system resilience perspective (snare) reveals that the aggregated platform efficiency may be masking distributed fragility: the loss of geographic redundancy and tacit knowledge creates hidden vulnerability to supply shocks. The regulatory framework perspective (piton) shows that institutions designed for the intermediary era persist through inertia without functional content. The analytical observer risks naturalizing the collapse as an inevitable economic law (mountain) — a false summit that obscures the contingency of the transformation (it depends on specific technological capabilities, regulatory choices, and labor law regimes).
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; experience genuine coordination function. Regional intermediaries: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; no exit option, no leverage. End consumers: Beneficiary (lower prices) + victim (hidden extraction, lock-in) + constrained exit → d≈0.55, f(d)≈0.75. Mixed; constrained exit prevents full beneficiary status. Last-mile workers: Victim + trapped → d≈0.90, f(d)≈1.38. High extraction; wage suppression, algorithmic control, no alternative employment at equivalent scale. System resilience: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; abstract collective that cannot organize or exit. Regulatory framework: Institutional + constrained → d≈0.60, f(d)≈0.85. Piton classification; performative authority without functional enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resilience_threshold_breach,
    'What degree of intermediary collapse triggers cascading system failures in supply chains, emergency response, or critical infrastructure?',
    'Empirical analysis of supply chain shock propagation in sectors with varying intermediary density; comparative study of 2020-2024 logistics disruptions across platform-heavy vs intermediary-rich regions',
    'If threshold is near current collapse: system is operating in a fragile regime, and extraction cost to resilience is severe (snare from system perspective confirmed). If threshold is far: some collapse can be tolerated without structural failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resilience_threshold_breach, empirical, 'System resilience threshold for intermediary layer collapse').

omega_variable(
    labor_precarity_wage_suppression,
    'Does direct-to-consumer platform scaling suppress wages and working conditions for last-mile workers compared to the intermediary-era baseline?',
    'Comparative wage analysis (real wages, benefits, scheduling predictability) for delivery, maintenance, and last-mile workers: 2010-2025 data; control for skill/location',
    'If wages suppressed: hidden extraction mechanism embedded in ''consumer benefit'' (tangled rope confirmed). If wages stable or higher: consumer benefit is more genuine coordination gain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_precarity_wage_suppression, empirical, 'Wage and precarity changes in platform-direct vs intermediary labor').

omega_variable(
    platform_dependency_lock_in,
    'Can end consumers and suppliers realistically switch away from dominant platforms, or are they locked in by network effects and data dependencies?',
    'Historical analysis of platform switching costs; survey of consumer/supplier switching behavior; comparison to earlier intermediary-era switching costs',
    'If high lock-in: consumer exit options are ''constrained'' not ''mobile'', elevating chi and confirming tangled rope / snare dynamics. If low lock-in: consumer perspective is genuinely rope or even beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_dependency_lock_in, empirical, 'Platform dependency and exit costs for consumers and suppliers').

omega_variable(
    knowledge_loss_tacit_skill_erosion,
    'Are tacit skills, local knowledge, and distributed problem-solving capacity genuinely lost when intermediary layer collapses, or can platforms capture and automate them?',
    'Documentation of failure modes in automated systems that previously relied on intermediary judgment; analysis of fault-response time in platform vs intermediary ecosystems; case studies of critical incidents',
    'If knowledge is genuinely lost: system resilience snare is confirmed (χ≈0.76). If platforms can capture all tacit knowledge: resilience collapse is manageable, and the snare perspective is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_loss_tacit_skill_erosion, empirical, 'Tacit knowledge retention and loss in platform-direct systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fragile_middle_layer_collapse, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmlc_tr_t0, fragile_middle_layer_collapse, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fmlc_tr_t7, fragile_middle_layer_collapse, theater_ratio, 7, 0.5).
narrative_ontology:measurement(fmlc_tr_t14, fragile_middle_layer_collapse, theater_ratio, 14, 0.62).

% Extraction over time
narrative_ontology:measurement(fmlc_be_t0, fragile_middle_layer_collapse, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fmlc_be_t7, fragile_middle_layer_collapse, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(fmlc_be_t14, fragile_middle_layer_collapse, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fragile_middle_layer_collapse, resource_allocation).
narrative_ontology:affects_constraint(fragile_middle_layer_collapse, supply_chain_concentration).
narrative_ontology:affects_constraint(fragile_middle_layer_collapse, gig_economy_precarity).
narrative_ontology:affects_constraint(fragile_middle_layer_collapse, last_mile_logistics_fragility).

% DUAL FORMULATION NOTE:
% The intermediary decay is downstream of several technological constraints (automation, direct-to-consumer platforms, algorithmic management) and upstream of supply chain concentration and gig economy precarity. The constraint family decomposes into: (1) technological automation (ε≈0.15, mountain-ish) — inherent efficiency limit, (2) intermediary decay (ε≈0.58, snare) — contingent economic displacement, (3) supply chain concentration (ε≈0.52, tangled rope) — contingent structural fragility. Each has different ε values and responds to different interventions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fragile_middle_layer_collapse, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
