% ============================================================================
% CONSTRAINT STORY: jevons_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jevons_paradox, []).

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
 *   constraint_id: jevons_paradox
 *   human_readable: Jevons Paradox (The Rebound Effect)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Jevons Paradox describes a structural tension in technological progress:
 *   when efficiency improvements reduce the effective price of a resource,
 *   rational economic actors increase consumption, partially or fully
 *   offsetting the initial efficiency gain. Named after 19th-century
 *   economist William Stanley Jevons, who observed that coal efficiency
 *   improvements in Victorian England led to increased coal consumption
 *   rather than decreased. The constraint exhibits mixed classification
 *   across perspectives: resource industries and capital-intensive producers
 *   benefit from sustained demand growth; climate goals and future
 *   generations bear the cost of offset emissions; consumers experience mixed
 *   benefit and extraction through their own demand elasticity; green
 *   technology advocates experience partial success undermined by rebound;
 *   efficiency certification becomes performative theater; and naive
 *   analytical observers may naturalize the effect as an immutable law of
 *   economics when it is actually contingent on policy design, behavioral
 *   satiation, and distributional choices. The theater ratio (0.58) reflects
 *   that efficiency improvement rhetoric is substantially performative when
 *   unaccompanied by demand-side constraints or pricing mechanisms — the
 *   ritual of certification and standard-setting persists despite failing to
 *   achieve stated climate goals.
 *
 * KEY AGENTS:
 *   - Resource Extractive Industries: Primary beneficiary (institutional/arbitrage) — capture demand growth enabled by efficiency-driven price reductions; can arbitrage across carbon-pricing regimes
 *   - Climate Stabilization Goal: Primary victim (powerless/trapped) — abstract goal that cannot exit; all efficiency gains partially offset by rebound
 *   - Future Generations: Secondary victim (powerless/trapped) — inherit climate damages from aggregated rebound effects; no negotiating power
 *   - Green Technology Advocates: Organized mixed agent (organized/constrained) — coordinate to deploy efficiency but constrained by rebound mechanism and political limits; experience both market opportunity and goal displacement
 *   - Individual Consumers: Moderate mixed agent (moderate/constrained) — benefit from lower-cost services but constrained by own demand elasticity; partially solve their own problem and partially create it
 *   - Carbon-Neutral Certification Bodies: Institutional observer (institutional/arbitrage) — maintain performative verification rituals; benefit from legitimacy-granting role
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choices as immutable economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jevons_paradox, 0.38).
domain_priors:suppression_score(jevons_paradox, 0.42).
domain_priors:theater_ratio(jevons_paradox, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jevons_paradox, extractiveness, 0.38).
narrative_ontology:constraint_metric(jevons_paradox, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jevons_paradox, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jevons_paradox, tangled_rope).
narrative_ontology:human_readable(jevons_paradox, "Jevons Paradox (The Rebound Effect)").
narrative_ontology:topic_domain(jevons_paradox, "economic/technological").

domain_priors:requires_active_enforcement(jevons_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jevons_paradox, resource_extractive_industries).
narrative_ontology:constraint_beneficiary(jevons_paradox, capital_intensive_producers).
narrative_ontology:constraint_victim(jevons_paradox, environmental_commons).
narrative_ontology:constraint_victim(jevons_paradox, climate_stabilization_goal).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE STABILIZATION GOAL (SNARE) — Cannot exit the rebound mechanism; all efficiency gains are partially or fully offset by increased consumption. The climate system bears the full cost of demand elasticity. No choice, no escape path. Maximum extraction experienced: efficiency improvements that should reduce emissions instead maintain or increase them.
constraint_indexing:constraint_classification(jevons_paradox, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Inherit a climate system damaged by rebound effects that cannot be undone. Trapped by aggregated demand elasticity from present consumption patterns. No ability to exit or negotiate terms. Full extraction via inherited carbon debt.
constraint_indexing:constraint_classification(jevons_paradox, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: RESOURCE EXTRACTIVE INDUSTRIES (ROPE) — Benefits from the rebound effect through sustained demand growth despite efficiency gains. Experiences the constraint as coordination: market mechanisms align efficiency with their profit incentives. Net beneficiary. Can arbitrage between jurisdictions with different carbon constraints.
constraint_indexing:constraint_classification(jevons_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GREEN TECHNOLOGY ADVOCATES (TANGLED ROPE) — Organized agents (renewable energy sector, efficiency standards bodies, climate policy advocates) experience mixed coordination and extraction. They coordinate to deploy efficiency improvements, but the rebound effect undermines their climate goals. Constrained by politics, economics, and consumer preference elasticity. Experience both benefit (market for new technologies) and extraction (goal displacement).
constraint_indexing:constraint_classification(jevons_paradox, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INDIVIDUAL CONSUMERS (TANGLED ROPE) — Benefit from efficiency gains (lower cost per unit of service), but constrained by the rebound mechanism itself: when energy becomes cheaper via efficiency, they increase consumption. Agents are both beneficiaries and victims of their own demand elasticity. Partial coordination (markets deliver what consumers want) and partial extraction (climate externalities).
constraint_indexing:constraint_classification(jevons_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: CARBON-NEUTRAL TECHNOLOGY CERTIFICATION (PITON) — The theater of 'carbon-neutral' or 'net-zero' technology claims persists despite rebound effects undermining them. Certification rituals (lifecycle analysis, carbon accounting) are largely performative when applied without demand-side behavioral constraints. Theater ratio high because compliance with efficiency standards is maintained (low functional output), but actual carbon reduction goals fail. Maintained through institutional inertia and regulatory theater.
constraint_indexing:constraint_classification(jevons_paradox, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PRICE ELASTICITY VIEW (MOUNTAIN) — From a civilizational analytical perspective, the rebound effect appears as an immutable law of economics: rational actors respond to price signals by increasing consumption when goods become cheaper. The constraint is presented as a natural law of demand elasticity rooted in human behavior and market mechanics. However, this naturalizes what is actually a contingent institutional arrangement — the magnitude of rebound varies by context (poor vs wealthy, time horizon, consumption category), and policy interventions (carbon pricing, behavior change campaigns, satiation effects) can substantially reduce or redirect rebound.
constraint_indexing:constraint_classification(jevons_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jevons_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jevons_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jevons_paradox, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(jevons_paradox, TR),
    TR >= 0.70.

:- end_tests(jevons_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The rebound effect is not pure resource extraction (ε would be >0.46) because it involves genuine consumer preference satisfaction and legitimate price response. But it is not pure coordination (ε would be ≤0.35) because resource industries systematically benefit from offset emissions while climate costs are externalized to future generations and non-human systems. The value reflects that the constraint combines real economic efficiency gains with asymmetric distribution of climate costs. Suppression (0.42): Moderate. Suppression is not maximal because exit paths exist in principle — consumers can choose not to increase consumption, resource industries can voluntarily constrain extraction, policy can implement carbon pricing. But suppression is significant because: (1) market mechanisms align extraction with profit incentives; (2) coordinating on demand reduction requires collective agreement that undermines individual welfare-maximizing choices; (3) information about rebound effects is not uniformly distributed; (4) wealthy actors can exit carbon constraints through arbitrage while poor actors are trapped in carbon-intensive consumption patterns. Theater ratio (0.58): Moderate-high. The increasing value over the 70-year interval reflects how efficiency improvement rhetoric has become increasingly performative. Certification of 'carbon-neutral' or 'net-zero' technologies proceeds without addressing rebound; lifecycle analysis ignores demand elasticity; efficiency standards are celebrated while total consumption accelerates. The theater persists because compliance with efficiency metrics is maintained (satisfying the performative requirement) while actual climate goals fail.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same mechanism produces radically different classifications depending on the observer's structural position. Resource industries see a coordination mechanism delivering what markets demand (Rope). Climate goals see an offset mechanism trapping them in a losing race against increasing total consumption (Snare). Consumers see a benefit (cheaper services) and a constraint (they cannot collectively coordinate to refuse the cheaper option without coordinating on consumption reduction) (Tangled Rope). Green technology advocates see partial success undermined by forces beyond their control (Tangled Rope). Efficiency certification bodies see their ritual as successful (maintained compliance with standards) while actual climate metrics fail (Piton). Naive analytical observers see an immutable law of economics (Mountain). The perspectival gap is not a measurement ambiguity — it reflects genuine structural differences in how agents experience extraction flow, power, and exit options relative to this constraint. The gap widens over the 70-year interval as rebound effects accumulate and theater increases.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent type. Resource industries (beneficiary status + arbitrage exit) experience low d → negative effective extraction → classify as Rope from their perspective. Climate goals and future generations (victim status + trapped exit) experience high d → high f(d) → high χ → classify as Snare from their perspective. Consumers (mixed beneficiary-victim status depending on framing + constrained exit) experience moderate d → moderate f(d) → moderate χ → classify as Tangled Rope. Green technology advocates (organized status + constrained exit) experience moderate-to-high d because they are partly captured by the efficiency-gains narrative → classify as Tangled Rope. The piton classification derives from the theater gate (high theater ratio) despite moderate χ, reflecting that efficiency certification persists through institutional ritual rather than functional climate impact. The analytical observer's mountain classification is a false summit: the price elasticity of demand is a contingent behavioral property in specific institutional contexts, not a universal law. Demand elasticity varies by consumption category (transport ~1.0, lighting ~0.3, heating ~0.3), by income level (poor households show higher elasticity to price changes), and by policy regime (satiation effects emerge under high carbon pricing). The 'inevitable rebound' framing naturalizes a policy choice to rely on market mechanisms rather than demand-side constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint exhibits both genuine coordination (market efficiency delivering consumer preferences, legitimate price response to cost changes) and asymmetric extraction (resource industries capture demand growth while climate costs are externalized). It is not a pure Rope because suppression is significant and extraction is real. It is not a pure Snare because beneficiaries are not only exploiters — consumers genuinely benefit from cheaper services, and efficiency improvements represent real technical progress. The mandatrophy is resolved by recognizing that the constraint's extractiveness depends on policy design: (1) If carbon pricing internalizes climate costs, rebound is limited and the constraint shifts toward Rope. (2) If demand-side constraints are implemented, extraction is suppressed and the constraint becomes Scaffold. (3) If the constraint remains unpriced with high suppression of alternatives, it is Snare. The current institutional arrangement (weak carbon pricing, no mandatory demand reduction, efficiency-focused policy) maintains Tangled Rope status by delivering efficiency gains (rope function) while systematically externalizing climate costs (rope + extraction). The analytical observer's mountain classification is a false summit masking this policy contingency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rebound_magnitude_empirical,
    'What is the true empirical magnitude of the rebound effect across different resource types and economic contexts?',
    'Meta-analysis of empirical studies on price elasticity, substitution effects, and direct vs indirect rebound; controlled experiments comparing efficiency improvements with and without behavioral constraints',
    'If rebound < 30%: efficiency improvements net positive; Rope perspective dominates. If rebound 30-100%: partial offset; Tangled Rope confirmed. If rebound > 100%: backfire scenario; full Snare. Magnitude varies by context (transport vs lighting vs heating), which may require constraint decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebound_magnitude_empirical, empirical, 'Empirical magnitude of rebound effect across contexts').

omega_variable(
    behavioral_satiation_ceiling,
    'Are there satiation effects or consumption ceilings that naturally limit rebound, or is demand growth truly unbounded by efficiency improvements?',
    'Cross-sectional analysis of wealthy societies with mature efficiency; behavioral studies on consumption preferences; testing whether demand for lighting, heating, or transportation continues to grow at constant elasticity as prices fall',
    'If satiation exists: rebound is self-limiting; constraint is temporary (Scaffold). If unbounded: rebound continues indefinitely; Snare classification is stable. Satiation is context-dependent (energy use vs air travel), suggesting constraint decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_satiation_ceiling, empirical, 'Whether consumption satiation limits rebound or demand is unbounded').

omega_variable(
    policy_intervention_effectiveness,
    'Can policy interventions (carbon pricing, demand-side constraints, behavior change) successfully suppress rebound, or is it inevitable?',
    'Comparison of jurisdictions with different policy regimes (high carbon tax vs low, regulatory constraints vs voluntary, information campaigns vs laissez-faire); time-series analysis of efficiency gains vs total consumption under different policy conditions',
    'If interventions effective: rebound is contingent, not inevitable; constraint shifts to Scaffold or Rope depending on policy design. If interventions fail: rebound is structural; Snare confirmed. This resolves whether the mountain classification is correct (true law of economics) or false (naturalization of policy choice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(policy_intervention_effectiveness, empirical, 'Whether policy can suppress rebound or it is inevitable').

omega_variable(
    extraction_asymmetry,
    'Is the Jevons Paradox mechanism fundamentally extractive (benefits accrue to resource industries while climate costs are externalized) or is it a coordination failure where all agents are victims of their own demand elasticity?',
    'Distributional analysis of who captures efficiency gains vs who bears climate costs; modeling of wealth concentration in resource sectors vs diffuse climate damages; comparison with scenarios where efficiency gains are taxed and recycled as climate investment',
    'If extraction confirmed: Snare perspective is correct; resource industries are the primary beneficiary. If coordination failure: all perspectives experience rebound; no clear beneficiary/victim split. This determines whether the constraint is inherently exploitative or inherently collective-action-problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_asymmetry, conceptual, 'Whether rebound is extraction or collective-action failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jevons_paradox, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jevons_tr_t0, jevons_paradox, theater_ratio, 0, 0.32).
narrative_ontology:measurement(jevons_tr_t35, jevons_paradox, theater_ratio, 35, 0.48).
narrative_ontology:measurement(jevons_tr_t70, jevons_paradox, theater_ratio, 70, 0.58).

% Extraction over time
narrative_ontology:measurement(jevons_be_t0, jevons_paradox, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(jevons_be_t35, jevons_paradox, base_extractiveness, 35, 0.32).
narrative_ontology:measurement(jevons_be_t70, jevons_paradox, base_extractiveness, 70, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jevons_paradox, resource_allocation).
narrative_ontology:affects_constraint(jevons_paradox, carbon_externality_pricing).
narrative_ontology:affects_constraint(jevons_paradox, energy_sufficiency_norm).
narrative_ontology:affects_constraint(jevons_paradox, demand_destruction_policy).

% DUAL FORMULATION NOTE:
% Jevons Paradox is a constraint family covering distinct structural claims: (1) the empirical rebound effect magnitude (downstream of specific energy/transport/material efficiency claims); (2) the policy-design contingency (whether rebound is addressed by pricing or demand-side constraints); (3) the asymmetric distribution of efficiency benefits vs climate costs. The stories are linked because magnitude determines policy necessity, and policy choice determines whether the constraint remains Tangled Rope (mixed benefit/extraction) or shifts toward Rope (pricing) or Scaffold (sunset via demand transformation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jevons_paradox, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
