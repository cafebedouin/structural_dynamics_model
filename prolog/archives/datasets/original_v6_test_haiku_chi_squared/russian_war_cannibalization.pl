% ============================================================================
% CONSTRAINT STORY: russian_war_cannibalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_russian_war_cannibalization, []).

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
 *   constraint_id: russian_war_cannibalization
 *   human_readable: Russian Military Cannibalization of Civilian Infrastructure
 *   domain: political_economy/state_capacity
 *
 * SUMMARY:
 *   The Russian state's mobilization for the Ukraine war represents a
 *   structural constraint that cannibilizes civilian infrastructure and
 *   diverts economic output from consumer production to military-industrial
 *   use. This constraint exhibits the 'Invisible Mandatrophy' pattern: the
 *   state maintains a formal mandate (defend the nation, achieve military
 *   objectives) but increasing evidence suggests the mandate is failing,
 *   causing the state to escalate civilian resource extraction not to achieve
 *   the stated purpose more effectively but to compensate for losses and
 *   maintain the appearance of mobilization. The constraint operates through
 *   a combination of price controls, import restrictions, electricity
 *   rationing, labor conscription, and production quota reallocation.
 *   Civilian light manufacturing, agriculture supply chains, and consumer
 *   goods sectors are systematically defunded in favor of defense
 *   contractors. The theater ratio (rising from 0.35 to 0.58) indicates that
 *   resource mobilization is increasingly performative: the state announces
 *   production targets and allocates resources by bureaucratic fiat, but
 *   actual military outputs may not scale proportionally with the economic
 *   inputs consumed. The suppression index (0.75) reflects comprehensive
 *   state control over markets, labor, and capital allocation — civilian
 *   agents have virtually no alternatives and cannot exit the constraint.
 *
 * KEY AGENTS:
 *   - Civilian Households: Primary victims (powerless/trapped) — face inflation, shortages, and declining purchasing power with no exit option
 *   - Regional Light Manufacturing Sector: Primary victims (powerless/trapped) — factory closures, resource redirection, labor conscription eliminate competitive production
 *   - Pensioner and Fixed-Income Population: Secondary victims (moderate/constrained) — dependent on state pensions that fail to track inflation; social services cut
 *   - Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — guaranteed contracts, preferential access to resources, state protection
 *   - Defense Contractors: Primary beneficiary (institutional/arbitrage) — rent extraction through inflated contracts and supply monopolies
 *   - Russian State Security Apparatus: Enforcer (organized/constrained) — executes the constraint through price controls, rationing, and conscription, but also bears costs of suppression management
 *   - Soviet-Era Industrial Legacy: Residual institutional structure (institutional/constrained) — degraded mechanism for implementing command-economy methods
 *   - Analytical Observer: Civilian systemic view (analytical/analytical) — sees the constraint as extraction without proportional military benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(russian_war_cannibalization, 0.68).
domain_priors:suppression_score(russian_war_cannibalization, 0.75).
domain_priors:theater_ratio(russian_war_cannibalization, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(russian_war_cannibalization, extractiveness, 0.68).
narrative_ontology:constraint_metric(russian_war_cannibalization, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(russian_war_cannibalization, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(russian_war_cannibalization, snare).
narrative_ontology:human_readable(russian_war_cannibalization, "Russian Military Cannibalization of Civilian Infrastructure").
narrative_ontology:topic_domain(russian_war_cannibalization, "political_economy/state_capacity").

domain_priors:requires_active_enforcement(russian_war_cannibalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(russian_war_cannibalization, military_industrial_complex).
narrative_ontology:constraint_beneficiary(russian_war_cannibalization, defense_contractors).
narrative_ontology:constraint_beneficiary(russian_war_cannibalization, state_security_apparatus).
narrative_ontology:constraint_victim(russian_war_cannibalization, civilian_consumer_economy).
narrative_ontology:constraint_victim(russian_war_cannibalization, civilian_population_livelihood).
narrative_ontology:constraint_victim(russian_war_cannibalization, regional_economic_development).
narrative_ontology:constraint_victim(russian_war_cannibalization, pensioners_and_fixed_income_earners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN HOUSEHOLD (SNARE) — Citizens cannot exit the constraint. War-driven inflation, import restrictions, and production redirects eliminate consumer goods availability and purchasing power. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.66.
constraint_indexing:constraint_classification(russian_war_cannibalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL LIGHT MANUFACTURING (SNARE) — Small factories producing appliances, textiles, consumer goods face mandatory resource redirection: electricity rationing favors military production, component supplies diverted to defense contractors, labor conscription removes workers. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(russian_war_cannibalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: PENSIONER POPULATION (TANGLED ROPE) — State pension system technically coordinated with welfare apparatus (ruble purchasing power is the coordination) but experiences asymmetric extraction: pensions fail to match inflation, social services cut for defense spending, yet elderly remain dependent on state system. d≈0.80, f(d)≈1.25, σ=1.0 → χ≈0.60. Mixed: coordination of dependency + extraction through inflation.
constraint_indexing:constraint_classification(russian_war_cannibalization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MILITARY-INDUSTRIAL COMPLEX (ROPE) — Defense contractors experience the constraint as pure coordination: state mandate to prioritize military production solves the collective action problem of private firms underinvesting in war economy goods. Guaranteed contracts, preferential electricity, labor priority, and supply chain protection are coordination mechanisms. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(russian_war_cannibalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE SECURITY APPARATUS (TANGLED ROPE) — The state sees itself as executing its core mandate (defense) through coordinated resource allocation, but also relies on suppressing alternatives (price controls, import restrictions, labor controls) to maintain the constraint. Active enforcement required. d≈0.15, f(d)≈0.05, σ=1.0 → χ≈0.02. The state benefits from coordination of the war economy but also incurs the costs of suppression and management, creating a tangled relationship to the constraint itself.
constraint_indexing:constraint_classification(russian_war_cannibalization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SOVIET INDUSTRIAL LEGACY (PITON) — The Russian defense-industrial complex is a degraded residue of the Soviet command economy. Its structure (vertical integration, state ownership, hard-wired supply chains) persists through institutional inertia rather than functional efficiency. Much of the 'cannibalization' is performative: mobilizing Soviet-era factories, restoring mothballed plants, maintaining sprawling defense bureaucracy. theater_ratio=0.58 approaches piton threshold but extraction remains high. This is a constraint where the mechanism (Soviet planning methods) is degraded but still forcibly activated. d≈0.20, f(d)≈0.08, σ=1.1 → χ≈0.06.
constraint_indexing:constraint_classification(russian_war_cannibalization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE from systemic view) — From a civilizational perspective, the constraint exhibits pure extraction without coordination: the war mobilization produces no net benefit to Russian civilian society, only destruction of economic capacity and diversion of human capital to unproductive military spending. This is extraction divorced from coordination. d≈0.88, f(d)≈1.35, σ=1.1 → χ≈0.64.
constraint_indexing:constraint_classification(russian_war_cannibalization, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russian_war_cannibalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(russian_war_cannibalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russian_war_cannibalization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(russian_war_cannibalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(russian_war_cannibalization, TR),
    TR >= 0.70.

:- end_tests(russian_war_cannibalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The war mobilization extracts approximately 15-20% of Russian GDP for military purposes (historically extreme, comparable to WWII Soviet levels). This is far above normal peacetime defense spending (3-4% of GDP pre-2022). The extraction is sustained through state power, not consent, and creates severe opportunity costs for the civilian economy. The metric has risen from 0.42 to 0.68 over 24 months, indicating accelerating cannibalization. Suppression (0.75): Very high. The Russian state suppresses alternatives through: (1) price controls preventing market signals; (2) import restrictions eliminating consumer goods; (3) labor conscription removing workers from civilian sectors; (4) production quotas mandating factory redirection; (5) capital controls preventing capital flight. Suppression is comprehensive and active. Theater ratio (0.58): Moderate-high. The mobilization narrative emphasizes production targets, factory restoration, and mobilization scale, but actual military outputs relative to resource inputs are questioned by military analysts. The increasing theater ratio from 0.35 to 0.58 suggests Goodhart drift: measuring success by resources mobilized rather than military outcomes achieved. This is characteristic of hidden mandatrophy — escalating resource claims despite degrading strategic position. Claimed type: SNARE. The constraint meets all snare thresholds: extractiveness ≥ 0.46 (actual 0.68), suppression ≥ 0.60 (actual 0.75), χ ≥ 0.66 (derived). The constraint's existence depends entirely on suppressing alternatives — without state power over markets, labor, and capital, civilians would shift production and consumption away from military inputs.
 *
 * PERSPECTIVAL GAP:
 *   The civilian household sees a pure extraction system with no coordination benefit (Snare) — resources are taken but no reciprocal service is delivered (security is not experienced as improved, purchasing power collapses). The military-industrial complex sees coordination (Rope) — the state mandate solves their collective action problem by guaranteeing demand and priority access. The state apparatus sees itself as executing both coordination (defense) and extraction (resource mobilization), creating a tangled relationship. The Soviet-era industrial legacy is a Piton — the mechanism for implementing the constraint (command-economy methods) is degraded and maintained through inertia rather than function. The analytical observer, seeing the constraint from a civilizational perspective, perceives that the extraction produces minimal net benefit and may be negatively correlated with the state's stated military objectives — suggesting hidden mandatrophy. This gap is wide: victim and observer agree on Snare; beneficiary and state apparatus see coordination or enforcement; the historical-institutional lens sees degradation (Piton).
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian households: Victims + trapped → d≈0.92, f(d)≈1.40. Maximum extraction directionality. Reallocation of production away from consumer goods directly harms household purchasing power and consumption. Regional light manufacturers: Victims + trapped → d≈0.95, f(d)≈1.42. Highest extraction directionality. Factories cannot exit; must comply with production quotas and resource allocation mandates. Pensioners: Victims + constrained → d≈0.80, f(d)≈1.25. High extraction but slightly less than maximum because elderly are at least included in state system (pensions exist, even if inadequate). Military-industrial complex: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Large negative extraction directionality (net benefit). State contracts are de facto guaranteed; no exit risk. Russian state apparatus: Mixed + constrained → d≈0.15, f(d)≈0.05. The state benefits from executing its mandated function but also incurs the costs of enforcing suppression and managing dissent. Soviet legacy system: Institutional + constrained → d≈0.20, f(d)≈0.08. Low extraction directionality because the system itself is inert — it does not benefit but is merely the mechanism through which others extract.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY DETECTION: The Russian war cannibalization constraint exhibits the signature pattern of hidden or escalating mandatrophy. The state's stated mandate is to defend Russia and achieve military objectives in Ukraine. However, the extractiveness has risen from 0.42 (early war, 2022) to 0.68 (2024), a 62% increase. If the mandate were being achieved efficiently, extractiveness should stabilize or decline as initial mobilization costs are absorbed. Instead, escalating extraction despite uncertain military outcomes indicates the state is compensating for mandate failure by increasing civilian resource claims. The theater ratio rising from 0.35 to 0.58 further confirms mandatrophy: the state increasingly emphasizes resource mobilization volume and factory production statistics while downplaying military outcome specificity. This is classic Goodhart drift — measuring success by the input (resources extracted) rather than the output (military objectives achieved). The mandatrophy resolution requires acknowledging that the constraint transitions from a (brutal) execution of a legitimate state mandate to an increasingly performative extraction system maintained to preserve the appearance of mobilization. This classification as SNARE with mandatrophy_resolved=true means the framework recognizes that the constraint cannot transition to Rope (no genuine coordination benefit) or Scaffold (no sunset clause despite expectations). It is locked in Snare classification with the additional property that the mandate it purports to serve is failing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civilian_economy_collapse_irreversibility,
    'Is the cannibalization of civilian infrastructure reversible (temporary war mobilization) or irreversible (permanent destruction of civilian productive capacity)?',
    'Post-conflict reconstruction assessment; analysis of capital stock destruction, human capital losses (emigration, conscription), technological degradation, and supply chain fragmentation. Comparison with post-WWII Soviet recovery vs post-Soviet 1990s contraction.',
    'If reversible: constraint frames as Scaffold with a sunset clause (temporary mobilization with eventual return to civilian economy). If irreversible: constraint is Snare with generational timescale (permanent extraction of value from civilian sector, with no recovery path).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_economy_collapse_irreversibility, empirical, 'Whether civilian economic cannibalization is reversible or permanent').

omega_variable(
    state_mandate_authenticity,
    'Does the Russian state genuinely execute a mandate to defend the state and prosecute the war, or does it use the war as cover for elite extraction (corruption, embezzlement, dispossession)?',
    'Analysis of defense spending allocation: proportion to actual military operations vs. contractor profiteering, ghost factories, inflated bids, and disappeared supplies. Investigation of defense minister and contract officer personal enrichment. Cross-correlation with patterns from prior Russian military mobilizations.',
    'If mandate is authentic: constraint is Snare (pure extraction for a stated purpose). If mandate is cover for elite extraction: constraint is Snare but with an additional layer of mandatrophy (the stated war purpose masks internal corruption, making the constraint even more opaque to victims).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_mandate_authenticity, empirical, 'Whether state mandate to defend is genuine or cover for elite extraction').

omega_variable(
    civilian_suppression_mechanism_sustainability,
    'Can price controls, import restrictions, and labor conscription be maintained indefinitely without triggering black markets, emigration, or internal collapse?',
    'Tracking of emigration rates, wage dynamics, black market premium ratios, food security metrics, and internal unrest indicators. Comparison with Soviet-era suppression sustainability periods (WWII, 1960s-1980s stagnation).',
    'If sustainable: suppression value (0.75) is stable and the Snare classification holds. If unsustainable: suppression index will degrade, classification may shift to Piton (degraded enforcement) or Scaffold (forced sunset as enforcement collapses).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_suppression_mechanism_sustainability, empirical, 'Whether suppression mechanisms can be sustained long-term').

omega_variable(
    hidden_mandatrophy_detection,
    'Is the constraint experiencing hidden mandatrophy: the state''s stated mandate (defend Russia/win the war) is failing, forcing escalating resource extraction from civilians with no corresponding strategic benefit?',
    'Assessment of military-strategic outcomes vs. economic cost. Analysis of whether additional civilian resource extraction produces proportional military capacity increases or merely compensates for losses. Tracking of state rhetoric vs. objective military position changes.',
    'If mandatrophy is occurring: the constraint transitions from a legitimate (if brutal) Snare executing a state mandate to a degraded Snare/Piton where the state maintains the extraction theater even as its purpose fails. This would explain why the theater_ratio is rising despite the constraint''s core function remaining, indicating Goodhart drift (measuring military mobilization success by resource extraction volume rather than military outcomes).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hidden_mandatrophy_detection, conceptual, 'Whether hidden mandatrophy is occurring in state resource extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russian_war_cannibalization, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rwc_tr_t0, russian_war_cannibalization, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rwc_tr_t12, russian_war_cannibalization, theater_ratio, 12, 0.48).
narrative_ontology:measurement(rwc_tr_t24, russian_war_cannibalization, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(rwc_be_t0, russian_war_cannibalization, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rwc_be_t12, russian_war_cannibalization, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(rwc_be_t24, russian_war_cannibalization, base_extractiveness, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(russian_war_cannibalization, resource_allocation).
narrative_ontology:affects_constraint(russian_war_cannibalization, russian_capital_flight_controls).
narrative_ontology:affects_constraint(russian_war_cannibalization, sanctions_induced_supply_substitution).
narrative_ontology:affects_constraint(russian_war_cannibalization, demographic_conscription_trap).

% DUAL FORMULATION NOTE:
% This constraint is structurally dependent on three upstream constraints: capital flight controls (which prevent civilians from protecting wealth), sanctions-induced supply substitution (which eliminates consumer goods imports), and conscription (which removes male labor from civilian economy). The cannibalization constraint integrates these three mechanisms into a unified extraction system. The upstream constraints have lower extractiveness individually; the downstream constraint exhibits higher extractiveness due to their interaction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(russian_war_cannibalization, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
