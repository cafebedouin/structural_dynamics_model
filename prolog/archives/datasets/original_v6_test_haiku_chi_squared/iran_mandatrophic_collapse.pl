% ============================================================================
% CONSTRAINT STORY: iran_mandatrophic_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_mandatrophic_collapse, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: iran_mandatrophic_collapse
 *   human_readable: Iranian Mandatrophy (The Water-Economic Choke)
 *   domain: political/economic/technological
 *
 * SUMMARY:
 *   Iran's mandatrophic collapse describes the structural wasting of
 *   ecological, economic, and social resilience caused by the rigid
 *   prioritization of the Revolutionary Mandate (regional proxy funding,
 *   nuclear program ambition, ideological self-sufficiency doctrine) over the
 *   maintenance of the state's organic margins (aquifers, agricultural
 *   viability, youth employment, currency stability, urban infrastructure).
 *   The constraint is mandatrophic because it exhibits the classic pattern: a
 *   central organizing principle (the Islamic Revolutionary ideology) that
 *   was historically functional and legitimate now persists as pure
 *   extraction mechanism despite delivering no material benefit to the
 *   majority population. The mandate survives through suppression (capital
 *   controls, media censorship, security apparatus loyalty), not through
 *   genuine popular support or tangible coordination function. Water crisis
 *   exemplifies this: Iran's renewable freshwater supply is ~135 billion
 *   cubic meters annually, but demand exceeds 200 billion cubic meters.
 *   Rather than managing scarcity through rational allocation (desalination,
 *   crop substitution, agricultural downsizing), the state prioritizes
 *   nuclear program funding and regional military presence, forcing rural
 *   populations into aquifer bankruptcy and urban centers into rationing. The
 *   Revolutionary Guard apparatus benefits from mandate prioritization
 *   (control of nuclear sector, smuggling networks, regional influence),
 *   while rural farmers, urban workers, youth, and future generations bear
 *   the extraction cost. The theater rises as the mandate becomes more
 *   performative: nationalist rhetoric about regional leadership and nuclear
 *   achievement replaces material delivery. The constraint manifests as a
 *   snare for victims (trapped aquifer users, constrained workers) and
 *   benefits the IRGC apparatus (arbitrage-rich institutional actor) while
 *   threatening to transition through piton (inertial degradation) into
 *   potential state failure.
 *
 * KEY AGENTS:
 *   - Rural Aquifer-Dependent Populations: Primary victim (powerless/trapped) — bear direct cost of water scarcity, face livelihood collapse, cannot migrate or exit
 *   - Urban Working-Class and Youth: Secondary victim (moderate/constrained) — face currency collapse, unemployment, fuel shortages; can organize but face suppression
 *   - Revolutionary Guard Apparatus (IRGC): Primary beneficiary (institutional/arbitrage) — controls nuclear program, regional proxy networks, sanctions-circumvention; benefits from mandate prioritization
 *   - Reform Coalition & State Technocrats: Secondary actor (organized/constrained) — understand the crisis, advocate for rationalization, but are overridden by mandate ideology
 *   - State Legitimacy Apparatus: Institutional maintenance (institutional/arbitrage) — perpetuates revolutionary ideology through performative theater despite functional erosion
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing human-chosen scarcity (mandate prioritization) as hydrological inevitability (aquifer limits)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_mandatrophic_collapse, 0.68).
domain_priors:suppression_score(iran_mandatrophic_collapse, 0.72).
domain_priors:theater_ratio(iran_mandatrophic_collapse, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(iran_mandatrophic_collapse, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_mandatrophic_collapse, snare).
narrative_ontology:human_readable(iran_mandatrophic_collapse, "Iranian Mandatrophy (The Water-Economic Choke)").
narrative_ontology:topic_domain(iran_mandatrophic_collapse, "political/economic/technological").

domain_priors:requires_active_enforcement(iran_mandatrophic_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_mandatrophic_collapse, revolutionary_guard_apparatus).
narrative_ontology:constraint_beneficiary(iran_mandatrophic_collapse, proxy_funding_recipients).
narrative_ontology:constraint_beneficiary(iran_mandatrophic_collapse, nuclear_program_stakeholders).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, rural_aquifer_dependent_populations).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, agricultural_sector).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, urban_water_security).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, currency_stability).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, youth_employment).
narrative_ontology:constraint_victim(iran_mandatrophic_collapse, future_generation_carrying_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL AQUIFER-DEPENDENT POPULATIONS (SNARE) — Powerless, trapped within national borders, cannot exit the water scarcity crisis. Faces increasing well-drilling costs, desertification, and complete livelihood collapse. No alternative migration path available (international visa barriers, domestic urban saturation). d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.96. Effective extraction is maximal.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: URBAN WORKING-CLASS AND YOUTH (SNARE) — Moderate power, constrained exit (capital controls, unemployment barriers). Faces currency collapse eroding purchasing power, fuel shortages, lack of job creation in non-revolutionary sectors. Can organize collectively but face suppression. d≈0.80, f(d)≈1.25, σ=1.0 → χ≈0.85.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REVOLUTIONARY GUARD APPARATUS (ROPE) — Institutional power, high exit optionality (can reallocate capital across sectors, operate black-market currency exchanges). Benefits from mandate prioritization through control of nuclear program, regional proxy networks, and sanctions-circumvention infrastructure. Experiences the constraint as coordination: consolidating mandate authority requires suppressing alternative economic pathways. d≈0.10, f(d)≈0.05, σ=0.9 → χ≈0.03. Net beneficiary.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REFORM COALITION & STATE TECHNOCRATS (TANGLED ROPE) — Organized actors (economists, water engineers, former JCPOA negotiators) see both coordination function (managing water-energy nexus) and asymmetric extraction (forced subordination of economic rationality to mandate). They benefit from technical problem-solving credit but are constrained by ideological override. Trapped between acknowledging crisis and preserving regime legitimacy. d≈0.60, f(d)≈0.80, σ=1.0 → χ≈0.54. Mixed coordination-extraction.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE LEGITIMACY APPARATUS (PITON) — Traditional mandate (Islamic Revolutionary ideology, anti-Western resistance, self-sufficiency) was once functionally coherent. Now largely performative — repeated invocations of revolutionary principles as costs mount. theater_ratio=0.65 reflects that nationalist/ideological theater (rallies, anti-sanctions rhetoric, nuclear 'achievements') masks accelerating real-world resource collapse. Institutional inertia: the apparatus persists because alternatives would require explicit regime reconstitution, not because the mandate works.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / HYDROLOGICAL LIMITS (MOUNTAIN) — From a civilizational/universal perspective, aquifer depletion in arid regions creates an immutable constraint: the Tigris-Euphrates and Persian Gulf aquifer systems have strict renewal rates (roughly 10-15 billion cubic meters annually for Iran's renewable freshwater). Demand (agriculture 92%, urban 6%, industrial 2%) exceeds supply by 40-50%. No policy can exceed physics. However, this framing risks naturalizing the mandatrophic choice: Iran could reallocate water from agriculture to drinking/industry, shift to drought-resistant crops, invest in desalination — but mandate prioritization forecloses these options. The 'mountain' is false if the mountain is human-chosen scarcity, not hydrological law.
constraint_indexing:constraint_classification(iran_mandatrophic_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_mandatrophic_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_mandatrophic_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_mandatrophic_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iran_mandatrophic_collapse, TR),
    TR >= 0.70.

:- end_tests(iran_mandatrophic_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The mandate prioritization extracts real economic value from aquifer-dependent populations (~90 million of whom depend directly or indirectly on irrigation) and urban consumers (~45% of population experiences water rationing). The extraction is not maximal (0.95) because some parts of the state still function (urban water systems, some industrial capacity) and because international trade partially buffers scarcity. But the trend is accelerating: aquifer depletion accelerates extraction as water becomes scarcer and more expensive to access. Suppression (0.72): High. Capital controls prevent citizens from relocating capital; visa barriers limit migration; media controls obscure the scale of crisis; security apparatus suppresses public demonstration and water-rights activism. But suppression is not absolute (0.95) because information leakage occurs (satellite imagery, diaspora networks) and because the scale of affected population makes total suppression structurally difficult. Theater ratio (0.65): Moderate-high. The state invokes revolutionary achievement (nuclear program, anti-Western resistance, regional leadership) as legitimation while delivering declining public goods (water access, employment, currency stability). The ratio increased from 0.42 (2010, post-JCPOA hope) to 0.65 (2024, as material delivery collapsed but rhetorical commitments remained).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a classic snare-rope gap. Rural farmers and urban workers see pure extraction (snare): they bear all costs (water scarcity, unemployment, currency collapse) while the mandate delivers nothing to them. The IRGC sees coordination (rope): mandate prioritization consolidates their institutional power and regional influence in a coherent strategy. The reform technocrats see mixed coordination-extraction (tangled rope): the mandate functions to organize state power (coordination) but does so by overriding economic rationality and suppressing alternatives (extraction). The state apparatus sees degradation (piton): the revolutionary mandate was once genuinely functional (organization of anti-Shah resistance, legitimacy for resource redistribution, coordination of Cold War alignment) but now persists as pure theater. The analytical observer risks seeing hydrological inevitability (mountain): aquifer depletion is a physics fact. But this naturalizes the choice — Iran could manage aquifer decline through rational policy, but mandate prioritization forecloses that option.
 *
 * DIRECTIONALITY LOGIC:
 *   Rural populations: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Urban workers: Victim + constrained → d≈0.80, f(d)≈1.25. High extraction (can organize but face suppression). IRGC apparatus: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary (high exit optionality, control over capital reallocation). Technocrats: Mixed (some beneficiary through problem-solving credit, some victim through override) + constrained → d≈0.60, f(d)≈0.80. Tangled rope signature. State legitimacy apparatus: Institutional + arbitrage → d≈0.10, f(d)≈0.05. Piton classification from theater gate, not high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival risk — naturalizing choice as law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint demonstrates the classic mandatrophic pattern and is analyzed in full structural detail. The 'mandate' is the Islamic Revolutionary ideology — historically a functional organizing principle for state legitimacy and social mobilization, now reduced to a pure extraction mechanism (maintaining IRGC authority and regional proxy networks) with zero coordination benefit for the general population. The key diagnostic features: (1) The mandate persists despite negative material delivery across all civilian sectors (water, employment, currency stability, infrastructure). (2) Beneficiaries are narrowly concentrated (IRGC apparatus, proxy recipients) while costs are broadly distributed (entire rural and urban populations). (3) The theater ratio rises over time as performance (nationalist rhetoric, nuclear 'achievements') replaces material delivery. (4) Suppression is required to maintain the extraction because the constraint delivers no genuine coordination function. (5) The constraint transitions toward Piton classification as the mandate becomes purely inertial — persisting because dismantling it would require admitting regime failure, not because it functions. The mandatrophy is RESOLVED by explicit declaration of the beneficiary-victim structure and the measurement of theater rising as material delivery collapses. This enables the engine to distinguish mandatrophic snares (high extraction, rising theater, concentrated beneficiaries, broad victims, high suppression required) from legitimate Snares (extraction with genuine coordination trade-offs) or from Mountains (immutable constraints accepted because they function across all populations). Iran's mandatrophic snare is human-constructed, reversible through political change, and maintained only by suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_functional_threshold,
    'At what level of aquifer depletion and currency collapse does the Revolutionary Mandate cease to provide any material benefit to its beneficiaries, and when will the IRGC apparatus face a choice between explicit reconstitution or rapid state failure?',
    'Tracking aquifer levels (satellite observation, well-drilling depth trends), currency black-market premium (Iranian rial to USD), IRGC revenue sources (sanctions-circumvention efficiency), and recruitment/defection rates within security apparatus',
    'If threshold approaches within 5-10 years: mandate faces a hard structural transition (Snare → no longer contains itself). If threshold is decades distant: current trajectory can persist in degraded state (Piton → extended inertial collapse). If threshold is already crossed but suppression masks it: hidden civil conflict waiting to emergent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_functional_threshold, empirical, 'When does mandate viability collapse beyond suppression-maintenance?').

omega_variable(
    agricultural_substitution_feasibility,
    'Can Iran shift to high-value, low-water crops (nuts, spices, aquaculture-independent protein) or desalination-based food security fast enough to prevent agricultural collapse while maintaining rural population viability?',
    'Comparative analysis of successful arid-nation agricultural transitions (Israel, UAE model), cost-benefit of desalination scaling, timeline to replant infrastructure, training lag for farmer adoption',
    'If feasible within 15 years: agricultural collapse is technocratic failure, not hydrological inevitability — shifts classification from Mountain to Tangled Rope (mandate choices, not nature). If not feasible: aquifer depletion is hydrological limit, but mandate prioritization ensures it manifests as human catastrophe rather than managed descent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(agricultural_substitution_feasibility, empirical, 'Can Iran transition to drought-resistant agriculture before collapse?').

omega_variable(
    suppression_breaking_threshold,
    'What combination of currency collapse severity, water rationing visibility, and youth unemployment triggers organized resistance that suppression apparatus cannot contain?',
    'Historical precedent from 1979 revolution, 2009 Green Movement, 2019 November protests — track triggering conditions (purchasing power loss >50%, visible rationing, unemployment >25% youth), capacity of security forces, organizational coordination of diaspora/internal networks',
    'If breakdown occurs before mandate reconstitution: Iran faces potential state fragmentation or violent transition. If suppression holds: mandatrophic collapse continues as extended managed decline (Piton behavior). This is the ''structural limit'' of the snare — can suppression scale with desperation?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_breaking_threshold, empirical, 'At what crisis threshold does suppression fail to contain resistance?').

omega_variable(
    regional_proxy_network_fragility,
    'How dependent is the IRGC''s legitimacy claim on sustained regional proxy funding and nuclear program advancement? If external pressure (Israeli action, sanctions intensification) disrupts either, does the domestic mandate collapse?',
    'Tracking IRGC resource allocation across proxy networks (Syria, Iraq, Lebanon, Yemen), sanctions impact on nuclear program timeline, adversary capability for infrastructure disruption, internal IRGC factional competition',
    'If proxy networks prove fragile: mandate legitimacy depends on external threats, creating a ''hostage to fortune'' dynamic where the IRGC must maintain high-stakes regional posture to justify internal suppression. If networks are durable: mandate has external security rationale even as internal ecosystem collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_proxy_network_fragility, empirical, 'How fragile are the proxy networks that justify the mandate?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_mandatrophic_collapse, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iran_tr_t0, iran_mandatrophic_collapse, theater_ratio, 0, 0.42).
narrative_ontology:measurement(iran_tr_t15, iran_mandatrophic_collapse, theater_ratio, 15, 0.54).
narrative_ontology:measurement(iran_tr_t30, iran_mandatrophic_collapse, theater_ratio, 30, 0.65).
narrative_ontology:measurement(iran_tr_t5, iran_mandatrophic_collapse, theater_ratio, 5, 0.46).

% Extraction over time
narrative_ontology:measurement(iran_be_t0, iran_mandatrophic_collapse, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(iran_be_t15, iran_mandatrophic_collapse, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(iran_be_t30, iran_mandatrophic_collapse, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(iran_be_t5, iran_mandatrophic_collapse, base_extractiveness, 5, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_mandatrophic_collapse, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(iran_mandatrophic_collapse, 0.55).
narrative_ontology:affects_constraint(iran_mandatrophic_collapse, middle_east_water_scarcity).
narrative_ontology:affects_constraint(iran_mandatrophic_collapse, sanctions_regime_effectiveness).
narrative_ontology:affects_constraint(iran_mandatrophic_collapse, regional_proxy_network_stability).

% DUAL FORMULATION NOTE:
% Iran's mandatrophic collapse is downstream of specific resource constraints (aquifer depletion, sanctions-induced capital loss) but represents a distinct structural constraint: the choice to prioritize mandate ideology over crisis management. Upstream constraints have their own ε values (aquifer physics ≤0.25 if managed rationally, sanctions extraction ≈0.50); the mandatrophic snare ε=0.68 reflects the compounding effect of rigid mandate prioritization that forecloses adaptive responses to upstream crises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iran_mandatrophic_collapse, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
