% ============================================================================
% CONSTRAINT STORY: regional_air_defense_procurement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_air_defense_procurement, []).

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
 *   constraint_id: regional_air_defense_procurement
 *   human_readable: Regional Air Defense Procurement System
 *   domain: defense/regional_security/procurement
 *
 * SUMMARY:
 *   Regional air defense procurement creates a hybrid coordination-extraction
 *   system where the defense requirement (genuine security coordination)
 *   becomes the cover for monopolistic market control and geopolitical
 *   leverage. Developing regional states face legitimate air defense needs
 *   but encounter a market structure that offers limited real alternatives:
 *   once committed to a weapons family, switching incurs prohibitive
 *   switching costs through incompatible logistics, training retraining
 *   requirements, and ammunition standardization. Defense contractors enforce
 *   ecosystem lock-in through proprietary software, restricted maintenance
 *   authorization, and spare parts scarcity. Wealthy supplier nations
 *   leverage this dependency for geopolitical advantage. The system exhibits
 *   all characteristics of tangled rope: genuine coordination function
 *   (states do need air defense systems), but asymmetric extraction
 *   (beneficiary captures disproportionate gains, victim bears switching
 *   costs and supply vulnerability). The theater ratio has increased over the
 *   interval as procurement processes have become more elaborate while actual
 *   capacity to conduct indigenous maintenance has declined, indicating
 *   Goodhart drift — the process has become decoupled from the function it
 *   was designed to serve.
 *
 * KEY AGENTS:
 *   - Defense Contractors (institutional/arbitrage): Primary beneficiary — captures high margins, lock-in rents, long-term service contracts. Benefits from supply dependency enforcement.
 *   - Developing Regional States (powerless/trapped): Primary victim — faces genuine security needs but trapped in lock-in. Bears high costs of switching, supply disruption vulnerability, and geopolitical pressure.
 *   - Regional Military Command (organized/constrained): Secondary victim/beneficiary — coordinates defense function but constrained by procurement terms, maintenance restrictions, and spare parts dependency.
 *   - Wealthy Supplier Nation (powerful/constrained): Beneficiary and enforcer — gains geopolitical leverage, export revenue, and military-industrial influence. Constrained by international arms control norms and competition from alternative suppliers.
 *   - International Arms Control Regime (institutional/mobile): Institutional actor — nominally enforces proliferation prevention but selectively enforces and routinely subordinates to geopolitical expedience. Maintains performative function (compliance theater) while actual prevention function is weak.
 *   - Analytical Observer (analytical/analytical): Sees risk of naturalizing contingent extraction as inherent security dilemma.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_air_defense_procurement, 0.58).
domain_priors:suppression_score(regional_air_defense_procurement, 0.62).
domain_priors:theater_ratio(regional_air_defense_procurement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_air_defense_procurement, extractiveness, 0.58).
narrative_ontology:constraint_metric(regional_air_defense_procurement, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(regional_air_defense_procurement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_air_defense_procurement, tangled_rope).
narrative_ontology:human_readable(regional_air_defense_procurement, "Regional Air Defense Procurement System").
narrative_ontology:topic_domain(regional_air_defense_procurement, "defense/regional_security/procurement").

domain_priors:requires_active_enforcement(regional_air_defense_procurement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_air_defense_procurement, defense_contractors).
narrative_ontology:constraint_beneficiary(regional_air_defense_procurement, wealthy_nation_states).
narrative_ontology:constraint_beneficiary(regional_air_defense_procurement, procurement_bureaucrats).
narrative_ontology:constraint_victim(regional_air_defense_procurement, developing_region_states).
narrative_ontology:constraint_victim(regional_air_defense_procurement, military_personnel).
narrative_ontology:constraint_victim(regional_air_defense_procurement, civilian_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING REGIONAL STATE (SNARE) — Trapped in procurement lock-in. Once committed to a weapons system family, switching to alternatives incurs catastrophic costs: incompatible supply chains, lost training investment, ammunition standardization failure, political humiliation. The state faces genuine security threats and must purchase systems, but the market structure offers no real alternatives — dominant suppliers enforce ecosystem lock-in through parts scarcity, proprietary software, and training monopolies. Maximum suppression through technical and economic barriers.
constraint_indexing:constraint_classification(regional_air_defense_procurement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DEFENSE CONTRACTOR (ROPE) — Experiences the constraint as coordination: enabling regional states to procure systems that address collective security concerns. The contractor benefits from demand and market position, but also coordinates a genuine security function — providing air defense systems that deter aggression. From the contractor's position, the constraint is a stable coordination mechanism with clear beneficiaries. The asymmetry in favor of the contractor is not experienced as extraction but as fair market exchange.
constraint_indexing:constraint_classification(regional_air_defense_procurement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL MILITARY COMMAND (TANGLED ROPE) — Organized but constrained. Benefits from having sophisticated air defense systems (coordination function: deters adversary air operations). But also bears extraction: limited maintenance autonomy, high spare parts costs, dependence on foreign training, vulnerability to supply disruption during conflict. The command has some agency (can negotiate terms, choose among competing systems) but faces high switching costs and geopolitical pressure to align with supplier nation.
constraint_indexing:constraint_classification(regional_air_defense_procurement, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: WEALTHY SUPPLIER NATION (TANGLED ROPE) — Powerful actor with constrained options in the long term. Benefits from defense contractor revenue, geopolitical leverage over purchasing states, and extension of its military-industrial influence. But also coordinates a genuine security function — supplying systems that stabilize regional power balances. The supplier nation is constrained by international arms control regimes, alliance relationships, and the risk that over-extraction drives purchasing states toward competitors or indigenous development. Active enforcement through export controls and diplomatic pressure.
constraint_indexing:constraint_classification(regional_air_defense_procurement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL ARMS CONTROL REGIME (PITON) — Ostensibly coordinates proliferation prevention and transparency. In practice, enforcement is highly selective, applied differentially to weak vs. powerful states, and routinely subordinated to geopolitical expedience. The regime persists through institutional inertia and ceremonial compliance despite widespread violation and inconsistent enforcement. Theater ratio high (compliance theater, transparency theater) because the actual function — preventing destabilizing proliferation — is underperformed while the ceremonial function — appearing to regulate arms markets — is overperformed.
constraint_indexing:constraint_classification(regional_air_defense_procurement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT) — At civilizational/universal scope, an observer might perceive air defense procurement as governed by immutable constraints: security dilemmas are inherent to anarchic international systems, technological asymmetries are inevitable, and arms race dynamics are natural consequences of strategic competition. This natural law framing naturalizes what are actually contingent institutional arrangements — monopolistic market structure, lock-in through proprietary standards, enforcement asymmetries. The engine's false summit detector identifies this mountain as naturalization of contingent extraction.
constraint_indexing:constraint_classification(regional_air_defense_procurement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_air_defense_procurement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_air_defense_procurement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_air_defense_procurement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_air_defense_procurement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_air_defense_procurement, TR),
    TR >= 0.70.

:- end_tests(regional_air_defense_procurement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting genuine coordination function layered with substantial extraction. The base value has increased from 0.35 to 0.58 over the interval, indicating accumulation of rent-seeking mechanisms on top of the core coordination. Suppression (0.62): Moderately high. Genuine barriers include technical complexity requiring supplier training, incompatible supply chains, ammunition standardization, and geopolitical pressure to align with supplier. But suppression is not total — some states have maintained indigenous development programs, and alternative suppliers (China, Russia) provide exit options for some regions. Theater ratio (0.68): Indicates substantial performative activity. Procurement processes have become increasingly elaborate (tender documentation, compliance assessment, environmental review) while actual capacity to conduct independent verification or indigenous maintenance has declined. The ceremony of procurement has decoupled from the function of capability acquisition.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates large perspectival gaps. The defense contractor sees pure coordination (Rope) — they are solving the legitimate problem of providing sophisticated air defense systems. The developing state sees pure extraction (Snare) — they are trapped in a system where technological dependence becomes political leverage. The supplier nation sees tangled rope with a coordinating role — they provide security while gaining leverage. The regional military command sees tangled rope with constrained agency — they benefit from capability but suffer from autonomy loss. The international arms control regime sees itself as maintaining order (Piton) but enforcement is performative. The analytical observer risks seeing an immutable security dilemma (Mountain) when the constraint actually reflects contingent monopolistic market structure and geopolitical asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Deriving directionality (d) from structural position: Defense contractors are institutional beneficiaries with arbitrage exit options — they capture extraction gains and can serve multiple markets. Their d is low (near 0.1), making their f(d) ≈ -0.01, producing negative experienced extractiveness (they benefit). Developing regional states are powerless victims with trapped exit options — they have no alternative and bear full extraction costs. Their d is high (near 0.95), making f(d) ≈ 1.42, producing maximum experienced extractiveness. Regional military commands are organized agents with constrained exit — they partially benefit (capability coordination) but face real switching costs. Their d is moderate (0.55), producing f(d) ≈ 0.75, intermediate extraction experience. Supplier nations are powerful actors with constrained exit — they benefit geopolitically but face long-term constraints from alliance costs and competition. Their d is moderate (0.45), producing positive f(d) ≈ 0.5, but the institution gains leverage rather than experiencing extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying how the same procurement mechanism serves both genuine coordination and systematic extraction. The mandatrophy question is: Is this a rope (pure coordination for air defense) or a snare (pure extraction through lock-in)? The resolution is tangled rope: it is genuinely both. The coordination function (states need air defense, contractors provide capability) is real. But the extraction mechanism (lock-in, switching costs, supply weaponization) is equally real. The constraint cannot be classified as rope because the asymmetry is not incidental — it is structurally embedded in the vendor lock-in design. It cannot be classified as snare because eliminating the coordination function would not eliminate the extraction — states would still need air defense, the problem would just shift to whether they could access it. The tangled rope classification correctly identifies this as a hybrid where both functions are structural and neither is dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_capability_threshold,
    'At what level of indigenous air defense capability development does a regional state achieve genuine exit options from lock-in?',
    'Technical analysis of developmental milestones; comparison of indigenous system performance vs. imported systems; assessment of maintenance and upgrade autonomy',
    'If threshold is low (achievable in 5-10 years): the snare may transition to constrained/tangled_rope as exit becomes feasible. If threshold is high (20+ years): lock-in persists across generations and extraction deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_capability_threshold, empirical, 'Development timeline for indigenous air defense capability').

omega_variable(
    supply_chain_weaponization,
    'During regional conflict, can the supplier nation weaponize supply chains by withholding spare parts, software updates, or training cooperation?',
    'Historical case studies of defense supply disruption during conflict; assessment of actual spare parts stockpiling vs. stated autonomy; monitoring of software update controls during geopolitical tensions',
    'If weaponization is routine: suppression mechanism is higher than measured (0.62), extraction is closer to snare (χ > 0.66). If suppliers maintain supply chain neutrality: suppression is lower, tangled_rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_weaponization, empirical, 'Whether supplier nations weaponize spare parts and software supply chains').

omega_variable(
    coalition_counter_procurement,
    'Can developing regional states coordinate joint procurement of alternative air defense systems to break monopoly lock-in?',
    'Analysis of existing regional defense consortia; assessment of barriers to joint procurement (political divergence, technical standards incompatibility); monitoring of Chinese/Russian systems adoption as coordinated alternatives',
    'If coalition procurement succeeds: powerless agents organize into moderate/organized agents, snare transitions to constrained/rope. If coalition fails: lock-in persists, suppression remains high, snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_counter_procurement, empirical, 'Whether developing states can coordinate alternative procurement').

omega_variable(
    geopolitical_alliance_leverage,
    'Is the primary extraction mechanism economic (high margins, spare parts pricing) or geopolitical (political leverage through supply dependency)?',
    'Financial analysis of defense contractor margins vs. civilian tech industry norms; tracking of supplier nations'' use of arms supply as political leverage in diplomatic disputes; assessment of whether extraction persists without geopolitical asymmetry',
    'If primarily economic: constraint is driven by market structure (monopolistic contractor behavior). If primarily geopolitical: constraint is driven by alliance enforcement, and classification depends on whether supplier nation is beneficiary or enforcer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_alliance_leverage, empirical, 'Whether extraction is economic monopoly or geopolitical leverage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_air_defense_procurement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(radp_tr_t0, regional_air_defense_procurement, theater_ratio, 0, 0.45).
narrative_ontology:measurement(radp_tr_t5, regional_air_defense_procurement, theater_ratio, 5, 0.58).
narrative_ontology:measurement(radp_tr_t10, regional_air_defense_procurement, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(radp_be_t0, regional_air_defense_procurement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(radp_be_t5, regional_air_defense_procurement, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(radp_be_t10, regional_air_defense_procurement, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_air_defense_procurement, resource_allocation).
narrative_ontology:affects_constraint(regional_air_defense_procurement, arms_embargoes_and_sanctions).
narrative_ontology:affects_constraint(regional_air_defense_procurement, defense_industrial_base_concentration).
narrative_ontology:affects_constraint(regional_air_defense_procurement, indigenous_military_technology_development).

% DUAL FORMULATION NOTE:
% Regional air defense procurement is downstream of arms market monopolization and upstream of geopolitical leverage dynamics. Separate constraints model the supplier market concentration (structural cause) and the sanctions weaponization (enforcement mechanism). This story focuses on the procurement constraint itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regional_air_defense_procurement, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
