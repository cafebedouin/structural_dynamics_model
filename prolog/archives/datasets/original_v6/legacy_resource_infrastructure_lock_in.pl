% ============================================================================
% CONSTRAINT STORY: legacy_resource_infrastructure_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legacy_resource_infrastructure_lock_in, []).

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
 *   constraint_id: legacy_resource_infrastructure_lock_in
 *   human_readable: Legacy Resource Infrastructure Lock-In
 *   domain: economic/infrastructure/institutional
 *
 * SUMMARY:
 *   Legacy resource infrastructure lock-in represents a class of constraints
 *   where past capital investments in established systems create structural
 *   dependencies that persist long after the systems become economically or
 *   environmentally suboptimal. The constraint exhibits genuine coordination
 *   functions (reliable supply, established distribution, network effects)
 *   embedded within asymmetric extraction (incumbent operators capture rents,
 *   alternatives face market barriers, consumers bear switching costs). The
 *   classification as Tangled Rope reflects that both coordination and
 *   extraction are real and structural — the lock-in cannot be understood as
 *   pure market power (Snare) because the infrastructure genuinely solves
 *   coordination problems, nor as pure coordination (Rope) because the
 *   beneficiary group actively maintains barriers to superior alternatives.
 *   The extractiveness has increased over the 45-year interval (0.32 → 0.58)
 *   as alternative technologies have become cost-competitive, intensifying
 *   the suppression required to maintain incumbent protection. The theater
 *   ratio has risen (0.55 → 0.68) as regulatory frameworks have become
 *   increasingly performative — safety standards and operational requirements
 *   designed for legacy systems persist despite clear obsolescence. This
 *   measurement signature indicates Goodhart drift: regulatory theater
 *   substitutes for actual function as the underlying justification for
 *   lock-in weakens.
 *
 * KEY AGENTS:
 *   - Incumbent Infrastructure Operators: Primary beneficiary (institutional/arbitrage) — extractors capturing rents through sunk-cost protection and regulatory moats
 *   - Alternative Technologies and Emerging Sectors: Primary victim (powerless/trapped) — cannot compete despite superior efficiency; face regulatory barriers, subsidy disadvantages, and network effect exclusion
 *   - Dependent Consumer Base and Regional Economies: Secondary victim (moderate/constrained) — structurally dependent on legacy infrastructure; bear switching costs and suppressed innovation benefits
 *   - Transition Governance and Policy Coalitions: Organized actors (organized/constrained) — environmental movements, next-generation firms, efficiency advocates building exit pathways through standards, carbon pricing, and infrastructure investment
 *   - Regulatory and Standardization Bodies: Institutional actors (institutional/arbitrage) — maintain legacy standards and licensing frameworks; beneficiaries of procedural legitimacy tied to incumbent protection
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional lock-in as immutable infrastructure physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legacy_resource_infrastructure_lock_in, 0.58).
domain_priors:suppression_score(legacy_resource_infrastructure_lock_in, 0.65).
domain_priors:theater_ratio(legacy_resource_infrastructure_lock_in, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legacy_resource_infrastructure_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(legacy_resource_infrastructure_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(legacy_resource_infrastructure_lock_in, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legacy_resource_infrastructure_lock_in, tangled_rope).
narrative_ontology:human_readable(legacy_resource_infrastructure_lock_in, "Legacy Resource Infrastructure Lock-In").
narrative_ontology:topic_domain(legacy_resource_infrastructure_lock_in, "economic/infrastructure/institutional").

domain_priors:requires_active_enforcement(legacy_resource_infrastructure_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legacy_resource_infrastructure_lock_in, incumbent_infrastructure_operators).
narrative_ontology:constraint_beneficiary(legacy_resource_infrastructure_lock_in, established_extraction_industries).
narrative_ontology:constraint_victim(legacy_resource_infrastructure_lock_in, emerging_resource_sectors).
narrative_ontology:constraint_victim(legacy_resource_infrastructure_lock_in, efficiency_gains_unrealized).
narrative_ontology:constraint_victim(legacy_resource_infrastructure_lock_in, alternative_technologies).
narrative_ontology:constraint_victim(legacy_resource_infrastructure_lock_in, end_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE TECHNOLOGIES (SNARE) — Trapped by sunk-cost infrastructure and regulatory frameworks designed around legacy systems. Cannot compete on equal terms despite superior efficiency profiles. Bears full extraction burden through market access barriers, subsidies flowing to incumbents, and regulatory licensing requirements designed to protect legacy operators. Maximum experienced extraction with no structural exit path within the constraint's time horizon.
constraint_indexing:constraint_classification(legacy_resource_infrastructure_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEPENDENT CONSUMER BASE (TANGLED ROPE) — Constrained by infrastructure dependency; switching costs are material and relocation is costly. But the legacy system does provide genuine coordination benefits (reliable supply, established distribution networks, stable pricing). High suppression of alternatives through network effects and compatibility lock-in, yet some benefit from the coordination infrastructure itself. Moderate effective extraction with partial agency.
constraint_indexing:constraint_classification(legacy_resource_infrastructure_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT OPERATORS (ROPE) — Benefits from sunk-cost coordination lock. Experiences the legacy infrastructure as a coordination solution: established supply chains, predictable demand, regulatory certainty, and amortized capital costs. Net extractors through regulatory capture and subsidy flows, but the core function is coordination. Low effective extraction from their perspective because they are the beneficiaries whose interests define the constraint.
constraint_indexing:constraint_classification(legacy_resource_infrastructure_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSITION GOVERNANCE (SCAFFOLD) — Organized agents (environmental movements, efficiency advocates, next-generation technology firms) see the lock-in as a temporary governance failure with a structured sunset. Carbon accounting mandates, renewable portfolio standards, and infrastructure investment funds create regulated pathways out of legacy dependence. High suppression now (incumbent lobbying blocks transition), but organized agents perceive and operate toward an exit timeline (net-zero mandates, stranded asset frameworks). Effective extraction is moderate because this perspective has exit agency.
constraint_indexing:constraint_classification(legacy_resource_infrastructure_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY BODIES (PITON) — Legacy regulatory frameworks, safety standards, and licensing procedures were designed to protect legacy infrastructure and have become substantially performative. New technologies must meet standards written for old technologies (e.g., electrical grid codes designed for central generation, transmission standards that don't accommodate distributed resources). The regulatory maintenance theater persists through institutional inertia — the bodies maintain the standards to preserve institutional legitimacy despite knowing the standards protect obsolete systems. Theater ratio high; functional verification low.
constraint_indexing:constraint_classification(legacy_resource_infrastructure_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, infrastructure lock-in appears as an immutable property of large-scale systems: sunk costs in physical infrastructure are real; network effects are structural; switching costs are irreducible. However, the base properties indicate this is a Tangled Rope with enforcement requirements, not a mountain. The 'immutability' framing naturalizes what is actually institutional design and policy choice. The engine will identify this as a false summit — a false natural law detection.
constraint_indexing:constraint_classification(legacy_resource_infrastructure_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legacy_resource_infrastructure_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legacy_resource_infrastructure_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legacy_resource_infrastructure_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legacy_resource_infrastructure_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legacy_resource_infrastructure_lock_in, TR),
    TR >= 0.70.

:- end_tests(legacy_resource_infrastructure_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through three mechanisms: (1) Regulatory barriers to alternative entry (licensing, interconnection standards, safety requirements written for legacy systems); (2) Subsidy and tax asymmetries favoring incumbents (production tax credits for legacy fuels, amortized capital cost advantages); (3) Network effects and switching costs that trap consumers and complementary sectors. The value reflects that while the extraction is real and measurable, it is not maximal — some extraction flows are justified as coordination costs (infrastructure maintenance, reliability assurance), and the intensity varies by sector and geography. Suppression (0.65): Moderate-high. Alternatives face material barriers including physical incompatibility (distributed resources cannot connect to legacy grid architectures), regulatory incompatibility (standards written for centralized generation), and incumbent lobbying that blocks standard changes. However, suppression is not total — coalitions are building transition pathways, and the regulatory architecture is beginning to shift in high-policy-salience sectors (renewable energy, electric vehicles). Theater ratio (0.68): High. Regulatory frameworks maintain substantial performative content: safety standards justify incumbent protection despite clear technical obsolescence; operational reliability arguments mask incumbent preference for predictable demand over system efficiency; 'proven technology' arguments in licensing delay alternatives despite superior performance data. The theater has increased (0.55 → 0.68) as the underlying justification for lock-in has weakened.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap opens between the incumbent operator's experience (Rope — coordination) and the alternative technology's experience (Snare — pure extraction). The incumbent sees a coordination solution: established supply chains, predictable demand, amortized capital recovery, regulatory certainty. The alternative technology sees only extraction: forbidden market entry, subsidized competitors, incompatible standards, capital requirements for market-making that incumbents don't face. This gap reveals the constraint's Tangled Rope structure: the coordination is real (infrastructure genuinely does coordinate supply and demand), but the coordination function is asymmetrically distributed — it coordinates in favor of incumbents and against alternatives. A third perspectival gap opens between the consumer's experience (Tangled Rope — constrained with some benefit) and the powerless alternative sector's experience (Snare — trapped with no benefit). The scaffold perspective introduces a temporal structure that the other perspectives lack: organized transition coalitions see a regulated sunset timeline that makes current suppression tolerable as temporary. This introduces what might be called 'perspectival hope' — the scaffold perspective can bear higher suppression now because it perceives the constraint as bounded in time.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position and exit options. Incumbent operators show beneficiary + arbitrage exit → low d (around 0.15) → negative or near-zero f(d) → they extract from the system. Alternative technologies show victim + trapped exit → high d (around 0.95) → high f(d) ≈ 1.42 → they experience maximum extraction. Dependent consumers show mixed positioning: they are partly victims (trapped by switching costs) and partly beneficiaries (genuine infrastructure services), with constrained exit → moderate d (around 0.70) → moderate f(d) ≈ 1.08. The organized transition coalition shows victim + constrained exit but with high power and explicit exit agency → moderate d but significantly reduced effective extraction because the agent perceives and operates toward an exit timeline. This directionality structure is why the constraint is Tangled Rope rather than Snare: if it were pure Snare, all perspectives except the beneficiary would show trapped agents with high d and high extraction. Instead, the scaffold perspective shows constrained agents with agency and a timeline, reducing effective extraction despite high suppression. This is the signature of a hybrid: genuine coordination (the infrastructure does work) embedded in asymmetric protection of who benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification resolves potential mandatrophy between 'this is pure incumbent extraction' (Snare) and 'this is just coordination inertia' (Piton or Rope). The mandatrophy is resolved by recognizing that both statements contain truth: the infrastructure does coordinate (Rope function is genuine), AND the incumbent protection mechanism extracts (asymmetric distribution of coordination benefits). The presence of beneficiaries (incumbent operators) combined with victims (alternatives and consumers) and active enforcement (regulatory capture, subsidy design) satisfies the Tangled Rope gates: requires_active_enforcement=true, beneficiaries array populated, victims array populated. The piton perspective is present (regulatory theater at 0.68) but does not dominate — the primary function is still extraction of incumbent benefits, not maintenance through pure theater. The measurement trajectory (extractiveness increasing from 0.32 to 0.58, theater rising from 0.55 to 0.68) shows the constraint intensifying over time as alternatives become cost-competitive, requiring more theater to maintain incumbent protection. This is consistent with Tangled Rope dynamics: as the genuine coordination justification for incumbent preference weakens, enforcement must increase to maintain protection. A Rope constraint would show declining theater as genuine coordination tightens; a Piton would show high constant theater as institutional maintenance continues. This constraint shows increasing theater despite increasing underlying extraction — a signature of hybrid constraint evolution under pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lock_in_irreversibility_threshold,
    'At what capital depreciation rate does legacy infrastructure transition from ''still-recovering-sunk-costs'' to ''genuinely-obsolete-but-protected''?',
    'Historical infrastructure lifecycle analysis; comparison of depreciation schedules across energy, transportation, and telecommunications sectors; identification of the point where replacement cost falls below protection cost',
    'If threshold is < 10 years: lock-in is temporary (scaffold perspective valid). If threshold is > 30 years: lock-in approaches mountain status (immutable infrastructure lifetime). Policy interventions timing depends critically on this threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_irreversibility_threshold, empirical, 'Threshold for distinguishing recoverable sunk costs from obsolete protection').

omega_variable(
    incumbent_exit_optionality,
    'Can incumbent operators themselves transition to new infrastructure models without existential business failure, or is their survival incompatible with the transition?',
    'Analysis of incumbent diversification success rates in previous infrastructure transitions (from coal to gas, from landline to mobile); identification of structural business model incompatibilities vs. pure competitiveness gaps',
    'If transition-compatible: beneficiary group can exit gracefully (reduces suppression). If incompatible: beneficiary group has no exit (increases suppression, may shift beneficiary group to successor institutions). Affects whether lock-in is Tangled Rope or Snare from incumbent perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_exit_optionality, empirical, 'Whether incumbent business models are compatible with infrastructure transition').

omega_variable(
    regulatory_capture_mechanism_durability,
    'Is the lock-in maintained by active incumbent lobbying and regulatory capture, or by passive institutional inertia and technical path-dependency?',
    'Litigation discovery on infrastructure lobbying; regulatory filing analysis on standards comment periods; comparison of lobbying expenditure to suppression magnitude',
    'If active capture: organized transition coalitions can break the lock through political mobilization (scaffold is viable). If passive inertia: the constraint will persist through sheer organizational weight even without capture (approaches mountain). Determines plausibility of mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism_durability, empirical, 'Whether lock-in is actively maintained or passively persistent').

omega_variable(
    alternative_technology_financial_viability,
    'At what subsidy level or carbon price do alternative technologies become economically dominant, independent of lock-in suppression?',
    'Marginal cost analysis of legacy vs alternative systems; break-even carbon pricing models; sensitivity analysis of technology cost trajectories',
    'If alternatives are cost-superior at realistic carbon prices: lock-in is pure extraction (Snare). If alternatives require permanent subsidies: lock-in may be hiding genuine coordination advantages (Tangled Rope is accurate). Affects whether Snare vs Tangled Rope classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_technology_financial_viability, empirical, 'Financial viability threshold for alternative technologies without lock-in suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legacy_resource_infrastructure_lock_in, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legacyres_tr_t0, legacy_resource_infrastructure_lock_in, theater_ratio, 0, 0.55).
narrative_ontology:measurement(legacyres_tr_t15, legacy_resource_infrastructure_lock_in, theater_ratio, 15, 0.62).
narrative_ontology:measurement(legacyres_tr_t30, legacy_resource_infrastructure_lock_in, theater_ratio, 30, 0.68).
narrative_ontology:measurement(legacyres_tr_t45, legacy_resource_infrastructure_lock_in, theater_ratio, 45, 0.71).

% Extraction over time
narrative_ontology:measurement(legacyres_be_t0, legacy_resource_infrastructure_lock_in, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(legacyres_be_t15, legacy_resource_infrastructure_lock_in, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(legacyres_be_t30, legacy_resource_infrastructure_lock_in, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(legacyres_be_t45, legacy_resource_infrastructure_lock_in, base_extractiveness, 45, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legacy_resource_infrastructure_lock_in, resource_allocation).
narrative_ontology:affects_constraint(legacy_resource_infrastructure_lock_in, carbon_emission_path_dependency).
narrative_ontology:affects_constraint(legacy_resource_infrastructure_lock_in, renewable_energy_grid_integration).
narrative_ontology:affects_constraint(legacy_resource_infrastructure_lock_in, transportation_infrastructure_transition).
narrative_ontology:affects_constraint(legacy_resource_infrastructure_lock_in, incumbent_subsidy_lock_in).

% DUAL FORMULATION NOTE:
% Legacy infrastructure lock-in is a constraint family with domain-specific manifestations (energy, transportation, telecommunications, water). The general constraint story models the structural pattern; specific implementations (coal infrastructure lock-in, internal combustion vehicle dominance, landline telephone systems) are downstream constraints with their own extractiveness values reflecting domain-specific switching costs and regulatory barriers. The family structure enables comparative analysis of which domains show the highest extractiveness and which show the most viable transition pathways (scaffold perspectives).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legacy_resource_infrastructure_lock_in, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
