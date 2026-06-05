% ============================================================================
% CONSTRAINT STORY: india_nuclear_liability_act_2010
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_india_nuclear_liability_act_2010, []).

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
 *   constraint_id: india_nuclear_liability_act_2010
 *   human_readable: India's Civil Liability for Nuclear Damage Act of 2010
 *   domain: economic/political
 *
 * SUMMARY:
 *   India's Civil Liability for Nuclear Damage Act of 2010 establishes a
 *   legal framework enabling India's nuclear energy expansion by capping
 *   operator liability at INR 1,500 crore (USD ~180M) for any single nuclear
 *   accident. The act embodies a structural tension between three legitimate
 *   policy objectives: (1) enabling nuclear investment and energy access
 *   through bankable risk allocation, (2) protecting accident victims through
 *   accessible compensation mechanisms, and (3) preserving operator safety
 *   incentives by avoiding catastrophic liability exposure. The act achieves
 *   objective (1) and partially addresses (2) while introducing moral hazard
 *   concerns around (3). The constraint exhibits mixed coordination (enabling
 *   infrastructure development) and asymmetric extraction (capping operator
 *   liability below expected tail-risk costs, externalizing catastrophic
 *   losses onto accident-affected populations and environmental commons).
 *   Theater has increased since 2010 as international nuclear deployment has
 *   decelerated, rendering the liability framework increasingly performative
 *   — maintaining legal form despite reduced economic fundamentals for
 *   nuclear expansion.
 *
 * KEY AGENTS:
 *   - Nuclear Operators (NPCIL, private operators): Primary beneficiary (institutional/arbitrage) — liability cap makes nuclear investment bankable by capping downside exposure; can invest in alternative energy projects if nuclear terms degrade.
 *   - Foreign Reactor Suppliers (Westinghouse, Areva, Larsen & Toubro): Primary beneficiary (powerful/arbitrage) — indemnified against accident liability; strong exit options to other countries with similar frameworks.
 *   - Accident-Affected Populations: Primary victim (powerless/trapped) — bear economic and health costs beyond liability cap; cannot relocate without financial ruin; no bargaining power in compensation negotiations.
 *   - Environmental Commons (agricultural regions, water resources, fishing grounds): Secondary victim (moderate/constrained) — externalize long-term soil contamination and radiation exposure beyond formal liability zones; regional communities experience concentrated damage.
 *   - Government Agencies and Civil Society: Organized actors (organized/constrained) — coordinate energy infrastructure development (genuine function) but constrained exit from framework; experience extracted environmental remediation costs.
 *   - International Nuclear Regime: Institutional actor (institutional/arbitrage) — Vienna Convention framework maintains liability cap as global standard; persists through inertia despite weakened nuclear deployment expectations.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing political liability choices as inherent to nuclear thermodynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_nuclear_liability_act_2010, 0.52).
domain_priors:suppression_score(india_nuclear_liability_act_2010, 0.68).
domain_priors:theater_ratio(india_nuclear_liability_act_2010, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, extractiveness, 0.52).
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(india_nuclear_liability_act_2010, tangled_rope).
narrative_ontology:human_readable(india_nuclear_liability_act_2010, "India's Civil Liability for Nuclear Damage Act of 2010").
narrative_ontology:topic_domain(india_nuclear_liability_act_2010, "economic/political").

domain_priors:requires_active_enforcement(india_nuclear_liability_act_2010).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(india_nuclear_liability_act_2010, nuclear_operators).
narrative_ontology:constraint_beneficiary(india_nuclear_liability_act_2010, foreign_reactor_suppliers).
narrative_ontology:constraint_victim(india_nuclear_liability_act_2010, accident_affected_populations).
narrative_ontology:constraint_victim(india_nuclear_liability_act_2010, environmental_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCIDENT VICTIMS (SNARE) — Populations in nuclear facility exclusion zones and downwind areas have no exit option and bear maximum extraction. Liability caps at INR 1,500 crore (approximately USD 180M) are insufficient for Fukushima-scale catastrophes. Victims cannot abandon property or relocate without financial ruin. The act provides operators with liability shields while victims bear economic and health costs beyond the capped compensation.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ENVIRONMENTAL COMMONS (TANGLED ROPE) — Agricultural regions, water resources, and soil integrity near nuclear facilities experience mixed costs and benefits. The act coordinates infrastructure development (genuine coordination function: energy access, industrial development) but asymmetrically extracts from environmental commons through radiation exposure risk and long-term soil contamination liability. Environmental remediation costs are externalized beyond the liability cap, concentrating damage on rural populations and farming communities with constrained exit.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NUCLEAR OPERATORS (ROPE) — Indian and state-owned reactor operators experience the act as enabling coordination: the liability framework makes nuclear investment bankable by capping their downside risk. They experience extraction as manageable operational cost and insurance premium. Operators have exit options (they can invest in alternative energy projects) and benefit from the liability structure that shifts catastrophic tail risk to public balance sheets.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FOREIGN SUPPLIERS (ROPE) — Westinghouse, Areva, and other international suppliers experience India's liability framework as pure coordination: it guarantees indemnification against accident liability, making India a bankable market. Suppliers have strong arbitrage options (they can exit to other countries with similar frameworks) and experience maximum institutional benefit. The act incentivizes equipment sales to India precisely because liability is capped.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE AND CIVIL SOCIETY (TANGLED ROPE) — Government agencies and activist organizations see a hybrid constraint: the act coordinates energy infrastructure development (genuine function: India needs ~450GW nuclear capacity by 2050 per national targets) but enforces asymmetric extraction through liability caps that externalize tail-risk costs. Civil society has constrained exit (national regulatory bodies cannot avoid the framework; NGOs can advocate but cannot opt out) and experiences extraction through underfunded environmental remediation and displaced communities.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL REGIME (PITON) — The Vienna Convention framework (1963) and its 1997 Protocol that India joined established liability caps globally as nuclear industry standard. India's 2010 act is largely theatrical compliance with international expectations, dressed in domestic legal authority. The international regime persists through inertia — the core function (enabling nuclear investment) is maintained, but the performative content (liability assessment, compensation disbursement) has atrophied as nuclear capacity has failed to grow according to 1960s projections. Theater ratio is high because the regime's legal form persists despite weakened economic fundamentals.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC RISK VIEW (MOUNTAIN) — From a civilizational analytical perspective, some asymmetry between low-probability catastrophic risk and manageable insurance/liability frameworks is inherent to nuclear energy thermodynamics. Tail-risk extraction and liability capping reflect fundamental constraints on how societies finance high-consequence-low-probability infrastructure. This perspective risks naturalizing contingent legal structures as immutable physical laws. However, the structural data contradicts this — the 'natural' liability cap reflects political choices (lobbying power, international coordination) not thermodynamic necessity.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_nuclear_liability_act_2010_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(india_nuclear_liability_act_2010, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(india_nuclear_liability_act_2010, TR),
    TR >= 0.70.

:- end_tests(india_nuclear_liability_act_2010_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The liability cap redistributes catastrophic accident risk from operators to accident-affected populations. Real Fukushima costs exceeded USD 100B; India's cap of ~USD 180M represents extraction of ~55% of likely tail-risk costs. However, extractiveness is not maximal (0.70+) because the act provides genuine coordination benefit — nuclear investment would not occur without liability capping, and energy expansion is a legitimate collective objective. Suppression (0.68): High. Populations near nuclear facilities have severely limited exit options: relocation is economically impossible without compensation, property abandonment is irreversible, and regulatory voice is constrained by technical complexity and political asymmetry. Operator exit options are high (they can invest elsewhere); victim exit options are near zero. Theater ratio (0.58): Moderate-high. The liability assessment and compensation disbursement process exhibits performative elements (lengthy claims procedures, technical evidence requirements that disadvantage affected populations) but retains some functional content. The theater has increased over 14 years as nuclear deployment expectations have declined and the framework's enabling function has weakened relative to its allocation function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival divergence reflects fundamental disagreement about whether the act solves a coordination problem or imposes extraction. Operators and suppliers frame it as enabling coordination: without liability capping, nuclear investment is impossible, and energy access requires nuclear expansion. Victims and environmental commons frame it as extractive: coordination is being achieved at their expense, and the coordination benefit (energy access) is unevenly distributed (benefits urban industrial centers, costs fall on rural populations). Civil society and government agencies occupy the middle: they recognize both functions but experience the constraint as enforced (they cannot exit the framework). The analytical observer risks mischaracterizing the extractive redistribution as a natural thermodynamic limit. The perspectival gap is widest between operators (who see coordination enabling) and victims (who see pure extraction with no coordination benefit to themselves).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position: power level relative to the constraint, exit options, and beneficiary/victim status. Operators with institutional power and arbitrage options (high exit capacity) experience low d → negative χ (they experience extraction running toward them, i.e., benefit). Accident victims with powerless status and trapped exit (zero exit capacity) experience high d → high χ (they experience extraction running away from them, i.e., cost). Environmental commons with moderate power and constrained exit (partial exit capacity) experience moderate d → moderate χ (mixed extraction-benefit experience). Civil society with organized power and constrained exit experiences moderate d but perceives the constraint as enforced, so effective χ includes suppression effects. The international regime with institutional power and arbitrage options experiences low d, positioning it as beneficiary. The directionality derives structurally from exit capacity and cost allocation, not from nominal power level.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves as genuine Tangled Rope (claimed_type), confirming mixed coordination and extraction rather than pure extraction (Snare) or pure coordination (Rope). The coordination function is real and substantial — the liability cap is essential for bankable nuclear investment, and energy expansion is a legitimate collective objective aligned with Indian development goals and climate mitigation. However, the coordination is achieved through asymmetric extraction: accident-affected populations and environmental commons are forced to subsidize the coordination benefit by bearing tail-risk costs exceeding statutory compensation. The act REQUIRES active enforcement (Section 17 strict liability, government indemnification clauses) confirming tangled_rope gate. Beneficiaries (operators, suppliers) are real and identifiable. Victims (accident-affected populations, environmental commons) are real and identifiable. The extraction is not a side effect of coordination but a structural feature: the coordination exists BECAUSE the extraction is imposed. Without the liability cap (extraction), there would be no bankable nuclear investment (coordination loss). This is the defining property of tangled rope — coordination and extraction are coupled, not separable. The false mandatrophy ('is this coordination or extraction?') dissolves when you recognize that the constraint enforces BOTH, asymmetrically. Operators experience coordination; victims experience extraction; the state coordinates around the distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_risk_valuation,
    'What is the true actuarial liability for a Fukushima-equivalent accident in India, and how does it compare to the INR 1,500 crore cap?',
    'Probabilistic risk assessment of Indian reactor designs in seismic/flood zones; modeling of evacuation costs, lost agricultural output, health expenses, property decontamination; comparison to actual Fukushima settlement costs (>USD 100B)',
    'If true liability > INR 10,000 crore: cap is severe extraction, classification shifts toward pure snare. If true liability < INR 2,000 crore: cap is reasonable risk allocation, classification shifts toward rope coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tail_risk_valuation, empirical, 'Actuarial valuation of catastrophic accident liability vs. statutory cap').

omega_variable(
    foreign_supplier_liability_shift,
    'Does the liability cap shift accident responsibility from equipment manufacturers to host-country victims, or does supply-chain indemnification adequately allocate design-defect liability?',
    'Analysis of supplier indemnification clauses in Westinghouse/NPCIL contracts; comparison with supplier liability in US, France, and Japan frameworks; tracking of accident causation across vendor-design vs. operator-error vs. external-force dimensions',
    'If suppliers use Act Section 17(b) to escape design-defect liability: extracts from victims toward suppliers (intensifies snare). If supplier contracts retain design-liability allocation: constraint is hybrid coordination (confirms tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_supplier_liability_shift, empirical, 'Whether supplier liability shields distort accident responsibility allocation').

omega_variable(
    operator_safety_incentive_erosion,
    'Does the liability cap reduce operator investment in safety infrastructure by capping their downside risk exposure?',
    'Comparative analysis of safety spending (capital and O&M) in Indian reactors vs. utilities in uncapped-liability jurisdictions (France, Canada); correlation between liability framework and safety-system redundancy; accident-rate statistics by regulatory regime',
    'If safety spending is correlated with liability exposure: cap induces moral hazard, confirming extraction (snare) from accident victims. If safety metrics are independent of cap: liability framework is neutral on operational safety (pure coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operator_safety_incentive_erosion, empirical, 'Causal relationship between liability caps and operator safety investment').

omega_variable(
    displacement_externality_scope,
    'What percentage of accident-cost externalities are borne by non-stakeholder populations (farmers, fishing communities) with no seat at compensation negotiations?',
    'Mapping of radiation exposure contours beyond designated liability zones; documentation of agricultural disinvestment and fishing-ground contamination outside formal accident zones; tracking of compensation claims filed and rejected',
    'If externalities > 40% of total costs: victims are categorically unable to participate in compensation (pure snare). If externalities < 10%: framework captures most stakeholders (tangled_rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(displacement_externality_scope, empirical, 'Scope of accident-cost externalities borne by non-negotiating populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(india_nuclear_liability_act_2010, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(india_nuc_tr_t0, india_nuclear_liability_act_2010, theater_ratio, 0, 0.42).
narrative_ontology:measurement(india_nuc_tr_t7, india_nuclear_liability_act_2010, theater_ratio, 7, 0.52).
narrative_ontology:measurement(india_nuc_tr_t14, india_nuclear_liability_act_2010, theater_ratio, 14, 0.58).

% Extraction over time
narrative_ontology:measurement(india_nuc_be_t0, india_nuclear_liability_act_2010, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(india_nuc_be_t7, india_nuclear_liability_act_2010, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(india_nuc_be_t14, india_nuclear_liability_act_2010, base_extractiveness, 14, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(india_nuclear_liability_act_2010, enforcement_mechanism).
narrative_ontology:affects_constraint(india_nuclear_liability_act_2010, nuclear_supply_chain_dependency).
narrative_ontology:affects_constraint(india_nuclear_liability_act_2010, india_energy_infrastructure_lock_in).
narrative_ontology:affects_constraint(india_nuclear_liability_act_2010, rural_land_externalities).

% DUAL FORMULATION NOTE:
% India's Civil Liability for Nuclear Damage Act of 2010 is a constraint family member. The upstream constraint is international_nuclear_liability_regime (ε=0.35, Piton) — the Vienna Convention framework that India adopted. The downstream constraints are specific materialized forms: nuclear_supply_chain_dependency reflects supplier indemnification structures; india_energy_infrastructure_lock_in reflects the energy policy commitment to nuclear expansion; rural_land_externalities reflects concentrated costs borne by non-negotiating populations. The act links these constraints through legal/institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(india_nuclear_liability_act_2010, powerful, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
