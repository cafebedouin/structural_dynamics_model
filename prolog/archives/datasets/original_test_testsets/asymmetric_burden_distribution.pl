% ============================================================================
% CONSTRAINT STORY: asymmetric_burden_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asymmetric_burden_distribution, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: asymmetric_burden_distribution
 *   human_readable: The Externalization Lever
 *   domain: economic/social/logistical
 *
 * SUMMARY:
 *   The 'Externalization Lever' is a structural pattern where the benefits of
 *   a system (e.g., global logistics, digital platforms, urban economies) are
 *   concentrated among a core group, while the operational costs (pollution,
 *   noise, stress, precarious labor) are systematically displaced onto a
 *   peripheral population. This creates a fundamental asymmetry in who
 *   benefits and who pays. The constraint is not merely an unfortunate
 *   byproduct but an active mechanism that enhances the efficiency and
 *   profitability of the core system by treating the periphery's environment
 *   and well-being as a free or low-cost resource.
 *
 * KEY AGENTS:
 *   - Peripheral Communities: Primary victims (powerless/trapped) — Residents near ports, warehouses, and freeways who bear health and quality-of-life costs.
 *   - Gig Economy Workers: Secondary victims (powerless/trapped) — Bear the costs of vehicle maintenance, lack of benefits, and algorithmic precarity.
 *   - Global Consumers: Primary beneficiaries (institutional/arbitrage) — Receive the benefits of low prices and fast delivery without directly experiencing the externalized costs.
 *   - Corporate Shareholders: Primary beneficiaries (institutional/arbitrage) — Profit from business models that minimize operational costs by externalizing them.
 *   - Regulators / Planners: Organized agents (organized/constrained) — Attempt to mitigate the harms through policy, viewing the current state as a temporary problem to be solved.
 *   - Corporate PR/Sustainability Offices: Institutional actors (institutional/arbitrage) — Manage the public perception of externalities, often through performative actions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asymmetric_burden_distribution, 0.65).
domain_priors:suppression_score(asymmetric_burden_distribution, 0.75).
domain_priors:theater_ratio(asymmetric_burden_distribution, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asymmetric_burden_distribution, extractiveness, 0.65).
narrative_ontology:constraint_metric(asymmetric_burden_distribution, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(asymmetric_burden_distribution, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asymmetric_burden_distribution, tangled_rope).
narrative_ontology:human_readable(asymmetric_burden_distribution, "The Externalization Lever").
narrative_ontology:topic_domain(asymmetric_burden_distribution, "economic/social/logistical").

domain_priors:requires_active_enforcement(asymmetric_burden_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asymmetric_burden_distribution, global_consumers).
narrative_ontology:constraint_beneficiary(asymmetric_burden_distribution, corporate_shareholders).
narrative_ontology:constraint_victim(asymmetric_burden_distribution, peripheral_communities).
narrative_ontology:constraint_victim(asymmetric_burden_distribution, gig_economy_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL COMMUNITY (SNARE) — Trapped by economic circumstance and zoning, this group bears the full cost of noise, pollution, and stress with no recourse or proportional benefit. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.74. This high effective extraction, combined with high suppression, meets the Snare classification.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GLOBAL CONSUMER (ROPE) — Experiences the system as pure coordination for convenience and low prices. Can switch between providers at will (arbitrage exit). As a primary beneficiary, directionality is low. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. Negative extraction indicates a net subsidy.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine coordination function (efficient supply chains) and the severe, asymmetric extraction it imposes. This balanced view recognizes the hybrid nature of the constraint. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90. This high χ value falls within the Tangled Rope range.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: URBAN PLANNER (SCAFFOLD) — Views the current system of externalized costs as a temporary, undesirable state to be superseded by better policy (e.g., green zones, fair wage laws). This implies a sunset clause on the current arrangement. d≈0.40, f(d)≈0.40, σ=0.9 → χ≈0.23. Low effective extraction meets the Scaffold gate (χ ≤ 0.30).
constraint_indexing:constraint_classification(asymmetric_burden_distribution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CORPORATE SUSTAINABILITY OFFICE (PITON) — Engages in performative mitigation (CSR reports, greenwashing) that has little functional impact on the core extractive model. The high theater_ratio (0.75) and low effective extraction for a beneficiary (χ≈-0.09) satisfy the Piton classification gates. The function is inertial, maintained for appearances.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MARKET FUNDAMENTALIST (MOUNTAIN) — Frames externalities as the unavoidable, natural 'price of progress' or an efficient outcome of market forces. This perspective naturalizes a contingent social arrangement. The engine will identify this as a false summit, as the base properties (ε=0.65, suppression=0.75) fail the Mountain gates.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asymmetric_burden_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asymmetric_burden_distribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(asymmetric_burden_distribution, TR),
    TR >= 0.70.

:- end_tests(asymmetric_burden_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65) is high, reflecting the severe, uncompensated health, environmental, and social costs imposed on victims. Suppression (0.75) is high because victims are structurally trapped; they lack the economic mobility to move away from infrastructure hubs or the political power to change zoning and labor laws. Theater Ratio (0.75) is high, representing the significant corporate and political effort spent on 'greenwashing' and CSR initiatives that perform concern without altering the fundamental extractive logic of the business model.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is a diagnostic exemplar, producing all six classifications from a single set of metrics. For the trapped victim, it is a Snare. For the consumer who only sees the benefits, it is a Rope. For the analyst who sees both sides, it is a Tangled Rope. For the regulator aiming for future improvements, it is a Scaffold. For the corporate PR department managing appearances, it is a Piton. For the ideologue who sees this as a natural law of economics, it is a Mountain. The gap is total, revealing that the 'type' of a constraint is a function of the observer's structural relationship to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (consumers, shareholders) have arbitrage exit options, leading to a low directionality (d) and negative effective extraction (χ), making the system appear as a pure coordination 'Rope'. Victims (peripheral communities, gig workers) are trapped, leading to a high directionality (d) and extremely high effective extraction (χ), making the system a coercive 'Snare'. The analytical observer's position results in a high χ that correctly identifies the mixed coordination/extraction nature of a 'Tangled Rope'.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy by demonstrating its premise: a single structural reality can be correctly perceived as multiple constraint types. The error is not in any single classification but in the attempt to assign one exclusive type to the entire system. The Deferential Realism framework's purpose is to hold all these valid, indexed perspectives simultaneously, with the analytical 'Tangled Rope' classification serving as the system's overall claim while acknowledging the legitimacy of the other views.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_quantification,
    'How can the non-monetary costs (health impacts, stress, environmental decay) be accurately quantified and compared to the monetary benefits of the system?',
    'Development of comprehensive social and environmental accounting metrics; longitudinal public health studies in affected vs. unaffected communities.',
    'Accurate quantification could shift the analytical classification from Tangled Rope to Snare by revealing a higher true extractiveness, or justify regulatory intervention (Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_quantification, empirical, 'Quantification of non-monetary externalities').

omega_variable(
    causal_attribution,
    'Are the negative externalities a direct result of deliberate corporate policy (a Snare) or an unavoidable emergent property of complex logistical systems (a Mountain)?',
    'Comparative analysis of firms with different externality-management policies; internal corporate document review; agent-based modeling of supply chains.',
    'Resolving this determines whether the solution is policy enforcement against specific actors or a fundamental redesign of the system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_attribution, conceptual, 'Distinguishing deliberate policy from emergent properties').

omega_variable(
    regulatory_effectiveness,
    'Are regulatory bodies genuinely attempting to mitigate harm (Scaffold perspective) or are their efforts largely performative due to regulatory capture (Piton perspective)?',
    'Analysis of enforcement actions versus policy statements; tracking lobbying expenditures and their correlation with weakened regulations.',
    'Determines whether the ''Scaffold'' view held by regulators is structurally sound or merely aspirational, collapsing into a Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_effectiveness, empirical, 'Effectiveness and intent of regulatory bodies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asymmetric_burden_distribution, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asym_tr_t0, asymmetric_burden_distribution, theater_ratio, 0, 0.3).
narrative_ontology:measurement(asym_tr_t10, asymmetric_burden_distribution, theater_ratio, 10, 0.6).
narrative_ontology:measurement(asym_tr_t20, asymmetric_burden_distribution, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(asym_be_t0, asymmetric_burden_distribution, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(asym_be_t10, asymmetric_burden_distribution, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(asym_be_t20, asymmetric_burden_distribution, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asymmetric_burden_distribution, resource_allocation).
narrative_ontology:affects_constraint(asymmetric_burden_distribution, fast_fashion_labor_practices).
narrative_ontology:affects_constraint(asymmetric_burden_distribution, amazon_warehouse_conditions).
narrative_ontology:affects_constraint(asymmetric_burden_distribution, urban_food_deserts).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
