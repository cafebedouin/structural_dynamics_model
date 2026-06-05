% ============================================================================
% CONSTRAINT STORY: digital_euro_cbdc
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_euro_cbdc, []).

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
 *   constraint_id: digital_euro_cbdc
 *   human_readable: The European Union's Central Bank Digital Currency (CBDC)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The proposed Digital Euro is a central bank-issued digital currency
 *   intended to complement cash and commercial bank deposits. Its
 *   implementation has several intended coordination benefits, but also
 *   carries the risk of extraction from various actors. The Digital Euro
 *   affects different actors differently, giving rise to diverse
 *   perspectives.
 *
 * KEY AGENTS:
 *   - European Central Bank: Primary beneficiary (institutional/arbitrage) - Seeks to maintain control over monetary policy.
 *   - EU Governments: Secondary beneficiary (institutional/constrained) - Aims to improve tax collection and welfare distribution.
 *   - Large Merchants: Tertiary beneficiary (powerful/mobile) - Expected to benefit from lower transaction fees and seamless e-commerce integration.
 *   - Small Merchants: Primary victim (moderate/constrained) - Faces increased regulatory burden and potential fees.
 *   - Privacy-Conscious Citizens: Secondary victim (powerless/trapped) - Concerned about surveillance and loss of financial privacy.
 *   - Competing Payment Systems: (moderate/mobile) - May be stifled by the dominance of the digital euro.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_euro_cbdc, 0.55).
domain_priors:suppression_score(digital_euro_cbdc, 0.45).
domain_priors:theater_ratio(digital_euro_cbdc, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_euro_cbdc, extractiveness, 0.55).
narrative_ontology:constraint_metric(digital_euro_cbdc, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(digital_euro_cbdc, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_euro_cbdc, tangled_rope).
narrative_ontology:human_readable(digital_euro_cbdc, "The European Union's Central Bank Digital Currency (CBDC)").
narrative_ontology:topic_domain(digital_euro_cbdc, "economic/technological").

domain_priors:requires_active_enforcement(digital_euro_cbdc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_euro_cbdc, european_central_bank).
narrative_ontology:constraint_beneficiary(digital_euro_cbdc, eu_governments).
narrative_ontology:constraint_beneficiary(digital_euro_cbdc, large_merchants).
narrative_ontology:constraint_victim(digital_euro_cbdc, small_merchants).
narrative_ontology:constraint_victim(digital_euro_cbdc, privacy_conscious_citizens).
narrative_ontology:constraint_victim(digital_euro_cbdc, competing_payment_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Privacy-conscious citizens, who may be forced to use the Digital Euro for certain transactions, experience this as a snare. They have limited exit options and are subject to surveillance.
constraint_indexing:constraint_classification(digital_euro_cbdc, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Small merchants are victims due to increased regulatory burden and potential fees. They are beneficiaries because it offers more access to digital commerce. Constrained exit options, and may be forced to adopt the digital euro, making it a tangled rope.
constraint_indexing:constraint_classification(digital_euro_cbdc, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: The ECB benefits from greater control over monetary policy and reduced transaction costs. They have significant arbitrage power due to its position and regulatory authority, therefore seeing it as a rope.
constraint_indexing:constraint_classification(digital_euro_cbdc, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% EU Governments are beneficiaries as it provides better tax revenue collection and efficient welfare distribution. Also victims because they are under ECB control. Thus seeing it as a tangled rope.
constraint_indexing:constraint_classification(digital_euro_cbdc, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% Large merchants benefit from lowered transaction fees and seamless integration with e-commerce systems but also have the cost of system change and the burden of regulatory compliance. Their mobility between systems makes it a tangled rope.
constraint_indexing:constraint_classification(digital_euro_cbdc, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(continental))).

% Perspective 6: From an analytical perspective, the Digital Euro is a tangled rope. It offers coordination benefits in terms of financial stability and integration, but also carries risks of surveillance and exclusion.
constraint_indexing:constraint_classification(digital_euro_cbdc, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_euro_cbdc_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_euro_cbdc, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_euro_cbdc, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_euro_cbdc, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_euro_cbdc_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Digital Euro has both coordination and extraction aspects. The ECB and EU governments see it as a means to improve financial stability and efficiency, while small merchants and privacy-conscious citizens are concerned about the costs and risks.
 *
 * PERSPECTIVAL GAP:
 *   The ECB and EU governments, being the primary beneficiaries, see the Digital Euro as a beneficial coordination mechanism. However, small merchants and privacy-conscious citizens see it as a snare or tangled rope due to increased costs, regulations, and surveillance risks. This difference creates a perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations determine the directionality. The ECB, EU governments, and Large Merchants benefit, while Small Merchants, Privacy-Conscious Citizens, and competing systems bear the costs. This generates the mixed classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The Digital Euro could be misinterpreted as pure coordination or pure extraction. However, considering all the perspectives helps to resolve this ambiguity. The entangled rope classification is the most accurate reflection of the complex situation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privacy_versus_efficiency,
    'What is the optimal trade-off between privacy and efficiency in the design of the Digital Euro?',
    'Economic modelling and surveys',
    'Determines the level of adoption and public trust in the digital euro.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_versus_efficiency, preference, 'Trade-off between privacy and efficiency.').

omega_variable(
    financial_stability,
    'How will the Digital Euro impact financial stability and the role of commercial banks?',
    'Economic modeling and stress tests.',
    'Affects the overall structure of the banking system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_stability, empirical, 'The impact on financial stability').

omega_variable(
    digital_exclusion,
    'How to prevent digital exclusion of vulnerable groups (elderly, low-income) that may not have access to the digital euro?',
    'Impact analysis of pilot programs',
    'Determines the accessibility of the digital euro.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_exclusion, empirical, 'Risk of digital exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_euro_cbdc, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_euro_cbdc, theater_ratio, 0, 0.1).
narrative_ontology:measurement(digi_tr_t5, digital_euro_cbdc, theater_ratio, 5, 0.2).
narrative_ontology:measurement(digi_tr_t10, digital_euro_cbdc, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_euro_cbdc, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(digi_be_t5, digital_euro_cbdc, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(digi_be_t10, digital_euro_cbdc, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_euro_cbdc, global_infrastructure).
narrative_ontology:affects_constraint(digital_euro_cbdc, financial_surveillance_risks).
narrative_ontology:affects_constraint(digital_euro_cbdc, eu_banking_regulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
