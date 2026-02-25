% ============================================================================
% CONSTRAINT STORY: bwb_adeg_rewesale_conditions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bwb_adeg_rewesale_conditions, []).

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
 *   constraint_id: bwb_adeg_rewesale_conditions
 *   human_readable: BWB Conditions on Rewe's Adeg Store Divestment
 *   domain: economic_regulation
 *
 * SUMMARY:
 *   The Austrian Federal Competition Authority (BWB) imposed binding
 *   conditions on the Rewe Group's divestment of 75 Adeg grocery stores to
 *   independent merchants. This regulatory action serves as a classic example
 *   of a state-enforced market-shaping constraint. The BWB's stated goal is
 *   to preserve competition in the Austrian food retail sector, preventing
 *   increased market concentration that could harm consumers and smaller
 *   businesses. The constraint is not a simple rule but a complex set of
 *   obligations that reallocates market power and opportunity.
 *
 * KEY AGENTS:
 *   - Rewe Group: Primary target/victim (institutional/constrained) — A major retail corporation whose transactional freedom is being curtailed.
 *   - Independent Merchants: Primary beneficiaries (organized/mobile) — Smaller businesses who gain an opportunity to acquire stores under favorable conditions.
 *   - Austrian Consumers: Secondary beneficiaries (powerless/trapped) — The public group intended to benefit from maintained competition.
 *   - BWB (Federal Competition Authority): Institutional enforcer/beneficiary (institutional/arbitrage) — The state body fulfilling its regulatory mandate.
 *   - Analytical Observer: External viewpoint (analytical/analytical) — Assesses the structure of the intervention impartially.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bwb_adeg_rewesale_conditions, 0.55).
domain_priors:suppression_score(bwb_adeg_rewesale_conditions, 0.75).
domain_priors:theater_ratio(bwb_adeg_rewesale_conditions, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, extractiveness, 0.55).
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bwb_adeg_rewesale_conditions, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bwb_adeg_rewesale_conditions, tangled_rope).
narrative_ontology:human_readable(bwb_adeg_rewesale_conditions, "BWB Conditions on Rewe's Adeg Store Divestment").
narrative_ontology:topic_domain(bwb_adeg_rewesale_conditions, "economic_regulation").

domain_priors:requires_active_enforcement(bwb_adeg_rewesale_conditions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bwb_adeg_rewesale_conditions, independent_merchants).
narrative_ontology:constraint_beneficiary(bwb_adeg_rewesale_conditions, austrian_consumers).
narrative_ontology:constraint_beneficiary(bwb_adeg_rewesale_conditions, bwb_regulatory_mandate).
narrative_ontology:constraint_victim(bwb_adeg_rewesale_conditions, rewe_group).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REWE GROUP (SNARE) — As the target of the regulation, Rewe experiences the conditions as pure coercive extraction. Their freedom to transact on their preferred terms is removed by state power. The coordination benefits to the wider market are an externality they are forced to subsidize. d is high due to victim status + constrained exit, pushing χ > 0.66.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT MERCHANTS (ROPE) — As beneficiaries, the merchants see the BWB's conditions as a pure coordination mechanism that creates a viable market for them to acquire stores and compete. The constraint reduces the power asymmetry with Rewe. d is low due to beneficiary status, resulting in low/negative χ.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: AUSTRIAN CONSUMERS (ROPE) — Consumers are the intended indirect beneficiaries. Despite being powerless and trapped within the market, the constraint is designed to work in their favor by preserving competition. The derivation chain prioritizes their beneficiary status, leading to a low d value and a Rope classification.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The analytical view recognizes both the genuine coordination function (preserving market competition) and the coercive, asymmetric extraction from Rewe. It is a textbook example of a state-enforced hybrid constraint, correctly classified as a Tangled Rope.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: BWB (ROPE) - The regulator itself experiences the constraint as the fulfillment of its mandate. It is a tool for coordination that it wields with full agency. As the primary institutional beneficiary with arbitrage exit (it can change the rules), it perceives no extraction.
constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bwb_adeg_rewesale_conditions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bwb_adeg_rewesale_conditions, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bwb_adeg_rewesale_conditions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bwb_adeg_rewesale_conditions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): Moderate. The constraint extracts significant value from Rewe, not just in monetary terms but in strategic flexibility and the freedom to dispose of assets on its own terms. Suppression (0.75): High. As a state regulator, the BWB's decision is backed by legal force. Rewe's alternatives are severely limited; non-compliance would result in legal penalties or the transaction being blocked entirely. Theater Ratio (0.15): Low. This is a functional, not performative, regulatory action with direct and intended economic consequences.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark and structurally determined. Rewe, the target, experiences the constraint as a Snare because the costs are direct and coercive, while the market-wide benefits are an externality. The merchants and the BWB, as beneficiaries, experience it as a Rope because it facilitates a desirable coordination outcome (market entry, mandate fulfillment). Consumers also see a Rope, as the rule is designed to protect them. The analytical observer, weighing both the coercive extraction from Rewe and the genuine coordination goal, classifies it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the clear structural roles. Rewe is the designated victim, and its constrained exit options lead to a high 'd' value, resulting in high effective extraction (χ) and a Snare classification. The merchants, consumers, and BWB are designated beneficiaries; their mobile or arbitrage exit options (or, for consumers, the protective intent of the law) result in low 'd' values, low/negative χ, and a Rope classification. The system correctly models how the same regulatory act is perceived differently based on one's position relative to the flow of value and coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a clear case for the necessity of the Tangled Rope category. Labeling it purely as a Snare (Rewe's view) would ignore its legitimate pro-competitive coordination function. Labeling it purely as a Rope (the BWB's view) would erase the coercive extraction imposed on a market actor. The Tangled Rope classification, adopted by the analytical perspective, correctly identifies the hybrid nature of most economic regulation: it is simultaneously a coordination mechanism for the market as a whole and an extractive mechanism against specific, targeted entities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_merchant_viability,
    'Will the independent merchants who acquire the stores remain competitive in the long term, or will they eventually fail or be re-acquired, rendering the intervention ineffective?',
    'Longitudinal market share analysis of the divested stores over a 5-10 year period.',
    'If merchants fail, the constraint''s coordination function was illusory, and it was closer to a pure Snare on Rewe with no lasting public benefit. If they thrive, the Tangled Rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_merchant_viability, empirical, 'Long-term competitive viability of merchants post-divestment').

omega_variable(
    consumer_price_impact,
    'Did the divestment conditions lead to demonstrably lower prices or higher quality for consumers compared to a counterfactual where the stores were closed or sold without conditions?',
    'Econometric analysis of grocery price indices in the affected regions, comparing them to control regions.',
    'A significant positive impact on consumers confirms the strength of the coordination function. No impact suggests the extraction from Rewe did not translate into public benefit, weakening the Tangled Rope case.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumer_price_impact, empirical, 'Measurable impact of the conditions on consumer welfare').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bwb_adeg_rewesale_conditions, 2022, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bwb__tr_t2022, bwb_adeg_rewesale_conditions, theater_ratio, 2022, 0.15).
narrative_ontology:measurement(bwb__tr_t2023, bwb_adeg_rewesale_conditions, theater_ratio, 2023, 0.15).
narrative_ontology:measurement(bwb__tr_t2024, bwb_adeg_rewesale_conditions, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(bwb__be_t2022, bwb_adeg_rewesale_conditions, base_extractiveness, 2022, 0.55).
narrative_ontology:measurement(bwb__be_t2023, bwb_adeg_rewesale_conditions, base_extractiveness, 2023, 0.55).
narrative_ontology:measurement(bwb__be_t2024, bwb_adeg_rewesale_conditions, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bwb_adeg_rewesale_conditions, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
