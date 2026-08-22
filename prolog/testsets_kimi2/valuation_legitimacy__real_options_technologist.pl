% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__real_options_technologist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__real_options_technologist, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: valuation_legitimacy__real_options_technologist
 *   human_readable: Real Options Technologist Reading of SpaceX Valuation Legitimacy
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the real_options_technologist reading
 *   of the valuation_legitimacy kernel as applied to SpaceX. The standing
 *   arrangement under contest is a $1.75T private valuation justified by the
 *   present value of a correlated portfolio of technological real options
 *   (Starlink, Starship, orbital compute, lunar economy, Mars), held together
 *   by vertical integration that is claimed to compound optionality. The
 *   reading treats this as legitimate finance; the structural assessment
 *   identifies both a genuine coordination function (pooling capital for
 *   deep-tech that DCF would starve) and asymmetric extraction (concentrated
 *   voting control, minority shareholder governance deprivation, and public
 *   subsidy capture).
 *
 * KEY AGENTS:
 *   - musk_control_group (agenda_setter / powerful / arbitrage): Controls capital allocation and the valuation narrative; captures compounding optionality through vertical integration and supermajority voting.
 *   - early_venture_investors (beneficiary / powerful / arbitrage): Hold upside equity exposure and can exit via secondary markets; benefit from narrative-driven valuation inflation.
 *   - minority_shareholders (payer / moderate / constrained): Provide capital without governance rights; bear downside risk if the option portfolio fails to materialize.
 *   - taxpayers (payer / institutional / constrained): De-risk early programs through public contracts; may overpay relative to competitive alternatives.
 *   - competing_aerospace_firms (excluded / organized / trapped): Locked out by vertical integration and cost dominance; absent from the legitimacy conversation.
 *   - technology_analysts (observer / analytical / analytical): Evaluate the real-options claims from outside the capital or governance structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.58).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.42).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.58).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Real Options Technologist Reading of SpaceX Valuation Legitimacy").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__real_options_technologist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, 'bb75a928-4617-41ab-bed7-b6d0743c5268').
narrative_ontology:cs_kernel_codification('bb75a928-4617-41ab-bed7-b6d0743c5268', formalized).
narrative_ontology:cs_authority_grounding('bb75a928-4617-41ab-bed7-b6d0743c5268', expertise).
narrative_ontology:cs_interpretation_layer_present('bb75a928-4617-41ab-bed7-b6d0743c5268').
narrative_ontology:cs_reading_relation('bb75a928-4617-41ab-bed7-b6d0743c5268', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('bb75a928-4617-41ab-bed7-b6d0743c5268', valuation_legitimacy__musk_cult_believer, coexists_with).
narrative_ontology:cs_reading_relation('bb75a928-4617-41ab-bed7-b6d0743c5268', valuation_legitimacy__governance_skeptic, influences).
narrative_ontology:cs_axiom('bb75a928-4617-41ab-bed7-b6d0743c5268', foundational, unproven_technology_may_legitimate_enterprise_value).
narrative_ontology:cs_axiom_status(unproven_technology_may_legitimate_enterprise_value, holdable).
narrative_ontology:cs_axiom_grounding('bb75a928-4617-41ab-bed7-b6d0743c5268', unproven_technology_may_legitimate_enterprise_value, instrumental).
narrative_ontology:cs_axiom('bb75a928-4617-41ab-bed7-b6d0743c5268', foundational, vertical_integration_necessary_for_compounding_optionality).
narrative_ontology:cs_axiom_status(vertical_integration_necessary_for_compounding_optionality, holdable).
narrative_ontology:cs_axiom_grounding('bb75a928-4617-41ab-bed7-b6d0743c5268', vertical_integration_necessary_for_compounding_optionality, instrumental).
narrative_ontology:cs_reference_frame('bb75a928-4617-41ab-bed7-b6d0743c5268', technological_option_space_valuation).
narrative_ontology:cs_drift_state('bb75a928-4617-41ab-bed7-b6d0743c5268', post_starlink_profitability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bb75a928-4617-41ab-bed7-b6d0743c5268', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, musk_control_group).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, early_venture_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, minority_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, taxpayers).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, real_options_theory).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, vertical_integration_synergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds 82.4% voting control on approximately 42% economic interest through trust and dual-class structures. Sets capital allocation across Starlink, Starship, orbital compute, lunar, and Mars programs. Frames valuation through real-options logic and controls the narrative layer. Captures compounding optionality from vertical integration.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, musk_control_group, agenda_setter,
    powerful, generational, arbitrage, global).

% Hold equity with asymmetric upside exposure to the technology option portfolio. Can exit via secondary sales or eventual liquidity events. Benefit from valuation inflation driven by the real-options narrative and scarcity of comparable deep-tech assets.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, early_venture_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Provide capital at valuations pricing in low-probability, high-payoff joint events. Bear economic risk without commensurate governance rights. Cannot influence capital allocation, dividend policy, or strategic pivots. Exit is available in secondary markets but at prices tied to the prevailing narrative.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, minority_shareholders, payer,
    moderate, biographical, constrained, global).

% Fund NASA, DoD, and subsidy programs that de-risk early-stage optionality through cost-plus and fixed-price contracts. Receive launch and satellite services in return, but may overpay relative to competitive modular alternatives due to sole-source structures and technological lock-in.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, taxpayers, payer,
    institutional, generational, constrained, national).

% Locked out of vertically integrated launch and satellite markets by SpaceX's cost advantages and bundled service offerings. Would advocate for modular contracting and antitrust scrutiny but are structurally excluded from the valuation narrative and from competing for the integrated stack.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, competing_aerospace_firms, excluded,
    organized, biographical, trapped, global).

% Evaluate the real-options claims, TAM sizing, and vertical integration economics. Some corroborate the optionality value using financial engineering; others argue the implied joint probabilities are implausible and the $1.75T valuation reflects narrative premium over fundamentals.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, technology_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__real_options_technologist, musk_control_group).
narrative_ontology:fixing_cost_class(valuation_legitimacy__real_options_technologist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools patient capital across high-variance, long-horizon technological bets that modular capital markets would underfund due to correlated uncertainty and distant cash flows. Vertical integration internalizes complementarities so that success in one segment increases the probability of success in others.
% TRANSFER_FUNCTION: Moves capital from minority shareholders and public funding sources into a vertically integrated technology stack controlled by a concentrated voting bloc, in exchange for exposure to a correlated option portfolio whose joint realization is priced at low probability but astronomical TAM.
% ABSENT_VOICES: DCF fundamentalists who would recognize only proven, discounted cash flows as legitimate value; modular aerospace contractors who would disaggregate the stack into competitive segments; governance skeptics who would demand independent board control or voting parity as a condition of legitimacy.
% DISAPPEARANCE_RATIONALE: If the real-options valuation framework and its vertical integration arrangement vanished, capital would reallocate toward modular, cash-flow-proven assets. The $1.75T valuation would compress toward DCF-implied levels. Starship and Mars programs would lose subsidy and equity funding. The space industry would fragment into specialized vendors, and the compounding-optionality narrative would no longer command premium capital.
% FOUNDING_PROBLEM: Capital markets systematically underprice and underfund civilization-scale, high-variance, long-horizon technology development because standard DCF discounts distant uncertain cash flows too heavily, and modular contracting destroys cross-project learning and complementarity.
% FOUNDING_PROBLEM_CORROBORATION: Academic finance (real options theory) and independent aerospace economists corroborate the deep-tech capital-market failure from outside the benefiting control group. SpaceX leadership and early venture investors attest the problem remains live from inside the beneficiary set. Governance skeptics and short sellers, also outside the beneficiary set, attest the founding problem is substantially solved by Starlink profitability and that the arrangement now persists as control rent.
narrative_ontology:disappearance_verdict(valuation_legitimacy__real_options_technologist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__real_options_technologist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(valuation_legitimacy__real_options_technologist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is authored as moderate-high because the valuation framework enables capital raising at prices decoupled from near-term cash flows, while the voting control structure extracts governance value from economic owners. Suppression (0.42) reflects the active marginalization of DCF and governance-skeptic discourse in tech finance and the structural exclusion of competing providers. Theater ratio (0.25) acknowledges genuine engineering progress but recognizes an increasing share of narrative maintenance (Mars demos, TAM presentations) relative to proven cash flow. Accessibility collapse (0.40) captures that modular alternatives and fundamental valuation are understood but structurally disadvantaged in this capital environment. Resistance (0.35) is moderate: short sellers and governance skeptics provide active critique but remain minority voices. The temporal series show slow accumulation of extraction and suppression as the control structure hardens and the narrative expands from launch to multiplanetary civilization.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (musk_control_group) experiences the constraint as a coordination mechanism it designed to solve a capital-market failure; from this seat, the arrangement is rope-like, preserving optionality that would otherwise dissipate. The payer seats (minority_shareholders, taxpayers) experience the same arrangement as extraction of governance rights and public funds under a narrative that renders the transfer legitimate. The engine will compute divergent per-seat classifications from these structural asymmetries: high scope and constrained exit amplify effective extraction for minority shareholders, while arbitrage exit and beneficiary role damp it for the control group.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to agents who capture asymmetric upside and narrative control: the control group and early venture investors. Victim declarations map to agents who bear cost without commensurate governance or pricing power: minority shareholders and taxpayers. The directionality derivation will produce low d (subsidy) for beneficiaries and high d (extraction) for victims. The excluded competitors receive high d via structural derivation from their trapped exit and organized power, even though they are not formal victims, because their exclusion is the enforcement boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcapital markets underfunding deep-tech due to DCF myopiaâis contested but not dead. Starlink's profitability supports the claim that the problem is partially solved, yet Starship and Mars remain unproven. Because the founding problem is contested rather than dead, and because the constraint still delivers genuine coordination (launch cost reduction, satellite internet), the classification resists piton mislabeling. It is not a snare because the coordination function is not cover: the engineering outputs are real and the optionality framework has analytical pedigree. It is tangled_rope because the same vertical integration that coordinates also compounds control rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vertical_integration_necessity,
    'Is vertical integration structurally necessary for compounding technological optionality, or is it a control mechanism that extracts governance rents under the cover of synergy?',
    'Natural experiment or regulatory mandate forcing modular contracting or spinoff of Starlink/Starship; observe whether optionality collapses or capital costs rise.',
    'If vertical integration is unnecessary, the control structure is extractive overlay and the constraint shifts toward snare. If necessary, the tangled_rope classification holds but the extraction fraction is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vertical_integration_necessity, conceptual, 'Whether vertical integration is genuine optionality preservation or governance extraction.').

omega_variable(
    portfolio_joint_probability,
    'Does the $1.75T valuation imply a joint probability of success across Starlink, Starship, orbital compute, lunar, and Mars options that is empirically plausible, or is the option space overstated?',
    'Independent probabilistic risk assessment of each program''s technical and regulatory milestones, with correlation analysis; compare implied probability to historical deep-tech base rates.',
    'If the joint probability is implausibly low, the valuation is narrative-driven extraction from minority capital. If plausible, the extractiveness metric overstates the case.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(portfolio_joint_probability, empirical, 'Whether the priced-in option portfolio probability is realistic.').

omega_variable(
    governance_as_extraction,
    'Does 82.4% voting control on 42% economic interest constitute extraction from minority shareholders, or is it a necessary incentive alignment mechanism for long-horizon optionality?',
    'Event study around governance shocks or regulatory proposals to mandate voting parity; observe capital flow and insider response.',
    'If control is extractive, directionality for minority shareholders rises and effective extraction is amplified. If alignment, the victim classification for minority shareholders weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_as_extraction, conceptual, 'Whether concentrated voting control is extraction or necessary incentive alignment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vlot_tr_t0, valuation_legitimacy__real_options_technologist, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vlot_tr_t5, valuation_legitimacy__real_options_technologist, theater_ratio, 5, 0.18).
narrative_ontology:measurement(vlot_tr_t10, valuation_legitimacy__real_options_technologist, theater_ratio, 10, 0.2).
narrative_ontology:measurement(vlot_tr_t15, valuation_legitimacy__real_options_technologist, theater_ratio, 15, 0.23).
narrative_ontology:measurement(vlot_tr_t20, valuation_legitimacy__real_options_technologist, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(vlot_be_t0, valuation_legitimacy__real_options_technologist, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vlot_be_t5, valuation_legitimacy__real_options_technologist, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(vlot_be_t10, valuation_legitimacy__real_options_technologist, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(vlot_be_t15, valuation_legitimacy__real_options_technologist, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(vlot_be_t20, valuation_legitimacy__real_options_technologist, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vlot_su_t0, valuation_legitimacy__real_options_technologist, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vlot_su_t5, valuation_legitimacy__real_options_technologist, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(vlot_su_t10, valuation_legitimacy__real_options_technologist, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(vlot_su_t15, valuation_legitimacy__real_options_technologist, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(vlot_su_t20, valuation_legitimacy__real_options_technologist, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, musk_cult_believer).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, governance_skeptic).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the valuation_legitimacy kernel, which decomposes into at least four structurally distinct claims about the source of valuation legitimacy for high-growth technology enterprises. Each reading carries a different epsilon, beneficiary structure, and classification. The real_options_technologist reading influences the governance_skeptic reading by changing legitimacy conditions in capital markets, while coexisting with dcf_fundamentalist and musk_cult_believer as live alternative positions held by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
