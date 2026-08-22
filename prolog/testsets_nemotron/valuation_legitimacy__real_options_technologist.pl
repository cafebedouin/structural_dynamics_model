% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__real_options_technologist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Real Options Valuation Legitimacy (Technologist Reading)
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the real_options_technologist reading
 *   of the contested valuation_legitimacy kernel. The reading holds that a
 *   company's legitimate valuation derives from the present value of its
 *   technological option space — the portfolio of future capabilities that
 *   current investments unlock — and that vertical integration compounds
 *   optionality because success in any segment increases the probability of
 *   success in others. SpaceX is the paradigmatic case: Starlink provides
 *   proven cash flow ($7.2B EBITDA), Starship is the high-variance enabler
 *   for all downstream options (orbital compute addressing 62 GW U.S. power
 *   gap, lunar economy first-mover advantage, Mars as civilizational hedge).
 *   The $1.75T valuation prices in ~6% probability of achieving $28.5T total
 *   addressable market across the portfolio. The reading asserts low
 *   extraction because investors voluntarily enter understanding the
 *   risk/reward profile, and the beneficiary set extends to humanity if
 *   multiplanetary civilization succeeds.
 *
 * KEY AGENTS:
 *   - spacex_investors: Primary beneficiaries (organized/constrained) — voluntarily allocate capital to option portfolio
 *   - humanity_multiplanetary: Diffuse civilizational beneficiary (analytical/civilizational) — gains if multiplanetary outcome materializes
 *   - space_economy_participants: Secondary beneficiaries (organized/biographical) — downstream companies, suppliers, talent pool
 *   - dcf_fundamentalist_analysts: Excluded observers (analytical/analytical) — apply competing valuation framework
 *   - governance_reform_advocates: Excluded observers (organized/generational) — argue control structure extracts from minority
 *   - competing_space_companies: Constrained payers (powerful/constrained) — face vertically integrated competitor with option-funded war chest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__real_options_technologist, 0.18).
domain_priors:suppression_score(valuation_legitimacy__real_options_technologist, 0.22).
domain_priors:theater_ratio(valuation_legitimacy__real_options_technologist, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, extractiveness, 0.18).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(valuation_legitimacy__real_options_technologist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__real_options_technologist, rope).
narrative_ontology:human_readable(valuation_legitimacy__real_options_technologist, "Real Options Valuation Legitimacy (Technologist Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__real_options_technologist, "corporate_finance/technology_governance/space_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__real_options_technologist, '6fa8155e-c9d9-4540-95bd-fb227b6c7f7f').
narrative_ontology:cs_kernel_codification('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', implicit).
narrative_ontology:cs_authority_grounding('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', practice).
narrative_ontology:cs_interpretation_layer_present('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f').
narrative_ontology:cs_reading_relation('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', valuation_legitimacy__governance_skeptic, coexists_with).
narrative_ontology:cs_reading_relation('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', valuation_legitimacy__musk_cult_believer, influences).
narrative_ontology:cs_axiom('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', foundational, option_space_has_present_value).
narrative_ontology:cs_axiom_status(option_space_has_present_value, holdable).
narrative_ontology:cs_axiom_grounding('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', option_space_has_present_value, empirically_contingent).
narrative_ontology:cs_axiom('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', foundational, vertical_integration_compounds_optionality).
narrative_ontology:cs_axiom_status(vertical_integration_compounds_optionality, holdable).
narrative_ontology:cs_axiom_grounding('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', vertical_integration_compounds_optionality, empirically_contingent).
narrative_ontology:cs_reference_frame('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', real_options_valuation_paradigm).
narrative_ontology:cs_drift_state('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', post_starlink_profitability, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6fa8155e-c9d9-4540-95bd-fb227b6c7f7f', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__real_options_technologist, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, spacex_investors).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, humanity_multiplanetary).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__real_options_technologist, space_economy_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(valuation_legitimacy__real_options_technologist, competing_space_companies).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, real_options_theory_applied_to_space).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, vertical_integration_compounds_optionality).
narrative_ontology:constraint_vindicates(valuation_legitimacy__real_options_technologist, technological_option_space_has_present_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Voluntarily allocate capital to SpaceX's option portfolio understanding the high-variance risk/reward profile. Benefit from upside if option portfolio pays off. Exit requires selling illiquid private shares or waiting for liquidity events; constrained by lockups and market depth.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, spacex_investors, beneficiary,
    organized, biographical, constrained, global).

% Diffuse civilizational beneficiary if multiplanetary outcome materializes. Not an agent that acts or chooses; the beneficiary framing is analytical — the reading claims this constraint's success creates positive externalities at civilizational scale. No exit because not a choosing agent.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, humanity_multiplanetary, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(valuation_legitimacy__real_options_technologist, humanity_multiplanetary).

% Downstream companies, suppliers, talent pool, and adjacent industries that benefit from SpaceX's infrastructure investments and demand creation. Can redirect to other platforms if SpaceX falters; mobile exit at organizational level.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, space_economy_participants, beneficiary,
    organized, biographical, mobile, global).

% Apply competing valuation framework (discounted proven cash flows) and argue real-options pricing is speculative. Would object to the constraint's legitimacy claim but are not in the capital allocation conversation for this specific allocation — they allocate elsewhere.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, dcf_fundamentalist_analysts, excluded,
    analytical, biographical, analytical, global).

% Argue that Musk's 82.4% voting control with 42% equity extracts from minority shareholders and that governance structure, not valuation methodology, is the real constraint. Would participate in governance reform but are structurally excluded by dual-class share structure and founder control.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, governance_reform_advocates, excluded,
    organized, generational, constrained, national).

% Face competitive pressure from a vertically integrated rival funded by option-valuation capital that tolerates losses DCF-valued competitors cannot. Bear costs of competing against an opponent with different cost of capital and longer horizon. Exit requires pivoting to niches SpaceX doesn't serve or exiting the sector; constrained by capital requirements.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__real_options_technologist, competing_space_companies, payer,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__real_options_technologist, diffuse).
narrative_ontology:fixing_cost_class(valuation_legitimacy__real_options_technologist, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation toward long-horizon, high-variance technological development that pure DCF frameworks systematically underfund — solves the 'valley of death' for deep tech by pricing optionality explicitly.
% TRANSFER_FUNCTION: Moves capital from investors seeking option exposure to SpaceX's vertically integrated technology portfolio; returns flow back as portfolio milestones reduce variance and unlock follow-on options. No forced transfer — all parties enter voluntarily.
% ABSENT_VOICES: Governance reform advocates (excluded by dual-class structure), workers in SpaceX supply chain (not consulted on option-portfolio strategy), communities affected by launch operations (Boca Chica, Cape Canaveral), taxpayers subsidizing government contracts — would object to various aspects but are not in the valuation-legitimacy conversation.
% DISAPPEARANCE_RATIONALE: If the real-options valuation framework disappeared overnight, capital would revert to DCF-only allocation, StarShip-scale projects would lose funding, the vertically integrated portfolio would fragment, and the multiplanetary option would likely close — the world rearranges because this constraint coordinates a specific capital-allocation pattern that would not exist under the alternative framework.
% FOUNDING_PROBLEM: Traditional DCF valuation systematically rejects multi-decade, binary-outcome technological development because it cannot price optionality — creating a 'valley of death' where civilization-critical technologies (orbital infrastructure, multiplanetary capability) cannot raise capital despite massive potential upside.
% FOUNDING_PROBLEM_CORROBORATION: Venture capital literature documents the deep-tech funding gap (Kerr et al. on 'tough tech', Azoulay on 'public science investment'); NASA's commercial crew/cargo programs explicitly created to bridge valley of death; even DCF advocates acknowledge the framework fails for binary-outcome long-horizon bets. Corroboration comes from outside the beneficiary set: academic finance, government program design, competing investors who use different frameworks but acknowledge the gap.
narrative_ontology:disappearance_verdict(valuation_legitimacy__real_options_technologist, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__real_options_technologist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__real_options_technologist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(valuation_legitimacy__real_options_technologist, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__real_options_technologist, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__real_options_technologist_tests).
:- end_tests(valuation_legitimacy__real_options_technologist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because capital allocation is voluntary and the valuation methodology is transparent about pricing optionality; suppression is low (0.22) because no party is coerced into participation — investors can choose DCF-valued companies instead. Theater ratio (0.31) reflects that some option-value discourse serves marketing/recruitment functions beyond pure analytical rigor. Accessibility collapse (0.42) is moderate: alternative valuation frameworks exist and are used by other investors, but the real-options framing becomes self-reinforcing once adopted. Resistance (0.55) comes from fundamentalist analysts and governance advocates who reject the framework's premises. The claimed_type rope reflects genuine coordination: the framework coordinates capital toward long-horizon technological bets that pure DCF would reject, solving a real allocation problem.
 *
 * DIRECTIONALITY LOGIC:
 *   SpaceX investors are beneficiaries with constrained exit (organized power, biographical horizon) — they chose this framework and can reallocate but face switching costs. Humanity_multiplanetary is a diffuse civilizational beneficiary with analytical seat. Space economy participants are beneficiaries at organizational level. DCF fundamentalists and governance advocates are excluded — they would object to the framework's legitimacy but are not in the capital allocation conversation. Competing space companies are constrained payers: they bear competitive pressure from an option-funded rival but this is market competition, not constraint extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating capital for multi-decade technological development with binary outcomes — remains live. The real-options framework was built to solve the 'valley of death' for deep tech where DCF fails. No mandatrophy: the coordination function is still needed and the framework has not atrophied into extraction. Theater ratio rise over time reflects increasing performative use of option-language for recruitment/marketing, but the core coordination function persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the valuation_legitimacy kernel a single commitment with multiple readings, or are these fundamentally different valuation frameworks masquerading as readings of the same thing?',
    'Map each reading''s structural relationship to the kernel''s authority_grounding and kernel_codification; if they require different authority_grounding values, they are different kernels.',
    'If multiple kernels, each reading gets its own constraint story without forced kinship; if single kernel, the reading_relations and axioms structure applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the four declared positions are readings of one kernel or four distinct kernels').

omega_variable(
    option_portfolio_independence,
    'Are the SpaceX option portfolio elements (Starlink, Starship, orbital compute, lunar, Mars) genuinely independent real options, or does vertical integration create hidden correlation that makes the portfolio behave as a single large bet?',
    'Correlation analysis of milestone achievement probabilities across segments; test whether Starship failure probabilities are truly independent of Starlink cash flow outcomes.',
    'If highly correlated, the real_options_technologist reading overstates optionality and extractiveness rises toward tangled_rope; if independent, the portfolio logic holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(option_portfolio_independence, empirical, 'Whether the option portfolio is genuinely diversified or a single correlated bet').

omega_variable(
    victim_set_completeness,
    'Is the victim set genuinely low (investors understand risk/reward), or are there diffuse victims (taxpayers via subsidies, workers via labor conditions, competitors via predatory pricing) that this reading excludes?',
    'Audit government contract terms, labor complaints, and competitor antitrust filings for extraction patterns not captured by investor consent.',
    'If diffuse victims exist, claimed_type shifts from rope toward snare/tangled_rope and beneficiary structure requires expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_completeness, empirical, 'Whether the low victim set claim survives scrutiny beyond consenting investors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__real_options_technologist, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_tr_t0, valuation_legitimacy__real_options_technologist, theater_ratio, 0, 0.15).
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_tr_t3, valuation_legitimacy__real_options_technologist, theater_ratio, 3, 0.19).
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_tr_t6, valuation_legitimacy__real_options_technologist, theater_ratio, 6, 0.23).
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_tr_t9, valuation_legitimacy__real_options_technologist, theater_ratio, 9, 0.27).
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_tr_t12, valuation_legitimacy__real_options_technologist, theater_ratio, 12, 0.29).
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_tr_t15, valuation_legitimacy__real_options_technologist, theater_ratio, 15, 0.31).

% Extraction over time
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_be_t0, valuation_legitimacy__real_options_technologist, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_be_t3, valuation_legitimacy__real_options_technologist, base_extractiveness, 3, 0.11).
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_be_t6, valuation_legitimacy__real_options_technologist, base_extractiveness, 6, 0.13).
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_be_t9, valuation_legitimacy__real_options_technologist, base_extractiveness, 9, 0.15).
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_be_t12, valuation_legitimacy__real_options_technologist, base_extractiveness, 12, 0.16).
narrative_ontology:measurement(valuation_legitimacy__real_options_technologist_be_t15, valuation_legitimacy__real_options_technologist, base_extractiveness, 15, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(valuation_legitimacy__real_options_technologist, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__real_options_technologist, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__real_options_technologist, 0.12).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__governance_skeptic).
narrative_ontology:affects_constraint(valuation_legitimacy__real_options_technologist, valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% Part of the valuation_legitimacy constraint family. This reading (real_options_technologist) emphasizes option-portfolio valuation with compounding vertical integration. The dcf_fundamentalist reading treats unproven technologies as options-not-assets. The governance_skeptic reading centers control structure as extraction. The musk_cult_believer reading substitutes founder track record for financial metrics. All four instantiate different structural relationships to the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__real_options_technologist, institutional, 0.15).
constraint_indexing:directionality_override(valuation_legitimacy__real_options_technologist, organized, 0.2).
constraint_indexing:directionality_override(valuation_legitimacy__real_options_technologist, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
