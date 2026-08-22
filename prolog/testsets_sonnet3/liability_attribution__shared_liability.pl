% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__shared_liability, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: AI Value-Chain Joint Liability Apportionment Regime
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   As AI systems moved from research artifacts to deployed commercial
 *   infrastructure, courts and legislators faced harm scenarios where neither
 *   the model developer nor the deploying organization was the sole proximate
 *   cause: a foundation model with a latent defect combined with a deployer's
 *   negligent fine-tuning or misuse-enabling configuration. The
 *   shared-liability reading of the liability_attribution kernel resolves
 *   this by apportioning responsibility across the value chain according to
 *   causal contribution and control, rather than assigning liability
 *   categorically to one role. This produces a genuine coordination function
 *   (multi-actor harms get multi-actor remedies) but the apportionment
 *   standard itself becomes a site of extraction: large integrated firms and
 *   the insurance industry that prices apportionment uncertainty benefit from
 *   the ambiguity, while small developers, resource-constrained deployers,
 *   and injured end users bear the coordination cost of a fact-intensive,
 *   expensive, unpredictable allocation process.
 *
 * KEY AGENTS:
 *   - model_developers: primary target, retain architectural opacity, bear open-ended contribution-proportional exposure
 *   - downstream_deployers: dual-positioned, exercise deployment control and negotiate contractual risk-shifting from a position of relative leverage
 *   - injured_end_users: primary target, bear the evidentiary and delay cost of proving apportionment in a system they did not build or configure
 *   - insurance_and_indemnification_industry: structural beneficiary, monetizes the very uncertainty the regime creates
 *   - large_integrated_platform_firms: structural beneficiary via internal role-spanning that avoids arm's-length contracting friction
 *   - regulatory_agencies_seeking_coverage: beneficiary of political cover, avoids the harder single-party allocation choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.58).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.47).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.58).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "AI Value-Chain Joint Liability Apportionment Regime").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, 'f0d3b412-91d5-4351-98a4-41b95ebf2b13').
narrative_ontology:cs_kernel_codification('f0d3b412-91d5-4351-98a4-41b95ebf2b13', distributed).
narrative_ontology:cs_authority_grounding('f0d3b412-91d5-4351-98a4-41b95ebf2b13', distributed).
narrative_ontology:cs_reading_relation('f0d3b412-91d5-4351-98a4-41b95ebf2b13', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('f0d3b412-91d5-4351-98a4-41b95ebf2b13', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('f0d3b412-91d5-4351-98a4-41b95ebf2b13', foundational, liability_should_track_causal_contribution_not_role_category).
narrative_ontology:cs_axiom_status(liability_should_track_causal_contribution_not_role_category, holdable).
narrative_ontology:cs_axiom_grounding('f0d3b412-91d5-4351-98a4-41b95ebf2b13', liability_should_track_causal_contribution_not_role_category, instrumental).
narrative_ontology:cs_axiom('f0d3b412-91d5-4351-98a4-41b95ebf2b13', secondary, multi_actor_harm_requires_multi_actor_remedy).
narrative_ontology:cs_axiom_status(multi_actor_harm_requires_multi_actor_remedy, holdable).
narrative_ontology:cs_axiom_grounding('f0d3b412-91d5-4351-98a4-41b95ebf2b13', multi_actor_harm_requires_multi_actor_remedy, conventional).
narrative_ontology:cs_reference_frame('f0d3b412-91d5-4351-98a4-41b95ebf2b13', mixed_causation_multi_actor_harm_model).
narrative_ontology:cs_drift_state('f0d3b412-91d5-4351-98a4-41b95ebf2b13', post_indemnification_market_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0d3b412-91d5-4351-98a4-41b95ebf2b13', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_and_indemnification_industry).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, large_integrated_platform_firms).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, regulatory_agencies_seeking_coverage).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, model_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, downstream_deployers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, injured_end_users).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, causal_contribution_apportionment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and train the underlying models, retaining architectural knowledge no downstream party can fully audit. Under shared liability they face open-ended exposure proportional to a contested 'causal contribution' finding, decided case by case, and must now carry contractual indemnification clauses and liability insurance premiums that scale with how opaque their systems remain to the parties they sell to.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, model_developers, payer,
    powerful, biographical, constrained, global).

% Configure, fine-tune, and put the model into a specific operational context, exercising real control over deployment decisions but limited visibility into the base model's internals. They negotiate contractual risk allocation with developers and often win favorable indemnification terms because they hold the contracting leverage at the point of sale, while still bearing liability for their own deployment choices.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, downstream_deployers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, downstream_deployers, agenda_setter).

% Suffer concrete harm from a system's output or failure and must now litigate a two-or-more-party causal apportionment fight to be made whole, since no single party is designated as the clear defendant. They bear the delay, discovery cost, and evidentiary burden of proving which upstream or downstream actor's contribution and control caused the harm.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, injured_end_users, payer,
    powerless, immediate, trapped, local).

% Underwrites the liability exposure created by apportionment uncertainty, pricing policies on both developer and deployer books. The very ambiguity that burdens the other parties is the raw material of this seat's revenue: more uncertainty about causal shares means more demand for indemnification products and higher premiums.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_and_indemnification_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Operate as both developer and deployer under one corporate roof, capturing the coordination benefit of internal risk allocation (no arm's-length contract needed) while smaller single-role firms must negotiate indemnification terms externally at a disadvantage. Their scale lets them absorb litigation cost that would sink a smaller developer or deployer.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, large_integrated_platform_firms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, large_integrated_platform_firms, agenda_setter).

% Adopted the shared-liability framework because it lets them avoid picking a single politically costly loser (developers or deployers) and instead defers hard allocation questions to case-by-case adjudication and private contract. They gain the appearance of comprehensive coverage without having to specify a bright-line rule.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulatory_agencies_seeking_coverage, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, regulatory_agencies_seeking_coverage, observer).

% Lack the scale to self-insure or to negotiate favorable indemnification terms with larger deployers, and are rarely consulted when the apportionment standards or model liability legislation is drafted. They would argue for a bright-line developer-liability-capped regime but are not represented in the standard-setting bodies that produced the joint framework.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, small_independent_developers, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, diffuse).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes the cost of AI-caused harm across every party that materially contributed to or controlled the harmful outcome, rather than forcing courts to pick one dispositive cause in a multi-actor causal chain where responsibility is genuinely distributed.
% TRANSFER_FUNCTION: Moves litigation risk, insurance premiums, and indemnification-negotiation cost from injured end users (who would otherwise have to prove which single party is liable) onto developers and deployers jointly, and moves risk-pricing revenue to the insurance and indemnification industry that prices the resulting uncertainty.
% ABSENT_VOICES: Small independent developers and individual end users are rarely present when apportionment standards, model liability legislation, or safe-harbor contractual templates are drafted; large integrated firms and insurers dominate the standard-setting process and shape the causal-contribution tests to their own risk profile.
% DISAPPEARANCE_RATIONALE: If joint liability apportionment disappeared, courts would revert to single-defendant doctrines (either strict developer liability or strict deployer liability per the sibling readings), indemnification markets built around apportionment uncertainty would collapse or reprice sharply, and large integrated firms would lose the internal-allocation advantage that comes from spanning both roles under one liability regime.
% FOUNDING_PROBLEM: AI system failures often result from a genuine combination of upstream model defects and downstream deployment/configuration choices, and neither pure developer liability nor pure deployer liability accurately tracked causal responsibility in multi-actor harm chains.
% FOUNDING_PROBLEM_CORROBORATION: Some plaintiff-side litigators and academic tort scholars outside the regulated industries corroborate that genuinely mixed-causation harms exist and need apportionment; but small developers and consumer advocates argue the 'causal contribution' standard as currently operationalized has drifted from tracking actual causation toward tracking which party has the contracting leverage to shift cost onto the other, which is not attested by any party outside the developers, deployers, and insurers who negotiate the allocation.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__shared_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a middle position between the sibling readings: it is lower than a pure-target regime would show for either developers or deployers alone, because responsibility genuinely is shared, but it rises over the measured interval as apportionment litigation matures into a predictable cost center that indemnification markets learn to price and pass through disproportionately to less-resourced parties. Suppression (0.47) is moderate — no party is forcibly denied recourse, but the fact-intensive causal-contribution standard functions as a de facto barrier for parties who cannot afford protracted multi-party litigation. Theater ratio (0.32) captures that a meaningful share of apportionment activity is genuine causal fact-finding, but a growing share is defensive documentation and indemnification-clause drafting whose real function is risk-shifting rather than harm prevention.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory agency and insurance seats, shared liability looks like a sophisticated, comprehensive coordination mechanism that finally matches legal responsibility to the multi-actor reality of AI harm. From the small developer and injured end-user seats, the same apportionment machinery looks like a mechanism that converts a hard allocation question into an expensive, leverage-driven negotiation that structurally favors whichever party can afford the litigation and insurance markets built around it.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and deployers are both declared victims because the joint framework's expected structural delta places both parties in the payer role — this is the reading's defining feature relative to its siblings. Injured end users are also victims: shared liability, intended to help them by covering multi-actor harms, in practice imposes a heavier proof burden than a single clear defendant would. Insurers, integrated platform firms, and regulators are beneficiaries because ambiguity itself is their resource: insurers price it, integrated firms internalize it away, and regulators are spared a politically costly single-target rule.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuinely mixed-causation harm — remains partly live (contested status), which prevents outright dismissal of the regime as pure extraction. But the corroboration record shows the operationalized 'causal contribution' standard has drifted toward tracking contracting leverage rather than actual causation, which is the seam the tangled_rope classification is built to hold open: real coordination function, real asymmetric extraction, both present simultaneously, requiring active enforcement (courts, contract law, insurance underwriting) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_contribution_standard_capture,
    'Has the ''causal contribution and control'' apportionment standard, as applied in practice, come to track actual causal responsibility, or has it drifted toward tracking which party holds superior contracting leverage at the point of sale?',
    'Longitudinal analysis of apportionment outcomes and contractual indemnification terms across a sample of adjudicated and settled AI harm cases, comparing outcomes to independent technical assessments of actual causal contribution.',
    'If the standard tracks leverage rather than causation, the shared_liability reading is closer to a snare wearing a tangled_rope''s coordination story; if it tracks genuine causal contribution, the tangled_rope classification is well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_contribution_standard_capture, empirical, 'Whether apportionment outcomes track causation or contracting leverage.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the shared_liability reading itself durable, or does it function as a transitional compromise that will collapse toward one of the sibling readings (developer_liability or deployer_liability) once enough case law and legislative clarity accumulates?',
    'Track whether jurisdictions adopting shared_liability frameworks show convergence toward de facto primary-party rules over a 10-15 year horizon, versus stable multi-party apportionment persisting as the equilibrium.',
    'If shared_liability is transitional, it more closely resembles a scaffold whose true justification is providing time for causal-attribution technology and case law to mature, not a steady-state coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether shared liability is a stable equilibrium or a transitional compromise between the sibling readings.').

omega_variable(
    insurance_market_extraction_share,
    'What share of the premium revenue collected by the indemnification/insurance industry reflects genuine risk-pooling value versus rent extracted from the apportionment ambiguity that the industry has an interest in preserving?',
    'Compare loss ratios and premium growth in AI liability insurance products against the growth rate of apportionment litigation complexity and settlement unpredictability over the same period.',
    'A high extraction share would strengthen the case that insurers are a structural beneficiary with an active interest in preserving ambiguity rather than a neutral risk-pooling intermediary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insurance_market_extraction_share, empirical, 'Whether insurance premiums reflect genuine risk-pooling or rent extraction from preserved ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.18).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__shared_liability, theater_ratio, 4, 0.21).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__shared_liability, theater_ratio, 8, 0.24).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__shared_liability, theater_ratio, 12, 0.27).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__shared_liability, theater_ratio, 16, 0.29).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.3).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__shared_liability, theater_ratio, 24, 0.32).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(liab_be_t4, liability_attribution__shared_liability, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(liab_be_t8, liability_attribution__shared_liability, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(liab_be_t12, liability_attribution__shared_liability, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(liab_be_t16, liability_attribution__shared_liability, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(liab_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(liab_be_t24, liability_attribution__shared_liability, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(liab_su_t4, liability_attribution__shared_liability, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(liab_su_t8, liability_attribution__shared_liability, suppression_requirement, 8, 0.39).
narrative_ontology:measurement(liab_su_t12, liability_attribution__shared_liability, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(liab_su_t16, liability_attribution__shared_liability, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(liab_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(liab_su_t24, liability_attribution__shared_liability, suppression_requirement, 24, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the liability_attribution kernel. developer_liability and deployer_liability each place a single role-category in the primary-liability position; shared_liability instead places both developers and deployers in the victim set jointly, introduces coordination costs, and creates an insurance/indemnification market absent from either single-party reading. All three share the same underlying contested kernel (who bears liability for AI-caused harm along the value chain) but instantiate structurally distinct constraints with distinct beneficiary/victim sets and distinct epsilon values — they are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
