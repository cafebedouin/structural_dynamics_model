% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Joint/Proportional Liability Along the AI Value Chain
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   As AI harm litigation matured, a single-defendant approach to liability —
 *   pinning fault entirely on either the model developer or the deploying
 *   organization — proved a poor fit for harms that plainly arose from the
 *   interaction of design choices and deployment configuration. The
 *   shared-liability reading of the liability_attribution kernel apportions
 *   fault by causal contribution and control, splitting exposure across the
 *   value chain. This produces a genuine coordination gain (better fit
 *   between fault-finding and actual causation, wider recovery pool for
 *   harmed parties) but also opens a market in apportionment complexity:
 *   insurers, large integrated vendors with negotiating leverage, and
 *   litigators specializing in multi-defendant AI suits all gain from the
 *   resulting uncertainty, while parties without bargaining power —
 *   independent integrators, smaller deployers, and developers exposed to
 *   downstream misuse they cannot audit or control — absorb costs
 *   disproportionate to their actual fault share.
 *
 * KEY AGENTS:
 *   - model_developers: primary target, powerful but constrained exit — bears apportioned exposure for downstream misuse
 *   - downstream_deployers: primary target, moderate power, constrained exit — co-defendant status regardless of configuration control
 *   - independent_integrators: powerless, trapped exit — cannot self-insure or negotiate indemnification
 *   - insurance_and_indemnification_intermediaries: organized beneficiary — monetizes apportionment ambiguity
 *   - large_integrated_vendors: institutional beneficiary/payer — converts compliance cost into competitive moat
 *   - litigating_plaintiffs_bar: organized beneficiary — gains leverage from a widened defendant pool
 *   - regulators_and_courts: agenda-setter — designs and can revise the apportionment standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.58).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.52).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.58).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Joint/Proportional Liability Along the AI Value Chain").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '097aeef5-36e9-4de9-8d3a-1c46a8d17b9d').
narrative_ontology:cs_kernel_codification('097aeef5-36e9-4de9-8d3a-1c46a8d17b9d', distributed).
narrative_ontology:cs_authority_grounding('097aeef5-36e9-4de9-8d3a-1c46a8d17b9d', distributed).
narrative_ontology:cs_reading_relation('097aeef5-36e9-4de9-8d3a-1c46a8d17b9d', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('097aeef5-36e9-4de9-8d3a-1c46a8d17b9d', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('097aeef5-36e9-4de9-8d3a-1c46a8d17b9d', foundational, liability_should_track_proportional_causal_contribution).
narrative_ontology:cs_axiom_status(liability_should_track_proportional_causal_contribution, holdable).
narrative_ontology:cs_axiom_grounding('097aeef5-36e9-4de9-8d3a-1c46a8d17b9d', liability_should_track_proportional_causal_contribution, instrumental).
narrative_ontology:cs_axiom('097aeef5-36e9-4de9-8d3a-1c46a8d17b9d', secondary, no_single_value_chain_party_should_bear_full_exposure_absent_full_control).
narrative_ontology:cs_axiom_status(no_single_value_chain_party_should_bear_full_exposure_absent_full_control, holdable).
narrative_ontology:cs_axiom_grounding('097aeef5-36e9-4de9-8d3a-1c46a8d17b9d', no_single_value_chain_party_should_bear_full_exposure_absent_full_control, deontological).
narrative_ontology:cs_created_at('097aeef5-36e9-4de9-8d3a-1c46a8d17b9d', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_and_indemnification_intermediaries).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, large_integrated_vendors).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, litigating_plaintiffs_bar).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, model_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, downstream_deployers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, independent_integrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, end_users_harmed_parties).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, large_integrated_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and release the underlying model or component. Under shared liability they face apportioned exposure even when harm arises from downstream fine-tuning, prompting, or misuse they did not control. They respond by drafting restrictive usage licenses, mandating audit hooks, and pricing indemnification into contracts, but cannot fully exit exposure because courts and regulators still look at their design and training choices as a causal contribution.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, model_developers, payer,
    powerful, biographical, constrained, global).

% Integrate the model into a product, configure it, and control the deployment context users actually encounter. Shared liability makes them co-defendants whenever their configuration or oversight choices contribute to harm, even where the underlying capability came from the developer. Smaller deployers cannot self-insure and depend on developer indemnification clauses they have little leverage to negotiate.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, downstream_deployers, payer,
    moderate, biographical, constrained, national).

% Small firms and individual developers who build applications on top of licensed models. They have neither the bargaining power to obtain indemnification nor the capital to carry independent liability insurance, and often cannot audit the model's internals to demonstrate they exercised due care. Exiting the market by not building on these platforms means losing access to viable technology.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, independent_integrators, payer,
    powerless, immediate, trapped, regional).

% Individuals harmed by an AI system's output or decision. Shared liability nominally benefits them by widening the pool of solvent defendants and reducing the risk that a plaintiff loses because the 'wrong' party in the chain was sued. In practice, proving each party's causal contribution and degree of control is expensive and slows recovery, so the benefit is real but partial.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, end_users_harmed_parties, beneficiary,
    powerless, immediate, trapped, national).

% Underwrite AI liability risk, draft indemnification templates, and price coverage based on modeled apportionment risk. The complexity and uncertainty of joint-liability apportionment is their raw material: the harder it is to predict how courts will split fault, the more valuable (and expensive) their products become. They gain from the ambiguity that costs developers and deployers money to navigate.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_and_indemnification_intermediaries, beneficiary,
    organized, generational, arbitrage, global).

% Firms that both develop and deploy their own models internally can absorb joint-liability compliance costs at scale, negotiate favorable contractual risk allocation with smaller partners, and use compliance infrastructure as a competitive moat. They pay real costs but convert them into barriers that disadvantage smaller pure-play developers or deployers who must contract at arm's length.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, large_integrated_vendors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, large_integrated_vendors, payer).

% Litigators who bring AI harm claims benefit from a wider set of viable defendants and greater settlement leverage when liability is distributed, since any single defendant with assets is a foothold. Their incentive is toward maximizing the number of parties named and the ambiguity of apportionment, which lengthens and enriches litigation rather than necessarily shortening the path to compensation.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, litigating_plaintiffs_bar, beneficiary,
    organized, biographical, arbitrage, national).

% Design and enforce the apportionment rules — statutory frameworks or common-law tests for causal contribution and control. They set the standard that determines how liability is split, and can revise it, but rely on case-by-case adjudication that is slow, expensive, and produces inconsistent early precedent.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, diffuse).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributing liability by causal contribution and control lets courts and regulators avoid an all-or-nothing choice between developer-only and deployer-only liability regimes, matching the imposed cost more closely to who actually made the decisions that produced the harm across a multi-party value chain.
% TRANSFER_FUNCTION: Moves litigation exposure, insurance premiums, and compliance/audit costs from any single party toward whichever combination of developer, deployer, and integrator a court finds contributed causally — and, in practice, toward whichever parties in that set have the least bargaining power to contract the exposure away, plus a share to insurers and litigators who monetize the resulting complexity.
% ABSENT_VOICES: Independent integrators and small deployers, who have no seat in the regulatory or standard-setting process and no capital to shape indemnification templates, would object that the apportionment doctrine assumes symmetric bargaining power that does not exist; they are not consulted when large vendors and their counsel negotiate the model liability-shifting contract language that becomes industry default.
% DISAPPEARANCE_RATIONALE: If shared liability disappeared and a single-party regime (developer-only or deployer-only) took its place instantly, contract drafting practices, insurance pricing models, and litigation strategy would all have to reorganize around the new bright-line rule; a large body of freshly signed indemnification and audit-cooperation clauses calibrated to apportionment risk would become moot overnight.
% FOUNDING_PROBLEM: Early AI harm cases produced inconsistent, arbitrary outcomes because plaintiffs could only recover from whichever single party the court happened to find liable, even when harm plainly resulted from an interaction between the model's design and the deployer's configuration choices — a mismatch between real multi-causal harm and single-defendant doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Consumer-protection litigators and several appellate opinions attest the multi-causal harm problem is real and unresolved by single-party rules. Independent integrators and smaller deployers — outside the set of parties who benefit from the resulting indemnification and insurance markets — attest that in practice the doctrine has shifted from solving multi-causal attribution toward generating negotiable, saleable risk categories that concentrate cost on parties without leverage to contract out of it.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects that the doctrine's coordination function — matching liability to actual causal contribution — is real but is layered with a genuine extraction channel: parties without bargaining power (independent integrators, smaller deployers) end up bearing costs disproportionate to fault because contractual risk-shifting flows toward whoever has the least leverage, not necessarily whoever had the least control. Suppression (0.52) captures active enforcement machinery (litigation, contractual indemnification requirements, audit mandates) needed to hold the apportionment regime in place; it is moderate rather than severe because courts retain discretion and the standard is still evolving. Theater ratio (0.31) is present but not dominant — compliance audits and 'AI risk allocation' contract boilerplate carry some performative weight, but the underlying litigation and insurance mechanics are functionally real. The temporal series show extraction and enforcement intensity rising over the interval as apportionment case law accumulated and indemnification markets matured, consistent with a coordination structure that has been progressively colonized by rent-seeking intermediaries as it stabilized.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers, deployers, and independent integrators are structurally positioned as targets: shared liability puts each at risk for harms partly outside their individual control, and none has full exit (developers cannot fully contract away downstream misuse exposure; deployers and integrators cannot always negotiate indemnification on favorable terms). Insurers, litigators, and large vertically-integrated vendors sit at the beneficiary end: they either monetize the apportionment uncertainty directly (insurers, litigators) or convert compliance capacity into competitive advantage (large vendors). End users are a partial, diffuse beneficiary — recovery odds improve, but proving each party's causal share is itself costly and slow, so the benefit is real but attenuated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — single-defendant doctrine mismatched to genuinely multi-causal AI harms — remains partly live (courts still encounter cases resistant to single-party attribution), which argues against pure mandatrophy. But the founding_problem_status is authored as contested because independent integrators and smaller deployers attest that the arrangement's dominant present function is generating negotiable risk categories for insurers and large vendors rather than accurately tracking causal contribution — a shift the classification as tangled_rope (rather than a clean rope) is designed to register: genuine coordination function, but co-existing with asymmetric extraction that requires active enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apportionment_standard_ambiguity,
    'Does ''causal contribution and control'' function as a genuinely administrable legal standard, or does its inherent vagueness make it a de facto license for courts and contracting parties to allocate liability by bargaining power rather than actual fault?',
    'Track dispersion in judicial apportionment outcomes across factually similar cases; high variance unexplained by factual differences would indicate the standard is not administrable and is instead a proxy for litigation resourcing.',
    'If the standard resolves to bargaining-power allocation in practice, the coordination story (matching liability to causation) is largely cover for a redistribution mechanism, pushing this reading structurally closer to snare; if outcomes track causation closely, the tangled_rope classification with a real coordination core is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apportionment_standard_ambiguity, empirical, 'Whether the apportionment standard is administrable or a proxy for bargaining power.').

omega_variable(
    kernel_reading_divergence,
    'Is shared_liability a distinct constraint from developer_liability and deployer_liability, or are all three simply different measurement conventions applied to the same underlying liability_attribution kernel?',
    'Compare victim sets, beneficiary sets, and ε across the three sibling stories: developer_liability and deployer_liability each concentrate the victim set on a single party with a cleaner (lower-coordination-cost) transfer, while shared_liability spreads the victim set across both roles and adds a distinct insurance/litigation beneficiary class not present in either single-party reading.',
    'Confirms that each reading warrants its own constraint story per the ε-invariance principle rather than a single story with an observable-selection parameter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Confirming that the three kernel readings are structurally distinct constraints, not one constraint measured three ways.').

omega_variable(
    insurance_market_capture_risk,
    'Will the indemnification/insurance market that emerges to price apportionment risk itself become a captured intermediary layer that lobbies to preserve apportionment ambiguity (since ambiguity is its raw material) rather than supporting standard clarification?',
    'Track whether insurance and indemnification industry associations lobby for or against legislative clarification of the causal-contribution-and-control standard over the next several years.',
    'If the intermediary layer actively opposes clarification, that is direct evidence the coordination function is being subordinated to an extraction interest, supporting eventual reclassification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insurance_market_capture_risk, empirical, 'Whether emergent insurance intermediaries will lobby to preserve rather than resolve apportionment ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.12).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__shared_liability, theater_ratio, 4, 0.16).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__shared_liability, theater_ratio, 8, 0.2).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__shared_liability, theater_ratio, 12, 0.24).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__shared_liability, theater_ratio, 16, 0.27).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.29).
narrative_ontology:measurement(liab_tr_t24, liability_attribution__shared_liability, theater_ratio, 24, 0.31).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(liab_be_t4, liability_attribution__shared_liability, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(liab_be_t8, liability_attribution__shared_liability, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(liab_be_t12, liability_attribution__shared_liability, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(liab_be_t16, liability_attribution__shared_liability, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(liab_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(liab_be_t24, liability_attribution__shared_liability, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(liab_su_t4, liability_attribution__shared_liability, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(liab_su_t8, liability_attribution__shared_liability, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(liab_su_t12, liability_attribution__shared_liability, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(liab_su_t16, liability_attribution__shared_liability, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(liab_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(liab_su_t24, liability_attribution__shared_liability, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the liability_attribution kernel. developer_liability and deployer_liability each concentrate the victim set on a single value-chain party with a simpler transfer function; shared_liability distributes the victim set across both developers and deployers, adds independent_integrators as a distinct powerless victim class, and introduces insurance/indemnification intermediaries as a beneficiary class absent from the single-party readings. All three share the same kernel (who bears liability for AI harm) but instantiate structurally distinct constraints with different ε, different beneficiaries, and different victims — consistent with the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
