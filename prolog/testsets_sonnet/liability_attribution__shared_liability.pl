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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: AI Value-Chain Joint Liability Apportionment (Causal Contribution / Control Standard)
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   As AI systems cause harm through failures that arise from an interaction
 *   of upstream model behavior and downstream deployment choices, regulators
 *   and courts face a design question: who pays when responsibility is
 *   genuinely distributed? This story instantiates the shared-liability
 *   reading of the liability_attribution kernel — joint liability apportioned
 *   along the value chain according to causal contribution and control, with
 *   both developers and deployers named as potential co-defendants and an
 *   indemnification/insurance market emerging to price and reallocate the
 *   resulting exposure. This is a distinct constraint from the
 *   developer_liability and deployer_liability readings of the same kernel:
 *   those assign primary liability to a single named party in the chain,
 *   producing a narrower victim set and a simpler (cheaper, less
 *   coordination-intensive) compliance picture. Under shared liability, the
 *   victim set expands to include both developer and deployer categories
 *   simultaneously, coordination costs rise because apportionment must be
 *   litigated or contractually pre-negotiated in every case, and a derivative
 *   insurance/indemnification market emerges that does not exist under a
 *   single-party standard. These are structurally different constraints with
 *   different ε profiles, not the same constraint viewed from different
 *   angles — hence three separate stories linked by kernel context rather
 *   than one story with a hedge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.52).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.44).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.52).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "AI Value-Chain Joint Liability Apportionment (Causal Contribution / Control Standard)").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '0c3aacf8-882f-4202-9c97-9061c62eef0f').
narrative_ontology:cs_kernel_codification('0c3aacf8-882f-4202-9c97-9061c62eef0f', distributed).
narrative_ontology:cs_authority_grounding('0c3aacf8-882f-4202-9c97-9061c62eef0f', distributed).
narrative_ontology:cs_reading_relation('0c3aacf8-882f-4202-9c97-9061c62eef0f', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('0c3aacf8-882f-4202-9c97-9061c62eef0f', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('0c3aacf8-882f-4202-9c97-9061c62eef0f', foundational, responsibility_is_apportionable_not_localizable).
narrative_ontology:cs_axiom_status(responsibility_is_apportionable_not_localizable, holdable).
narrative_ontology:cs_axiom_grounding('0c3aacf8-882f-4202-9c97-9061c62eef0f', responsibility_is_apportionable_not_localizable, conventional).
narrative_ontology:cs_axiom('0c3aacf8-882f-4202-9c97-9061c62eef0f', secondary, control_and_causal_contribution_jointly_determine_share).
narrative_ontology:cs_axiom_status(control_and_causal_contribution_jointly_determine_share, holdable).
narrative_ontology:cs_axiom_grounding('0c3aacf8-882f-4202-9c97-9061c62eef0f', control_and_causal_contribution_jointly_determine_share, instrumental).
narrative_ontology:cs_created_at('0c3aacf8-882f-4202-9c97-9061c62eef0f', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_and_indemnification_industry).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, large_integrated_platform_vendors).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, harmed_end_users).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_model_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, downstream_deployers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, small_and_midsize_deployers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, large_integrated_platform_vendors).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, polluter_pays_along_causal_chain).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, control_tracks_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Builds and trains the underlying model, sets architecture and training data choices, and licenses it downstream. Under the shared-liability standard, is co-defendant on any harm traceable in part to model behavior even when a deployer configured or fine-tuned it into the specific harmful use. Bears substantial legal exposure and discovery cost proportional to a causal-contribution test that is expensive to litigate and hard to predict ex ante; cannot fully exit the liability regime while continuing to license into the jurisdiction, though it can renegotiate contract terms to shift some exposure downstream via indemnification clauses.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_model_developers, payer,
    powerful, biographical, constrained, global).

% Integrates the model into a product, controls deployment context, user-facing configuration, and guardrails. Under shared liability, is co-defendant alongside the developer whenever its deployment choices contributed to a harm, even when the underlying capability originated upstream. Negotiates indemnification and insurance to manage exposure, but the negotiation itself is a resource-intensive activity larger deployers manage more effectively than smaller ones.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, downstream_deployers, payer,
    powerful, biographical, constrained, national).

% Deploys licensed models into products without the legal or actuarial staff to negotiate favorable indemnification terms or carry standalone liability insurance at reasonable rates. Faces the same joint-and-several exposure as large deployers but without the bargaining leverage to shift the apportionment contractually, effectively absorbing a larger relative share of the coordination cost the regime imposes. Exiting the market rather than accepting the liability terms is often the only real alternative.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, small_and_midsize_deployers, payer,
    moderate, biographical, trapped, national).

% Suffers direct harm from an AI system's output or behavior and seeks compensation. Under shared liability, no longer needs to identify a single culpable party before recovering — can proceed against the value chain collectively and let apportionment be litigated among defendants. Benefits from a larger, more solvent pool of potential compensators but still bears the initial burden of proving harm and causal contribution occurred somewhere in the chain.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, harmed_end_users, beneficiary,
    powerless, immediate, trapped, national).

% Underwrites AI liability risk and drafts indemnification products for both developers and deployers now that both sides face joint exposure. The uncertainty created by a causal-contribution-and-control standard is exactly the risk this industry prices and sells protection against; more distributed liability means a larger addressable market for coverage, risk audits, and contractual allocation instruments.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_and_indemnification_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Operates both as developer and deployer within a single vertically integrated stack, or has the scale to dictate indemnification terms to smaller partners in either direction. Pays into the liability pool like anyone else but can internalize the coordination costs across a large balance sheet and can use its bargaining power to push disproportionate contractual exposure onto smaller counterparties, turning the joint-liability regime into a competitive moat against less integrated rivals.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, large_integrated_platform_vendors, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, large_integrated_platform_vendors, payer).

% Designs and administers the causal-contribution-and-control test, sets apportionment procedures, and adjudicates disputes over relative fault. Determines how much discovery burden, litigation cost, and predictability the regime imposes on all parties through the specificity (or vagueness) of the standard it enforces.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulators_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Releases models or components without the resources to negotiate indemnification chains, obtain liability insurance at institutional rates, or participate meaningfully in the regulatory rulemaking that defines the causal-contribution standard. Bears joint-liability exposure identical in form to well-resourced developers but without comparable capacity to manage or transfer it, and is rarely consulted when the standard's contours are set.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, open_source_and_independent_developers, excluded,
    moderate, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single apportionment procedure so that a harmed party need not first litigate which single actor in a multi-party AI value chain is 'the' responsible one; liability is distributed across developer and deployer according to their actual causal contribution and degree of control, avoiding both under-compensation (no party found solely liable) and arbitrary over-targeting of whichever party is easiest to sue.
% TRANSFER_FUNCTION: Moves compensation obligations from harmed end users' uncompensated losses toward a shared pool drawn from both developers and deployers; simultaneously moves coordination and insurance costs from the state (which would otherwise need to arbitrate single-party liability disputes) onto private contractual and actuarial markets, and moves relative cost burden from well-resourced integrated firms (who can price and shift it) toward smaller developers and deployers (who absorb it directly).
% ABSENT_VOICES: Open-source and independent developers, plus small deployers with no seat in the standard-setting process, would argue that a causal-contribution test calibrated for well-lawyered defendants imposes fixed compliance and insurance costs that scale regressively against firm size — effectively taxing market entry. They are largely absent from the regulatory and industry-association tables where apportionment procedures are negotiated.
% DISAPPEARANCE_RATIONALE: If shared liability disappeared and reverted to a single-party standard (either pure developer or pure deployer liability), harmed users would face a narrower, easier-to-litigate but less complete compensation pool; indemnification and insurance markets built around apportioned risk would need to reprice or dissolve; and either developers or deployers would suddenly bear concentrated liability they currently diffuse contractually — a substantial reorganization of contracting practice, pricing, and litigation strategy across the industry.
% FOUNDING_PROBLEM: Early AI harm litigation under single-party liability standards produced inconsistent, often unjust outcomes: developers escaped liability by pointing to deployer misuse, deployers escaped liability by pointing to inherent model defects, and harmed parties were left without a clear path to compensation when responsibility was genuinely distributed across the chain.
% FOUNDING_PROBLEM_CORROBORATION: Plaintiffs' bar and consumer-harm advocacy groups (outside the developer/deployer beneficiary set) corroborate that the single-party gap was real and that shared liability closes it. Small developers and deployers, also outside the primary beneficiary set, corroborate a different founding-problem reading: that the joint standard, as currently administered, has become less about closing the compensation gap and more about a stable indemnification-market structure that primarily benefits insurers and large integrated vendors who can absorb its coordination costs.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52, rising over the interval from 0.34) reflects the growing cost of an apportionment standard whose predictability declines the more contested causal-contribution litigation becomes, layered onto a rent stream captured by the insurance/indemnification industry as it matures. Suppression (0.44) is moderate: the regime does not forbid exit from the market outright, but small developers and deployers face effectively trapped exit because abandoning the market (rather than accepting joint exposure) is the only real alternative to participation. Theater ratio (0.31) captures a meaningful and growing share of compliance activity — indemnification paperwork, contractual risk-shifting clauses, insurance audits — that manages liability allocation on paper more than it prevents underlying harm. Accessibility collapse is moderate (0.4): the single-party liability alternatives remain conceptually available and are litigated as competing legal theories in other jurisdictions, so alternatives have not fully disappeared. Resistance is elevated (0.58) because both developer and deployer trade associations actively lobby against aspects of the joint standard they experience as costly, and small-firm coalitions specifically resist the regressive cost structure.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/court seat, the standard is coordination: a genuine improvement on single-party liability's under- and mis-compensation failures. From the small-deployer or independent-developer seat, the same apportionment machinery computes as an extractive cost center — one they cannot meaningfully negotiate down and that functions, in practice, as a scale-dependent tax on market participation. The engine should register this divergence as a structural fact about differential exit options and bargaining power, not as disagreement about what the standard is for.
 *
 * DIRECTIONALITY LOGIC:
 *   Harmed end users are the clearest beneficiaries — the shared standard removes their burden of identifying a single culpable party, moving their directionality toward the subsidized end. The insurance/indemnification industry is a structural beneficiary of the uncertainty itself: more distributed, harder-to-predict liability is a larger market for its products. Large integrated platform vendors sit closer to the beneficiary end than their nominal payer role suggests because they can price the coordination cost into a diversified balance sheet and, critically, can use bargaining leverage to shift disproportionate contractual exposure onto smaller counterparties — this is why they carry a secondary payer role rather than a pure beneficiary one. Small and midsize deployers and open-source/independent developers sit at the target end: trapped exit options, no comparable bargaining leverage, and full exposure to the same joint-and-several standard as well-resourced parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — single-party liability leaving genuinely distributed harm uncompensated — remains partially live (contested, not dead): AI harms genuinely do arise from interaction effects across the chain, so the coordination function has not been fully obsoleted. But the corroboration split (plaintiffs' advocates vs. small-firm coalitions) signals a possible mandatrophy trajectory: if the indemnification-market structure that emerged to manage the standard has itself become the primary beneficiary structure — displacing the compensation-gap-closing function that justified the standard originally — that would be the signature of a coordination mechanism drifting toward tangled-rope status even while its founding rationale remains partially true. The rising theater_ratio and suppression_requirement trends over the measured interval are consistent with this drift and warrant continued monitoring rather than an early declaration of mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_contribution_test_specificity,
    'Can ''causal contribution and control'' be operationalized as a predictable, litigable standard, or does its inherent vagueness function as a permanent discovery-cost generator that primarily benefits parties with more litigation resources?',
    'Track case outcomes and settlement patterns over several years: if apportionment percentages converge to predictable ranges for similar fact patterns, the standard is maturing into workable doctrine; if outcomes remain highly variable and settlement-cost-driven, the vagueness is functioning as a structural extraction mechanism.',
    'A maturing, predictable standard would support reclassifying this constraint toward rope (a converged coordination mechanism with declining transaction cost); persistent unpredictability would support tangled_rope or drift toward snare as litigation cost becomes the dominant feature experienced by smaller parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_contribution_test_specificity, empirical, 'Whether the causal-contribution-and-control standard converges to predictable doctrine or remains a persistent cost generator.').

omega_variable(
    sibling_reading_selection_pressure,
    'Is the choice among the three liability_attribution readings (developer, deployer, shared) being made on the coordination merits of compensating genuinely distributed harm, or is it being selected by whichever industry coalition has more lobbying capacity in a given jurisdiction?',
    'Compare the legislative and regulatory drafting history across multiple jurisdictions adopting different readings: does the choice track independent harm-compensation analysis, or does it track which industry segment (upstream model developers vs. downstream application deployers) has stronger lobbying presence in that jurisdiction?',
    'If reading-selection tracks lobbying capacity rather than harm-compensation merits, all three readings in the kernel family should be understood partly as artifacts of relative political power rather than purely as competing legal theories — this would deepen the extraction reading of whichever standard prevails in a captured jurisdiction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_selection_pressure, conceptual, 'Whether cross-jurisdictional adoption of different kernel readings tracks coordination merit or lobbying power.').

omega_variable(
    insurance_market_capture_risk,
    'Does the indemnification/insurance market that emerges under shared liability primarily transfer risk efficiently, or does it primarily generate a new rent stream that persists independent of whether it reduces actual harm?',
    'Compare loss ratios and pricing trends in the AI liability insurance market over time against actual harm-reduction outcomes; a persistently profitable, growing market segment with flat or worsening underlying harm rates would indicate rent extraction rather than efficient risk transfer.',
    'Efficient risk transfer supports the coordination reading of the shared-liability standard; rent extraction without harm reduction supports reclassifying the insurance-industry beneficiary role as a primary rather than incidental driver of the constraint''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_market_capture_risk, empirical, 'Whether the derivative insurance market is efficient risk transfer or independent rent extraction.').


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
narrative_ontology:measurement(liab_be_t4, liability_attribution__shared_liability, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(liab_be_t8, liability_attribution__shared_liability, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(liab_be_t12, liability_attribution__shared_liability, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(liab_be_t16, liability_attribution__shared_liability, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(liab_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(liab_be_t24, liability_attribution__shared_liability, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(liab_su_t4, liability_attribution__shared_liability, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(liab_su_t8, liability_attribution__shared_liability, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(liab_su_t12, liability_attribution__shared_liability, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(liab_su_t16, liability_attribution__shared_liability, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(liab_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(liab_su_t24, liability_attribution__shared_liability, suppression_requirement, 24, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the liability_attribution kernel. developer_liability and deployer_liability each assign primary responsibility to a single named party and have narrower victim sets and lower coordination overhead; shared_liability names both developers and deployers as victims simultaneously and carries higher coordination cost plus an emergent insurance/indemnification market absent from the single-party readings. Each story carries its own ε and its own stakeholder structure; they are linked here rather than merged because adopting one reading in a jurisdiction structurally forecloses simultaneous adoption of a sibling reading for the same harm event, even though all three remain live legislative and doctrinal proposals across different jurisdictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
