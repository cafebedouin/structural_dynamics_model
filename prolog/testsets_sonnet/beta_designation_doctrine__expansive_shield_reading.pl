% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__expansive_shield_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Beta Designation as Comprehensive, Indefinite, Universal Liability Shield
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This story instantiates the expansive shield reading of the beta
 *   designation kernel: the claim that labeling software 'beta' constitutes a
 *   comprehensive liability waiver, that this designation may persist
 *   indefinitely, and that it applies across all deployment contexts
 *   including safety- and finance-adjacent systems. Under this reading there
 *   is no temporal boundary requiring the developer to eventually graduate
 *   the software to full liability status, and no severity carve-out
 *   excluding critical systems. This produces a wide and growing extraction:
 *   developers externalize essentially all defect costs onto users and
 *   downstream parties, for as long as they choose to retain the label. Two
 *   sibling readings of the same kernel — a narrow, time-bounded warning
 *   reading, and a severity carve-out reading excluding critical systems —
 *   are separate constraint stories with their own ε values; this story does
 *   not average over them, hedge between them, or describe their contest
 *   internally.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.81).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.68).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation as Comprehensive, Indefinite, Universal Liability Shield").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, '66776cb6-e001-4b17-a7bc-758d34316193').
narrative_ontology:cs_kernel_codification('66776cb6-e001-4b17-a7bc-758d34316193', distributed).
narrative_ontology:cs_authority_grounding('66776cb6-e001-4b17-a7bc-758d34316193', extraction).
narrative_ontology:cs_interpretation_layer_present('66776cb6-e001-4b17-a7bc-758d34316193').
narrative_ontology:cs_reading_relation('66776cb6-e001-4b17-a7bc-758d34316193', beta_designation_doctrine__narrow_warning_reading, forecloses).
narrative_ontology:cs_reading_relation('66776cb6-e001-4b17-a7bc-758d34316193', beta_designation_doctrine__severity_carve_out_reading, forecloses).
narrative_ontology:cs_axiom('66776cb6-e001-4b17-a7bc-758d34316193', foundational, designation_alone_waives_liability_without_temporal_or_severity_limit).
narrative_ontology:cs_axiom_status(designation_alone_waives_liability_without_temporal_or_severity_limit, holdable).
narrative_ontology:cs_axiom_grounding('66776cb6-e001-4b17-a7bc-758d34316193', designation_alone_waives_liability_without_temporal_or_severity_limit, conventional).
narrative_ontology:cs_axiom('66776cb6-e001-4b17-a7bc-758d34316193', secondary, publisher_unilateral_control_over_designation_scope_is_legitimate).
narrative_ontology:cs_axiom_status(publisher_unilateral_control_over_designation_scope_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('66776cb6-e001-4b17-a7bc-758d34316193', publisher_unilateral_control_over_designation_scope_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('66776cb6-e001-4b17-a7bc-758d34316193', bounded_testing_disclosure_norm).
narrative_ontology:cs_drift_state('66776cb6-e001-4b17-a7bc-758d34316193', contemporary_permanent_beta_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('66776cb6-e001-4b17-a7bc-758d34316193', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_publishers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, platform_operators_hosting_beta_channels).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, beta_program_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, downstream_consumers_of_beta_dependent_systems).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, small_business_integrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, small_business_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the click-through terms attaching the 'beta' label to any release they choose, for as long as they choose, across any context including safety-adjacent or financially consequential deployments. The label is asserted to waive all defect liability regardless of how long the software has run in production or how the label was disclosed. Publishers set the designation, enforce it through terms of service, and collect the commercial benefit of shipping without a maturity or liability threshold.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_publishers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, software_publishers, beneficiary).

% Operate app stores and distribution channels that permit indefinite beta labeling without requiring graduation criteria or time limits. They benefit from a large volume of nominally-beta software remaining available and monetizable without triggering the platform's standard liability or support obligations.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, platform_operators_hosting_beta_channels, beneficiary,
    institutional, generational, arbitrage, global).

% Install or are defaulted into software carrying a perpetual 'beta' tag — sometimes years or decades old — and bear the full cost of defects: data loss, financial miscalculation, security compromise. Their consent was to a checkbox at install time, not to an open-ended waiver covering a designation that never expires and that they have no way to verify or contest. Leaving is not viable when the beta-labeled component is embedded in tools they depend on for work or daily function.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, beta_program_users, payer,
    powerless, biographical, trapped, global).

% Are affected by failures in systems (medical scheduling software, payment processors, infrastructure dashboards) that themselves depend on components still labeled beta after years of production use. They never agreed to any terms at all — the waiver was signed upstream by an integrator — yet under the expansive reading they have no independent claim against the beta-labeled component's publisher.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, downstream_consumers_of_beta_dependent_systems, payer,
    powerless, biographical, trapped, national).

% Build products on top of beta-labeled APIs and SDKs because they are cheaper or the only option available, then discover the beta tag insulates the upstream publisher from any liability for defects that cascade into the integrator's own customer commitments. They benefit from early access to functionality but absorb the liability risk the publisher has shed.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, small_business_integrators, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, small_business_integrators, beneficiary).

% Would argue that a liability waiver's scope should be bounded by genuine testing purpose and duration, not usable as a permanent shield. Under the expansive reading their enforcement doctrine has not yet caught up with indefinite beta labeling, and industry standard-setting bodies have not invited them into the terms-of-service drafting process.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_protection_regulators, excluded,
    institutional, generational, analytical, national).

% Rule case-by-case on whether a beta designation defeats a products-liability claim. Their decisions currently vary, which is part of why the expansive reading persists in some jurisdictions and contracts while being rejected in others.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, courts_adjudicating_beta_disclaimers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__expansive_shield_reading, software_publishers).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__expansive_shield_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables developers to release early or experimental functionality to willing testers with reduced liability exposure, in principle allowing faster iteration and broader real-world testing than a fully warranted release would permit.
% TRANSFER_FUNCTION: Moves the cost of defects, failures, and downstream harm from the software's publisher to whoever uses, integrates, or is affected by the beta-labeled component — indefinitely, and regardless of whether the deployment context is safety- or finance-adjacent.
% ABSENT_VOICES: Consumer protection regulators and the downstream consumers who never saw or agreed to any terms are structurally absent from the contract formation that establishes the waiver; small business integrators are present as counterparties but lack bargaining power to negotiate the designation's scope or duration.
% DISAPPEARANCE_RATIONALE: If the expansive shield reading disappeared and beta status reverted to a genuinely time-bounded, severity-limited disclosure, publishers would face pressure to graduate long-running beta components to full liability status, insurance and contract terms for embedded software would be renegotiated, and a body of currently-uncompensated harm would become actionable.
% FOUNDING_PROBLEM: Early software testing needed a legal and commercial mechanism to let developers ship experimental code to real users without full production-grade liability, so that genuine bugs found during a bounded testing period would not expose the developer to catastrophic claims for known-incomplete software.
% FOUNDING_PROBLEM_CORROBORATION: Software engineering literature and several court opinions (cited in products-liability treatises) note that 'beta' as used by major publishers today frequently denotes commercially complete, revenue-generating, indefinitely-maintained products rather than a bounded test phase; this observation comes from judicial opinions and independent software-engineering commentary outside the publishers relying on the designation, not from the publishers themselves.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__expansive_shield_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__expansive_shield_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.38) reflecting the doctrine's origin in genuine early-testing disclosure, and rises sharply (to 0.81) as the practice of indefinite, cross-context beta labeling becomes normalized and litigated — the metric tracks the doctrine's drift from bounded disclosure to permanent shield, not a static snapshot. Suppression rises in parallel (0.35 to 0.68) as publishers build increasingly explicit and enforced click-through terms asserting the waiver's scope. Theater ratio rises to 0.42 as the 'testing' framing becomes increasingly performative relative to the commercial permanence of the labeled software. Accessibility collapse (0.62) reflects that once a user has accepted the terms and become dependent on the software, alternatives are functionally foreclosed even though formally the terms were 'agreed to.'
 *
 * PERSPECTIVAL GAP:
 *   From the publisher's seat the designation is a legitimate, contractually-agreed risk allocation mechanism they are free to structure as they see fit. From the trapped user's seat the same designation is an unbounded waiver they had no real opportunity to negotiate or understand the scope of. The engine computes these as structurally different seat outcomes from the same declared power/exit data; the divergence itself is the object of interest, not a contradiction to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Software publishers and the platforms that host their beta channels sit at the beneficiary end: they set the designation, control its duration, and capture the commercial value of avoiding liability while still shipping and monetizing the product. Beta program users and downstream consumers sit at the target end — trapped exit options, no bargaining power over the designation's scope, and in the downstream consumer case no contractual privity at all. Small business integrators occupy a mixed position: they benefit from early access to functionality but absorb liability risk shed by the upstream publisher, making them simultaneously payer and secondary beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling bounded real-world testing without catastrophic liability exposure — is genuinely dead under this reading's actual practice: software labeled beta for years while generating revenue and supporting critical functions is not undergoing testing in any meaningful sense. The classification as snare rather than tangled_rope reflects that under the expansive reading, the coordination story (testing disclosure) has become pure cover; there is no active testing function left to weigh against the extraction, only the label's persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expansive_reading_legal_validity,
    'Does any binding contract or precedent actually license a beta designation to function as a comprehensive, indefinite, all-context liability waiver, or is this reading an aspirational drafting posture that courts have not uniformly upheld?',
    'Survey of jurisdictional case law on beta-clause enforceability, focused on cases where courts either upheld or struck down indefinite-duration or critical-system beta disclaimers.',
    'If courts systematically reject the expansive reading, its effective extraction is bounded by litigation risk despite the doctrine''s drafted scope; if courts routinely uphold it, the extraction measured here understates the doctrine''s real-world reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansive_reading_legal_validity, empirical, 'Whether the expansive reading is actually enforceable as drafted or mostly untested boilerplate.').

omega_variable(
    kernel_reading_dominance,
    'Which of the three kernel readings (expansive, narrow, severity carve-out) actually governs the majority of real-world beta-labeled software in practice, and is that dominance shifting?',
    'Empirical audit of terms-of-service language and litigation outcomes across a sample of major software publishers, tracked over time.',
    'If the expansive reading is losing ground to the severity carve-out reading (e.g., through sector-specific regulation for medical or financial software), this story''s rising extractiveness trajectory may need to be read as a peak rather than a stable state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_dominance, empirical, 'Whether the expansive shield reading is the dominant, ascendant, or declining reading of the kernel in practice.').

omega_variable(
    downstream_privity_gap,
    'Should downstream consumers who never accepted any beta terms at all be treated as victims of this constraint, or does the absence of contractual privity place them outside its scope entirely?',
    'Doctrinal analysis of third-party beneficiary and products-liability law as applied to embedded/OEM software components.',
    'If downstream consumers are outside the doctrine''s scope as a matter of law, the victim set narrows and the story''s extraction figure should be revised downward for that population, even though the harm they experience is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_privity_gap, conceptual, 'Whether privity-less downstream consumers are properly counted within this constraint''s victim set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 20, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(beta_su_t4, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(beta_su_t8, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(beta_su_t16, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__expansive_shield_reading, 0.05).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'beta designation liability doctrine.' The expansive_shield_reading (this story) treats beta status as a comprehensive, indefinite, universal waiver and is authored as substantially extractive (snare). The narrow_warning_reading treats beta status as a bounded testing disclosure preserving base liability and is authored with much lower extraction. The severity_carve_out_reading treats beta status as categorically unavailable for critical systems and functions as a partial mountain/rope hybrid bounding the other two readings' reach. All three share the same underlying doctrinal text and label but instantiate structurally distinct claims with different ε values, different victim sets, and different classifications — per the ε-invariance principle they are authored as separate constraints linked here rather than as one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
