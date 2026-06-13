% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Boundary: Hybrid Commercial/Non-Commercial Carveout
 *   domain: intellectual_property_law/information_economics
 *
 * SUMMARY:
 *   This constraint defines the boundary of a 'derivative work' in
 *   intellectual property law, specifically through a 'hybrid carveout'
 *   reading. Under this interpretation, non-commercial transformative uses of
 *   copyrighted material are generally permitted without authorization, while
 *   commercial uses, even if transformative, typically require licensing.
 *   This creates a bifurcated system where different actors experience the
 *   constraint as either a coordination mechanism or an extractive gate,
 *   depending on their commercial intent. The constraint is actively enforced
 *   through copyright litigation and licensing demands.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.45).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.6).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary: Hybrid Commercial/Non-Commercial Carveout").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property_law/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '0e571e10-d186-4f92-ae3c-e4133b97a322').
narrative_ontology:cs_kernel_codification('0e571e10-d186-4f92-ae3c-e4133b97a322', formalized).
narrative_ontology:cs_authority_grounding('0e571e10-d186-4f92-ae3c-e4133b97a322', lineage).
narrative_ontology:cs_interpretation_layer_present('0e571e10-d186-4f92-ae3c-e4133b97a322').
narrative_ontology:cs_reading_relation('0e571e10-d186-4f92-ae3c-e4133b97a322', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e571e10-d186-4f92-ae3c-e4133b97a322', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('0e571e10-d186-4f92-ae3c-e4133b97a322', foundational, commercial_use_requires_authorization).
narrative_ontology:cs_axiom_status(commercial_use_requires_authorization, holdable).
narrative_ontology:cs_axiom_grounding('0e571e10-d186-4f92-ae3c-e4133b97a322', commercial_use_requires_authorization, conventional).
narrative_ontology:cs_axiom('0e571e10-d186-4f92-ae3c-e4133b97a322', foundational, non_commercial_transformative_use_permitted).
narrative_ontology:cs_axiom_status(non_commercial_transformative_use_permitted, holdable).
narrative_ontology:cs_axiom_grounding('0e571e10-d186-4f92-ae3c-e4133b97a322', non_commercial_transformative_use_permitted, conventional).
narrative_ontology:cs_reference_frame('0e571e10-d186-4f92-ae3c-e4133b97a322', balanced_incentive_and_access).
narrative_ontology:cs_drift_state('0e571e10-d186-4f92-ae3c-e4133b97a322', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('0e571e10-d186-4f92-ae3c-e4133b97a322', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, original_content_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, digital_platforms).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, digital_platforms).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, incentive_to_create_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, fair_use_balancing_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the initial copyright and benefit from the ability to license commercial derivative uses, providing an economic incentive for their creative work. They also benefit from the non-commercial carveout fostering a vibrant ecosystem around their work.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, original_content_creators, beneficiary,
    powerful, generational, mobile, global).

% Seek to create new works for profit based on existing copyrighted material. They face licensing costs and legal risks, which are a direct extraction from their commercial ventures. Their exit options are to pay, litigate, or abandon commercial development.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_developers, payer,
    moderate, biographical, constrained, global).

% Create new works for non-profit purposes (e.g., fan fiction, academic commentary, parody). They benefit from the carveout that largely exempts them from licensing requirements, fostering a culture of free expression and creativity.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users, beneficiary,
    moderate, immediate, mobile, global).

% Enforce the derivative work boundary through legal action, representing original content creators. They shape the interpretation and application of the law through case precedent, and their activity is essential for the constraint's persistence and extractiveness.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_litigators, agenda_setter,
    institutional, biographical, analytical, national).

% Host and distribute both original and derivative content. They face legal liability for infringement and often implement content filtering or takedown policies, incurring costs. They also benefit from the content ecosystem fostered by the rules.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, digital_platforms, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, digital_platforms, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, original_content_creators).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the incentive for original creation with the public interest in transformative use, by differentiating between commercial and non-commercial exploitation of copyrighted works.
% TRANSFER_FUNCTION: Transfers licensing fees and potential litigation costs from commercial derivative developers to original content creators, while granting free use to non-commercial transformative users.
% ABSENT_VOICES: Advocates for a broader 'fair use' or 'free culture' approach, who would argue for less restrictive derivative work definitions even for commercial uses, are often marginalized in legislative and judicial debates dominated by established content industries.
% DISAPPEARANCE_RATIONALE: If this specific boundary vanished, the legal landscape for derivative works would become highly uncertain. Either all derivative works would require authorization (enclosure), stifling creativity, or none would (coordination), undermining creator incentives. The current balance, however imperfect, structures a significant portion of the digital economy.
% FOUNDING_PROBLEM: To incentivize the creation of original works by granting creators control over their commercial exploitation, while also allowing for new creative expression and public discourse built upon existing works.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and economists outside the direct beneficiary groups corroborate that the tension between creator incentives and transformative use remains a live problem in the digital age, though they often contest whether the current 'hybrid carveout' is the optimal solution. Judicial opinions also frequently articulate this ongoing balancing act.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).
:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because it applies selectively to commercial uses, allowing a significant sphere of non-commercial activity. Suppression (0.6) is present due to the legal enforcement mechanisms (litigation, injunctions) that compel commercial actors to license. Theater ratio is low (0.1) as the distinction between commercial and non-commercial use, while sometimes ambiguous, is generally functional and actively adjudicated. The metrics reflect a system that genuinely attempts to balance creator incentives with public access and transformative use, but with an inherent bias towards extraction from commercial ventures.
 *
 * PERSPECTIVAL GAP:
 *   Original content creators and non-commercial transformative users experience this as a Rope or Scaffold, facilitating creative ecosystems and public access. Commercial derivative developers, however, experience it as a Tangled Rope or Snare, facing significant licensing costs and legal risks. The engine will compute these divergent classifications from the declared structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Original content creators are beneficiaries (d=0.0-0.1) as their rights are protected, incentivizing creation. Non-commercial transformative users are also beneficiaries (d=0.0-0.2) as they are largely exempt from licensing. Commercial derivative developers are targets (d=0.7-0.9) as they bear the costs of licensing and legal risk. The constraint subsidizes non-commercial creativity while extracting from commercial exploitation.
 *
 * MANDATROPHY ANALYSIS:
 *   This hybrid carveout reading attempts to prevent the 'enclosure' reading from becoming a pure Snare by preserving a sphere of non-commercial activity, and prevents the 'coordination' reading from becoming a Piton by ensuring creators retain economic control over commercial exploitation. The challenge is maintaining the balance such that the commercial extraction doesn't overwhelm the non-commercial coordination function, leading to a drift towards a Snare for all derivative uses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''hybrid carveout'' reading of the derivative work boundary, or is it better understood as a contested outcome of the ''enclosure'' vs ''coordination'' readings?',
    'Analysis of judicial precedent and legislative intent: if the carveout is consistently applied as a distinct principle rather than a compromise, it is a genuine reading.',
    'If a genuine reading, its classification stands. If a contested outcome, its stability is lower, and its classification might shift with changes in the underlying contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''hybrid_carveout_reading'' of the ''derivative_work_statutory_boundary'' kernel. Sibling readings are ''enclosure_reading'' and ''coordination_reading''.').

omega_variable(
    commercial_transformative_ambiguity,
    'How is ''commercial'' transformative use distinguished from ''non-commercial'' transformative use in practice, and does this distinction create an arbitrary boundary for extraction?',
    'Empirical study of licensing practices and litigation outcomes for borderline cases; analysis of economic impact on small commercial creators.',
    'If the distinction is arbitrary or disproportionately burdens small commercial creators, the extractiveness for ''commercial_derivative_developers'' is higher than measured, and the constraint leans more towards a Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_transformative_ambiguity, empirical, 'Ambiguity in applying the commercial/non-commercial distinction for transformative works.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'derivative_work_statutory_boundary' kernel, each with distinct structural properties and classifications. They are linked to show their conceptual and practical interdependencies within intellectual property law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
