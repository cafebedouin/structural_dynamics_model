% ============================================================================
% CONSTRAINT STORY: behavioral_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_mechanism_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: behavioral_mechanism_reading
 *   human_readable: Persona-as-Steerability-Mechanism Reading of Simulated-User Evaluation
 *   domain: AI evaluation infrastructure / simulated-user research methodology
 *
 * SUMMARY:
 *   This story instantiates the 'behavioral mechanism' reading of the
 *   contested kernel persona_as_valid_proxy: the claim that what matters
 *   about a simulated-user persona is not whether it correspondingly
 *   represents a real person, but whether assigning it causally and traceably
 *   shifts an LLM's output in the declared direction, legibly, auditably, and
 *   reproducibly. Under this reading the load-bearing evidence is the
 *   controlled adherence probe's cited-evidence audit trail (Table 17) and
 *   the cross-model steerability comparison (Opus 91.5% vs GPT-5.6-sol
 *   79.2%). Population-level product outcomes (paid-plan conversion rates,
 *   purchase intent) are explicitly out of this reading's scope — they are
 *   downstream noise, not evidence for or against the mechanism claim. This
 *   is a narrower and more defensible claim than either the instrumentalist
 *   screening-tool reading or the representational correspondence reading of
 *   the same underlying persona-assignment kernel; it trades ambition for
 *   auditability. The two documented failure boundaries this reading
 *   foregrounds as central findings, not embarrassments, are the
 *   cog-verbosity 0/5 failure under GPT-5.6-sol and the OS-App politeness 0/5
 *   suppression failure — cases where persona conditioning failed to override
 *   the model's alignment priors, which under this reading is exactly the
 *   kind of boundary-of-steerability data the methodology exists to surface.
 *
 * KEY AGENTS:
 *   - eval_infrastructure_teams: designs and runs the probe, sets the scope of the claim
 *   - model_vendors_citing_steerability_metrics: benefits from a defensible, narrower controllability claim
 *   - persona_conditioning_researchers: builds a legitimate research program insulated from correspondence-validity attacks
 *   - downstream_product_decision_makers: receives the report but risks misapplying scoped findings to product questions
 *   - policy_readers_of_eval_reports: may read adherence percentages as safety assurances beyond the claim's actual scope
 *   - methodology_auditors: analytical observer checking whether the audit trail supports the claim and whether scope discipline holds downstream
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_mechanism_reading, 0.44).
domain_priors:suppression_score(behavioral_mechanism_reading, 0.31).
domain_priors:theater_ratio(behavioral_mechanism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_mechanism_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(behavioral_mechanism_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(behavioral_mechanism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(behavioral_mechanism_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(behavioral_mechanism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_mechanism_reading, tangled_rope).
narrative_ontology:human_readable(behavioral_mechanism_reading, "Persona-as-Steerability-Mechanism Reading of Simulated-User Evaluation").
narrative_ontology:topic_domain(behavioral_mechanism_reading, "AI evaluation infrastructure / simulated-user research methodology").

domain_priors:requires_active_enforcement(behavioral_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(behavioral_mechanism_reading, '7846093b-7a8a-44a8-8a37-8055d23eeb02').
narrative_ontology:cs_kernel_codification('7846093b-7a8a-44a8-8a37-8055d23eeb02', distributed).
narrative_ontology:cs_authority_grounding('7846093b-7a8a-44a8-8a37-8055d23eeb02', expertise).
narrative_ontology:cs_interpretation_layer_present('7846093b-7a8a-44a8-8a37-8055d23eeb02').
narrative_ontology:cs_reading_relation('7846093b-7a8a-44a8-8a37-8055d23eeb02', behavioral_mechanism_reading__instrumentalist_screening_reading, coexists_with).
narrative_ontology:cs_reading_relation('7846093b-7a8a-44a8-8a37-8055d23eeb02', behavioral_mechanism_reading__representational_correspondence_reading, influences).
narrative_ontology:cs_reading_relation('7846093b-7a8a-44a8-8a37-8055d23eeb02', behavioral_mechanism_reading__sociotechnical_risk_reading, coexists_with).
narrative_ontology:cs_axiom('7846093b-7a8a-44a8-8a37-8055d23eeb02', foundational, steerability_is_the_load_bearing_claim).
narrative_ontology:cs_axiom_status(steerability_is_the_load_bearing_claim, holdable).
narrative_ontology:cs_axiom_grounding('7846093b-7a8a-44a8-8a37-8055d23eeb02', steerability_is_the_load_bearing_claim, empirically_contingent).
narrative_ontology:cs_axiom('7846093b-7a8a-44a8-8a37-8055d23eeb02', foundational, no_ground_truth_claim_is_made_or_needed).
narrative_ontology:cs_axiom_status(no_ground_truth_claim_is_made_or_needed, holdable).
narrative_ontology:cs_axiom_grounding('7846093b-7a8a-44a8-8a37-8055d23eeb02', no_ground_truth_claim_is_made_or_needed, conventional).
narrative_ontology:cs_reference_frame('7846093b-7a8a-44a8-8a37-8055d23eeb02', correspondence_theory_validity_standard).
narrative_ontology:cs_drift_state('7846093b-7a8a-44a8-8a37-8055d23eeb02', post_steerability_audit_publication, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7846093b-7a8a-44a8-8a37-8055d23eeb02', '').
narrative_ontology:cs_kernel_id(behavioral_mechanism_reading, persona_as_valid_proxy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behavioral_mechanism_reading, eval_infrastructure_teams).
narrative_ontology:constraint_beneficiary(behavioral_mechanism_reading, model_vendors_citing_steerability_metrics).
narrative_ontology:constraint_beneficiary(behavioral_mechanism_reading, persona_conditioning_researchers).
narrative_ontology:constraint_victim(behavioral_mechanism_reading, downstream_product_decision_makers).
narrative_ontology:constraint_victim(behavioral_mechanism_reading, policy_readers_of_eval_reports).
narrative_ontology:constraint_vindicates(behavioral_mechanism_reading, persona_assignment_causally_shifts_output).
narrative_ontology:constraint_vindicates(behavioral_mechanism_reading, steerability_is_measurable_independent_of_ground_truth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and runs the controlled adherence probe, builds the cited-evidence audit trail (Table 17), and sets the standard by which persona-conditioning success is measured. Chooses to report steerability comparisons (Opus 91.5% vs GPT-5.6-sol 79.2%) as the headline finding, insulating the methodology from correspondence-theory objections by redefining what the persona claim is about.
narrative_ontology:constraint_stakeholder(behavioral_mechanism_reading, eval_infrastructure_teams, agenda_setter,
    institutional, biographical, arbitrage, national).

% Cites cross-model steerability numbers as evidence of controllability and alignment robustness in marketing and safety documentation. Benefits from a narrower, more defensible claim that cannot be falsified by disputes over whether personas represent real users, since the claim is about conditioning, not correspondence.
narrative_ontology:constraint_stakeholder(behavioral_mechanism_reading, model_vendors_citing_steerability_metrics, beneficiary,
    powerful, biographical, mobile, global).

% Builds career and publication capital on steerability and conditioning as a legitimate, well-scoped research object. The behavioral mechanism reading protects this research program from population-validity critiques by explicitly disclaiming any correspondence-to-real-users claim.
narrative_ontology:constraint_stakeholder(behavioral_mechanism_reading, persona_conditioning_researchers, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(behavioral_mechanism_reading, persona_conditioning_researchers, agenda_setter).

% Receives eval reports and steerability tables but is structurally positioned to misread 'persona shifts output in declared direction' as 'persona reliably predicts population behavior' unless the report's scope disclaimer is read carefully. Bears the cost when product decisions (paid-plan rate assumptions, purchase-intent forecasts) are made on findings this reading explicitly places outside its own scope.
narrative_ontology:constraint_stakeholder(behavioral_mechanism_reading, downstream_product_decision_makers, payer,
    moderate, immediate, constrained, national).

% Regulators, auditors, or internal governance reviewers who consume the same report and may treat headline adherence percentages as safety assurances about real-world deployment risk, when this reading's own boundary condition is that no claim about real users is being made.
narrative_ontology:constraint_stakeholder(behavioral_mechanism_reading, policy_readers_of_eval_reports, payer,
    moderate, biographical, constrained, national).

% The 0/5 cog-verbosity failure under GPT-5.6-sol and the 0/5 OS-App politeness suppression failure are the reading's own central findings — boundary cases where persona conditioning fails against alignment priors — yet they receive little visibility outside the methodology audience because the report's headline framing foregrounds success percentages, not failure boundaries.
narrative_ontology:constraint_stakeholder(behavioral_mechanism_reading, cog_verbosity_and_politeness_failure_cases, excluded,
    analytical, immediate, analytical, local).
narrative_ontology:stakeholder_non_agent(behavioral_mechanism_reading, cog_verbosity_and_politeness_failure_cases).

% Independent reviewers assessing whether the audit trail in Table 17 actually supports the steerability claim, and whether the reading's scope discipline (no ground-truth claim) is maintained in how the report is actually used downstream.
narrative_ontology:constraint_stakeholder(behavioral_mechanism_reading, methodology_auditors, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a narrower, more defensible evaluation claim — persona assignment causally and traceably shifts LLM output in a declared direction, legibly and reproducibly — that can be tested and audited without resolving the much harder question of whether personas correspond to real people. This lets model builders and researchers make falsifiable, auditable claims about conditioning without overclaiming population validity.
% TRANSFER_FUNCTION: Moves interpretive authority from the harder, contestable claim (persona validity as population proxy) to the narrower, defensible claim (persona as steerability mechanism), and moves attention and citation credit toward the audit-trail and cross-model comparison results (Table 17, Opus vs GPT-5.6-sol) while product-level outcomes are explicitly demoted to out-of-scope noise — a cost that lands on whoever downstream still needs an answer to the product question and receives this report instead.
% ABSENT_VOICES: End users whose simulated personas are used in the probes are not consulted about whether the mechanism-only framing changes how the results will be used; product teams who commissioned the study for population-level insight are structurally present as report recipients but not as co-authors of the reading's scope boundary, so their expectations and the reading's actual claim frequently diverge without correction.
% DISAPPEARANCE_RATIONALE: If this narrower behavioral-mechanism reading disappeared and only the population-proxy or screening-tool framings remained, the underlying steerability data (Table 17, cross-model percentages) would still exist, but the field would lose its most defensible way of talking about what persona conditioning demonstrates — forcing every citation of these results back into the correspondence-theory dispute this reading was built to sidestep. Methodology researchers say the world rearranges (the defensible claim vanishes and disputes return); product-facing consumers of the reports say the world stays unchanged (they were already misreading the results as population claims regardless of the reading's official scope).
% FOUNDING_PROBLEM: Simulated-user personas were being defended and attacked using validity criteria borrowed from survey sampling and psychometrics (does this persona correspond to a real population segment?), a standard the personas could not meet and were never designed to meet, which stalled methodological progress and invited blanket dismissal of persona-based evaluation.
% FOUNDING_PROBLEM_CORROBORATION: Independent methodology reviewers outside the eval infrastructure teams (cited in the omega on correspondence-vs-steerability conflation) corroborate that population-proxy critiques of persona evaluation remain unresolved and unresolvable on current evidence, which is precisely why a narrower, mechanism-only claim has argumentative value; however, no corroboration exists from the downstream product decision makers who are the ones bearing the cost of scope confusion — they were not asked whether the narrower reading actually reaches them intact.
narrative_ontology:disappearance_verdict(behavioral_mechanism_reading, contested).
narrative_ontology:founding_problem_status(behavioral_mechanism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(behavioral_mechanism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-10',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(behavioral_mechanism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(behavioral_mechanism_reading, 0.44, 'claude-sonnet-5', 'matraix_persona_simulation_2026_20260810_114056', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_mechanism_reading_tests).
:- end_tests(behavioral_mechanism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.44) and rising slowly over the interval: the reading itself is not extractive by design — it is a genuine methodological narrowing that makes weaker claims more defensible — but its downstream use as a substitute for population-level validity evidence (a use this reading explicitly disclaims) creates real cost for product decision-makers and policy readers who consume the same reports without absorbing the scope boundary. Suppression (0.31) is moderate: there is no coercive enforcement preventing people from reading the disclaimers, but the framing effect of headline steerability percentages (91.5% vs 79.2%) does functionally suppress attention to the scope boundary and to the failure-case findings (cog-verbosity, politeness suppression) that this reading's own methodology treats as central. Theater ratio (0.28, rising to 0.28 by interval end) captures a modest and growing gap between the rigor of the audit trail and the selective headline framing used when citing it externally.
 *
 * DIRECTIONALITY LOGIC:
 *   Eval infrastructure teams and persona conditioning researchers are structural beneficiaries: they get a defensible, citable, career-sustaining claim that survives correspondence-theory attacks by explicitly declining to make a correspondence claim. Model vendors benefit similarly by citing steerability numbers as controllability evidence. Downstream product decision-makers and policy readers are structural payers: they receive reports whose headline framing (cross-model percentages) invites exactly the population-level inference the reading's own text disclaims, and they bear the cost when decisions are made on that inference. The failure-case findings (cog-verbosity, politeness suppression) are marked as a non-agent excluded entity because they are the reading's own central evidence yet are structurally deprioritized in how results are surfaced to non-methodology audiences.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that persona evaluation was being judged by an unattainable correspondence standard — remains live: no resolution of the population-proxy validity question exists, so the narrower steerability claim continues to do real work. This blocks a mandatrophy reading in the strict sense (the mandate has not obviously outlived its function), but it does not resolve the scope-confusion cost this reading imposes when the narrower claim is packaged and cited in ways that let audiences infer the broader one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    steerability_claim_boundary_erosion,
    'Does the narrower steerability claim (persona shifts output legibly and reproducibly) stay narrow in practice, or does its citation in vendor and policy documents functionally collapse back into a population-validity claim the reading explicitly disclaims?',
    'Trace citations of the cross-model steerability comparison (Opus 91.5% vs GPT-5.6-sol 79.2%) in downstream product and policy documents; code each citation for whether it is used to support a mechanism claim (in-scope) or a population/real-user claim (out-of-scope, scope violation).',
    'If citations systematically drift toward population-level use, the reading''s disclaimed scope is not actually load-bearing in practice — the defensibility the reading buys is theoretical only, and the extractiveness score should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(steerability_claim_boundary_erosion, empirical, 'Whether the narrow mechanism claim''s scope boundary survives downstream citation practice.').

omega_variable(
    failure_case_visibility_asymmetry,
    'Are the cog-verbosity 0/5 and politeness-suppression 0/5 failure cases genuinely treated as central findings by report authors, or do they function as buried caveats beneath headline success percentages?',
    'Compare document structure and emphasis (page placement, executive summary inclusion, abstract mention) of the failure cases against the headline steerability percentages across multiple report versions or audiences.',
    'If failure cases are structurally marginalized despite being methodologically central, the theater_ratio understates the actual gap between the reading''s stated epistemics and its practiced communication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(failure_case_visibility_asymmetry, empirical, 'Whether stated centrality of failure-boundary findings matches actual report emphasis.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the choice to read persona-as-valid-proxy through the behavioral-mechanism lens (rather than the correspondence, screening, or sociotechnical-risk lens) itself a defensible methodological narrowing, or a motivated retreat to the most defensible claim available once the harder claims came under attack?',
    'Examine the historical sequence: was the mechanism framing adopted before or after correspondence-based critiques of persona validity gained traction? A framing adopted in direct response to critique, without independent methodological motivation, would support the motivated-retreat reading.',
    'If the framing was adopted reactively rather than independently motivated, the reading''s claim to represent a principled methodological advance (rather than a defensive narrowing) weakens, though the underlying audit-trail evidence remains valid on its own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the mechanism-reading''s adoption was independently motivated or a defensive response to correspondence-validity critique.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_mechanism_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beha_tr_t0, behavioral_mechanism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(beha_tr_t4, behavioral_mechanism_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(beha_tr_t8, behavioral_mechanism_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(beha_tr_t12, behavioral_mechanism_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(beha_tr_t16, behavioral_mechanism_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(beha_tr_t20, behavioral_mechanism_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(beha_tr_t24, behavioral_mechanism_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(beha_be_t0, behavioral_mechanism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(beha_be_t4, behavioral_mechanism_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(beha_be_t8, behavioral_mechanism_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(beha_be_t12, behavioral_mechanism_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(beha_be_t16, behavioral_mechanism_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(beha_be_t20, behavioral_mechanism_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(beha_be_t24, behavioral_mechanism_reading, base_extractiveness, 24, 0.44).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(behavioral_mechanism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_mechanism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(behavioral_mechanism_reading, 0.05).
narrative_ontology:affects_constraint(behavioral_mechanism_reading, instrumentalist_screening_reading).
narrative_ontology:affects_constraint(behavioral_mechanism_reading, representational_correspondence_reading).
narrative_ontology:affects_constraint(behavioral_mechanism_reading, sociotechnical_risk_reading).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the kernel persona_as_valid_proxy. behavioral_mechanism_reading claims the narrowest, most defensible scope (causal steerability only, no ground-truth or population claim). representational_correspondence_reading claims the broadest scope (personas approximate real population segments) and would carry substantially higher epsilon under this reading's own lights, since correspondence claims invite direct falsification by population data this reading never attempts. instrumentalist_screening_reading and sociotechnical_risk_reading occupy intermediate positions. Each reading is authored as its own constraint with its own epsilon per the epsilon-invariance principle; they share only the underlying persona-assignment kernel and the empirical steerability data (Table 17, cross-model comparison), not a classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
