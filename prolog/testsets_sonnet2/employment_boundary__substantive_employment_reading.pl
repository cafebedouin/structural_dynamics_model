% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__substantive_employment_reading, []).

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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Reading of the Platform Work Boundary
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the substantive-employment reading of the
 *   employment boundary kernel as applied to platform-mediated gig work: the
 *   reading holds that algorithmic dispatch, rating-based discipline, and
 *   economic dependence on a platform constitute the same functional
 *   supervision and dependence that employment law was built to protect,
 *   regardless of the independent-contractor label in the platform's terms of
 *   service. Under this reading, platform operators are the structural
 *   beneficiaries of the current misclassification (avoiding payroll tax,
 *   insurance, and bargaining obligations) and platform workers are the
 *   victims of the resulting precarity. The reading is contested: two sibling
 *   readings — a formalist reading holding platform workers are genuinely
 *   independent contractors, and a hybrid reading proposing a third legal
 *   category — describe structurally different constraints with different
 *   beneficiary/victim sets and are authored as separate stories, linked via
 *   network.affects_constraints. Extraction here is moderate (0.58) rather
 *   than severe because platforms actively resist reclassification through
 *   litigation, lobbying, and ballot initiatives, keeping the arrangement
 *   contested rather than settled either way.
 *
 * KEY AGENTS:
 *   - platform_operators: primary beneficiary/agenda_setter (institutional/arbitrage) — retains cost savings from misclassification, resists reclassification
 *   - platform_workers: primary target (powerless/constrained) — bears cost of denied protections; entitled to employee status under this reading
 *   - state_labor_regulators: agenda_setter (institutional/analytical) — administers the substantive test that would reclassify workers
 *   - gig_economy_consumers: secondary beneficiary (organized/mobile) — benefits from platform pricing subsidized by non-employee status
 *   - traditional_employers_in_competing_sectors: excluded competitor — bears full employer costs while competing against misclassified platforms
 *   - gig_worker_advocacy_coalitions: excluded advocate — represents worker interests without formal standing in most proceedings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.58).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.62).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Reading of the Platform Work Boundary").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '792fd7c5-27d0-4a9a-b0e1-06e44b896cc6').
narrative_ontology:cs_kernel_codification('792fd7c5-27d0-4a9a-b0e1-06e44b896cc6', distributed).
narrative_ontology:cs_authority_grounding('792fd7c5-27d0-4a9a-b0e1-06e44b896cc6', distributed).
narrative_ontology:cs_reading_relation('792fd7c5-27d0-4a9a-b0e1-06e44b896cc6', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('792fd7c5-27d0-4a9a-b0e1-06e44b896cc6', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('792fd7c5-27d0-4a9a-b0e1-06e44b896cc6', foundational, economic_substance_over_contract_form).
narrative_ontology:cs_axiom_status(economic_substance_over_contract_form, holdable).
narrative_ontology:cs_axiom_grounding('792fd7c5-27d0-4a9a-b0e1-06e44b896cc6', economic_substance_over_contract_form, conventional).
narrative_ontology:cs_axiom('792fd7c5-27d0-4a9a-b0e1-06e44b896cc6', foundational, algorithmic_direction_constitutes_control).
narrative_ontology:cs_axiom_status(algorithmic_direction_constitutes_control, holdable).
narrative_ontology:cs_axiom_grounding('792fd7c5-27d0-4a9a-b0e1-06e44b896cc6', algorithmic_direction_constitutes_control, empirically_contingent).
narrative_ontology:cs_reference_frame('792fd7c5-27d0-4a9a-b0e1-06e44b896cc6', industrial_era_direct_supervision_test).
narrative_ontology:cs_drift_state('792fd7c5-27d0-4a9a-b0e1-06e44b896cc6', platform_dispatch_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('792fd7c5-27d0-4a9a-b0e1-06e44b896cc6', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_operators).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, gig_economy_consumers).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, gig_worker_advocacy_coalitions).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, economic_reality_test_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, algorithmic_control_as_supervision_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Structure work through algorithmic dispatch, rating systems, and dynamic pricing that functionally direct how, when, and at what rate workers perform tasks, while classifying workers as independent contractors to avoid payroll tax, minimum wage floors, unemployment insurance contributions, and collective bargaining obligations. Under this reading they are the obligated party: the classification they have chosen is read as legal form contradicting economic substance, and they become liable for back-pay, benefits, and insurance contributions once reclassified.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, platform_operators, agenda_setter).

% Depend on a single or small number of platforms for most or all income, cannot set their own rates, cannot decline dispatched work without algorithmic penalty, and bear the full cost of vehicles, insurance, and downtime themselves. Under the formal contract they have none of the protections of employees; under this reading their situation IS the employment relationship the law is meant to reach, entitling them to minimum wage, overtime, unemployment insurance, and collective bargaining rights they currently lack.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, payer,
    powerless, biographical, constrained, national).

% Administer misclassification enforcement, audit platform practices, and adjudicate worker classification disputes. Under this reading they are obligated to apply an economic-realities or ABC-style test that looks past contract labels to actual control and dependence, and to pursue back-tax and penalty assessments against platforms found to misclassify.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, state_labor_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Receive low-cost, on-demand delivery and transport services subsidized in part by the absence of employer payroll obligations. If this reading prevails and platforms pass reclassification costs to prices, consumers face higher prices or reduced service availability; they have no voice in the classification dispute but experience its downstream price effects.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, gig_economy_consumers, beneficiary,
    organized, biographical, mobile, national).

% Compete against platforms in adjacent labor markets (traditional taxi companies, retail delivery, staffing agencies) while bearing full employer costs. They have argued for reclassification as a matter of competitive fairness but are not parties to the classification proceedings that would resolve it in their favor.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, traditional_employers_in_competing_sectors, excluded,
    organized, generational, constrained, national).

% Organize platform workers to press for reclassification and file test-case litigation, but lack direct standing in most regulatory proceedings and face platform-funded ballot initiatives and lobbying that outspend them substantially.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, gig_worker_advocacy_coalitions, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, gig_worker_advocacy_coalitions, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__substantive_employment_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__substantive_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal test that sorts working arrangements into employment or non-employment by looking at economic dependence and actual behavioral control rather than the label the parties chose, so that protections attach to the substance of the relationship rather than to contract drafting.
% TRANSFER_FUNCTION: Under this reading, the arrangement would move payroll tax contributions, minimum wage and overtime pay, unemployment insurance premiums, and collective bargaining leverage from platforms (who currently retain them as cost savings) to workers (who currently absorb the corresponding risk and cost individually).
% ABSENT_VOICES: Individual platform workers rarely appear as named parties in the classification disputes that determine their status; advocacy coalitions speak for them but are structurally outmatched in lobbying and ballot-initiative spending by platform operators who have direct financial stake in the formalist alternative.
% DISAPPEARANCE_RATIONALE: If the substantive-employment reading were adopted and enforced, platforms would owe payroll taxes, minimum wage guarantees, unemployment insurance, and would face collective bargaining obligations across their workforce — a restructuring of platform economics, pricing, and possibly service availability. If instead abandoned, workers revert to the unprotected status quo. Either direction materially rearranges the arrangements built on the current classification.
% FOUNDING_PROBLEM: Wage-and-hour and social-insurance law was built on the premise that a worker's protections should track economic reality (dependence on an employer for livelihood, subjection to that employer's direction) rather than the label a contract uses, precisely because contract labels can be drafted to evade the protections the law intends.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying algorithmic management systems (outside both platforms and worker advocacy groups) have documented that dispatch algorithms, rating thresholds, and deactivation policies function as directive control comparable to traditional supervision; several state and national courts and regulators, applying long-standing economic-realities tests independently of this specific dispute, have reached findings consistent with the substantive reading. Platforms themselves do not corroborate the founding problem's continued relevance to their arrangements.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__substantive_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate, not severe) because under this reading the withheld protections (minimum wage, overtime, unemployment insurance, bargaining rights) represent a real and substantial transfer, but the reading itself is actively contested — platforms have not yet succeeded in permanently foreclosing reclassification, so the extraction is a live transfer under dispute rather than a settled, uncontested one. Suppression (0.62) reflects the active enforcement machinery platforms deploy: arbitration clauses foreclosing class action, ballot initiatives (e.g., worker-classification carve-outs), and lobbying that raises the practical cost of reclassification litigation for individual workers. Accessibility collapse is moderate (0.40) because alternative classifications remain legally live and contested in courts and legislatures — the boundary has not fully hardened in either direction. Resistance is high (0.70) because worker advocacy coalitions, some regulators, and academic economists actively contest the formalist status quo, which is exactly the coordination/extraction tension a tangled_rope structure requires.
 *
 * PERSPECTIVAL GAP:
 *   From the platform_operators' seat, the current contractor classification is read as efficient labor-market matching enabling flexible work; from the platform_workers' seat, the identical dispatch-and-rating architecture is experienced as direction and control indistinguishable from employment supervision, absent employment's protections. The engine computes these as structurally different seat experiences from the same underlying algorithmic control data — this divergence is the substantive-employment reading's entire analytical claim, not a reconciliation problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are declared beneficiaries because the classification choice directly reduces their labor costs (no payroll tax, no insurance contributions, no bargaining exposure) — this drives d toward the beneficiary end. Platform workers are declared victims because they bear the substantive economic dependence and behavioral control the reading identifies as employment, without receiving employment's protections — this drives d toward the full-target end, amplified by their powerless/constrained structural position. State regulators sit in an agenda_setter role with analytical exit — their directionality is not extraction-driven but administratively determined by which test they apply.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is warranted because a genuine coordination function persists under this reading: platforms do solve a real matching problem (connecting intermittent labor supply with intermittent demand) that neither pure formalism nor pure abolition of the platform model would preserve as efficiently. The extraction is asymmetric and requires active enforcement (arbitration clauses, ballot initiatives, litigation) to persist — exactly the tangled_rope signature, distinguishing it from a pure snare where no coordination value exists, and from a pure rope where costs and benefits would be roughly symmetric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_control_as_supervision_ambiguity,
    'Does algorithmic dispatch, rating-based deactivation, and dynamic pricing constitute ''control'' in the legal sense that traditional direct supervision does, or is it a structurally different form of coordination that existing employment tests were not built to evaluate?',
    'Comparative doctrinal analysis of how courts and regulators across jurisdictions apply economic-realities and ABC tests to algorithmic management specifically, plus empirical study of whether algorithmic control produces the same dependency and risk-shifting outcomes as direct human supervision.',
    'If algorithmic control is functionally equivalent to supervision, the substantive reading is on strong doctrinal ground and extraction under the current classification is more clearly unjustified. If algorithmic coordination is genuinely distinct, the formalist or hybrid readings gain ground and this reading''s victim classification of platform workers weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_as_supervision_ambiguity, conceptual, 'Whether algorithmic dispatch/rating systems are legally equivalent to direct supervision.').

omega_variable(
    kernel_reading_contest_location,
    'Where exactly does the disagreement between the three employment_boundary readings live — in the facts of platform control (empirical), in what counts as ''employment'' (definitional/legal), or in what protections workers ought to have regardless of label (normative/policy)?',
    'This is the committer-structure ambiguity for the kernel as a whole: the formalist reading disputes the facts of control; the hybrid reading accepts substantive dependence but disputes that binary employment categories are the right instrument; this reading asserts both the facts and the binary-category framing. Resolution requires separating factual findings (does the algorithm functionally supervise?) from category-design choices (should there be a third category?) across all three sibling stories.',
    'If the disagreement is primarily factual, courts applying existing tests should converge toward this reading as evidence accumulates. If primarily about category design, no amount of factual resolution settles it — the hybrid reading''s third-category proposal would remain a live alternative regardless of what the algorithmic-control facts show.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Locating whether the kernel contest is empirical (facts of control) or definitional (category design).').

omega_variable(
    platform_business_model_dependency,
    'Would platform operators'' current pricing and matching model survive full reclassification costs, or does the model structurally depend on non-employee classification such that reclassification would force a fundamentally different service (higher prices, reduced worker count, geographic contraction)?',
    'Economic modeling using jurisdictions that have already implemented reclassification (e.g., specific state-level tests) to observe actual platform pricing, worker-count, and service-availability responses post-reclassification.',
    'If the model survives with modest price increases, the beneficiary classification of platform_operators as resisting a sustainable obligation is strongly supported. If the model requires fundamental restructuring, the coordination function this story attributes to platforms may be more fragile than assumed, affecting the tangled_rope vs. snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_business_model_dependency, empirical, 'Whether platform business models can absorb reclassification costs or depend structurally on non-employee status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(empl_tr_t4, employment_boundary__substantive_employment_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__substantive_employment_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__substantive_employment_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__substantive_employment_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__substantive_employment_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(empl_be_t4, employment_boundary__substantive_employment_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(empl_be_t8, employment_boundary__substantive_employment_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(empl_be_t12, employment_boundary__substantive_employment_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(empl_be_t16, employment_boundary__substantive_employment_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(empl_be_t20, employment_boundary__substantive_employment_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(empl_su_t4, employment_boundary__substantive_employment_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(empl_su_t8, employment_boundary__substantive_employment_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(empl_su_t12, employment_boundary__substantive_employment_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(empl_su_t16, employment_boundary__substantive_employment_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(empl_su_t20, employment_boundary__substantive_employment_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, resource_allocation).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, employment_boundary__hybrid_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'gig worker classification' question, each a distinct reading of the employment_boundary kernel. formalist_employment_reading claims platform workers are genuine independent contractors (near-zero ε under its own premises — no protections are being wrongly withheld if the formal contract accurately describes the relationship). hybrid_security_reading proposes a third category with tailored protections, producing a different beneficiary/victim structure again (platforms owe partial obligations, workers gain some but not full employment protections). This story (substantive_employment_reading) claims full employee status applies and authors moderate-to-substantial ε reflecting the withheld protections plus active platform resistance. All three share the same underlying facts about platform labor practices but diverge in which legal test resolves those facts into employment status — per the ε-invariance principle, this warrants three separate constraint stories rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
