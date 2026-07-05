% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Substantive Employment Reading of the Employment Boundary (Economic Dependence / Algorithmic Control Test)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the substantive-employment reading of the
 *   contested employment boundary kernel: employment status is determined by
 *   economic dependence and the degree of algorithmic control exercised over
 *   a worker, not by the formal label the contract uses. Under this reading,
 *   platform workers subject to algorithmic dispatch, rating-based
 *   discipline, and deactivation are employees as a matter of substance, and
 *   platforms that have structured around contractor status become obligated
 *   to provide the full employment protection package — minimum wage,
 *   unemployment insurance, workers' compensation, collective bargaining
 *   access. This is a distinct constraint from the formalist reading (which
 *   asks about contract form and direct supervision) and the hybrid reading
 *   (which rejects the binary entirely in favor of a third category) — each
 *   of those is a separate story with its own beneficiary/victim structure
 *   and its own epsilon, linked here only by shared kernel membership, per
 *   the epsilon-invariance principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.58).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.52).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Reading of the Employment Boundary (Economic Dependence / Algorithmic Control Test)").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '8f90edff-818a-49a2-a8d3-4023d32c5083').
narrative_ontology:cs_kernel_codification('8f90edff-818a-49a2-a8d3-4023d32c5083', distributed).
narrative_ontology:cs_authority_grounding('8f90edff-818a-49a2-a8d3-4023d32c5083', distributed).
narrative_ontology:cs_reading_relation('8f90edff-818a-49a2-a8d3-4023d32c5083', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('8f90edff-818a-49a2-a8d3-4023d32c5083', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('8f90edff-818a-49a2-a8d3-4023d32c5083', foundational, functional_control_determines_status).
narrative_ontology:cs_axiom_status(functional_control_determines_status, holdable).
narrative_ontology:cs_axiom_grounding('8f90edff-818a-49a2-a8d3-4023d32c5083', functional_control_determines_status, empirically_contingent).
narrative_ontology:cs_axiom('8f90edff-818a-49a2-a8d3-4023d32c5083', secondary, contract_labeling_cannot_override_substantive_relationship).
narrative_ontology:cs_axiom_status(contract_labeling_cannot_override_substantive_relationship, holdable).
narrative_ontology:cs_axiom_grounding('8f90edff-818a-49a2-a8d3-4023d32c5083', contract_labeling_cannot_override_substantive_relationship, conventional).
narrative_ontology:cs_reference_frame('8f90edff-818a-49a2-a8d3-4023d32c5083', common_law_economic_reality_test).
narrative_ontology:cs_drift_state('8f90edff-818a-49a2-a8d3-4023d32c5083', platform_economy_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8f90edff-818a-49a2-a8d3-4023d32c5083', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers_reclassified).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, social_insurance_funds).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, traditional_employers_competing_with_platforms).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_workers_precarious).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, gig_platform_operators).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, control_test_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, economic_reality_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work through app-dispatched tasks under algorithmic scheduling, rating, and deactivation systems that function as direct supervision, but are classified as independent contractors. Under the substantive reading, they gain employee status and benefits, but many also face reduced platform hours, loss of the flexibility they valued, and algorithmic gatekeeping that intensifies once platforms restructure to control litigation exposure. They bear the transition costs of reclassification even as they are its intended beneficiaries.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers_precarious, payer,
    powerless, biographical, trapped, national).

% Workers who successfully obtain employee status under this reading gain minimum wage floors, unemployment insurance, workers' compensation, and collective bargaining rights. They are represented in litigation and legislative campaigns by worker organizing coalitions. Their gains are real but contingent on continued enforcement and on platforms not simply exiting the jurisdiction or automating the roles away.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers_reclassified, beneficiary,
    organized, biographical, constrained, national).

% Built business models on treating workers as independent contractors, avoiding payroll tax, benefits, and minimum wage obligations. Under this reading they must reclassify, pay backpay and penalties, and restructure algorithmic management (which is itself cited as evidence of the control the reading relies on). They lobby for the hybrid or formalist reading instead, threaten market exit or reduced worker hours, and have resources to litigate reclassification for years.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, gig_platform_operators, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, gig_platform_operators, agenda_setter).

% Unemployment insurance and workers' compensation systems have been excluded from platform-worker payroll contributions under contractor classification, creating funding gaps as displaced workers still draw on public safety nets when platforms deactivate them. Reclassification under this reading brings platform payroll into the contribution base.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, social_insurance_funds, beneficiary,
    institutional, generational, analytical, national).

% Taxi companies, delivery firms, and other incumbents who classify their workers as employees compete against platforms that avoid the same costs by using contractor classification. This reading levels the competitive field by imposing equivalent labor costs on platforms.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, traditional_employers_competing_with_platforms, beneficiary,
    powerful, biographical, constrained, national).

% Departments of labor and courts apply the economic-reality and control tests to determine employment status. They administer enforcement, adjudicate disputes, and can be the target of platform lobbying to adopt the alternative readings instead.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Pay for rides, deliveries, and tasks priced partly on the assumption of low labor costs. They are not party to the classification dispute but would face price increases if platforms pass reclassification costs through, and are not consulted in the legal proceedings that determine the outcome.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, consumers_of_platform_services, excluded,
    moderate, immediate, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__substantive_employment_reading, diffuse).
narrative_ontology:fixing_cost_class(employment_boundary__substantive_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal test — economic dependence plus algorithmic control — that determines which workers receive the baseline social insurance and labor protections built for the employment relationship, preventing employers from opting out of that system merely by relabeling the contract.
% TRANSFER_FUNCTION: Moves the cost of social insurance, minimum wage floors, and employment protections from public assistance systems and from the workers themselves back onto the platforms whose algorithmic management functions as direction and control, and secondarily from platforms to competing traditional employers who already bear these costs.
% ABSENT_VOICES: Consumers who would face price changes are not represented in the litigation or rulemaking. Workers who prefer contractor flexibility over employee status and would be reclassified against their preference are underrepresented relative to worker-advocacy coalitions that favor reclassification.
% DISAPPEARANCE_RATIONALE: If this reading's control test disappeared and only the formalist contract-form reading governed, millions of platform workers currently mid-transition to employee status would revert to contractor status, unemployment and workers' comp funding gaps would reopen, and platforms would face no legal pressure to alter algorithmic management practices currently cited as evidence of control.
% FOUNDING_PROBLEM: Platforms structured contracts to avoid the legal indicia (direct supervision, fixed schedules) that traditionally triggered employee status, while still exercising comparable control through algorithmic dispatch, ratings, and deactivation — leaving workers economically dependent on a single employer-like entity without any of the protections that dependence traditionally required.
% FOUNDING_PROBLEM_CORROBORATION: Independent labor economists and multiple national courts (e.g., rulings finding algorithmic management functionally equivalent to direct supervision) attest the substantive dependence persists regardless of contract labeling; this corroboration comes from judicial and academic sources outside the worker-advocacy coalitions that are direct beneficiaries of the reading.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at a moderate 0.58, rising over the interval as regulatory enforcement of the control test intensifies and platforms increasingly resist through litigation, restructuring, and threatened market exit — this is the cost imposed on platform operators who built margin structures around contractor classification. Suppression is moderate (0.52): platforms retain real exit options (relocating operations, automating roles, exiting jurisdictions) so the constraint is not fully coercive, but enforcement backed by courts and labor departments constrains those options over time. Accessibility collapse is moderate-low (0.4) because alternative classifications (formalist, hybrid) remain live legal possibilities being actively litigated in parallel jurisdictions — this reading has not achieved uncontested dominance. Resistance is high (0.72), reflecting sustained, well-resourced platform-operator opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the labor regulator seat, this reading closes an evasion loophole and restores employment law's substantive purpose. From the platform operator seat, the same control test recharacterizes routine dispatch software as supervisory direction, imposing employer-scale costs on a business model built around their absence. The engine computes these as structurally different experiences of the identical control test from the beneficiary/victim and power/exit declarations, not from any claim this story makes about which view is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform workers split across two stakeholder entries because the reclassification transition itself produces winners and losers within the same population: workers who obtain employee status become beneficiaries of new protections (low-to-moderate d), while workers experiencing the destabilizing transition — reduced hours, algorithmic gatekeeping intensification as platforms manage legal exposure, uncertainty during litigation — sit closer to the target end (higher d) despite formally belonging to the group the constraint is designed to protect. Platform operators are the clearest targets: they bear the reclassification costs directly and are named as payer with organized agenda-setting power to resist. Traditional employers and social insurance funds are beneficiaries through competitive leveling and funding-base restoration respectively, with no operational role in enforcing the constraint themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — platforms using contract labeling to escape protections triggered by functional control — remains live and independently corroborated by courts and labor economists outside the worker-advocacy coalitions that benefit from the reclassification. This blocks a mandatrophy read: the arrangement is not a vestigial mandate persisting past its function, because the substantive dependence the reading targets continues to be documented as present and growing (algorithmic management sophistication increasing over the measured interval alongside rising extractiveness).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_control_equivalence_to_direct_supervision,
    'Is algorithmic dispatch, rating-based discipline, and deactivation authority functionally equivalent to the direct supervision that traditionally triggered employee status, or is it a structurally different form of coordination that only superficially resembles supervision?',
    'Comparative analysis of worker autonomy under algorithmic management versus traditional supervisory regimes: measure actual discretion over work acceptance, scheduling, and method, controlling for the platform''s stated flexibility claims.',
    'If algorithmic control is found functionally equivalent, this reading''s core premise is strongly supported and reclassification is well-grounded; if the mechanisms are structurally distinct, the hybrid_security_reading''s argument for a genuinely new category gains ground and this reading''s foreclosure of the formalist premise weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_equivalence_to_direct_supervision, empirical, 'Whether algorithmic control is legally and functionally equivalent to traditional direct supervision.').

omega_variable(
    worker_preference_heterogeneity,
    'Do platform workers, as a class, actually prefer full employee status over contractor flexibility, or does preference vary enough that a uniform reclassification imposes a status some workers would not choose?',
    'Large-sample surveys of platform workers disaggregated by hours worked, dependency on platform income, and stated preference for schedule flexibility versus benefits.',
    'If preferences are genuinely heterogeneous, forcing uniform employee status may itself be a cost imposed on a subset of workers who valued the contractor arrangement, complicating the clean beneficiary framing and strengthening the hybrid_security_reading as a better fit for that heterogeneity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_preference_heterogeneity, empirical, 'Whether platform workers uniformly benefit from reclassification or preferences diverge by worker segment.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the employment boundary kernel itself under-determined such that no single reading (formalist, substantive, or hybrid) is uniquely correct, or does the substantive/economic-reality test represent the historically dominant common-law standard that the formalist reading actually departs from?',
    'Doctrinal history review: trace whether economic-reality/control tests predate and are more continuous with historical employment law than the narrower formalist contract-form test, or whether formalism is the older baseline this reading revises.',
    'If the substantive reading is the historically dominant baseline, this reading''s claim to restore rather than expand employment law is stronger, affecting how courts and legislators weigh it against the sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the substantive/control-test reading is a restoration of historical doctrine or a novel expansion, relative to its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(empl_tr_t4, employment_boundary__substantive_employment_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__substantive_employment_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__substantive_employment_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__substantive_employment_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__substantive_employment_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(empl_tr_t24, employment_boundary__substantive_employment_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(empl_be_t4, employment_boundary__substantive_employment_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(empl_be_t8, employment_boundary__substantive_employment_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(empl_be_t12, employment_boundary__substantive_employment_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(empl_be_t16, employment_boundary__substantive_employment_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(empl_be_t20, employment_boundary__substantive_employment_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(empl_be_t24, employment_boundary__substantive_employment_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(empl_su_t4, employment_boundary__substantive_employment_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(empl_su_t8, employment_boundary__substantive_employment_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(empl_su_t12, employment_boundary__substantive_employment_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(empl_su_t16, employment_boundary__substantive_employment_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(empl_su_t20, employment_boundary__substantive_employment_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(empl_su_t24, employment_boundary__substantive_employment_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'the employment status of platform workers' (the employment_boundary kernel). formalist_employment_reading covers the contract-form/direct-supervision test with a different beneficiary/victim structure (platforms as beneficiaries, workers as victims of exclusion from protection). hybrid_security_reading covers a proposed third legal category with its own tailored, lesser protection set. Each reading has a distinct epsilon and distinct stakeholder declarations; they are linked here via affects_constraints and via cs_structure.reading_relations, not merged into a single averaged constraint, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__substantive_employment_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
