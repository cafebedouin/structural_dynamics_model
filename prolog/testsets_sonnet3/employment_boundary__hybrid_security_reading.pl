% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Third-Category Platform Worker Status (Hybrid Security Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the 'hybrid security' reading of the employment
 *   boundary kernel: platform workers are neither traditional employees nor
 *   pure independent contractors but a legislatively created third category
 *   with tailored, partial protections. Under this reading, the arrangement
 *   is claimed as tangled_rope — it has a real coordination function
 *   (delivering baseline medical and injury coverage to workers who
 *   previously had none, and legal certainty to platforms who previously
 *   faced protracted classification litigation) coupled with genuine
 *   asymmetric extraction (workers permanently forfeit full employment
 *   protections — retirement security, unemployment insurance, overtime,
 *   bargaining rights — in exchange for a thinner package platforms can fund
 *   at lower cost than full employment). The reading does not describe the
 *   formalist reading's claim (contractors, no obligations) or the
 *   substantive reading's claim (full employees); it describes a specific
 *   institutionalized compromise with its own beneficiary and victim
 *   structure.
 *
 * KEY AGENTS:
 *   - platform_operators: primary beneficiary and agenda-setter — funds minimum protections, retains algorithmic control, avoids full employer costs
 *   - platform_gig_workers: primary target — receives partial coverage but forfeits durable employment protections; classification is assigned regardless of economic dependence
 *   - policy_intermediary_agencies: secondary beneficiary — institutional interest in administering the category persists independent of worker outcomes
 *   - traditional_employers and labor_unions: excluded from the statute's core design bargain despite having stakes
 *   - labor_economists: analytical observer tracking longitudinal coverage and outcome data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.52).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.48).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Third-Category Platform Worker Status (Hybrid Security Reading)").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, 'c86a6d46-999f-43a7-ae57-a57c9e3c9f96').
narrative_ontology:cs_kernel_codification('c86a6d46-999f-43a7-ae57-a57c9e3c9f96', distributed).
narrative_ontology:cs_authority_grounding('c86a6d46-999f-43a7-ae57-a57c9e3c9f96', distributed).
narrative_ontology:cs_reading_relation('c86a6d46-999f-43a7-ae57-a57c9e3c9f96', employment_boundary__formalist_employment_reading, influences).
narrative_ontology:cs_reading_relation('c86a6d46-999f-43a7-ae57-a57c9e3c9f96', employment_boundary__substantive_employment_reading, influences).
narrative_ontology:cs_axiom('c86a6d46-999f-43a7-ae57-a57c9e3c9f96', foundational, employment_status_requires_tailored_third_category).
narrative_ontology:cs_axiom_status(employment_status_requires_tailored_third_category, holdable).
narrative_ontology:cs_axiom_grounding('c86a6d46-999f-43a7-ae57-a57c9e3c9f96', employment_status_requires_tailored_third_category, instrumental).
narrative_ontology:cs_axiom('c86a6d46-999f-43a7-ae57-a57c9e3c9f96', secondary, partial_protection_with_platform_flexibility_preferable_to_binary_reclassification).
narrative_ontology:cs_axiom_status(partial_protection_with_platform_flexibility_preferable_to_binary_reclassification, holdable).
narrative_ontology:cs_axiom_grounding('c86a6d46-999f-43a7-ae57-a57c9e3c9f96', partial_protection_with_platform_flexibility_preferable_to_binary_reclassification, conventional).
narrative_ontology:cs_reference_frame('c86a6d46-999f-43a7-ae57-a57c9e3c9f96', binary_employee_contractor_framework_inadequate).
narrative_ontology:cs_drift_state('c86a6d46-999f-43a7-ae57-a57c9e3c9f96', post_statutory_enactment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c86a6d46-999f-43a7-ae57-a57c9e3c9f96', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, policy_intermediary_agencies).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_gig_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_gig_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lobbies for and helps design the third-category statute, funds the injury-insurance and medical-subsidy schemes that satisfy the hybrid status's minimum protections, and in exchange is exempted from employer obligations like minimum wage floors, overtime, unemployment insurance, and collective bargaining duties. Retains full algorithmic control over work allocation and deactivation while avoiding the cost structure of employment.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_operators, beneficiary).

% Receives medical benefit coverage (91.5% enrollment) and injury insurance (86.2% coverage) under the new hybrid category, which is real and better than having nothing. But permanently loses access to retirement contributions, seniority-based advancement, unemployment insurance, collective bargaining rights, and protection from unilateral algorithmic rate changes. Cannot exit the classification by working harder or longer; the category is assigned by statute regardless of hours worked or economic dependence, and mobility between platforms does not change status.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_gig_workers, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_gig_workers, beneficiary).

% Administers the new benefit funds and certification schemes created by the hybrid statute, drawing budget and staffing from managing the third category. Has an institutional interest in the category's continuation independent of whether it actually serves workers, since the agency's mandate and funding are tied to administering it rather than to worker outcomes.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, policy_intermediary_agencies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, policy_intermediary_agencies, agenda_setter).

% Compete in labor-adjacent markets against platforms that now bear a lower compliance cost than a full employer while gaining more predictable classification certainty than they would under a pure contractor fight. Their objection — that the hybrid category creates an uneven playing field advantaging platform business models — is raised in comment periods but has limited influence on the statute's design.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employers, excluded,
    powerful, biographical, trapped, national).

% Argue the hybrid category was constructed specifically to foreclose the substantive-employment claim before it could be litigated or organized around, trading full labor rights for a thinner package platforms could unilaterally fund. Retains some voice through legislative testimony but was not a co-author of the statute's core bargain.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Studies enrollment and coverage data, compares outcomes for hybrid-category workers against both W-2 employees and pure contractors, and publishes findings on whether the third category narrows or widens the security gap over time.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally stable middle classification so platforms are not locked into an unresolved and costly employee/contractor fight, while giving workers some baseline protections (medical, injury insurance) they previously had none of as classified contractors.
% TRANSFER_FUNCTION: Moves a reduced package of benefit funding from platforms to workers (medical and injury coverage) in exchange for workers permanently forfeiting claims to full employment protections — minimum wage floors, overtime, unemployment insurance, retirement contributions, and collective bargaining rights they would receive under a substantive-employment finding.
% ABSENT_VOICES: Labor unions and worker advocacy groups that pushed for substantive-employment reclassification were largely outmaneuvered in the legislative drafting process; traditional employers who compete against platforms under a lighter compliance regime raised competitive-fairness objections that were not substantially addressed in the final statute.
% DISAPPEARANCE_RATIONALE: Platforms and the administering agency would say the medical and injury coverage infrastructure would collapse, leaving workers with nothing; labor advocates would say its disappearance would simply force the underlying employment-status question back into courts and legislatures, where a substantive-employment finding might deliver workers a larger and more durable protection package than the hybrid category currently does.
% FOUNDING_PROBLEM: Platform workers under pure independent-contractor classification had zero baseline protections — no injury coverage, no medical subsidy, no minimum earnings floor — while litigation over whether they were really employees dragged on for years with uncertain and inconsistent outcomes across jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and the administering agency attest the hybrid category solved the coverage gap and cite enrollment figures as proof of function. Independent labor economists and union-affiliated researchers, outside the benefiting parties, attest the founding problem (total absence of protection) has been only partially solved and that the category's design appears calibrated to foreclose litigation risk for platforms rather than to maximize worker security — no fully independent source corroborates the platforms' framing that this is the durable, correct solution rather than an interim settlement favoring one side.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, contested).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52 at interval end, rising from 0.38) because the hybrid category delivers real, measurable protections (91.5% medical enrollment, 86.2% injury coverage) that a pure-extraction reading of the same facts would not produce — this is not a snare. But extraction is not low, because the category's design forecloses workers' access to protections a substantive-employment finding would deliver, and that forfeiture is structural and durable, not incidental. Theater ratio rises over the interval (0.25 to 0.44) as the administering agency's public communications increasingly emphasize the coverage percentages as proof of adequacy while the gap in retirement/unemployment/bargaining protection remains unaddressed — a classic proxy-metric substitution pattern. Suppression rises moderately (0.35 to 0.48) as litigation avenues for reclassification narrow once the statute is in force and precedent accumulates around the third category as settled law.
 *
 * PERSPECTIVAL GAP:
 *   From the platform operator seat, this looks like successful, cost-effective coordination: a durable, litigation-resistant framework that delivers meaningful worker benefits at predictable cost. From the platform worker seat, particularly workers who would qualify as employees under an economic-dependence test, the same structure looks like an institutionalized ceiling on their protections — real gains relative to nothing, but a permanent foreclosure relative to what they might otherwise obtain. The engine should compute these as structurally different experiences of the identical arrangement, driven by the asymmetry between who wrote the statute's terms and who has no exit from its classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators sit near the beneficiary end: they fund a bounded, actuarially predictable benefit package in exchange for durable exemption from the much larger cost structure of full employment, and their exit options (arbitrage across jurisdictions, ability to reincorporate operations) are strong. Platform workers sit toward the target end but not at the extreme: they gain real protections relative to pure contractor status (pulling d down somewhat from a pure-victim reading) but lose durable claims relative to employee status (pushing d up from a pure-beneficiary reading) — the moderate ε reflects this genuine mixture rather than averaging away a real asymmetry. Policy intermediary agencies benefit from administering the scheme regardless of whether it serves workers optimally, which is a subtler and less visible beneficiary position than the platforms' direct cost avoidance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a total absence of protection for platform workers under pure contractor status — was real and is partially solved (medical and injury coverage exist where none did). But the mismatch check (founding_problem_status: contested, disappearance_verdict: contested) signals the classic hybrid-category risk: a genuine coordination achievement (baseline coverage) is being used to foreclose a larger and possibly more appropriate resolution (substantive employment) before that question could be adjudicated or organized around. This is neither a mandate that has become pure zombie extraction nor a mandate that fully still serves its original function — it is a live contest over whether 'partial protection' is a stepping stone or a permanent ceiling, and the tangled_rope classification is designed to hold both truths simultaneously rather than force a premature verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    third_category_as_genuine_innovation_or_foreclosure_device,
    'Is the hybrid third category a genuine policy innovation responsive to platform work''s real structural novelty, or is it primarily a legislative device constructed to foreclose the substantive-employment claim before courts or unions could establish it?',
    'Comparative analysis of drafting history and lobbying records across jurisdictions that adopted third-category statutes versus those that litigated substantive-employment claims to conclusion; compare eventual worker-outcome trajectories (wage floors, benefit adequacy, bargaining power) between the two paths over a 10+ year horizon.',
    'If the category functions primarily as a foreclosure device, the tangled_rope classification understates extraction and the arrangement is closer to a snare wearing coordination language; if it is a genuine response to a real classificatory gap that neither prior category fit, tangled_rope is the accurate reading and the coordination function is not merely cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_category_as_genuine_innovation_or_foreclosure_device, conceptual, 'Whether the hybrid category is genuine innovation or a foreclosure device against the substantive-employment claim.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three employment_boundary kernel readings (formalist, hybrid_security, substantive) best describes the ground truth of platform worker economic dependence — and is the hybrid reading a stable equilibrium or a transitional compromise en route to one of the other two?',
    'Track legislative and judicial developments across jurisdictions over the next decade: if hybrid statutes are progressively amended toward fuller employment protections, treat as transitional toward substantive_employment_reading; if they stabilize or are used as precedent to defeat substantive-employment claims elsewhere, treat as a durable foreclosure of that reading.',
    'If the hybrid reading proves transitional toward substantive employment, its ε and extraction profile understate its function as a stepping stone; if it proves a durable foreclosure device, the coexists_with relation to substantive_employment_reading understates the actual competitive/foreclosing dynamic between the two readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, empirical, 'Whether the hybrid reading is a stable third path or a transitional/foreclosing device relative to the sibling readings.').

omega_variable(
    coverage_percentage_vs_protection_adequacy,
    'Do the high enrollment percentages (91.5% medical, 86.2% injury) reflect genuinely adequate protection, or do they measure enrollment/eligibility while masking inadequate benefit levels, claim denial rates, or coverage gaps during the periods workers most need protection (between platform gigs, during deactivation disputes)?',
    'Audit claim approval rates, benefit adequacy relative to cost of living, and coverage continuity during platform account suspensions or algorithmic deactivation — not just enrollment headcounts.',
    'If enrollment percentages substantially overstate real protection, the theater_ratio trajectory in this story understates the degree of metric substitution already underway, and ε may be higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverage_percentage_vs_protection_adequacy, empirical, 'Whether high enrollment figures reflect real adequacy or measure the wrong thing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empl_tr_t4, employment_boundary__hybrid_security_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__hybrid_security_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__hybrid_security_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__hybrid_security_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__hybrid_security_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(empl_tr_t24, employment_boundary__hybrid_security_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(empl_be_t4, employment_boundary__hybrid_security_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(empl_be_t8, employment_boundary__hybrid_security_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(empl_be_t12, employment_boundary__hybrid_security_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(empl_be_t16, employment_boundary__hybrid_security_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(empl_be_t20, employment_boundary__hybrid_security_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(empl_be_t24, employment_boundary__hybrid_security_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(empl_su_t4, employment_boundary__hybrid_security_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(empl_su_t8, employment_boundary__hybrid_security_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(empl_su_t12, employment_boundary__hybrid_security_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(empl_su_t16, employment_boundary__hybrid_security_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(empl_su_t20, employment_boundary__hybrid_security_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(empl_su_t24, employment_boundary__hybrid_security_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__substantive_employment_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked stories decomposing the natural-language 'platform worker classification' concept per the employment_boundary kernel. formalist_employment_reading claims platform workers are contractors outside employment entirely (low ε, few/no beneficiary-victim asymmetries claimed by that reading's own lights). substantive_employment_reading claims platform workers are employees regardless of contract form (high ε under that reading, larger victim set, full employment protections withheld). This hybrid_security_reading claims a third category with a genuine partial coordination function and genuine partial extraction — moderate ε reflecting a mixed structure that is a distinct constraint from either sibling, not an average of them. All three share the same underlying kernel (the employment/contractor boundary as applied to platform work) but instantiate structurally different claims with different beneficiary/victim sets and different ε values, consistent with the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
