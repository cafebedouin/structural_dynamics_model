% ============================================================================
% CONSTRAINT STORY: adjunctification_of_university_teaching_c0
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adjunctification_of_university_teaching_c0, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: adjunctification_of_university_teaching_c0
 *   human_readable: Adjunctification of University Teaching Labor
 *   domain: labor/education/institutional_organization
 *
 * SUMMARY:
 *   Over 25 years, U.S. universities systematically converted tenured faculty
 *   lines into contingent adjunct positions. The constraint is claimed as
 *   snare from the adjunct seat: structural extraction with active
 *   suppression of exit. The metrics are authored independently: extraction
 *   is high (0.78) because wage savings are captured by administration rather
 *   than returned to instruction; suppression is higher (0.81) because exit
 *   requires abandoning professional identity built through PhD training;
 *   theater is moderate (0.42) because shared governance persists as form
 *   while losing function. The claim and metrics are independent facts; the
 *   engine measures their alignment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adjunctification_of_university_teaching_c0, 0.78).
domain_priors:suppression_score(adjunctification_of_university_teaching_c0, 0.81).
domain_priors:theater_ratio(adjunctification_of_university_teaching_c0, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adjunctification_of_university_teaching_c0, extractiveness, 0.78).
narrative_ontology:constraint_metric(adjunctification_of_university_teaching_c0, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(adjunctification_of_university_teaching_c0, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(adjunctification_of_university_teaching_c0, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(adjunctification_of_university_teaching_c0, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adjunctification_of_university_teaching_c0, snare).
narrative_ontology:human_readable(adjunctification_of_university_teaching_c0, "Adjunctification of University Teaching Labor").
narrative_ontology:topic_domain(adjunctification_of_university_teaching_c0, "labor/education/institutional_organization").

domain_priors:requires_active_enforcement(adjunctification_of_university_teaching_c0).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adjunctification_of_university_teaching_c0, university_administration).
narrative_ontology:constraint_beneficiary(adjunctification_of_university_teaching_c0, endowment_managers).
narrative_ontology:constraint_beneficiary(adjunctification_of_university_teaching_c0, tenured_faculty).
narrative_ontology:constraint_victim(adjunctification_of_university_teaching_c0, adjunct_instructors).
narrative_ontology:constraint_victim(adjunctification_of_university_teaching_c0, graduate_student_instructors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(adjunctification_of_university_teaching_c0, undergraduate_students).
narrative_ontology:constraint_vindicates(adjunctification_of_university_teaching_c0, labor_market_flexibility_doctrine).
narrative_ontology:constraint_vindicates(adjunctification_of_university_teaching_c0, higher_education_cost_disease_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach the majority of undergraduate courses at multiple institutions simultaneously to survive. Paid per-course with no benefits, no office space, no job security beyond the current semester. Cannot leave academia without abandoning professional identity built through PhD training. Course assignments arrive weeks before term starts; multi-year contracts are rare. The PhD itself becomes the trap: overqualified for non-academic work, undercompensated within it.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching_c0, adjunct_instructors, payer,
    powerless, immediate, identity_locked, regional).

% Teach while completing dissertations, paid stipends below living wage in exchange for tuition remission. The arrangement is framed as training, but they carry the same teaching loads as adjuncts while also producing research. Leaving means abandoning sunk costs of years in the program and the professional identity the PhD represents. Union organizing is met with threats to funding.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching_c0, graduate_student_instructors, payer,
    powerless, biographical, identity_locked, local).

% Freed from undergraduate teaching loads by adjunct labor, they focus on research and graduate training. They set curriculum and hiring priorities through faculty governance but do not bear the costs of the labor model they vote to maintain. Their complicity is structural: they benefit from research time purchased by adjunct exploitation, but most did not design the system and many oppose it in principle while depending on it in practice.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching_c0, tenured_faculty, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(adjunctification_of_university_teaching_c0, tenured_faculty, agenda_setter).

% Converts tenure lines to adjunct positions to reduce fixed labor costs and increase budget flexibility. Frames the conversion as responding to enrollment uncertainty and state funding cuts, but the practice accelerates even as endowments grow. Captures the wage savings as administrative expansion and capital projects. Controls hiring, course assignments, and contract terms; adjuncts have no negotiating power.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching_c0, university_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Manage university endowments that grow while instructional spending per student declines. The labor cost savings from adjunctification flow into endowment growth and capital campaigns rather than instructional quality. They are structurally insulated from the teaching mission; their performance metrics are financial returns, not educational outcomes.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching_c0, endowment_managers, beneficiary,
    institutional, generational, arbitrage, global).

% Pay rising tuition while receiving instruction from overworked, under-resourced adjuncts with no job security or institutional support. They experience high instructor turnover, limited office hours, and reduced mentorship. The cost increase and quality decrease are simultaneous. Their exit options are constrained by credential requirements and sunk costs.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching_c0, undergraduate_students, payer,
    organized, immediate, constrained, national).

% Monitor instructional quality and faculty qualifications but do not enforce limits on adjunct ratios or contingent labor conditions. They collect data on the conversion, issue reports noting the trend, but do not threaten accreditation over labor practices. Their standards are written to accommodate the status quo.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching_c0, accreditation_bodies, observer,
    institutional, generational, analytical, national).

% Model the adjunctification as efficient labor market adjustment to demand uncertainty and technological change. They treat the conversion as a revealed preference for flexibility rather than as a power asymmetry. Their models naturalize the outcome by assuming competitive markets and voluntary exchange, backgrounding the identity-lock and suppression mechanisms that make exit non-viable for adjuncts.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching_c0, labor_economists, observer,
    analytical, generational, analytical, national).

% Attempt to organize adjuncts across institutions but face structural barriers: high turnover, geographic dispersion, employer retaliation, and legal exclusion of graduate students from collective bargaining at private universities. They would negotiate for multi-year contracts, benefits, and pay equity, but most adjuncts are not unionized and cannot risk organizing.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching_c0, faculty_unions, excluded,
    organized, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(adjunctification_of_university_teaching_c0, university_administration).
narrative_ontology:fixing_cost_class(adjunctification_of_university_teaching_c0, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Universities need flexible staffing to match enrollment fluctuations and to allocate senior faculty time toward research and graduate training where institutional prestige is built.
% TRANSFER_FUNCTION: Moves wage savings from eliminated tenure lines and suppressed adjunct compensation to administrative budgets, capital projects, and endowment growth. Transfers teaching labor from credentialed professionals with job security to credentialed professionals without it, at a fraction of the cost.
% ABSENT_VOICES: Faculty unions and adjunct organizers are structurally excluded from governance; they would demand pay equity, multi-year contracts, and limits on contingent labor ratios, but they are kept out of the hiring and budget processes where the conversion is enacted.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, universities would face immediate staffing crises and budget shortfalls. Adjuncts would demand tenure-track conversion or leave; undergraduate teaching would collapse without them. Tuition revenue would have to flow back into instructional spending rather than administration and capital. The entire financial model of contemporary higher education depends on adjunct labor suppression.
% FOUNDING_PROBLEM: In the 1970s, universities faced genuine enrollment uncertainty from demographic shifts and state funding cuts. Contingent hiring was introduced as a temporary buffer to avoid tenuring faculty into permanent positions during a contraction.
% FOUNDING_PROBLEM_CORROBORATION: University administrations attest the problem is still live, citing continued funding volatility. Adjunct advocates, faculty unions, and independent higher education researchers attest the founding problem is dead: enrollment has stabilized, endowments have grown massively, and the conversion has continued through decades of growth. Legislative testimony and academic studies from outside the benefiting institutions document that adjunctification persists in the absence of the founding crisis and has become a profit-extraction mechanism.
narrative_ontology:disappearance_verdict(adjunctification_of_university_teaching_c0, world_rearranges).
narrative_ontology:founding_problem_status(adjunctification_of_university_teaching_c0, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(adjunctification_of_university_teaching_c0, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-12',
    'cohort_zero_regen', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'temperature=0.2').
narrative_ontology:story_seed(adjunctification_of_university_teaching_c0, 'adjunctification_of_university_teaching', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adjunctification_of_university_teaching_c0_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(adjunctification_of_university_teaching_c0, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(adjunctification_of_university_teaching_c0_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises steadily as the conversion deepens and wage savings accumulate in endowments rather than instructional budgets. Suppression rises because the identity-lock mechanism intensifies: as tenure-track positions disappear, adjuncts face a choice between permanent contingency and abandoning the profession entirely. Theater rises as faculty governance continues to meet and vote while actual hiring and budget authority migrates to administration. The founding problem (enrollment uncertainty) is dead, but the arrangement persists and intensifies, which is the mandatrophy signature.
 *
 * PERSPECTIVAL GAP:
 *   From the adjunct seat, the constraint is pure extraction maintained by identity-lock and retaliation threats. From the administration seat, it is efficient resource allocation responding to market conditions. From the tenured faculty seat, it is a troubling trend they benefit from but did not choose. The engine computes these divergences from the structural data; the claimed type (snare) reflects the adjunct seat's experience, which is the seat where extraction and suppression are concentrated.
 *
 * DIRECTIONALITY LOGIC:
 *   Adjuncts and graduate instructors are full targets: they bear the extraction (wage suppression, no benefits, no security) and are identity-locked (PhD training makes exit unthinkable without abandoning professional self-concept). Administration and endowment managers are full beneficiaries: they capture the wage savings and control the terms. Tenured faculty are asymmetric: they benefit from reduced teaching loads but did not design the system and many oppose it in principle. Undergraduates are diffuse payers: they pay rising tuition while receiving lower-quality instruction. Labor economists are analytical observers whose models naturalize the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enrollment uncertainty from 1970s demographic shifts) is dead. Enrollment stabilized, endowments grew massively, and the conversion accelerated through decades of growth. The constraint persists because it extracts wealth for administration and endowments, not because it solves the problem it was built for. This is the mandatrophy pattern: a temporary measure (contingent hiring as enrollment buffer) becomes permanent extraction (adjunctification as labor cost suppression) after the founding problem disappears.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism,
    'Is the adjunct''s inability to exit due to external labor market barriers (PhD holders are overqualified for non-academic work) or internalized identity fusion (leaving academia means abandoning self-concept as scholar)?',
    'Longitudinal study of PhD holders who left academia: if they report the decision as abandoning identity rather than changing jobs, the lock is internalized. If they report external barriers (employer discrimination, skill mismatch) as primary, the lock is structural.',
    'If internalized, the suppression is higher than the structural measure suggests because the target carries the constraint with them after exit. If structural, policy interventions that improve PhD-holder employability outside academia would reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-lock is structural or internalized.').

omega_variable(
    tenured_faculty_complicity,
    'Are tenured faculty structurally complicit beneficiaries (they vote to maintain the system through governance) or structurally powerless bystanders (administration controls hiring regardless of faculty votes)?',
    'Analysis of faculty governance records: do tenure-track hiring proposals from faculty get vetoed by administration, or do faculty votes themselves reject tenure-track expansion? If the former, faculty are excluded; if the latter, they are complicit.',
    'If complicit, tenured faculty are beneficiaries whose coordination with administration maintains the snare. If powerless, they are excluded voices and the constraint is purely administrative extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenured_faculty_complicity, empirical, 'Whether tenured faculty are complicit beneficiaries or excluded voices.').

omega_variable(
    endowment_counterfactual,
    'If universities were required to spend endowment returns on instructional labor rather than capital projects, would adjunctification reverse, or would administration find other budget lines to cut?',
    'Natural experiment from states that mandate minimum instructional spending ratios: do adjunct ratios decline, or do universities comply by reclassifying spending categories?',
    'If adjunctification reverses, the constraint is purely extractive and endowment growth is the mechanism. If it persists, the extraction has multiple sources and endowment policy alone cannot resolve it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endowment_counterfactual, empirical, 'Whether endowment spending rules would reverse adjunctification.').

omega_variable(
    naturalness_claim,
    'Is adjunctification a natural market response to technological change and demand uncertainty (the labor economist framing), or a constructed policy choice that benefits identifiable actors (the labor advocate framing)?',
    'Cross-national comparison: do countries with stronger labor protections and public university funding exhibit the same conversion? If not, the U.S. pattern is policy-contingent, not natural.',
    'If natural, the constraint is a mountain (inevitable adjustment) and resistance is futile. If constructed, it is a snare (extractive policy) and resistance is justified. The labor economist models that naturalize it are false-summit candidates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalness_claim, conceptual, 'Whether adjunctification is natural or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adjunctification_of_university_teaching_c0, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adju_tr_t0, adjunctification_of_university_teaching_c0, theater_ratio, 0, 0.22).
narrative_ontology:measurement(adju_tr_t5, adjunctification_of_university_teaching_c0, theater_ratio, 5, 0.27).
narrative_ontology:measurement(adju_tr_t10, adjunctification_of_university_teaching_c0, theater_ratio, 10, 0.32).
narrative_ontology:measurement(adju_tr_t15, adjunctification_of_university_teaching_c0, theater_ratio, 15, 0.36).
narrative_ontology:measurement(adju_tr_t20, adjunctification_of_university_teaching_c0, theater_ratio, 20, 0.39).
narrative_ontology:measurement(adju_tr_t25, adjunctification_of_university_teaching_c0, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(adju_be_t0, adjunctification_of_university_teaching_c0, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(adju_be_t5, adjunctification_of_university_teaching_c0, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(adju_be_t10, adjunctification_of_university_teaching_c0, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(adju_be_t15, adjunctification_of_university_teaching_c0, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(adju_be_t20, adjunctification_of_university_teaching_c0, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(adju_be_t25, adjunctification_of_university_teaching_c0, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(adju_su_t0, adjunctification_of_university_teaching_c0, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(adju_su_t5, adjunctification_of_university_teaching_c0, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(adju_su_t10, adjunctification_of_university_teaching_c0, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(adju_su_t15, adjunctification_of_university_teaching_c0, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(adju_su_t20, adjunctification_of_university_teaching_c0, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(adju_su_t25, adjunctification_of_university_teaching_c0, suppression_requirement, 25, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adjunctification_of_university_teaching_c0, resource_allocation).
narrative_ontology:affects_constraint(adjunctification_of_university_teaching_c0, graduate_student_labor_exploitation).
narrative_ontology:affects_constraint(adjunctification_of_university_teaching_c0, credential_inflation_dynamics).
narrative_ontology:affects_constraint(adjunctification_of_university_teaching_c0, shared_governance_atrophy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
