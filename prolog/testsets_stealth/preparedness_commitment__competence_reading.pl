% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness Regime as Live Exercised Knowledge (Competence Reading)
 *   domain: institutional/emergency_management
 *
 * SUMMARY:
 *   This story instantiates the competence reading of the
 *   preparedness-commitment kernel: the claim that a preparedness regime
 *   built on exercised routines — scenario drills that test judgment,
 *   certification cycles, and after-action review — retains real operational
 *   capacity across generational turnover. The arrangement described is a
 *   standing drill-and-certification program at a regional response
 *   organization: an exercise directorate designs and enforces it, career
 *   responders carry its hours, successor cohorts acquire through it the
 *   incident judgment they cannot get from documents, and the communities
 *   served collect the difference when events arrive. The epsilon referent is
 *   the standing arrangement itself, assessed by this reading's own lights:
 *   the routines work, so what they cost beyond their coordination floor is
 *   modest — drill hours that build the competence they certify, plus a
 *   certification layer whose fees are the regime's visible excess. This file
 *   is one of three readings of the same kernel; the husk and hybrid readings
 *   are separate constraints with their own epsilon, beneficiaries, and
 *   classifications (see network.dual_formulation_note and
 *   commentary.kernel_context). Nothing about those readings is averaged into
 *   this one. KEY AGENTS (by structural relationship): exercise_directorate —
 *   agenda setter (institutional/mobile), designs, enforces, and revises the
 *   routine regime; career_responders — primary cost-bearers and dual
 *   beneficiaries (organized/constrained), carry drill hours and hold
 *   operational judgment; successor_cohorts — primary beneficiaries
 *   (moderate/mobile), inherit transmitted capacity; served_communities —
 *   ultimate beneficiaries (moderate/constrained), fund and receive the
 *   protection; accredited_training_vendors — secondary beneficiaries and
 *   receipt seat (organized/arbitrage), collect certification and materials
 *   revenue; host_institution — funder and residual beneficiary
 *   (institutional/constrained); emergency_management_oversight — analytical
 *   observer (institutional/analytical), audits realism against incident
 *   performance; underresourced_volunteer_departments — excluded voice
 *   (powerless/trapped), bound by standards they did not help write.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.17).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.28).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.17).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness Regime as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "institutional/emergency_management").

domain_priors:requires_active_enforcement(preparedness_commitment__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, '0a6f86c9-31d4-451f-bbf9-aa0bed11e913').
narrative_ontology:cs_kernel_codification('0a6f86c9-31d4-451f-bbf9-aa0bed11e913', formalized).
narrative_ontology:cs_authority_grounding('0a6f86c9-31d4-451f-bbf9-aa0bed11e913', expertise).
narrative_ontology:cs_interpretation_layer_present('0a6f86c9-31d4-451f-bbf9-aa0bed11e913').
narrative_ontology:cs_reading_relation('0a6f86c9-31d4-451f-bbf9-aa0bed11e913', preparedness_commitment__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('0a6f86c9-31d4-451f-bbf9-aa0bed11e913', preparedness_commitment__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('0a6f86c9-31d4-451f-bbf9-aa0bed11e913', foundational, exercised_practice_preserves_operational_capacity).
narrative_ontology:cs_axiom_status(exercised_practice_preserves_operational_capacity, holdable).
narrative_ontology:cs_axiom_grounding('0a6f86c9-31d4-451f-bbf9-aa0bed11e913', exercised_practice_preserves_operational_capacity, empirically_contingent).
narrative_ontology:cs_axiom('0a6f86c9-31d4-451f-bbf9-aa0bed11e913', secondary, drills_must_test_judgment_not_recall).
narrative_ontology:cs_axiom_status(drills_must_test_judgment_not_recall, holdable).
narrative_ontology:cs_axiom_grounding('0a6f86c9-31d4-451f-bbf9-aa0bed11e913', drills_must_test_judgment_not_recall, instrumental).
narrative_ontology:cs_reference_frame('0a6f86c9-31d4-451f-bbf9-aa0bed11e913', exercised_competence_regime).
narrative_ontology:cs_drift_state('0a6f86c9-31d4-451f-bbf9-aa0bed11e913', post_second_turnover_wave, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('0a6f86c9-31d4-451f-bbf9-aa0bed11e913', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, successor_cohorts).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, served_communities).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, accredited_training_vendors).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, host_institution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, career_responders).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, career_responders).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, host_institution).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, exercised_competence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs the drill calendar, writes exercise scenarios, sets certification standards, and runs after-action review. Its professional standing rests on the exercises being taken seriously; it can redesign scenarios when incident data shows a gap, and its members move between agencies carrying exercise-design expertise with them.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, exercise_directorate, agenda_setter,
    institutional, generational, mobile, national).

% Staff the response units and carry the drill load: recurring exercises, recertification cycles, and after-action participation come out of their working hours. The same hours build the judgment and certifications that make them employable and keep their units functional. Leaving mid-career means abandoning seniority and unit-specific knowledge that no rival employer fully credits.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, career_responders, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, career_responders, beneficiary).

% Enter the organization without incident experience and acquire it through the academy, mentored drills, and graded exercises. What they receive is operational competence their predecessors paid for. Early in their careers they can take their certifications to other agencies, so the training they receive raises their outside options rather than binding them.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, successor_cohorts, beneficiary,
    moderate, biographical, mobile, regional).

% Live with the consequences of the units' readiness: response times, error rates, and recovery quality after floods, fires, and industrial accidents. They fund the program through taxes and rates and cannot relocate away from the hazards the drills rehearse; their contact with the program runs mostly through its outcomes rather than its exercises.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, served_communities, beneficiary,
    moderate, generational, constrained, regional).

% Deliver accredited instruction, certification testing, and the curriculum materials the standards require departments to purchase. Revenue recurs with every recertification cycle. The same firms sell comparable compliance training into other regulated industries, so a change in preparedness standards redirects rather than destroys their business.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, accredited_training_vendors, beneficiary,
    organized, immediate, arbitrage, national).

% Appropriates the drill budget, employs the directorate, and answers politically for response failures. It pays for the program every budget cycle and collects organizational survival in return: capacity that does not leave with each retiring cohort, and a defensible record when incidents are investigated.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, host_institution, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, host_institution, beneficiary).

% Audits exercise records, samples drill realism, and compares declared readiness against incident performance. It can flag a department whose exercises have become compliance rituals and condition accreditation on redesign. It holds no stake in the program's continuation beyond the accuracy of its findings.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, emergency_management_oversight, observer,
    institutional, generational, analytical, national).

% Staff response duty with volunteers and part-time crews under certification standards written for funded agencies. Meeting the drill hours means unpaid evenings and travel to training centers they did not help design; declining them risks losing mutual-aid eligibility, and their communities have no alternative coverage to fall back on.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, underresourced_volunteer_departments, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__competence_reading, accredited_training_vendors).
narrative_ontology:fixing_cost_class(preparedness_commitment__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operational decision-making capacity across generational turnover: scenario drills, certification cycles, and after-action review embed response competence in practiced routines rather than in individual memory, so capacity survives the exit of the people who hold it.
% TRANSFER_FUNCTION: Moves time and budget from current operations and current members into exercised training; moves operational competence and certifiable skill from the exercise program to current and successor cohorts; moves accountability, via after-action findings, from incidents back into the drill program's design.
% ABSENT_VOICES: Under-resourced volunteer departments and the residents they serve are not seated in the accreditation conversation that sets drill standards; they would object that the competence regime is priced for well-funded agencies and that its certification burden falls on the cohorts least able to carry it. Responders who left during the theater-heavy period would also contest the liveness claim from firsthand experience.
% DISAPPEARANCE_RATIONALE: Without the exercised routines, competence would reside in individuals and leave with them: each retirement wave would take decision-making capacity that no document restores, and the organization would re-purchase the lost capacity through incident failures. Response times and error rates would drift upward over one to two turnover cycles, and successor cohorts would inherit a credential without the competence it certifies.
% FOUNDING_PROBLEM: Organizations lose operational capacity when experienced members leave: the judgment that handles rare, high-stakes events walks out the door with each retirement, and written procedures alone do not carry it. The drill-and-certification regime was built to convert individual experience into transmissible practiced routine.
% FOUNDING_PROBLEM_CORROBORATION: External incident investigations and oversight-body after-action reviews, seated outside the program's beneficiary set, corroborate both the founding problem — capacity loss at turnover is documented in organizations without exercised routines — and its continuing salience. Independent academic research on high-reliability organizations attests the same mechanism. The residual circularity is flagged rather than resolved: most qualified investigators come from the preparedness profession itself, so no corroboration is fully outside the profession's self-interest.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.17, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.17) because the regime's costs sit near the identity-coordination floor: drill hours build the judgment they are said to build, and the visible excess is the certification layer's fees and mandated materials — a rent the receipt surface assigns to the vendor seat rather than laundering into coordination cost. Suppression (0.28) is structural, not internalized: certification is a gate on lawful practice and drill attendance an employment condition, but no cognitive dependency carries the arrangement beyond its enforcement — an individual who exits keeps their judgment. Theater (0.15) is residual compliance documentation, not the regime's content. Accessibility collapse (0.45) is partial: apprenticeship-only and hire-for-experience alternatives remain thinkable, but once the turnover-decay problem is understood they demonstrably underperform, so the exercised-routine design crowds them out without eliminating them. Resistance (0.35) is drill fatigue, budget-cycle pushback, and the volunteer departments' burden complaints. The measurement series share one grid; both tracked metrics show a generational cycle — theater and extractiveness peak as a founding cohort retires and documentation substitutes for practice, then fall as scenario-based reform restores decision-testing. The second cycle is smaller and shorter than the first: the training system learned to absorb turnover, which is this reading's central claim. The oscillation is the turnover cycle itself, not an intermittent-reinforcement mechanism — the regime does not profit from the peaks; it pays to contain them. No suppression_requirement series is authored because the enforcement picture is static across the interval: the mandate's intensity does not change, only the quality of what it enforces.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the exercise directorate the regime is the institution's spine — the thing that lets the organization survive its own retirements. From career responders it is a real but burdensome duty: hours paid now for competence that is partly theirs and partly the unit's. From successor cohorts it is an inheritance they did not pay for. From the vendor seat it is a recurring revenue line attached to a mandate. From oversight it is an auditable claim that must be checked against incident outcomes rather than exercise counts. The divergence between the payer seat and the agenda-setter seat is structural — the same hours are cost where they are borne and function where they are designed — and the engine computes it from the declared positions rather than from this claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for successor cohorts, served communities, and the host institution; the vendor seat is a beneficiary whose benefit is the regime's visible excess. Career responders are the genuinely dual seat: they bear the drill hours directly, which places them nearer symmetric than a pure-beneficiary derivation from the array alone would suggest, but their competence, certifications, and employability are also the regime's product, so their net position under this reading remains on the beneficiary side. No directionality overrides are authored: the override key is the power atom, and the two organized seats (career responders and vendors) sit at opposite ends of the beneficiary-target axis, so a per-atom override would mis-correct one of them; the dual-role declaration carries the asymmetry instead. Victims are not declared: under this reading's lights no seat pays without commensurate return — the closest candidate, the volunteer departments, is authored as an excluded voice because their objection concerns distribution across resource levels, not the arrangement's core exchange. Scope amplification is modest: the regime operates at national scale with regional verification, so the engine's scope modifier applies mildly to the already-low base.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is double-sided. Against the husk temptation: the regime does exhibit theater, and a theater-only glance would read the drill calendar as commemorative performance — but the theater is cyclical residue that peaks at generational turnover and is contained by reform, not the regime's primary content; a piton reading would mistake the symptom for the structure. Against rope complacency: the certification layer collects a real rent, and the receipt surface names the vendor seat so the excess is not absorbed into coordination cost by fiat. The founding problem — capacity that leaves with retiring cohorts — is live: turnover has not stopped, and the regime's function is the steady-state answer to it, so there is no mandate outliving its function and no sunset to declare. Fixing cost is authored prohibitive in the relative sense: the institution demonstrated at the first turnover peak that it reforms what pays to reform (the scenario-based redesign), and the absence of equivalent action against the certification layer's rent indicates the restructuring cost exceeds the modest benefit — a fact about the rent's size, not about institutional incapacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates the competence_reading of the preparedness_commitment kernel — the claim that the drill-and-certification regime transmits real operational capacity across generational turnover. The husk_reading of the same routines would author high theater and high epsilon and classify toward the piton/snare side. Which reading describes the standing arrangement?',
    'Outcome-linked audit across many organizations: correlate exercise design (decision-testing versus script-recall) and exercise hours with measured incident performance and post-turnover capacity retention. Performance that tracks decision-testing supports this reading; performance independent of exercise content supports the husk reading.',
    'If the husk reading is correct about the same routines, this story''s low epsilon is a misreading rather than a measurement: theater_ratio rises toward the husk profile and the classification moves off the rope toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, empirical, 'Committer structure: one reading of kernel preparedness_commitment; the disagreement with the husk sibling is located in the liveness premise — whether exercised routines transmit capacity or merely commemorate it.').

omega_variable(
    hybrid_layer_composition,
    'Do memorial elements inside the drill program — anniversary exercises, ceremonial pass-downs, retiree invitations — perform stabilizing work the competence elements cannot (the hybrid_reading''s claim), or are they functionally redundant with exercised practice?',
    'Ablation comparison: organizations that stripped commemorative elements while keeping decision-testing drills, against matched organizations that kept both, tracked for recruitment, retention, and commitment stability across a turnover wave.',
    'If memorial elements carry irreducible stabilizing function, this reading under-describes the arrangement and the hybrid sibling becomes the better model; if not, the memorial residue is drift to be pruned and this reading stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_layer_composition, conceptual, 'Locates the disagreement with the hybrid sibling: whether a memorial layer does work that competence elements cannot.').

omega_variable(
    survivorship_inflation,
    'Is the competence profile survivorship-biased — do organizations showing it persist because they were competent, while adopters that failed dissolved and left the observable record?',
    'Cohort study of all adopters of the routine regime, including dissolved organizations, comparing capacity retention and dissolution against regime fidelity.',
    'If survivorship inflates the profile, the regime''s true epsilon across all adopters is higher and this reading''s generality shrinks to a selected subset of organizations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_inflation, empirical, 'Whether the low-epsilon profile is a regime property or a selection effect.').

omega_variable(
    certification_rent_share,
    'What share of the measured extraction above the coordination floor is inherent delivery cost versus rent collected by the accredited training and certification layer?',
    'Cost decomposition of program budgets: open-market rates for equivalent instruction and exercise logistics against certification fees and mandated-materials pricing.',
    'If rent dominates the excess, the arrangement decomposes into a clean coordination regime plus a parasitic certification layer (two stories); if the excess is genuine delivery cost, the single-story reading is clean.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_rent_share, empirical, 'Decomposition of the excess extraction into delivery cost versus certification-layer rent.').

omega_variable(
    resource_generalization_gap,
    'Does the competence profile generalize to under-resourced volunteer departments, or is exercised-competence maintenance affordable only to funded agencies — in which case the regime imposes drill burden on volunteer cohorts without delivering the competence benefit?',
    'Stratified audit of exercise realism and post-turnover capacity retention across budget quintiles, including volunteer-staffed departments.',
    'If the profile is resource-dependent, a distributional cost-bearing structure exists that this reading''s seat set under-represents, and the arrangement is a hybrid of function and burden at the resource margin rather than clean coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_generalization_gap, empirical, 'Generalizability of the competence profile across resource levels.').

omega_variable(
    scenario_currency_lag,
    'Do the exercises test judgment against current threat profiles, or against a scenario library inherited from earlier threat regimes — such that drills increasingly rehearse the last generation''s disasters?',
    'Audit of scenario libraries against recent incident after-action findings and forward-looking risk assessments; measure the lag between emerging hazards and their first appearance in graded exercises.',
    'If scenario lag is large, the regime''s liveness degrades toward partial husk — drills test recall of outdated scripts — and epsilon rises with no change in drill volume; if the directorate''s revision cycle keeps scenarios current, this reading''s adaptive-capacity claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scenario_currency_lag, empirical, 'Whether drill scenarios track current threats or lag behind them — the adaptive-capacity premise of this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__competence_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__competence_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__competence_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__competence_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_commitment__competence_reading, theater_ratio, 25, 0.16).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(prep_tr_t30, observed).
narrative_ontology:measurement(prep_tr_t35, preparedness_commitment__competence_reading, theater_ratio, 35, 0.19).
narrative_ontology:measurement_basis(prep_tr_t35, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__competence_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__competence_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__competence_reading, base_extractiveness, 15, 0.23).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__competence_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_commitment__competence_reading, base_extractiveness, 25, 0.17).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement_basis(prep_be_t30, observed).
narrative_ontology:measurement(prep_be_t35, preparedness_commitment__competence_reading, base_extractiveness, 35, 0.2).
narrative_ontology:measurement_basis(prep_be_t35, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_commitment__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the preparedness_commitment kernel decomposes into three readings with different epsilon over the same referent (the standing drill-and-certification regime). This file (competence_reading) authors low epsilon: the routines work, and the excess over the coordination floor is the certification layer's rent. preparedness_commitment__husk_reading authors the same routines as memorial performance: high theater, high epsilon, piton/snare-side classification. preparedness_commitment__hybrid_reading authors a layered structure with intermediate epsilon. The epsilon values differ because epsilon is reading-indexed (OQ-26): each reading assesses the same arrangement by its own lights. Structurally, this reading forecloses the husk reading (their core premises are contradictories about the same routines) and coexists with the hybrid reading (competing decompositions, both live).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
