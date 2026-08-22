% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__actuarial_risk_acceptance, []).

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
 *   constraint_id: rogers_commission_findings__actuarial_risk_acceptance
 *   human_readable: Post-Rogers Actuarial Risk-Acceptance Requirement (Documented Failure Probability Plus Recorded Informed Acceptance)
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   After the 1986 Challenger loss, the Rogers Commission diagnosed a
 *   launch-governance failure: known hazards had not been presented to
 *   decision-makers as explicit quantified risks, and flights proceeded
 *   without anyone formally accepting those risks. This story instantiates
 *   ONE reading of those findings — the actuarial risk-acceptance reading —
 *   under which the findings establish a risk-quantification requirement: a
 *   flight is acceptable when its failure probability is documented and its
 *   residual risk is formally accepted by informed decision-makers. The
 *   standing arrangement this story is about is the post-Rogers
 *   flight-readiness regime built on that reading: probabilistic risk
 *   assessments, hazard documentation chains, review boards, and recorded
 *   acceptance signatures that together define the conditions under which
 *   flight continues. The regime solved a real decision problem and
 *   simultaneously built a channel through which residual risk flows to those
 *   who do not decide and categorical safety objections are converted into
 *   negotiable numbers. KEY AGENTS (by structural relationship):
 *
 * KEY AGENTS:
 *   - - nasa_program_management: Agenda-setter (institutional/arbitrage) — administers the flight-readiness process, records acceptance, collects schedule continuity and the distributed paper trail
 *   - - mission_planners: Primary beneficiary (organized/mobile) — receives launch cadence enabled by documented acceptance
 *   - - astronaut_flight_crews: Primary target (moderate/identity_locked) — rides the accepted residual risk; assent is presumed by the record, not negotiated
 *   - - dissenting_safety_engineers: Target (moderate/constrained) — categorical objections must be translated into probability estimates to have standing
 *   - - shuttle_contractor_managers: Dual-positioned beneficiary/payer (powerful/constrained) — contract continuation depends on cadence while schedule pressure lands on them
 *   - - aerospace_safety_advisory_panel: Excluded voice (institutional/analytical) — statutory advisor whose written warnings carried no acceptance weight
 *   - - congressional_appropriators: Observer (institutional/analytical) — funds and investigates but holds no acceptance seat
 *   - - commercial_crew_operators: Downstream beneficiary (powerful/arbitrage) — inherits the legitimating template for flying with quantified, accepted risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.63).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.6).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.63).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Post-Rogers Actuarial Risk-Acceptance Requirement (Documented Failure Probability Plus Recorded Informed Acceptance)").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, '535e9af9-2655-4b32-aea1-bb58f59a3e29').
narrative_ontology:cs_kernel_codification('535e9af9-2655-4b32-aea1-bb58f59a3e29', fixed_text).
narrative_ontology:cs_authority_grounding('535e9af9-2655-4b32-aea1-bb58f59a3e29', extraction).
narrative_ontology:cs_interpretation_layer_present('535e9af9-2655-4b32-aea1-bb58f59a3e29').
narrative_ontology:cs_reading_relation('535e9af9-2655-4b32-aea1-bb58f59a3e29', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('535e9af9-2655-4b32-aea1-bb58f59a3e29', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('535e9af9-2655-4b32-aea1-bb58f59a3e29', foundational, quantified_informed_acceptance_suffices_for_flight).
narrative_ontology:cs_axiom_status(quantified_informed_acceptance_suffices_for_flight, holdable).
narrative_ontology:cs_axiom_grounding('535e9af9-2655-4b32-aea1-bb58f59a3e29', quantified_informed_acceptance_suffices_for_flight, instrumental).
narrative_ontology:cs_axiom('535e9af9-2655-4b32-aea1-bb58f59a3e29', secondary, unquantified_objections_lack_decision_standing).
narrative_ontology:cs_axiom_status(unquantified_objections_lack_decision_standing, holdable).
narrative_ontology:cs_axiom_grounding('535e9af9-2655-4b32-aea1-bb58f59a3e29', unquantified_objections_lack_decision_standing, conventional).
narrative_ontology:cs_reference_frame('535e9af9-2655-4b32-aea1-bb58f59a3e29', documented_probability_acceptance_baseline).
narrative_ontology:cs_drift_state('535e9af9-2655-4b32-aea1-bb58f59a3e29', contemporary_post_caib_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('535e9af9-2655-4b32-aea1-bb58f59a3e29', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, nasa_program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, shuttle_contractor_managers).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, commercial_crew_operators).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, astronaut_flight_crews).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, dissenting_safety_engineers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, shuttle_contractor_managers).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, probabilistic_risk_assessment_doctrine).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_maker_authorization_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the flight-readiness review chain: convenes hazard reviews, adjudicates what counts as adequate documentation, and records the acceptance signatures that authorize flight. Collects the direct returns of the process — launch cadence, budget justification, and a distributed paper trail that spreads responsibility across many signatures. Can restructure the process itself, and has historically reshaped review requirements after each accident investigation.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, nasa_program_management, agenda_setter,
    institutional, generational, arbitrage, national).

% Build manifests and schedules that assume flights proceed once acceptance is recorded. Gains continuity of operations from the arrangement; their plans are the demand side that documented acceptance satisfies. Can shift between programs and agencies when a manifest stalls.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, beneficiary,
    organized, biographical, mobile, national).

% Train for and fly the missions whose residual risk the signatures accept. Risk levels are set in reviews they attend as briefees rather than parties; declining a mission carries career and crew-identity costs, and crews have historically flown through publicly stated personal reservations. Their assent is presumed by the acceptance record rather than negotiated.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, astronaut_flight_crews, payer,
    moderate, biographical, identity_locked, national).

% Hold categorical safety judgments formed from hands-on hardware experience. To enter the review they must translate those judgments into probability estimates and mitigation plans; objections that resist quantification lose formal standing. Persistent dissent carries assignment and career risk, as the post-Challenger treatment of engineers who refused to concur illustrated.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, dissenting_safety_engineers, payer,
    moderate, biographical, constrained, national).

% Operate under contracts whose continuation depends on flight cadence while absorbing schedule pressure from both the agency and their own corporate commitments. They supply the hazard data the reviews consume and sign concurrences that bind their workforces to the accepted risk. Leaving means surrendering the contract base.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, shuttle_contractor_managers, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, shuttle_contractor_managers, payer).

% Statutorily chartered to advise the agency on safety. Issued repeated written warnings that the acceptance process was degrading, most prominently in the months before the 2003 loss; the warnings entered the record without altering any acceptance decision. Its leverage depends entirely on being heeded by the seats it advises.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, aerospace_safety_advisory_panel, excluded,
    institutional, generational, analytical, national).

% Fund the programs and hold investigative hearings after failures. They receive testimony about the acceptance process but do not sit in reviews; their instruments are budgets and post-hoc investigation rather than acceptance signatures.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, congressional_appropriators, observer,
    institutional, generational, analytical, national).

% Certify and fly vehicles under requirements descended from the post-Rogers process. They inherit the legitimating template — quantify, document, obtain informed acceptance — and gain market access by satisfying it. Capital mobility lets them relocate programs or press jurisdictions competitively when requirements bind.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, commercial_crew_operators, beneficiary,
    powerful, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__actuarial_risk_acceptance, nasa_program_management).
narrative_ontology:fixing_cost_class(rogers_commission_findings__actuarial_risk_acceptance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the launch-decision problem under irreducible uncertainty: hazards are enumerated once, centrally; failure probabilities are estimated on a common methodology; and a recorded acceptance step converts dispersed technical judgment into a single authorized go/no-go with an audit trail — replacing ad hoc managerial discretion while preventing both unexamined flight and indefinite paralysis.
% TRANSFER_FUNCTION: Moves residual operational risk from the institutions that decide (which accept it on paper and diffuse responsibility across signatures) to the people who occupy the hardware — crews above all — and moves open-ended safety objections from engineers into bounded numerical inputs that management weighs against schedule and budget.
% ABSENT_VOICES: Crews are briefed on accepted risk but hold no acceptance seat; engineers whose objections cannot be quantified have no format in which to be heard; the statutory safety advisory panel's written warnings entered the record without an acceptance consequence. Each absent voice would insist that acceptance requires the assent of those who bear the risk, not merely the signature of those who impose it.
% DISAPPEARANCE_RATIONALE: Without the documented-probability-and-recorded-acceptance requirement, launch authorization reverts to either categorical engineering vetoes — with frequent groundings whenever uncertainty is unresolved — or unconstrained managerial discretion. The review-board infrastructure, contractor hazard-reporting pipelines, and the audit-trail economy built around acceptance signatures would lose their organizing purpose and reorganize around whichever successor rule prevails.
% FOUNDING_PROBLEM: After the 1986 Challenger loss, the Rogers Commission found that known hazards had not been presented to decision-makers as explicit quantified risks and that flights had proceeded without anyone formally accepting those risks. The arrangement was built so that no flight proceeds unless its failure probability is documented and its residual risk is formally accepted by informed decision-makers.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Columbia Accident Investigation Board — an external investigative body — reaffirmed both that the founding problem is real and that the documented-acceptance mechanism had decayed toward ritual; the statutorily independent Aerospace Safety Advisory Panel attests in writing that the problem remains live; and the academic safety literature on normalization of deviance corroborates the mechanism from outside the agency entirely.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__actuarial_risk_acceptance, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.63 at interval end: the referent is the standing post-Rogers acceptance regime assessed by this reading's own lights — the reading endorses quantification and informed acceptance as legitimate, yet its own terms register that the arrangement transfers residual risk onto people who hold no acceptance seat and dilutes categorical safety judgment into one weighted input. Suppression is 0.60 and unscaled by construction: it acts procedurally rather than physically — objections that resist quantification lose formal standing, and persistent dissent carries assignment and career risk — so the enforcement machinery must be continuously maintained for the arrangement to hold. Theater is 0.50: the review function is real, but a growing share of activity produces signatures rather than decisions, peaking around the 2003 loss when debris-assessment analysis was conducted to confirm a predetermined answer. Accessibility collapse is 0.42: the rival readings remain operable — the fleet was in fact grounded for 32 months under threshold logic after Challenger, proving the categorical alternative works — so alternatives persist rather than collapsing. Resistance is 0.55: sustained engineer dissent, external board criticism, and the safety-culture literature keep contesting the frame. The three measurement series run on one shared time grid (1986, 1991, 1997, 2003, 2011, 2025) with every metric authored at every point; trajectories show accumulation through 2003, partial correction after the external investigation, and re-accumulation as the template spread to successor programs.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical documents. From the program-management seat the arrangement is a governed, auditable decision process it built and legitimately administers; from the crew seat it is a record that someone else accepted the risk of riding the vehicle; from the dissenting-engineer seat it is a format that renders categorical judgment illegible. Same signatures, different constraints experienced. The engine computes this per-seat divergence from the structural data — power, exit, and role — and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place mission_planners, nasa_program_management, shuttle_contractor_managers, and commercial_crew_operators near the beneficiary end of d; program management sits lowest as the seat that both administers the process and demonstrably accrues its gains (schedule continuity, budget justification, diffused responsibility). Victim declarations place astronaut_flight_crews and dissenting_safety_engineers near the target end; crews' identity_locked exit pushes them toward full-target — they cannot credibly refuse without dissolving the professional identity the acceptance record presumes — while engineers' constrained exit (public dissent at career cost) leaves them slightly short of full target. The advisory panel is excluded rather than coordinated: its exclusion is maintained by the same signature structure that defines the arrangement. Observers take the analytical seat and feed no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists two symmetric mislabels. Calling the arrangement pure coordination ignores the documented transfer of residual risk onto non-deciding parties and the displacement of categorical norms — the exact casualties this reading's own structural delta names. Calling it pure extraction ignores the surviving decision-protocol function that external investigations repeatedly reaffirmed as necessary under irreducible uncertainty. Tangled rope holds both truths: genuine coordination function, asymmetric extraction through the same structure, active enforcement required to maintain the conversion of objection into number. On obsolescence: the founding problem — authorizing flight under irreducible uncertainty with asymmetric information — remains live, so no mandatrophy resolution is declared; the monitored degradation path is the theater series, which tracks the acceptance ritual drifting toward performance of acceptance rather than acceptance itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Which reading of the Rogers findings does the standing flight-readiness regime actually instantiate in operation — quantified acceptance (this reading), absolute engineering thresholds, or compliance documentation?',
    'Observe regime behavior when quantified risk exceeds historical baselines: grounding pending redesign indicates the threshold reading governs; document-and-fly indicates this reading; paperwork without recalculation indicates the compliance reading.',
    'If the threshold reading governs in practice, this story''s epsilon is misattributed and the operative constraint is near-non-extractive; if the compliance reading governs, theater dominates and effective extraction rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer structure: this constraint is one of three competing readings of the Rogers kernel; which reading governs in operation is unresolved.').

omega_variable(
    probability_estimate_calibration,
    'Are the documented failure probabilities entering acceptance decisions calibrated engineering estimates, or figures negotiated under schedule pressure?',
    'Calibration audit comparing pre-flight probabilistic risk assessment estimates against realized failure rates across Shuttle, Station, and commercial crew programs; discovery of review-chain edits to probability figures.',
    'If probabilities are negotiated rather than estimated, the documented half of the arrangement is cover and effective extraction rises sharply toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_estimate_calibration, empirical, 'Integrity of the quantification the acceptance regime consumes.').

omega_variable(
    informed_acceptance_information_asymmetry,
    'Does acceptance by decision-makers constitute informed acceptance, given what reviewing engineers knew versus what reached the acceptance signature?',
    'Communication-chain audits reconstructing the information set available at each acceptance signature; the Columbia-era investigation board reconstruction provides the template.',
    'If acceptance is systematically uninformed, the legitimation function fails and the arrangement operates as unilateral risk imposition dressed as consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_acceptance_information_asymmetry, empirical, 'Whether the informed-decision-maker element of the arrangement is substantively met.').

omega_variable(
    crew_assent_structural_vs_internalized,
    'Is flight crews'' continued acceptance of residual risk free professional assent, or identity-fused inability to refuse — is the suppression acting on crews structural or internalized?',
    'Post-career testimony patterns versus in-service statements; comparison with programs where crews hold formal refusal rights; exit interviews.',
    'If assent is substantially internalized, the accepted element is compromised — measured extraction understates the burden crews carry, and effective suppression is higher than the structural measure shows because crews carry it with them after any individual exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crew_assent_structural_vs_internalized, empirical, 'Suppression-mechanism ambiguity for the crew seat: structural barriers versus internalized mission identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 1986, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t1986, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 1986, 0.25).
narrative_ontology:measurement_basis(roge_tr_t1986, observed).
narrative_ontology:measurement(roge_tr_t1991, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 1991, 0.32).
narrative_ontology:measurement_basis(roge_tr_t1991, observed).
narrative_ontology:measurement(roge_tr_t1997, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 1997, 0.4).
narrative_ontology:measurement_basis(roge_tr_t1997, observed).
narrative_ontology:measurement(roge_tr_t2003, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2003, 0.52).
narrative_ontology:measurement_basis(roge_tr_t2003, observed).
narrative_ontology:measurement(roge_tr_t2011, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2011, 0.46).
narrative_ontology:measurement_basis(roge_tr_t2011, observed).
narrative_ontology:measurement(roge_tr_t2025, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 2025, 0.5).
narrative_ontology:measurement_basis(roge_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(roge_be_t1986, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement_basis(roge_be_t1986, observed).
narrative_ontology:measurement(roge_be_t1991, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 1991, 0.54).
narrative_ontology:measurement_basis(roge_be_t1991, observed).
narrative_ontology:measurement(roge_be_t1997, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 1997, 0.58).
narrative_ontology:measurement_basis(roge_be_t1997, observed).
narrative_ontology:measurement(roge_be_t2003, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2003, 0.66).
narrative_ontology:measurement_basis(roge_be_t2003, observed).
narrative_ontology:measurement(roge_be_t2011, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2011, 0.6).
narrative_ontology:measurement_basis(roge_be_t2011, observed).
narrative_ontology:measurement(roge_be_t2025, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 2025, 0.63).
narrative_ontology:measurement_basis(roge_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t1986, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 1986, 0.45).
narrative_ontology:measurement_basis(roge_su_t1986, observed).
narrative_ontology:measurement(roge_su_t1991, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 1991, 0.52).
narrative_ontology:measurement_basis(roge_su_t1991, observed).
narrative_ontology:measurement(roge_su_t1997, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 1997, 0.57).
narrative_ontology:measurement_basis(roge_su_t1997, observed).
narrative_ontology:measurement(roge_su_t2003, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2003, 0.62).
narrative_ontology:measurement_basis(roge_su_t2003, observed).
narrative_ontology:measurement(roge_su_t2011, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2011, 0.58).
narrative_ontology:measurement_basis(roge_su_t2011, observed).
narrative_ontology:measurement(roge_su_t2025, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement_basis(roge_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Rogers Commission findings' into three structurally distinct constraints with different epsilon values: the engineering_absolute_threshold reading (a hard boundary; negligible extraction where operative), this actuarial_risk_acceptance reading (a working protocol that transfers residual risk; moderate-high extraction), and the management_compliance_narrative reading (documentation as cover; high theater). All three cite the same upstream text for incompatible requirements; the epsilon values differ because the referent arrangements differ — what must be shown for flight to proceed — not because of observable selection. Family members are linked via affects_constraints in both directions of citation dependence: the actuarial reading's documentary outputs feed the compliance reading's evidentiary substrate, while the threshold reading stands as the categorical alternative the other two displace.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
