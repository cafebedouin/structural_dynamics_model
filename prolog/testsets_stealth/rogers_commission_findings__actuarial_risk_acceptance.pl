% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Rogers Findings - Actuarial Risk Acceptance Reading (Documented Probability, Informed Acceptance)
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   After STS-51-L (January 1986), the Rogers Commission investigated why
 *   Challenger launched against known O-ring hazards. Its findings admit
 *   multiple readings; this file instantiates ONE - the actuarial reading:
 *   the findings establish that flight is acceptable when failure probability
 *   is documented and formally accepted by informed decision-makers. The
 *   standing arrangement under contest is therefore the
 *   quantification-and-acceptance regime governing flight readiness reviews
 *   from 1986 to the Columbia rupture (2003). Claim and metrics are authored
 *   independently: claimed_type tangled_rope states my structural belief (a
 *   genuine coordination function - standardized risk communication - plus
 *   asymmetric extraction - planner discretion purchased with engineer veto
 *   authority and crew risk-bearing); the metrics describe observed operation
 *   without tuning toward any engine verdict. Sibling readings are separate
 *   files linked via network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - program_executives: Agenda setter
 *   (institutional/constrained) - chairs flight readiness reviews, designates
 *   accepting officials - mission_planners: Primary beneficiary
 *   (organized/constrained) - schedules against accepted risks -
 *   flight_readiness_engineers: Primary target (moderate/identity_locked) -
 *   hazard finders whose categorical judgments enter only as quantified
 *   entries - astronaut_crews: Risk bearer (powerless/trapped) - physical
 *   residual risk accepted on their behalf - shuttle_program_contractors:
 *   Secondary beneficiary/payer (institutional/constrained) - revenue rides
 *   on flight tempo - independent_safety_authority: Excluded seat
 *   (institutional/analytical) - would-be stop-fly veto holder absent until
 *   after Columbia - external_aerospace_safety_community: Analytical observer
 *   (analytical/analytical) - calibrates PRA claims against outcomes
 *
 * KEY AGENTS:
 *   - program_executives: Agenda setter (institutional/constrained) - chairs flight readiness reviews, sets documentation standards, designates which officials may accept residual risk; collects launch tempo and the defensible decision record
 *   - mission_planners: Primary beneficiary (organized/constrained) - plans manifests against the acceptance framework; once a hazard is quantified and accepted they can schedule around it, and their alternative is indefinite slip
 *   - flight_readiness_engineers: Primary target (moderate/identity_locked) - surface hazards and assign probabilities; their categorical judgments enter the process only as quantified entries an official above them may accept, and dissent beyond the quantified slot carries career cost
 *   - astronaut_crews: Risk bearer (powerless/trapped) - bear the physical residual risk that accepting officials sign off on their behalf; declining an assignment ends a flying career, and they hold no standing seat in reviews
 *   - shuttle_program_contractors: Secondary beneficiary/payer (institutional/constrained) - cost-plus contracts continue only while flights continue; the framework protects tempo and revenue while imposing documentation burden and liability exposure
 *   - independent_safety_authority: Excluded seat (institutional/analytical) - a would-be independent technical authority with stop-fly power, absent from flight readiness reviews for the entire interval; safety offices sat inside the program chain they were meant to check
 *   - external_aerospace_safety_community: Analytical observer (analytical/analytical) - researchers and accident investigators who compare PRA estimates against observed failure rates and publish analyses the program cannot silence; neither collects nor pays
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.7).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.62).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.7).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Findings - Actuarial Risk Acceptance Reading (Documented Probability, Informed Acceptance)").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, 'f90a489e-68a1-45e3-a9e8-c37094448f37').
narrative_ontology:cs_kernel_codification('f90a489e-68a1-45e3-a9e8-c37094448f37', fixed_text).
narrative_ontology:cs_authority_grounding('f90a489e-68a1-45e3-a9e8-c37094448f37', expertise).
narrative_ontology:cs_interpretation_layer_present('f90a489e-68a1-45e3-a9e8-c37094448f37').
narrative_ontology:cs_reading_relation('f90a489e-68a1-45e3-a9e8-c37094448f37', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('f90a489e-68a1-45e3-a9e8-c37094448f37', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('f90a489e-68a1-45e3-a9e8-c37094448f37', foundational, quantified_failure_probability_prerequisite_for_launch).
narrative_ontology:cs_axiom_status(quantified_failure_probability_prerequisite_for_launch, holdable).
narrative_ontology:cs_axiom_grounding('f90a489e-68a1-45e3-a9e8-c37094448f37', quantified_failure_probability_prerequisite_for_launch, instrumental).
narrative_ontology:cs_axiom('f90a489e-68a1-45e3-a9e8-c37094448f37', secondary, informed_official_acceptance_discharges_objection).
narrative_ontology:cs_axiom_status(informed_official_acceptance_discharges_objection, holdable).
narrative_ontology:cs_axiom_grounding('f90a489e-68a1-45e3-a9e8-c37094448f37', informed_official_acceptance_discharges_objection, conventional).
narrative_ontology:cs_reference_frame('f90a489e-68a1-45e3-a9e8-c37094448f37', quantified_informed_acceptance_standard).
narrative_ontology:cs_drift_state('f90a489e-68a1-45e3-a9e8-c37094448f37', post_columbia_caib_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f90a489e-68a1-45e3-a9e8-c37094448f37', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, program_executives).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, shuttle_program_contractors).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, flight_readiness_engineers).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, astronaut_crews).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, shuttle_program_contractors).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, probabilistic_risk_assessment_methodology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior NASA program officials chair flight readiness reviews, set the documentation standards, and designate which officials may accept residual risk. They collect launch tempo and a defensible decision record; their careers ride on the program's continuation, so leaving the framework means leaving the agency's leadership track. Post-accident accountability lands on them, but between accidents the arrangement works in their favor.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, program_executives, agenda_setter,
    institutional, biographical, constrained, national).

% Plan launch sequences and payload manifests against the acceptance framework: once a hazard is quantified and accepted, they can schedule around it. The framework gives them a stable planning surface; their alternative is indefinite slip, which ends their relevance. They do not run the acceptance machinery, but everything they plan depends on it continuing to produce acceptable entries.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, beneficiary,
    organized, biographical, constrained, national).

% Surface hazards and assign failure probabilities; their categorical judgments (this seal is not safe below 53 degrees F) enter the process only as quantified entries that an official above them may accept. Dissent beyond the quantified slot carries career cost - the post-Challenger fate of engineers who testified publicly demonstrated the price. Their professional identity is fused with making spaceflight safe, which makes exiting the framework feel like abandoning the vocation itself.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, flight_readiness_engineers, payer,
    moderate, biographical, identity_locked, national).

% Bear the physical residual risk that accepting officials sign off on their behalf. Individually an astronaut can decline a flight assignment, but doing so ends a flying career and invites replacement; collectively they held no standing seat in flight readiness reviews. Their consent is mediated entirely through the acceptance chain.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, astronaut_crews, payer,
    powerless, immediate, trapped, national).

% Build and maintain the vehicle under cost-plus contracts that continue only while flights continue; the acceptance framework protects launch tempo and hence revenue. They also bear the documentation burden and post-accident liability exposure, and their engineering judgments feed the same quantification mill as NASA's own.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, shuttle_program_contractors, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, shuttle_program_contractors, payer).

% The normative standard that some hazards are unacceptable at any documented probability - the standard engineers invoke when they say a vehicle is not safe to fly rather than that its failure probability is high. The acceptance framework consumes this standard operationally: every flight proceeds over a registered objection that has been converted into an accepted number. It collects nothing and bears the erosion; it is recorded here as the doctrinal casualty, not as an actor.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).

% A would-be independent technical authority with stop-fly power, absent from flight readiness reviews for the entire interval; NASA operated its safety offices inside the program chain they were meant to check. Such an authority would contest the equivalence of documented acceptance with safety and would halt flights over unaccepted residuals. It gained a real seat only after Columbia.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, independent_safety_authority, excluded,
    institutional, generational, analytical, national).

% Academic researchers, accident investigators, and foreign-agency counterparts who study the framework's outputs: they compare PRA estimates against observed failure rates, trace how acceptance decisions were made, and publish analyses the program cannot silence. They neither collect nor pay; they see the full structure.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, external_aerospace_safety_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__actuarial_risk_acceptance, program_executives).
narrative_ontology:fixing_cost_class(rogers_commission_findings__actuarial_risk_acceptance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the multi-level risk-communication problem: converts heterogeneous qualitative engineering concerns into commensurable probability statements, gives a layered hierarchy one decision format, and produces an auditable record binding each accepted hazard to a named accepting official.
% TRANSFER_FUNCTION: Moves decision discretion over known hazards upward - from technical staff who surface them to designated officials who accept them - and moves residual physical risk onto flight crews whose participation the acceptance chain secures without direct consent; incidentally moves schedule protection to mission planners and liability cover to the program.
% ABSENT_VOICES: Three voices are structurally absent: an independent technical authority with stop-fly power (safety offices sat inside the program chain they were meant to check until post-Columbia reforms); crew representatives with standing in flight readiness reviews; and engineers holding categorical refusals, for whom the framework provides no procedural slot - their objection enters only as a number someone else may accept.
% DISAPPEARANCE_RATIONALE: Overnight removal forces immediate reorganization: categorical engineering vetoes resurface (flights halt pending redesign - the threshold sibling's world), or undocumented managerial assertion replaces acceptance and officials refuse the exposed accountability. Manifests, contractor deliverables, crew assignments, and international partner commitments all presuppose the acceptance chain; the program cannot schedule a single flight without it.
% FOUNDING_PROBLEM: Challenger exposed that risk information lost force as it climbed the hierarchy: engineers' qualitative warnings carried no procedural weight, no official was required to confront a quantified failure probability and personally accept it, and launches proceeded against known hazards.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: the Columbia Accident Investigation Board (2003) found the founding failure mode operating unchanged under this framework; Diane Vaughan's organizational analysis and Richard Feynman's appendix to the Rogers report both document that documentation-based acceptance did not dissolve the problem. No beneficiary-seat source attests the problem solved.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__actuarial_risk_acceptance, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.70: the framework's operation converts categorical safety judgment (not-safe-to-fly) into administrable quantity (failure probability x, accepted), transferring discretion over known hazards from the engineers who surface them to officials who sign - a real transfer borne by the target seats. Suppression 0.62 is authored as a raw structural property, unscaled by power or scope: procedural dominance (no slot exists for unquantifiable refusal) plus career consequence for public dissent, with a substantial internalized component (mission identification, normalization of deviance) routed to an omega. Theater 0.50: roughly half the documentation activity now performs compliance rather than informs decision - the drift toward the management_compliance_narrative sibling is visible in the rising series. Accessibility collapse 0.60: once the framework is understood as the decision form, categorical refusal has no procedural home, but escalation channels and the post-Columbia rupture kept alternatives partly alive. Resistance 0.50: persistent engineer memos, withheld concurrences, and external critique met the framework throughout the interval. All three tracked series share one time grid (t=0,3,6,9,12,15,17) so no metric row borrows another's endpoints; the interval runs in years from the Rogers report (1986) to Columbia (2003).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (program_executives) the framework is the institution's own decision machinery - the thing that makes flight schedulable and officials defensible; it presents as legitimate coordination. From the mission_planner seat it is nearly pure enablement. From the flight_readiness_engineer seat the same structure operates as digestion of their veto - their strongest output becomes an input someone else accepts. From the astronaut seat it is others signing away bodily risk through a chain they cannot reach. Same constraint, divergent per-seat types, computed by the engine from the power/exit asymmetries in the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (mission_planners, program_executives, shuttle_program_contractors) drive d toward the beneficiary end; victim declarations (flight_readiness_engineers, astronaut_crews, categorical_safety_norms) drive d toward the target end. Exit modulation sharpens this: identity_locked engineers sit nearer full-target than their nominal mobility would predict, and trapped crews sit nearest of all. categorical_safety_norms is authored agent:false - it records the doctrinal casualty the expected structural delta names and is excluded from derivation, mirroring the non-agent registry principle. National spatial scope modestly amplifies effective extraction, since verification spans multiple centers and contractors. No directionality overrides were needed: the derivation from declarations plus exit atoms captures each seat's relationship, including the executives' partial accountability exposure, which does not overturn their net beneficiary position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - risk information losing force as it climbs the hierarchy - remains live, corroborated by the Columbia Accident Investigation Board's finding that the Challenger pattern reproduced itself under this very framework. Nothing sunsets and the mandate has not outlived its function, so no mandatrophy resolution is declared. The tangled_rope classification prevents two mislabels: calling the framework pure rope ignores that its operation consumes categorical safety judgment and engineer veto authority; calling it pure snare ignores the genuine analytic achievement (PRA surfaced real failure modes and created an auditable record binding hazards to named acceptors). The rising theater series marks the live risk: if the actuarial content fully hollows into compliance performance, this constraint migrates toward the management_compliance_narrative sibling and toward piton dynamics - the temporal data tracks that migration rather than freezing the type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_assignment,
    'Which content do the Rogers findings actually establish - a quantification-and-acceptance requirement (this reading), a categorical cease-until-certified threshold, or a compliance-documentation process?',
    'Comparative textual analysis of the report''s operative language against each institutional actor''s subsequent implementation behavior; the reading each actor''s practice actually instantiated reveals the kernel''s effective content.',
    'If the engineering_absolute_threshold reading is the true content, this constraint''s epsilon referent shifts to a cessation mandate and the actuarial framework stands as a deviation from the kernel rather than its instantiation; if the management_compliance_narrative reading is true, the quantification requirement is decorative and effective extraction rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_assignment, conceptual, 'Whether this reading correctly identifies what the Rogers kernel establishes.').

omega_variable(
    pra_calibration_gap,
    'Do the failure probabilities that inform acceptance actually track reality - pre-Challenger estimates ran near 1-in-100,000 while Feynman estimated roughly 1-in-100; did post-Rogers probabilistic risk assessment close that gap?',
    'Calibration audit comparing published PRA estimates against observed fleet failure rates across the interval.',
    'If systematically optimistic, informed acceptance is vacuous - officials accept numbers that misinform - and effective extraction rises well above the authored epsilon; if calibrated, the actuarial reading''s legitimacy strengthens materially.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pra_calibration_gap, empirical, 'Whether the numbers mediating informed acceptance are truth-tracking.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is engineer-side suppression structural (procedural slots, career gates, hierarchy) or internalized (mission identification, normalization of deviance)?',
    'Post-exit trajectory of engineers who left the program: whether dissent capacity and categorical judgment persist outside the framework (structural) or attenuate (internalized).',
    'If substantially internalized, effective suppression exceeds the structural measure - the framework recruits its own enforcement from its targets, and the target seats compute as more deeply locked than exit-option atoms alone indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism among flight readiness engineers.').

omega_variable(
    coordination_extraction_separability,
    'Is the quantification requirement separable from its acceptance-ritual function - could documented probabilities discipline decisions without converting objections into acceptable entries?',
    'Compare programs that adopted PRA without acceptance-signature chains (other agencies, commercial operators): did hazard-driven flight stops occur at rates the NASA framework''s record suggests it suppressed?',
    'If separable, the acceptance ritual is extraction riding a real analytic function and the snare-drift hypothesis strengthens; if inseparable, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s analytic and ritual components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(roge_tr_t0, observed).
narrative_ontology:measurement(roge_tr_t3, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 3, 0.26).
narrative_ontology:measurement_basis(roge_tr_t3, observed).
narrative_ontology:measurement(roge_tr_t6, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 6, 0.32).
narrative_ontology:measurement_basis(roge_tr_t6, observed).
narrative_ontology:measurement(roge_tr_t9, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 9, 0.38).
narrative_ontology:measurement_basis(roge_tr_t9, observed).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 12, 0.43).
narrative_ontology:measurement_basis(roge_tr_t12, observed).
narrative_ontology:measurement(roge_tr_t15, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 15, 0.47).
narrative_ontology:measurement_basis(roge_tr_t15, observed).
narrative_ontology:measurement(roge_tr_t17, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 17, 0.5).
narrative_ontology:measurement_basis(roge_tr_t17, observed).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 0, 0.46).
narrative_ontology:measurement_basis(roge_be_t0, observed).
narrative_ontology:measurement(roge_be_t3, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 3, 0.52).
narrative_ontology:measurement_basis(roge_be_t3, observed).
narrative_ontology:measurement(roge_be_t6, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 6, 0.57).
narrative_ontology:measurement_basis(roge_be_t6, observed).
narrative_ontology:measurement(roge_be_t9, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 9, 0.61).
narrative_ontology:measurement_basis(roge_be_t9, observed).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 12, 0.64).
narrative_ontology:measurement_basis(roge_be_t12, observed).
narrative_ontology:measurement(roge_be_t15, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(roge_be_t15, observed).
narrative_ontology:measurement(roge_be_t17, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 17, 0.7).
narrative_ontology:measurement_basis(roge_be_t17, observed).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(roge_su_t0, observed).
narrative_ontology:measurement(roge_su_t3, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 3, 0.54).
narrative_ontology:measurement_basis(roge_su_t3, observed).
narrative_ontology:measurement(roge_su_t6, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 6, 0.57).
narrative_ontology:measurement_basis(roge_su_t6, observed).
narrative_ontology:measurement(roge_su_t9, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 9, 0.59).
narrative_ontology:measurement_basis(roge_su_t9, observed).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(roge_su_t12, observed).
narrative_ontology:measurement(roge_su_t15, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(roge_su_t15, observed).
narrative_ontology:measurement(roge_su_t17, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 17, 0.62).
narrative_ontology:measurement_basis(roge_su_t17, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, management_compliance_narrative).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Rogers findings' decomposes (per epsilon-invariance) into three structurally distinct constraints: a quantification-acceptance requirement (this file, epsilon 0.70), a categorical engineering threshold (a restraint on flight whose epsilon reflects what it extracts from schedule), and a compliance-documentation process (high theater, low informational content). Each carries its own beneficiaries, victims, and classification; this file links both siblings via affects_constraints. Structural position: the actuarial reading is the operational middle - the threshold reading constrains what it may accept, and the compliance narrative parasitizes its artifacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
