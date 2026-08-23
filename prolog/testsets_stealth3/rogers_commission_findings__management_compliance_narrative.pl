% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__management_compliance_narrative, []).

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
 *   constraint_id: rogers_commission_findings__management_compliance_narrative
 *   human_readable: Post-Challenger Flight-Readiness Compliance Process (Management Reading of the Rogers Findings)
 *   domain: organizational/institutional-governance
 *
 * SUMMARY:
 *   After the Challenger accident, the Rogers Commission report entered NASA
 *   as a contested kernel: a single persisting commitment about what the
 *   findings established, read differently by different parties. This story
 *   instantiates ONE reading only, the management_compliance_narrative: the
 *   findings establish a compliance process in which demonstrating documented
 *   risk awareness and mitigation effort is sufficient warrant to proceed.
 *   Under this reading the operative reform is procedural: hazard-analysis
 *   packages, review boards, signed risk acknowledgments. Management retains
 *   launch authority with documented rationale; program continuity is
 *   preserved; engineering concurrence becomes an advisory input rather than
 *   a stop condition. The claim and the metrics are independent authored
 *   facts: the claimed type is what I believe structurally true of this
 *   reading's arrangement (a hybrid with a genuine coordination function and
 *   asymmetric authority conversion), while the metrics describe its actual
 *   operation, which degraded over the interval. The sibling readings
 *   (engineering_absolute_threshold, actuarial_risk_acceptance) are separate
 *   constraint stories with their own epsilon values and are linked only
 *   through the network edges; nothing about them is averaged into this file.
 *
 * KEY AGENTS:
 *   - nasa_program_management: agenda-setter (institutional/arbitrage) — administers the review process, defines what documentation suffices, retains launch authority
 *   - safety_review_bureaucracy: beneficiary (institutional/identity_locked) — staffs the panels and offices the process created; collects mandate, budget, careers
 *   - contractor_flight_programs: beneficiary with payer underside (powerful/constrained) — collects continued launch cadence and contracts; pays documentation production costs
 *   - field_engineers: primary target (moderate/constrained) — produce the risk documentation; former veto holders now hold advisory seats
 *   - astronaut_corps: target (organized/identity_locked) — flies the residual risk the documentation acknowledges rather than removes
 *   - independent_technical_authority: excluded seat (powerful/trapped) — the stop-work authority the process never created
 *   - external_investigation_bodies: analytical observer (institutional/analytical) — congressional committees, GAO, accident boards examining the process from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.66).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.68).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.66).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Post-Challenger Flight-Readiness Compliance Process (Management Reading of the Rogers Findings)").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational/institutional-governance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '14322208-95cc-444a-aefb-3636fb4d96dd').
narrative_ontology:cs_kernel_codification('14322208-95cc-444a-aefb-3636fb4d96dd', fixed_text).
narrative_ontology:cs_authority_grounding('14322208-95cc-444a-aefb-3636fb4d96dd', lineage).
narrative_ontology:cs_interpretation_layer_present('14322208-95cc-444a-aefb-3636fb4d96dd').
narrative_ontology:cs_reading_relation('14322208-95cc-444a-aefb-3636fb4d96dd', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('14322208-95cc-444a-aefb-3636fb4d96dd', rogers_commission_findings__actuarial_risk_acceptance, influences).
narrative_ontology:cs_axiom('14322208-95cc-444a-aefb-3636fb4d96dd', foundational, documented_risk_awareness_suffices_to_proceed).
narrative_ontology:cs_axiom_status(documented_risk_awareness_suffices_to_proceed, holdable).
narrative_ontology:cs_axiom_grounding('14322208-95cc-444a-aefb-3636fb4d96dd', documented_risk_awareness_suffices_to_proceed, empirically_contingent).
narrative_ontology:cs_axiom('14322208-95cc-444a-aefb-3636fb4d96dd', secondary, engineering_assessment_is_advisory_not_binding).
narrative_ontology:cs_axiom_status(engineering_assessment_is_advisory_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('14322208-95cc-444a-aefb-3636fb4d96dd', engineering_assessment_is_advisory_not_binding, conventional).
narrative_ontology:cs_reference_frame('14322208-95cc-444a-aefb-3636fb4d96dd', documented_risk_acknowledgment_sufficiency).
narrative_ontology:cs_drift_state('14322208-95cc-444a-aefb-3636fb4d96dd', post_columbia_caib_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('14322208-95cc-444a-aefb-3636fb4d96dd', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, nasa_program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, safety_review_bureaucracy).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, contractor_flight_programs).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, field_engineers).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, astronaut_corps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, contractor_flight_programs).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, procedural_accountability_doctrine).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, chain_of_command_launch_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the launch-readiness review process: defines what documentation counts as sufficient risk awareness, chairs the boards that accept or reject the packages, and signs the flight-readiness certificates. Final launch authority remains here throughout; the process adds steps this office controls rather than relocating the decision. Schedule commitments to headquarters, Congress, and international partners flow through its hands, giving it durable reasons to keep the paper moving and the calendar intact.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, nasa_program_management, agenda_setter,
    institutional, generational, arbitrage, national).

% Staffs the review panels, safety offices, and documentation-standards groups created to receive and adjudicate risk packages. Its budget, headcount, and institutional purpose exist because the documentation requirement exists, and members' careers advance through mastery of the review process itself. Over the interval the office's self-concept has fused with administering the paperwork, making its members poor candidates for recommending the process's own retirement.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, safety_review_bureaucracy, beneficiary,
    institutional, generational, identity_locked, national).

% Builds and operates the flight hardware under continuing contracts that depend on flights happening. Collects continued launch cadence and contract renewal; pays the cost of producing risk-documentation packages, certifying analyses, and absorbing schedule slips when reviews demand more paper. Walking away would forfeit decades of accumulated contract revenue and specialized standing, so it funds the process it also grumbles about.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, contractor_flight_programs, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__management_compliance_narrative, contractor_flight_programs, payer).

% Produce the hazard analyses, anomaly reports, and mitigation justifications the process consumes, and sit in reviews answering questions about their own documentation. Before 1986 their concurrence or objection could stop a countdown; the post-accident process records their assessments as inputs while the launch decision stays above them. Escalating past the process means going outside the agency with career consequences; staying inside means documenting risks they cannot force anyone to eliminate.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, field_engineers, payer,
    moderate, biographical, constrained, national).

% Flies the missions whose residual risks the documentation acknowledges rather than removes. Holds seats in some review forums and can raise concerns, but has no stop authority over a flight its members are assigned to. Professional identity is built on accepting flight risk in exchange for the mission; declining an assignment is effectively leaving the corps, so the exit door opens onto the loss of the thing the identity is made of.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, astronaut_corps, payer,
    organized, biographical, identity_locked, national).

% A standing technical authority with independent stop-work power over flight decisions, the seat investigative boards repeatedly recommended and the compliance process never created. It has no chair in the launch-review structure; the people who would occupy it remain embedded in the line organizations whose schedules the reviews serve. It enters the conversation only when an external board convenes after a failure.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, independent_technical_authority, excluded,
    powerful, generational, trapped, national).

% Congressional committees, the GAO, and ad hoc accident boards examine the process from outside, subpoena records, and publish findings on whether the documentation regime changed decisions. They can recommend restructuring but do not run launches; their leverage arrives episodically, concentrated in the aftermath of failures.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, external_investigation_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__management_compliance_narrative, nasa_program_management).
narrative_ontology:fixing_cost_class(rogers_commission_findings__management_compliance_narrative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a mandatory shared record of known hazards and mitigation status across every organizational level involved in a launch: contractors document analyses, review boards adjudicate completeness, and managers sign acknowledgments, so that no flight proceeds without a written trail connecting engineering findings to the launch decision.
% TRANSFER_FUNCTION: Moves decision-authority retention upward: engineering concurrence is converted from a binding stop condition into an advisory input recorded in the package. Moves labor time from design and test work into documentation production. Schedule control and launch authority stay with program management throughout.
% ABSENT_VOICES: An independent technical authority with stop-work power is the missing seat: accident boards recommended it, the compliance process never seated it, and the engineers closest to the hardware hold advisory rather than binding voices. External investigators enter only after failures, which is precisely when their testimony can no longer alter the decision under review.
% DISAPPEARANCE_RATIONALE: Overnight removal would strip the launch decision of its documentary trail and legitimacy shield: flight-readiness signings would revert to informal verbal judgments, the safety-review bureaucracy would lose its mandate and reason to exist, contractors would shed the documentation burden, and program management would face launch decisions with no recorded risk acknowledgment behind them. The decision structure the process organizes would rearrange around whatever replaced it, which is why every post-accident board rebuilt some version of it rather than letting it lapse.
% FOUNDING_PROBLEM: The Rogers Commission found that the Challenger launch proceeded despite known O-ring vulnerability because risk information failed to travel intact up the hierarchy and no formal record compelled decision-makers to confront what engineers had reported. The compliance process was built to solve undocumented, unstructured risk communication across organizational levels.
% FOUNDING_PROBLEM_CORROBORATION: Program management attests that the recording-and-review problem is solved and the process works. Corroboration from outside the benefiting parties cuts the other way: the Columbia Accident Investigation Board, convened after the compliance regime had operated for seventeen years, found the same failure signature (a known hazard documented and accepted across many flights) and concluded the paperwork apparatus had grown without changing decision authority; Diane Vaughan's organizational analysis of the launch decision and Feynman's Rogers-Commission appendix likewise attest that documentation alone did not repair the underlying decision structure. No neutral source attests the founding problem fully resolved.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__management_compliance_narrative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__management_compliance_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: the process converts binding engineering authority into advisory documentation and transfers substantial labor time into package production, while the decision right stays where it was; it stops short of higher values because the shared risk record has real informational value across hierarchy levels. Suppression 0.68: persistence depends on keeping the veto suppressed, achieved procedurally (escalation past the process carries career cost, dissent is absorbed as another documented input); suppression is authored as a raw structural property and is not scaled by power or scope anywhere in the engine's arithmetic. Theater 0.58: a growing share of activity is sign-off performance that does not alter decisions, the pattern the Columbia board described as paperwork proliferating while the decision structure stayed fixed. Accessibility_collapse 0.50: within the compliance frame the alternative of no-flight-without-certified-fix is largely foreclosed, but escalation channels, whistleblowing, and external boards keep partial alternatives alive. Resistance 0.45: engineer escalations, whistleblower episodes, and successive external investigations constitute sustained but non-paralyzing friction. The temporal series run on one shared seven-point grid (years since 1986) so every tracked metric is authored at every examined time point; suppression_requirement is tracked deliberately because the story traces enforcement-capacity change, the machinery building up and hardening from 1986 onward, with a step around year 18 corresponding to the post-Columbia layering. Base_extractiveness rises monotonically, the extraction-accumulation signature of coordination functions accreting rent-seeking layers; no cyclical oscillation is asserted.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from identical documents. From the agenda-setter seat the arrangement is an accountability process it designed, administers, and can pass through on schedule; from the field-engineer seat it is a structure that consumes their analyses, records their objections without binding anyone, and returns the launch decision untouched; from the astronaut seat it is an apparatus that acknowledges their risk in writing rather than removing it. Coalition potential matters here: engineers and astronauts are separately moderate and organized, but a joint escalation front (engineering findings carried by crew refusal risk) is the one combination the process has no procedural answer for, which is why resistance is nonzero despite constrained individual exits. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for nasa_program_management (collects retained authority and schedule control, designs the rules it obeys, arbitrage-grade exit), for safety_review_bureaucracy (collects mandate and careers, identity-fused with administering the process), and mid-low for contractor_flight_programs (collects cadence and contract renewal but pays real documentation costs, hence the dual declaration). Victim declarations drive high directionality for field_engineers (constrained exit: career dependence, escalation cost) and highest for astronaut_corps (identity-locked: refusing flight means leaving the corps, so the trap is internal as well as structural). The excluded and observer seats carry no beneficiary or victim declaration and feed the consensus-provenance picture rather than the directionality arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the beneficiary/victim split this arrangement invites two symmetrical misreadings: as pure coordination (an accountability standard any safety program needs) or as pure extraction (paperwork tyranny serving nobody). The declarations keep both faces legible: the coordination function is genuine, a mandatory cross-level record of hazards and mitigations that even the harshest external critics wanted strengthened, and the extraction is equally genuine, the quiet conversion of a binding engineering stop into an advisory checkbox. On the genealogy side, the founding problem (undocumented, unstructured risk communication) is partially addressed, hence status contested rather than dead, and the arrangement persists with a rising share of its activity theatrical; it resists the piton reading because gains still concentrate demonstrably in the agenda-setter seat, which is exactly the capture signature the receipt surface records. The status-contested-plus-world-rearranges pairing flags the arrangement for zombie-watch without asserting the flag's conclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rogers_reading_instantiation_ambiguity,
    'Does the standing post-Challenger arrangement actually implement the compliance reading of the Rogers findings, or does it carry the compliance label while operating closer to one of the sibling readings?',
    'Compare implemented flight-readiness procedures against each reading''s operative rule: count launches where documented risk packages existed and flight proceeded (compliance-consistent), launches halted pending hardware certification (threshold-consistent), and decisions citing quantified failure probabilities accepted by named deciders (actuarial-consistent).',
    'If the arrangement predominantly instantiates a sibling reading, this story''s epsilon referent is mislabeled and the classification belongs to the sibling story; the family network edges would reroute accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rogers_reading_instantiation_ambiguity, conceptual, 'Which reading of the Rogers kernel the standing arrangement instantiates.').

omega_variable(
    documentation_sufficiency_empirical_status,
    'Does documented risk awareness actually reduce accident probability, or does the documentation ritual normalize deviation, with recurring risk signatures reviewed and accepted as familiar?',
    'Longitudinal comparison of documented-anomaly recurrence versus outcome across the shuttle and successor programs; Vaughan''s normalization-of-deviance analysis and the Columbia board''s finding that foam strikes were documented and accepted across dozens of flights supply the test cases.',
    'If documentation normalizes rather than constrains, the coordination half of the hybrid reading weakens, measured extractiveness understates the true transfer, and the reading drifts toward pure extraction with program management as capturer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_sufficiency_empirical_status, empirical, 'Whether the compliance documentation constrains decisions or launders them.').

omega_variable(
    veto_conversion_reversibility,
    'Is the conversion of engineering concurrence from binding to advisory a settled structural feature, or reversible through the independent technical authority reforms adopted after Columbia?',
    'Track post-2003 technical authority structures: whether any documented case exists of a technical authority stopping or modifying a flight against program-management preference, and whether that authority survived leadership turnover.',
    'If veto power is durably restored, the asymmetry halves, effective extraction falls toward coordination cost, and the reading migrates toward a coordination-dominant classification; if the reforms are themselves absorbed into the documentation apparatus, extraction consolidates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_conversion_reversibility, empirical, 'Reversibility of the engineering-veto conversion.').

omega_variable(
    deference_internalization_component,
    'How much of the observed engineer acquiescence is structural (no binding channel exists) versus internalized (professional socialization toward document-and-defer)?',
    'Post-exit trajectory: compare engineers'' willingness to escalate in organizations with binding technical authority after leaving the program; interview studies of reviewers who escalated internally versus those who signed off.',
    'If a large share is internalized, effective suppression exceeds the structural measure because the constraint travels with the engineers, and remedies limited to adding channels will underperform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deference_internalization_component, empirical, 'Structural versus internalized share of engineer deference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rcf_mgmt_compliance_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_tr_t0, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_tr_t6, rogers_commission_findings__management_compliance_narrative, theater_ratio, 6, 0.3).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_tr_t6, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_tr_t12, rogers_commission_findings__management_compliance_narrative, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_tr_t12, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_tr_t18, rogers_commission_findings__management_compliance_narrative, theater_ratio, 18, 0.46).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_tr_t18, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_tr_t24, rogers_commission_findings__management_compliance_narrative, theater_ratio, 24, 0.52).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_tr_t24, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_tr_t30, rogers_commission_findings__management_compliance_narrative, theater_ratio, 30, 0.56).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_tr_t30, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_tr_t36, rogers_commission_findings__management_compliance_narrative, theater_ratio, 36, 0.58).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(rcf_mgmt_compliance_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_be_t0, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_be_t6, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 6, 0.46).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_be_t6, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_be_t12, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 12, 0.53).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_be_t12, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_be_t18, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 18, 0.59).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_be_t18, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_be_t24, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_be_t24, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_be_t30, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 30, 0.64).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_be_t30, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_be_t36, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 36, 0.66).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(rcf_mgmt_compliance_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_su_t0, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_su_t6, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 6, 0.5).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_su_t6, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_su_t12, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 12, 0.57).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_su_t12, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_su_t18, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 18, 0.62).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_su_t18, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_su_t24, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 24, 0.65).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_su_t24, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_su_t30, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 30, 0.67).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_su_t30, observed).
narrative_ontology:measurement(rcf_mgmt_compliance_su_t36, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 36, 0.68).
narrative_ontology:measurement_basis(rcf_mgmt_compliance_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the Rogers kernel per the epsilon-invariance principle: 'what the Rogers findings established' is a single colloquial label covering three structurally distinct claims with materially different epsilon values. The compliance reading (this story) authors moderate-high extractiveness over the standing documentation regime; the threshold reading authors a near-zero-extraction hard boundary claim; the actuarial reading authors a quantification-and-acceptance requirement with its own beneficiary structure (decision-makers collecting informed-consent legitimacy). The upstream/downstream structure runs from this reading outward: the compliance apparatus built the documentation infrastructure and the legitimacy precedent within which the other two readings operate, which is why this story declares influence-type pressure toward the actuarial sibling and coexistence with the threshold sibling. All three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
