% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__engineering_absolute_threshold, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: rogers_commission_findings__engineering_absolute_threshold
 *   human_readable: Rogers Commission Engineering Absolute Threshold Reading
 *   domain: organizational_safety/technology_governance
 *
 * SUMMARY:
 *   The Rogers Commission investigated the Challenger disaster and concluded
 *   that the O-ring failure was known to engineers pre-launch but overridden
 *   by management schedule pressure. The Commission's primary recommendation
 *   was that flight operations must be redesigned with an absolute technical
 *   safety boundary: no launch until O-ring (and comparable critical-path
 *   failures) are redesigned and physically certified. This reading
 *   instantiates that absolute-threshold framing. The constraint is CLAIMED
 *   as mountain (a physical/technical boundary that cannot be negotiated) and
 *   the metrics describe what enforcement of that boundary looks like: very
 *   high suppression (launch cadence is halted until certification), very low
 *   extraction (the goal is crew safety, not program benefit), and minimal
 *   theater (engineering assessment is functional, not performative). The
 *   foundational premise of this reading is that some physical failure modes
 *   are unacceptable regardless of risk quantification or political will —
 *   they must be physically fixed before operations resume.
 *
 * KEY AGENTS:
 *   - engineering_veto_seat: institutional authority that holds absolute veto over Flight Readiness Reviews; if this seat says 'not ready', no launch occurs
 *   - nasa_launch_operations: bears the cost of suppressed launch cadence; politically pressured but constrained by engineering veto
 *   - political_leadership: excluded from readiness decisions under this reading; would prefer resumed launch cadence
 *   - actuarial_risk_decision_makers: would sit in the management_compliance_narrative and actuarial_risk_acceptance readings; foreclosed from this reading because the constraint asserts a pre-risk technical precondition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.15).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.92).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.15).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, mountain).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Commission Engineering Absolute Threshold Reading").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational_safety/technology_governance").

domain_priors:requires_active_enforcement(rogers_commission_findings__engineering_absolute_threshold).
domain_priors:emerges_naturally(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, 'cd7bcbe4-b6fd-44e6-890a-d232321b02ab').
narrative_ontology:cs_kernel_codification('cd7bcbe4-b6fd-44e6-890a-d232321b02ab', fixed_text).
narrative_ontology:cs_authority_grounding('cd7bcbe4-b6fd-44e6-890a-d232321b02ab', expertise).
narrative_ontology:cs_interpretation_layer_present('cd7bcbe4-b6fd-44e6-890a-d232321b02ab').
narrative_ontology:cs_reading_relation('cd7bcbe4-b6fd-44e6-890a-d232321b02ab', rogers_commission_findings__actuarial_risk_acceptance, forecloses).
narrative_ontology:cs_reading_relation('cd7bcbe4-b6fd-44e6-890a-d232321b02ab', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('cd7bcbe4-b6fd-44e6-890a-d232321b02ab', foundational, physical_failure_modes_nonnegotiable).
narrative_ontology:cs_axiom_status(physical_failure_modes_nonnegotiable, holdable).
narrative_ontology:cs_axiom_grounding('cd7bcbe4-b6fd-44e6-890a-d232321b02ab', physical_failure_modes_nonnegotiable, deontological).
narrative_ontology:cs_axiom('cd7bcbe4-b6fd-44e6-890a-d232321b02ab', foundational, engineering_veto_precedes_risk_acceptance).
narrative_ontology:cs_axiom_status(engineering_veto_precedes_risk_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('cd7bcbe4-b6fd-44e6-890a-d232321b02ab', engineering_veto_precedes_risk_acceptance, deontological).
narrative_ontology:cs_reference_frame('cd7bcbe4-b6fd-44e6-890a-d232321b02ab', absolute_technical_safety_boundary).
narrative_ontology:cs_drift_state('cd7bcbe4-b6fd-44e6-890a-d232321b02ab', contemporary_post_challenger_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cd7bcbe4-b6fd-44e6-890a-d232321b02ab', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crew_safety).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, nasa_launch_operations).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, engineering_veto_authority_in_flight_operations).
narrative_ontology:constraint_vindicates(rogers_commission_findings__engineering_absolute_threshold, physical_failure_modes_precede_risk_calculus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstract good that motivates the constraint. The reading asserts that crew survival is not a risk to be accepted but a hard boundary: no launch until physical redesign is certified. This is not an actor collecting benefit; it is the value the constraint vindicates.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, flight_crew_safety, beneficiary,
    analytical, immediate, analytical, universal).
narrative_ontology:stakeholder_non_agent(rogers_commission_findings__engineering_absolute_threshold, flight_crew_safety).

% The structural seat that adjudicates whether the O-ring redesign meets the absolute threshold. Under this reading, engineers hold veto authority over Flight Readiness Reviews: no launch occurs unless engineering certification is issued. This is not a person but an institutionalized authority structure — the authority to say 'not yet' and have it stick.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, engineering_veto_seat, agenda_setter,
    institutional, generational, analytical, universal).

% Bears the cost of the constraint: launch cadence is suppressed until O-ring redesign is physically certified and tested. They face political pressure to demonstrate progress and mission accomplishment; the constraint forces them to deprioritize schedule over safety. Their exit option is to override engineering certification (violate the constraint), which under this reading is structurally impossible without dismantling the veto authority.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, nasa_launch_operations, payer,
    institutional, biographical, constrained, national).

% Would have preferred continued launch cadence to demonstrate program success and political achievement. Under this reading they are excluded from the decision because the constraint places the decision in engineering authority, not political calculus. They can attempt to override the constraint but doing so violates the reading's core premise (engineering veto is absolute).
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, political_leadership, excluded,
    institutional, biographical, constrained, national).

% Under the management_compliance_narrative reading, this seat would adjudicate documented risk and informed acceptance. Under this engineering_absolute_threshold reading, their authority is logically foreclosed: no informed acceptance of unacceptable physics is permitted. They are not at the table because the constraint asserts a technical precondition (redesign certification) that is prior to and independent of any risk calculation.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, actuarial_risk_decision_makers, excluded,
    institutional, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a binding epistemic authority for flight operations readiness: a technical jury (engineering) whose assessment precedes and overrides political/commercial deadlines. Solves the coordination problem of preventing a principal-agent failure where launch-decision-makers ignore engineering concerns under schedule pressure.
% TRANSFER_FUNCTION: Transfers veto authority from political/commercial decision-makers to engineering authority. Crew safety is protected by making launch impossible without engineering certification. The cost is paid by the program (delayed missions, budget extensions).
% ABSENT_VOICES: Schedule-driven stakeholders and political leadership are excluded from the readiness decision under this reading. They would argue for informed-risk-acceptance frameworks (the actuarial and management readings) but are kept out by the constraint's premise that some risks are unacceptable regardless of political will.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, launch authority would revert to political/commercial decision-making and engineering veto would become advisory rather than binding. Missions would resume on shorter timelines; crew fatalities would follow if O-ring failures recurred (as happened before Rogers).
% FOUNDING_PROBLEM: The Space Shuttle Challenger disaster (January 28, 1986) revealed that engineering safety concerns were overridden by launch-schedule pressure and management hierarchy, resulting in catastrophic failure and crew death. The foundational O-ring failure was known to engineers; it was launched anyway.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission itself (external, non-NASA body) established that the founding problem was structural: 'failures in communication... between levels of the organization... resulted in a decision to launch without full understanding of the risks involved' (Rogers Report, Volume I). Independent aerospace safety analysis and engineering ethics literature corroborate that principal-agent failures in launch authority are recurrent in space programs.
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, ExtMetricName, E),
    domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rogers_commission_findings__engineering_absolute_threshold),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Suppression is extremely high (0.92) because the constraint completely halts launch operations until a physical redesign is certified — there is no marginal launch, no informed-acceptance path, no schedule override. This is not punishment; it is the enforcement of a technical boundary. Extractiveness is negligible (0.15) because the constraint's purpose is crew safety, not program/institutional benefit — the only 'extraction' is the opportunity cost to NASA of delayed missions, which is the price of the safety boundary, not the purpose. Theater ratio is minimal (0.08) because engineering assessment is genuinely evaluative: the O-ring redesign is tested, analyzed, and certified on its engineering merits, not on political optics. The measurements show all three metrics are stable over the interval: suppression, extractiveness, and theater do not drift. This is the expected pattern for a mountain — a stable technical boundary maintained by epistemic authority (engineering veto) over decades.
 *
 * PERSPECTIVAL GAP:
 *   From the engineering seat, this constraint is a hard physical boundary that cannot be negotiated: O-rings must be redesigned and certified. From the launch-operations seat, it is an indefinite suppression of schedule that must be endured until that certification is achieved. From the political leadership seat, it is an institutional constraint that prevents decisive action on timeline. The engine computes these perspectives from the structural data: engineering holds veto authority (d near full beneficiary end for safety, analytical position); launch operations pays the schedule cost (d near target end); political leadership is excluded and experiences the constraint as a loss of authority (varies depending on coded power). The claim/metric independence principle applies: this is CLAIMED as mountain and the metrics describe enforcement of a natural/technical boundary; the engine determines whether the computed classification matches the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, no stakeholder is 'fully target' in the extractive sense because the constraint is not extractive — it is suppressive of launch operations in service of a technical safety boundary. Engineering authority is the beneficiary of the constraint (crew safety is the good it vindicates); launch operations is the payer (cadence cost); political leadership is excluded. The directionality for engineering approaches 0.0 (full beneficiary of safety outcome), for launch operations approaches 1.0 (full target of suppression cost), for political leadership reflects exclusion (authority removed from readiness decision). No override is needed; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading rejects mandatrophy: the founding problem (principal-agent failure in launch authority leading to crew death) remains live, the constraint (engineering veto) directly addresses it by preventing that failure, and the founding problem's status is 'live' not 'dead'. A mandatrophy reading would arise only if the founding problem were solved (crew safety sufficiently assured by redesign) and the engineering veto were kept in place as institutional theater. The measurement series shows no drift toward theater; the veto remains functionally necessary. No mandatrophy flag is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_versus_engineered_boundary,
    'Is the O-ring constraint a discovery of physical law (the O-ring material cannot perform below a certain temperature regardless of engineering judgment) or an engineered threshold (engineers set a redesign requirement based on post-disaster risk assessment)?',
    'Historical and technical analysis: did Rogers Commission findings establish a new physical discovery, or did they establish an engineering decision to rebuild the system to a higher safety margin? The distinction determines whether the constraint is truly natural or constructed.',
    'If the constraint is a physical discovery, it is a true mountain (unacceptable regardless of context). If it is an engineered redesign threshold (set by human judgment on how safe is safe enough), it carries engineered authority, not natural necessity, and is more accurately a tangled_rope (engineers coordinating on a binding technical standard).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_versus_engineered_boundary, empirical, 'Whether the O-ring constraint reflects immutable physics or an engineering design choice.').

omega_variable(
    veto_authority_stability,
    'Does engineering veto authority persist as decision-making power, or does it gradually decay into advisory input once the initial redesign is certified and politics normalizes?',
    'Observation of subsequent space program readiness reviews: does engineering retain absolute veto on new technical concerns, or does flight-readiness authority shift back toward management/political calculus?',
    'If veto decays into advisory, the constraint transitions from mountain to piton (theater-maintained institutional remnant of the original veto). If veto holds stable, the constraint remains mountain-class or tangled_rope depending on the first omega''s resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_authority_stability, empirical, 'Long-term stability of engineering veto as binding authority.').

omega_variable(
    suppression_cost_asymmetry,
    'Is the suppression of launch cadence borne symmetrically across the program, or is it asymmetrically concentrated on particular constituencies (e.g., contractors, flight crews, political leadership)?',
    'Budget analysis, schedule impact study, and stakeholder testimony: who bears the financial and reputational cost of delayed missions?',
    'Symmetric suppression supports the mountain reading (everyone pays the price of the safety boundary). Asymmetric suppression (concentrated on contractors or political leadership while crew safety gains are universal) would suggest the constraint is better modeled as a tangled_rope where crew benefits from asymmetric suppression of others'' schedule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_cost_asymmetry, empirical, 'Distribution of suppression costs across stakeholders.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the engineering_absolute_threshold reading logically foreclose the actuarial_risk_acceptance reading, or do they coexist as genuinely distinct decision frameworks?',
    'Logical analysis: can a single institutional actor simultaneously hold (a) an absolute threshold requiring physical redesign AND (b) an informed-acceptance framework that permits flying with known risks if the probability is quantified? Do these premises logically contradict each other or merely compete?',
    'If they logically contradict (a single veto authority cannot both enforce an absolute boundary AND permit informed acceptance of unacceptable physics), the relation is ''forecloses''. If different institutional actors hold different readings (engineering enforces absolute threshold, management seeks risk quantification), the relation is ''coexists_with''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between absolute threshold and risk-acceptance readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(roge_tr_t0, observed).
narrative_ontology:measurement(roge_tr_t6, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 6, 0.06).
narrative_ontology:measurement_basis(roge_tr_t6, observed).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 12, 0.07).
narrative_ontology:measurement_basis(roge_tr_t12, observed).
narrative_ontology:measurement(roge_tr_t18, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 18, 0.08).
narrative_ontology:measurement_basis(roge_tr_t18, observed).
narrative_ontology:measurement(roge_tr_t24, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 24, 0.08).
narrative_ontology:measurement_basis(roge_tr_t24, observed).
narrative_ontology:measurement(roge_tr_t30, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(roge_tr_t30, observed).
narrative_ontology:measurement(roge_tr_t36, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 36, 0.08).
narrative_ontology:measurement_basis(roge_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(roge_be_t0, observed).
narrative_ontology:measurement(roge_be_t6, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 6, 0.14).
narrative_ontology:measurement_basis(roge_be_t6, observed).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 12, 0.15).
narrative_ontology:measurement_basis(roge_be_t12, observed).
narrative_ontology:measurement(roge_be_t18, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 18, 0.15).
narrative_ontology:measurement_basis(roge_be_t18, observed).
narrative_ontology:measurement(roge_be_t24, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 24, 0.15).
narrative_ontology:measurement_basis(roge_be_t24, observed).
narrative_ontology:measurement(roge_be_t30, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 30, 0.15).
narrative_ontology:measurement_basis(roge_be_t30, observed).
narrative_ontology:measurement(roge_be_t36, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 36, 0.15).
narrative_ontology:measurement_basis(roge_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0, 0.9).
narrative_ontology:measurement_basis(roge_su_t0, observed).
narrative_ontology:measurement(roge_su_t6, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 6, 0.91).
narrative_ontology:measurement_basis(roge_su_t6, observed).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 12, 0.92).
narrative_ontology:measurement_basis(roge_su_t12, observed).
narrative_ontology:measurement(roge_su_t18, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 18, 0.92).
narrative_ontology:measurement_basis(roge_su_t18, observed).
narrative_ontology:measurement(roge_su_t24, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 24, 0.92).
narrative_ontology:measurement_basis(roge_su_t24, observed).
narrative_ontology:measurement(roge_su_t30, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 30, 0.92).
narrative_ontology:measurement_basis(roge_su_t30, observed).
narrative_ontology:measurement(roge_su_t36, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 36, 0.92).
narrative_ontology:measurement_basis(roge_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__engineering_absolute_threshold, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__actuarial_risk_acceptance).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% The Rogers Commission kernel has three structurally distinct readings corresponding to three different interpretations of what the findings establish. All three readings interpret the same foundational events (Challenger disaster, O-ring failure, commission investigation) but instantiate different constraints: engineering_absolute_threshold (THIS file) asserts a hard physical redesign boundary; actuarial_risk_acceptance asserts a quantified-risk framework for informed acceptance; management_compliance_narrative asserts a documented-compliance process. Each reading has its own ε, beneficiary/victim structure, and enforcement mechanism. They are linked via network.affects_constraints because the Commission's authority is shared across all three readings — if one reading's epistemic authority erodes (e.g., engineering veto is overridden repeatedly), the others are affected. This is a constraint family under the ε-invariance principle: one kernel, three ε-divergent readings, three separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
