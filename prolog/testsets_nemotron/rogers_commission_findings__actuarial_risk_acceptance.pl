% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: rogers_commission_findings__actuarial_risk_acceptance
 *   human_readable: Rogers Commission Actuarial Risk Acceptance Standard
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   The Rogers Commission findings on the Challenger disaster are interpreted
 *   through an actuarial lens: flight operations are acceptable if failure
 *   probability is quantified, documented, and accepted by informed
 *   decision-makers. This reading instantiates a tangled_rope constraint — it
 *   provides genuine coordination for complex system governance
 *   (probabilistic risk assessment, structured decision authority) while
 *   simultaneously extracting from categorical safety norms and engineering
 *   dissent channels. Mission planners and launch decision authorities
 *   benefit from the legitimization of proceeding with known, quantified
 *   risks. The constraint requires active enforcement through
 *   institutionalized risk acceptance processes (Flight Readiness Reviews,
 *   waiver systems) and suppresses alternatives by framing categorical safety
 *   demands as impractical or uninformed.
 *
 * KEY AGENTS:
 *   - mission_planners: Primary beneficiary (institutional/arbitrage) — gain operational flexibility and schedule authority through quantified risk acceptance
 *   - program_management: Beneficiary (institutional/arbitrage) — controls the risk quantification process and the 'informed decision-maker' designation
 *   - launch_decision_authorities: Beneficiary (powerful/arbitrage) — holds final acceptance authority, structurally positioned to approve quantified risks
 *   - categorical_safety_norms: Victim (organized/constrained) — the engineering veto tradition is displaced by probabilistic acceptance criteria
 *   - engineering_dissent_channels: Victim (moderate/constrained) — technical objections must be translated into probability arguments or be overridden by management acceptance
 *   - frontline_operators: Victim (powerless/trapped) — astronauts and ground crews bear the realized risk without meaningful exit from the acceptance decision
 *   - regulatory_oversight: Observer (institutional/analytical) — monitors whether the actuarial process is rigorous or performative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.58).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.42).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.58).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Commission Actuarial Risk Acceptance Standard").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, 'ea7abbe0-8fad-493a-9fc1-620f6b78e08e').
narrative_ontology:cs_kernel_codification('ea7abbe0-8fad-493a-9fc1-620f6b78e08e', formalized).
narrative_ontology:cs_authority_grounding('ea7abbe0-8fad-493a-9fc1-620f6b78e08e', lineage).
narrative_ontology:cs_interpretation_layer_present('ea7abbe0-8fad-493a-9fc1-620f6b78e08e').
narrative_ontology:cs_reading_relation('ea7abbe0-8fad-493a-9fc1-620f6b78e08e', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('ea7abbe0-8fad-493a-9fc1-620f6b78e08e', rogers_commission_findings__management_compliance_narrative, coexists_with).
narrative_ontology:cs_axiom('ea7abbe0-8fad-493a-9fc1-620f6b78e08e', foundational, quantified_risk_acceptance_legitimate).
narrative_ontology:cs_axiom_status(quantified_risk_acceptance_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('ea7abbe0-8fad-493a-9fc1-620f6b78e08e', quantified_risk_acceptance_legitimate, instrumental).
narrative_ontology:cs_axiom('ea7abbe0-8fad-493a-9fc1-620f6b78e08e', foundational, informed_decision_maker_authority_valid).
narrative_ontology:cs_axiom_status(informed_decision_maker_authority_valid, holdable).
narrative_ontology:cs_axiom_grounding('ea7abbe0-8fad-493a-9fc1-620f6b78e08e', informed_decision_maker_authority_valid, conventional).
narrative_ontology:cs_reference_frame('ea7abbe0-8fad-493a-9fc1-620f6b78e08e', rogers_commission_actuarial_mandate).
narrative_ontology:cs_drift_state('ea7abbe0-8fad-493a-9fc1-620f6b78e08e', post_columbia_investigation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea7abbe0-8fad-493a-9fc1-620f6b78e08e', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, program_management).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, launch_decision_authorities).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, engineering_dissent_channels).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, frontline_operators).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, quantified_risk_management_framework).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, informed_consent_governance_model).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, probabilistic_safety_assessment_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control mission architecture, scheduling, and risk acceptance criteria. The actuarial standard gives them authority to proceed with quantified risks rather than waiting for categorical engineering certification. They can move between NASA centers, commercial providers, and defense programs — their skills are portable across the high-reliability organization ecosystem.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, beneficiary,
    institutional, generational, arbitrage, global).

% Administers the risk quantification process (Probabilistic Risk Assessment, Failure Modes and Effects Analysis) and controls the 'informed decision-maker' designation for Flight Readiness Reviews. They determine what risks get quantified, what assumptions feed the models, and who sits at the acceptance table. Career mobility across aerospace and defense sectors.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, program_management, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, program_management, agenda_setter).

% Hold final launch/no-launch authority. The actuarial framework legitimizes their acceptance of quantified risks — they sign the waiver, they own the decision, but the framework structures the decision as 'informed' rather than 'reckless.' Position is tied to specific program but transferable to equivalent roles.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, launch_decision_authorities, agenda_setter,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, launch_decision_authorities, beneficiary).

% The engineering tradition of 'no-go until proven safe' — categorical veto authority based on technical criteria, not probability thresholds. Displaced by the actuarial standard: their authority is converted into an input (reliability estimates) rather than a gate. Reinstatement would require institutional restructuring, not just policy change.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms, payer,
    organized, generational, constrained, global).

% Engineers who identify hazards must translate concerns into probability arguments to be heard in the actuarial framework. Dissent that cannot be quantified (unknown failure modes, qualitative judgment) is structurally marginalized. Exit options: internal reassignment, whistleblowing (career-ending), or compliance. The framework does not forbid dissent but changes its currency.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, engineering_dissent_channels, payer,
    moderate, biographical, constrained, national).

% Astronauts, test pilots, and critical ground personnel who bear the realized consequences of accepted risks. They have no voice in the risk acceptance decision — the 'informed decision-maker' set does not include them. Career identity is fused to the mission (identity_locked in practice, though exit_options coded as trapped because the constraint is the career itself). They cannot exit without abandoning their professional identity.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, frontline_operators, payer,
    powerless, biographical, trapped, global).

% Congressional committees, GAO, NASA Aerospace Safety Advisory Panel, and commercial space regulators (FAA/AST). They monitor whether the actuarial process is rigorous or performative, whether 'informed decision-makers' are genuinely informed, and whether the quantification models are valid. They can impose reforms but operate on political/administrative timelines.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, regulatory_oversight, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured governance framework for making launch decisions under irreducible uncertainty: replaces ad-hoc judgment with quantified risk assessment, defined decision authority, and documented acceptance — enabling complex multi-organizational programs to proceed without requiring categorical engineering certainty for every element.
% TRANSFER_FUNCTION: Transfers veto authority from categorical engineering standards to management acceptance of quantified risk. Mission planners and program management gain operational flexibility (schedule, cost, scope) while engineering dissent loses its gatekeeping power and frontline operators bear the realized risk without decision authority.
% ABSENT_VOICES: Frontline operators (astronauts, test pilots) are structurally excluded from the 'informed decision-maker' set — they would object to risks they must bear but cannot influence. Independent technical authorities (outside the program chain) are excluded by the 'informed' criterion which privileges program insiders. The families of crew members are excluded entirely.
% DISAPPEARANCE_RATIONALE: If the actuarial risk acceptance standard vanished overnight, launch decisions would revert to either categorical engineering veto (engineering_absolute_threshold reading) or ad-hoc management judgment without structured quantification. Programs would face schedule delays, redesign costs, and potential cancellations — the aerospace industry would reorganize around a different risk governance model.
% FOUNDING_PROBLEM: After Challenger, NASA needed a governance framework for launch decisions when engineering data was incomplete, failure modes were not fully characterized, and schedule pressure was intense. The actuarial standard promised to make implicit risk-taking explicit, structured, and accountable.
% FOUNDING_PROBLEM_CORROBORATION: The Rogers Commission itself attested the problem was real (incomplete data, schedule pressure, implicit risk-taking). NASA's own Aerospace Safety Advisory Panel (ASAP) has consistently attested the problem remains live — spaceflight still operates with unknown failure modes. However, the Columbia Accident Investigation Board (CAIB) attested that the actuarial solution had become performative: 'the quantification process had become a ritual rather than a reality.' The CAIB is an external investigator with no beneficiary stake in the actuarial framework.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__actuarial_risk_acceptance, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.58) reflects the structural transfer: mission planners gain schedule/operational flexibility while categorical safety norms lose veto power. The extraction is not zero because the actuarial framework enables proceeding with risks that a categorical standard would forbid — the 'benefit' to planners is the legitimized proceeds of that transfer. Suppression (0.42) is moderate: the constraint doesn't ban engineering dissent outright but structurally marginalizes it by requiring translation into actuarial terms. Theater ratio (0.38) captures the growing gap between the idealized 'informed decision-maker' model (genuine deliberation among equals) and the operational reality where management controls the quantification inputs and the acceptance decision. Accessibility collapse (0.45) is moderate: alternatives (categorical thresholds, independent engineering authority) remain conceptually available but are institutionally difficult to reinstate. Resistance (0.55) reflects sustained pushback from engineering cultures and external oversight bodies.
 *
 * PERSPECTIVAL GAP:
 *   From the mission planner seat (d ~ 0.15), the constraint appears as essential coordination — the only way to operate complex systems under uncertainty. From the engineering dissent seat (d ~ 0.85), the same structure operates as extraction — their veto power is converted into an advisory input that management can accept or override. From the frontline operator seat (d ~ 0.95), the constraint is near-pure extraction: they bear the realized consequence of accepted risks with zero influence on the acceptance decision. The engine computes these divergences from the declared structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (mission_planners, program_management, launch_decision_authorities) hold institutional power with arbitrage-grade exit — they control the risk quantification process and can move between programs/agencies. Victims (categorical_safety_norms, engineering_dissent_channels, frontline_operators) are structurally positioned: categorical norms are institutionalized but displaced; engineering dissent has constrained exit (internal transfer or whistleblowing); frontline operators are trapped by career and mission commitment. The 'informed decision-maker' criterion is the mechanism that concentrates directionality toward the beneficiary end — it defines the acceptance authority as the management chain, not the engineering chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Challenger: 'how to make launch decisions under irreducible uncertainty with incomplete engineering data') remains live — spaceflight still operates with unknown failure modes. However, the actuarial solution has accumulated extraction: the quantification requirement has become a ritual that legitimizes proceeding rather than a genuine integration of engineering judgment. The constraint is tangled_rope, not snare, because the coordination function (structured risk deliberation) is real and valued by all parties — but the extraction is asymmetric and growing. Mandatrophy is not resolved: the arrangement has outlived its pure coordination function but the coordination function itself has not disappeared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_framing,
    'Is the actuarial risk acceptance standard a genuine coordination mechanism for complex systems, or a constructed constraint that legitimizes proceeding with known hazards?',
    'Historical counterfactual analysis: if the Challenger decision-makers had faced a categorical engineering veto (engineering_absolute_threshold reading), would the launch have been scrubbed? Compare outcomes in similar high-reliability organizations that adopted categorical vs. actuarial standards.',
    'If constructed, the constraint operates as a tangled_rope with mission planners as beneficiaries and categorical safety as victim — extraction is the legitimization of proceeding despite known risk. If genuine coordination, the actuarial framework is the only viable governance for irreducible uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_framing, conceptual, 'Whether the risk quantification requirement is natural coordination or constructed extraction cover').

omega_variable(
    kernel_reading_structure,
    'This constraint is one reading (actuarial_risk_acceptance) of the contested rogers_commission_findings kernel. What would the sibling readings (engineering_absolute_threshold, management_compliance_narrative) change structurally?',
    'Trace the beneficiary/victim swap across readings: engineering_absolute_threshold makes engineers the agenda_setters and program management the payers; management_compliance_narrative makes compliance officers the beneficiaries and frontline operators the victims. The structural delta is the beneficiary/victim realignment.',
    'Confirms the kernel is genuinely contested — each reading instantiates a different constraint with different extraction geometry. The engine should treat them as separate constraints linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Committee frame: this reading''s structural position within the Rogers findings kernel family').

omega_variable(
    informed_decision_maker_circularity,
    'Who qualifies as an ''informed decision-maker'' and does the designation process itself concentrate authority among the beneficiaries?',
    'Analyze the historical record of launch decision authority: were engineers with dissenting views included in the ''informed decision-maker'' set, or was the set limited to program management?',
    'If the ''informed decision-maker'' circle excludes engineering dissent, the constraint''s coordination function is compromised — it becomes a mechanism for authority capture rather than genuine risk integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_decision_maker_circularity, empirical, 'Whether the ''informed decision-maker'' criterion is structurally inclusive or exclusionary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0, 0.15).
narrative_ontology:measurement(roge_tr_t8, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 8, 0.22).
narrative_ontology:measurement(roge_tr_t16, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 16, 0.28).
narrative_ontology:measurement(roge_tr_t24, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 24, 0.32).
narrative_ontology:measurement(roge_tr_t32, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 32, 0.35).
narrative_ontology:measurement(roge_tr_t40, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(roge_be_t8, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(roge_be_t16, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(roge_be_t24, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(roge_be_t32, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(roge_be_t40, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(roge_su_t8, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(roge_su_t16, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(roge_su_t24, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(roge_su_t32, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(roge_su_t40, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__actuarial_risk_acceptance, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, nasa_flight_readiness_review_process).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, commercial_spaceflight_licensing_framework).

% DUAL FORMULATION NOTE:
% This constraint and its two sibling readings form the rogers_commission_findings constraint family. Each reading decomposes the Commission's findings into a structurally distinct claim with its own ε, beneficiaries, and victims. The actuarial reading (this story) centers mission planner authority and probabilistic governance. The engineering reading centers categorical technical boundaries. The compliance reading centers process documentation. They are linked because each is cited as 'what Rogers actually established' in different institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__actuarial_risk_acceptance, institutional, 0.15).
constraint_indexing:directionality_override(rogers_commission_findings__actuarial_risk_acceptance, organized, 0.7).
constraint_indexing:directionality_override(rogers_commission_findings__actuarial_risk_acceptance, moderate, 0.85).
constraint_indexing:directionality_override(rogers_commission_findings__actuarial_risk_acceptance, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
