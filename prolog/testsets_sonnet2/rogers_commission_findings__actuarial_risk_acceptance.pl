% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Rogers Commission Actuarial Risk-Acceptance Reading
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Rogers Commission findings
 *   kernel: the actuarial risk-acceptance reading, under which the
 *   Commission's post-Challenger findings are read as establishing a decision
 *   procedure rather than a technical threshold or a compliance ritual. Under
 *   this reading, flight is acceptable so long as failure probability is
 *   quantified and formally accepted by informed decision-makers — the
 *   categorical engineering threshold (a separate constraint,
 *   engineering_absolute_threshold) and the compliance-narrative reading
 *   (management_compliance_narrative) are sibling constraints, not
 *   alternative measurements of this one. This story's ε is authored solely
 *   for the actuarial-acceptance arrangement as its own advocates understand
 *   it: a real coordination function (bounded, documented decision-making
 *   under uncertainty) riding alongside a real extraction (categorical safety
 *   norms and flight-crew physical exposure absorbed into a probability
 *   figure that program stakeholders benefit from accepting).
 *
 * KEY AGENTS:
 *   - mission_planners: agenda_setter/beneficiary (institutional/arbitrage) — set schedules, benefit from bounded risk framing
 *   - program_schedule_stakeholders: beneficiary (powerful/constrained) — benefit from continued cadence
 *   - categorical_safety_norms: payer (analytical/trapped) — displaced standard, no advocate
 *   - flight_crews: payer (powerless/trapped) — bear physical consequence, no decision seat
 *   - engineering_dissenters: excluded (moderate/constrained) — procedurally logged, substantively overridden
 *   - accident_investigators: observer (analytical/analytical) — retrospective assessment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.62).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.55).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.62).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Commission Actuarial Risk-Acceptance Reading").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, 'dfb9ebb1-218a-41e3-ac5c-70737f6793f5').
narrative_ontology:cs_kernel_codification('dfb9ebb1-218a-41e3-ac5c-70737f6793f5', formalized).
narrative_ontology:cs_authority_grounding('dfb9ebb1-218a-41e3-ac5c-70737f6793f5', extraction).
narrative_ontology:cs_interpretation_layer_present('dfb9ebb1-218a-41e3-ac5c-70737f6793f5').
narrative_ontology:cs_reading_relation('dfb9ebb1-218a-41e3-ac5c-70737f6793f5', rogers_commission_findings__engineering_absolute_threshold, forecloses).
narrative_ontology:cs_reading_relation('dfb9ebb1-218a-41e3-ac5c-70737f6793f5', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('dfb9ebb1-218a-41e3-ac5c-70737f6793f5', foundational, quantified_and_accepted_risk_is_sufficient_legitimacy).
narrative_ontology:cs_axiom_status(quantified_and_accepted_risk_is_sufficient_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('dfb9ebb1-218a-41e3-ac5c-70737f6793f5', quantified_and_accepted_risk_is_sufficient_legitimacy, instrumental).
narrative_ontology:cs_axiom('dfb9ebb1-218a-41e3-ac5c-70737f6793f5', secondary, engineering_veto_subordinate_to_program_acceptance).
narrative_ontology:cs_axiom_status(engineering_veto_subordinate_to_program_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('dfb9ebb1-218a-41e3-ac5c-70737f6793f5', engineering_veto_subordinate_to_program_acceptance, conventional).
narrative_ontology:cs_reference_frame('dfb9ebb1-218a-41e3-ac5c-70737f6793f5', post_challenger_reform_commitment).
narrative_ontology:cs_drift_state('dfb9ebb1-218a-41e3-ac5c-70737f6793f5', pre_columbia_operational_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dfb9ebb1-218a-41e3-ac5c-70737f6793f5', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, program_schedule_stakeholders).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, flight_crews).
narrative_ontology:constraint_vindicates(rogers_commission_findings__actuarial_risk_acceptance, informed_decision_making_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set launch schedules and program milestones. Under the actuarial reading, they can proceed with flights once a failure probability is quantified and formally accepted by designated decision-makers, regardless of whether that probability is high by engineering standards. They benefit directly: schedule pressure is relieved, program continuity is preserved, and documented probability-acceptance becomes a defensible paper trail rather than a design gate.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__actuarial_risk_acceptance, mission_planners, beneficiary).

% Contractors, launch-manifest customers, and political sponsors whose interests are served by continued flight cadence. They benefit from the actuarial framing because it converts an open-ended engineering fix into a bounded, documentable risk figure that can be signed off without halting operations.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, program_schedule_stakeholders, beneficiary,
    powerful, biographical, constrained, national).

% The prior engineering norm — that known catastrophic failure modes must be resolved before flight, not merely quantified and accepted — is displaced by this reading. It has no advocate with power to resist reinterpretation; it is simply the standard that stops functioning as a hard gate once probability-and-acceptance becomes the operative test.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_norms, payer,
    analytical, civilizational, trapped, national).

% Bear the physical consequence of any flight approved under a documented-and-accepted failure probability. They have no seat in the acceptance decision, cannot independently verify the quantification, and cannot decline a mission once assigned without career-ending consequence. Their exposure is the direct, irreversible cost of the actuarial framing being wrong or optimistic.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, flight_crews, payer,
    powerless, immediate, trapped, national).

% Engineers who raised the O-ring temperature sensitivity issue and argued for a hard no-fly threshold. Under the actuarial reading their objection is procedurally satisfied once their probability estimate is logged and overridden by informed decision-makers further up the chain — their technical judgment is captured as an input to the acceptance record rather than a veto.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, engineering_dissenters, excluded,
    moderate, biographical, constrained, national).

% Retrospectively examine whether the documented-probability-and-acceptance process functioned as genuine informed consent or as a liability-shifting ritual performed after the risk had already been normalized through repeated flights without incident.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__actuarial_risk_acceptance, accident_investigators, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:fixing_cost_class(rogers_commission_findings__actuarial_risk_acceptance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal mechanism for continuing complex, high-stakes operations under known, non-zero failure probabilities by requiring that the probability be quantified and explicitly accepted by designated decision-makers, rather than requiring elimination of all identified risk before proceeding.
% TRANSFER_FUNCTION: Moves the authority to halt operations away from engineers holding a categorical safety threshold and toward program-level decision-makers who can accept quantified risk; moves physical exposure toward flight crews and moves schedule and reputational benefit toward program stakeholders.
% ABSENT_VOICES: Flight crews have no formal seat in the risk-acceptance decision despite bearing its entire physical consequence. Engineering dissenters who favored a hard threshold are procedurally included (their estimates are logged) but substantively excluded (their conclusion is overridden, not adopted).
% DISAPPEARANCE_RATIONALE: If the actuarial risk-acceptance framing were removed and replaced by a categorical engineering threshold, flights with any documented catastrophic failure mode would halt until redesign, program schedules would slip substantially, and the decision authority would shift back to engineering veto rather than program-level sign-off — a materially different operating regime.
% FOUNDING_PROBLEM: In the aftermath of the Challenger disaster, the Rogers Commission needed to establish some operative standard for when flight could resume: an absolute technical threshold was one option, but the actuarial reading instead formalized a decision procedure — document the failure probability, have it accepted by informed decision-makers — that lets operations continue without requiring the underlying failure mode to be eliminated.
% FOUNDING_PROBLEM_CORROBORATION: Program management and mission planners attest the founding problem (unmanaged, undocumented risk-taking) is solved by the acceptance procedure itself. Independent accident investigators and safety historians attest the underlying founding problem — flying with a known, poorly-bounded catastrophic failure mode — was never actually resolved by documentation and sign-off alone, citing the pre-Challenger normalization-of-deviance pattern the same commission also documented.
narrative_ontology:disappearance_verdict(rogers_commission_findings__actuarial_risk_acceptance, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__actuarial_risk_acceptance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__actuarial_risk_acceptance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__actuarial_risk_acceptance, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__actuarial_risk_acceptance, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.62) reflects that the actuarial framing converts what was previously a hard engineering gate into a negotiable, documentable acceptance event — a real transfer of decision authority and risk exposure, not merely a measurement convention. Suppression (0.55) is moderate: the mechanism does not forcibly silence dissent, but it structurally reroutes dissent into a logged-and-overridden input rather than a veto, which functions as suppression of consequence even without suppression of speech. Theater ratio rises across the interval (0.2 to 0.4) because, absent a subsequent catastrophic failure, the documented-acceptance ritual increasingly substitutes for renewed independent risk assessment — each successful flight cycle reinforces the acceptance record as sufficient, a Goodhart-style substitution of the proxy (documented acceptance) for the goal (actual risk reduction).
 *
 * DIRECTIONALITY LOGIC:
 *   Mission planners and program stakeholders are structural beneficiaries: they retain schedule control and gain a defensible risk-acceptance record, so their derived directionality sits near the beneficiary end. Flight crews are the clearest targets: trapped exit, powerless, immediate horizon, and zero seat in the acceptance decision — d sits near the full-target end regardless of the formal quantification exercise performed above them. Categorical safety norms are listed as a victim in the structural sense required by this reading (a displaced standard, not an actor) — it is included to satisfy the tangled_rope beneficiary/victim gate honestly, representing the norm's displacement rather than a compensable agent's loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The actuarial reading is not automatically extractive: quantified, accepted risk is a legitimate decision procedure for genuinely irreducible-risk operations (spaceflight cannot be made zero-risk). The tangled_rope classification — rather than snare — reflects that a real coordination function exists (bounded decision-making replaces paralysis or denial) alongside the asymmetric extraction (flight crews absorb consequence without proportional voice). Collapsing this into a pure snare would erase the genuine coordination problem the procedure solves; collapsing it into a pure rope would erase the flight-crew exposure and the norm displacement documented above.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actuarial_vs_engineering_kernel_reading,
    'Do the Rogers Commission findings actually license an actuarial acceptance standard, or does that reading retroactively rationalize continued flight that the Commission''s own report would have foreclosed under a threshold reading?',
    'Close textual and contextual analysis of the Commission''s report language and recommendations against subsequent NASA decision records; comparison with how the engineering_absolute_threshold and management_compliance_narrative readings characterize the same findings.',
    'If the threshold reading is the more faithful one, the actuarial reading is itself an extractive reinterpretation layered on top of the findings rather than a legitimate application of them — raising this story''s own ε further and shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_vs_engineering_kernel_reading, conceptual, 'Whether the actuarial reading is a faithful instantiation of the kernel or a beneficiary-favoring reinterpretation.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the three sibling readings (actuarial, engineering-threshold, compliance-narrative) diverge — is it in what counts as evidence of acceptable risk, or in who holds authority to declare risk acceptable?',
    'Structural comparison of the three constraint stories'' beneficiary/victim declarations and decision-authority stakeholder seats.',
    'If the divergence is primarily about authority location rather than evidentiary standard, the actuarial reading''s extraction is better characterized as an authority transfer (engineering veto to program sign-off) than as a risk-quantification innovation per se.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Locating the structural disagreement among the kernel''s readings.').

omega_variable(
    post_challenger_recurrence_evidence,
    'Did the actuarial acceptance procedure, as institutionalized after Rogers, prevent or fail to prevent a structurally similar accident (e.g., Columbia/foam strike risk acceptance)?',
    'Comparison with the Columbia Accident Investigation Board findings on foam-strike risk normalization under a documented-acceptance regime.',
    'A documented recurrence under the same procedural logic would substantially strengthen the case that theater_ratio was understated and that the acceptance ritual substitutes for rather than achieves risk reduction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_challenger_recurrence_evidence, empirical, 'Whether the actuarial acceptance procedure recurred in a subsequent accident with the same structural signature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0, 0.2).
narrative_ontology:measurement(roge_tr_t5, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 5, 0.26).
narrative_ontology:measurement(roge_tr_t10, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 10, 0.31).
narrative_ontology:measurement(roge_tr_t15, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 15, 0.34).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 20, 0.37).
narrative_ontology:measurement(roge_tr_t25, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 25, 0.39).
narrative_ontology:measurement(roge_tr_t30, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(roge_be_t5, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(roge_be_t10, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(roge_be_t15, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(roge_be_t25, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(roge_be_t30, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(roge_su_t5, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(roge_su_t10, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(roge_su_t15, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(roge_su_t25, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 25, 0.54).
narrative_ontology:measurement(roge_su_t30, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__actuarial_risk_acceptance, 0.1).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, management_compliance_narrative).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the single natural-language label 'Rogers Commission findings' per the ε-invariance principle. engineering_absolute_threshold treats the findings as licensing a hard no-fly gate (low ε, categorical); management_compliance_narrative treats them as licensing a documentation-sufficiency process (moderate ε, procedural); this story treats them as licensing quantified-and-accepted risk continuation (this story's own ε, actuarial). All three are linked bidirectionally in intent; this file declares the downstream edges to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
