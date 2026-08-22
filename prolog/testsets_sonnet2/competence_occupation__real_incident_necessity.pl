% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real-Incident-Necessity Reading of the Competence Kernel
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This story instantiates one contested reading of the 'competence
 *   occupation' kernel in high-reliability organizations: the claim that only
 *   having actually lived through a catastrophic incident can furnish the
 *   authentic conditions needed to genuinely occupy the competence kernel —
 *   that is, to actually possess (not merely be certified as possessing) the
 *   readiness the organization needs. This reading is generated on its own
 *   terms, ε-invariant, without folding in the sibling readings
 *   (simulation_sufficiency, hybrid_occupation) as alternatives inside this
 *   file. As authored, the reading produces a structurally unresolvable
 *   maintenance problem: the standard it sets cannot be met without the
 *   catastrophe it exists to prevent, so every operator and supervisor lives
 *   under a competence claim that can never be honestly closed by ordinary
 *   organizational means. There is no viable beneficiary — nobody profits
 *   from catastrophes being the measuring stick — which routes this reading
 *   toward extraction-without-a-collector rather than coordination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.68).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.42).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, snare).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real-Incident-Necessity Reading of the Competence Kernel").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '7fd6126b-89f2-418a-982d-4bf0459f97af').
narrative_ontology:cs_kernel_codification('7fd6126b-89f2-418a-982d-4bf0459f97af', distributed).
narrative_ontology:cs_authority_grounding('7fd6126b-89f2-418a-982d-4bf0459f97af', practice).
narrative_ontology:cs_interpretation_layer_present('7fd6126b-89f2-418a-982d-4bf0459f97af').
narrative_ontology:cs_reading_relation('7fd6126b-89f2-418a-982d-4bf0459f97af', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('7fd6126b-89f2-418a-982d-4bf0459f97af', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('7fd6126b-89f2-418a-982d-4bf0459f97af', foundational, authenticity_requires_real_stakes).
narrative_ontology:cs_axiom_status(authenticity_requires_real_stakes, holdable).
narrative_ontology:cs_axiom_grounding('7fd6126b-89f2-418a-982d-4bf0459f97af', authenticity_requires_real_stakes, empirically_contingent).
narrative_ontology:cs_axiom('7fd6126b-89f2-418a-982d-4bf0459f97af', secondary, simulated_conditions_cannot_replicate_true_stakes).
narrative_ontology:cs_axiom_status(simulated_conditions_cannot_replicate_true_stakes, holdable).
narrative_ontology:cs_axiom_grounding('7fd6126b-89f2-418a-982d-4bf0459f97af', simulated_conditions_cannot_replicate_true_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('7fd6126b-89f2-418a-982d-4bf0459f97af', lived_catastrophe_as_ground_truth).
narrative_ontology:cs_drift_state('7fd6126b-89f2-418a-982d-4bf0459f97af', contemporary_high_reliability_practice, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7fd6126b-89f2-418a-982d-4bf0459f97af', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, shift_supervisors).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, downstream_public_exposed_to_lapsed_competence).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, authenticity_of_experience_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform safety-critical work (reactor control, surgical response, wildland fire command) under a professional norm that only having lived through an actual catastrophic event confers real competence. They cannot manufacture such an event, cannot certify themselves as 'truly' competent absent one, and carry a standing, unresolvable doubt about their own readiness that no amount of drilling relieves under this reading.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_operators, payer,
    moderate, biographical, trapped, local).

% Responsible for certifying crew readiness and for organizational reporting after incidents. Under this reading, their own authority to declare a crew 'competent' is hollow until tested by a real event, which puts them in the position of either overstating confidence they cannot ground, or admitting the organization has no way to know if it is ready — both costly.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, shift_supervisors, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, shift_supervisors, agenda_setter).

% Set minimum training and certification standards built around drills, simulations, and audits — mechanisms this reading treats as inherently inauthentic. Their entire regulatory apparatus is structurally sidelined by the reading's own premise, yet they are not consulted inside the reading's logic about whether that premise is correct.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, regulatory_bodies, excluded,
    institutional, generational, analytical, national).

% Must certify organizational readiness to boards, insurers, and the public. Under this reading they inherit an unresolvable epistemic gap: they can never truthfully claim tested competence without wishing for the catastrophe that would supply the test, and cannot honestly deny the gap without undermining public confidence.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, organizational_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Study near-miss and incident data across organizations, comparing outcomes at sites with real-incident history against sites without. They document the reading's structural bind without holding a stake in either legitimating or dismantling it.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__real_incident_necessity, diffuse).
narrative_ontology:fixing_cost_class(competence_occupation__real_incident_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Names a genuine problem — simulated and procedural training may fail to reproduce the physiological stress, ambiguity, and improvisational demand of a true catastrophic event, so competence claims resting solely on drills may be overstated.
% TRANSFER_FUNCTION: Moves epistemic legitimacy away from every certification and training mechanism an organization can actually deploy (drills, simulators, refreshers, audits) toward a condition (a real catastrophic incident) that cannot be produced, scheduled, or ethically sought — leaving operators and supervisors permanently short of the standard the reading itself sets.
% ABSENT_VOICES: Regulatory bodies and simulation-technology vendors, whose entire mandate rests on drills being adequate proxies for real events, are structurally excluded from adjudicating this reading's premise even though the premise directly devalues their instruments.
% DISAPPEARANCE_RATIONALE: If this reading of the kernel disappeared, organizations would stop treating tested-by-real-catastrophe as the benchmark of competence, certification would re-anchor on achievable mechanisms (simulation, procedural reinforcement, audits), and the standing anxiety among operators and supervisors about unverifiable readiness would dissolve — training investment and regulatory posture would visibly shift.
% FOUNDING_PROBLEM: Post-incident investigations (e.g., nuclear near-misses, aviation disasters) repeatedly found that personnel who had only drilled froze, deviated from procedure, or missed cues that survivors of prior real events recognized instantly — suggesting drills miss something real events supply.
% FOUNDING_PROBLEM_CORROBORATION: Some investigators and veteran operators who lived through real incidents attest that their own reflexes changed in ways drills never produced. But training researchers and regulators — outside any incentive to inflate the value of catastrophe — counter that the observed post-incident deficits are better explained by inadequate drill fidelity and infrequent refresh cycles than by an intrinsic authenticity gap, and no controlled study isolates 'lived catastrophe' as a distinct causal factor from drill quality.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the standing cost imposed on operators and supervisors who must operate under, and periodically re-assert, a competence claim the reading's own logic says cannot be genuinely grounded absent a catastrophic event — a psychological and organizational tax paid continuously for a condition that is by design almost never met. Suppression (0.42) is moderate rather than high: no one is coercively preventing exit from this belief, but professional culture and post-incident narrative reinforcement (survivors' authority) make the belief self-sustaining. Theater ratio (0.61) is elevated and rising because organizations respond to the unresolvable gap by manufacturing proxy rituals — solemn post-incident debriefs, veteran-operator mentorship programs, 'battle scar' status hierarchies — that perform authenticity without closing it. Accessibility collapse is comparatively low (0.35): alternative framings (simulation sufficiency, hybrid approaches) remain visibly available and practiced elsewhere, which is precisely why this reading must be actively re-asserted rather than simply persisting by default. Resistance is high (0.72) because training researchers, regulators, and much of the simulation industry structurally reject the premise.
 *
 * DIRECTIONALITY LOGIC:
 *   There is no beneficiary group under this reading — that absence is itself the analytically significant finding for the FSM-style check on any 'natural' framing of competence claims. Frontline operators and shift supervisors are victims: they bear the cost of an ungroundable standard directly, in the form of persistent self-doubt, performance anxiety, and vulnerability to the narrative authority of anyone who has 'really been through it.' Downstream publics who depend on these organizations' actual readiness are secondary victims: an organization that internalizes this reading may under-invest in achievable readiness mechanisms (simulation, procedural rigor) while waiting for a real event to validate itself, degrading actual safety margins.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is a candidate mandatrophy case in the making: it names a real founding concern (drills sometimes fail to reproduce catastrophic conditions) but its own logic offers no closing mechanism — competence can never be certified as 'truly' occupied because the only sufficient test is the disaster the organization exists to prevent. Classifying this as snare (rather than mountain, despite its framing as an inevitable truth about human psychology) captures that the standard functions to extract ongoing anxiety and status-hierarchy compliance from operators without a corresponding coordination payoff, and without any party positioned to certify closure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_thesis_empirical_status,
    'Is there any controlled evidence that lived catastrophic experience produces a distinct competence advantage over high-fidelity simulation, independent of drill quality and frequency confounds?',
    'Comparative outcome studies matching organizations with real-incident-experienced personnel against organizations with equivalent drill investment but no real-incident history, controlling for drill fidelity and refresh cadence.',
    'If no distinct advantage is found, the reading collapses into a pure status/authority narrative with no coordination substance; if a genuine advantage is found even controlling for drill quality, this reading gains partial mountain-like standing for the specific residual effect, though the unresolvable-maintenance problem remains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authenticity_thesis_empirical_status, empirical, 'Whether lived-catastrophe competence advantage is real or a confound of drill quality.').

omega_variable(
    kernel_framing_choice,
    'Is the more defensible framing of this constraint ''the intrinsic value of real-incident experience'' (as authored here) or ''the social authority claimed by real-incident survivors within organizational hierarchies'' (a status-extraction framing one level up)?',
    'Trace whether competence deference tracks documented incident-response performance or simply tenure/survivor status; interview junior staff on whether veteran authority is challengeable with contrary evidence.',
    'If authority tracks status rather than demonstrated performance, this reading is better modeled as a status-hierarchy-preserving mechanism dressed in a competence-authenticity vocabulary — the extraction is more concentrated (on veteran-adjacent authority) than the diffuse ''no beneficiary'' picture assumed here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Alternative framing: intrinsic experiential value vs. survivor-status authority extraction.').

omega_variable(
    resolvability_of_the_bind,
    'Can any organizational practice (e.g., high-fidelity immersive simulation, structured psychological debrief of near-misses treated as partial incidents) substitute enough of the claimed authentic conditions to functionally close the gap this reading asserts?',
    'Track whether organizations investing heavily in near-miss analysis and immersive simulation report declining reliance on real-incident-survivor authority over time.',
    'A demonstrated substitute would support reclassifying organizational practice toward the hybrid_occupation reading; failure to find one would reinforce this reading''s unresolvable-snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resolvability_of_the_bind, empirical, 'Whether practical substitutes can close the gap this reading claims only real incidents can close.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.4).
narrative_ontology:measurement(comp_tr_t4, competence_occupation__real_incident_necessity, theater_ratio, 4, 0.45).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__real_incident_necessity, theater_ratio, 8, 0.5).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__real_incident_necessity, theater_ratio, 12, 0.54).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__real_incident_necessity, theater_ratio, 16, 0.57).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__real_incident_necessity, theater_ratio, 20, 0.59).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__real_incident_necessity, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(comp_be_t4, competence_occupation__real_incident_necessity, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(comp_be_t8, competence_occupation__real_incident_necessity, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(comp_be_t12, competence_occupation__real_incident_necessity, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(comp_be_t16, competence_occupation__real_incident_necessity, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(comp_be_t20, competence_occupation__real_incident_necessity, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(comp_be_t24, competence_occupation__real_incident_necessity, base_extractiveness, 24, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(competence_occupation__real_incident_necessity, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, identity_coordination).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the competence_occupation kernel. simulation_sufficiency claims drills suffice (lower ε, coordination-favorable, likely rope/tangled_rope); hybrid_occupation claims a contested multi-mechanism configuration suffices (moderate ε, contested coordination); real_incident_necessity (this file) claims only lived catastrophe suffices, which structurally forecloses simulation_sufficiency's premise (both cannot hold in the same certifying framework) while placing downstream pressure on hybrid_occupation's configuration debate (if real incidents are held necessary, no hybrid configuration of achievable mechanisms can ever be declared sufficient). Each story carries its own ε per the ε-invariance principle; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
