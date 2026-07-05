% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real-Catastrophe-Only Doctrine of Competence Validity
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the 'real_catastrophe_only' reading of the
 *   competence_exercise_validity kernel: the claim that simulation, however
 *   rigorous, cannot validate competence — only a genuine catastrophic event
 *   exercises the relevant capacities under conditions that cannot be
 *   manufactured. Under this reading, organizational safety records reflect
 *   either luck or redundant engineering margin rather than proven human
 *   competence, and every peacetime certification is provisional pending the
 *   disaster that has not yet happened. The doctrine concentrates authority
 *   in those who have survived a real event and generates recurring demand
 *   for post-incident review services, while permanently denying
 *   simulation-trained operators and never-yet-tested organizations any path
 *   to demonstrated readiness.
 *
 * KEY AGENTS:
 *   - veteran_incident_commanders: primary beneficiary, holds unearnable-by-others authority
 *   - post_incident_consultancies: secondary beneficiary, profits from recurring unfalsifiability
 *   - frontline_operators_between_incidents: primary target, career stagnation with no remedy
 *   - simulation_program_staff: target, professional judgment structurally devalued
 *   - organizations_awaiting_disaster_to_validate: institutional target, chronic uncertifiable status
 *   - safety_regulators: analytical observer, forced to choose between unfalsifiable doctrine and quiet workaround
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.58).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.52).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real-Catastrophe-Only Doctrine of Competence Validity").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, 'c0cef953-517c-4a18-9705-81eb2d683031').
narrative_ontology:cs_kernel_codification('c0cef953-517c-4a18-9705-81eb2d683031', distributed).
narrative_ontology:cs_authority_grounding('c0cef953-517c-4a18-9705-81eb2d683031', practice).
narrative_ontology:cs_interpretation_layer_present('c0cef953-517c-4a18-9705-81eb2d683031').
narrative_ontology:cs_reading_relation('c0cef953-517c-4a18-9705-81eb2d683031', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('c0cef953-517c-4a18-9705-81eb2d683031', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('c0cef953-517c-4a18-9705-81eb2d683031', foundational, simulation_categorically_cannot_exercise_true_competence).
narrative_ontology:cs_axiom_status(simulation_categorically_cannot_exercise_true_competence, holdable).
narrative_ontology:cs_axiom_grounding('c0cef953-517c-4a18-9705-81eb2d683031', simulation_categorically_cannot_exercise_true_competence, empirically_contingent).
narrative_ontology:cs_axiom('c0cef953-517c-4a18-9705-81eb2d683031', secondary, safety_record_absent_catastrophe_is_unproven_not_validated).
narrative_ontology:cs_axiom_status(safety_record_absent_catastrophe_is_unproven_not_validated, holdable).
narrative_ontology:cs_axiom_grounding('c0cef953-517c-4a18-9705-81eb2d683031', safety_record_absent_catastrophe_is_unproven_not_validated, empirically_contingent).
narrative_ontology:cs_reference_frame('c0cef953-517c-4a18-9705-81eb2d683031', post_incident_hard_experience_primacy).
narrative_ontology:cs_drift_state('c0cef953-517c-4a18-9705-81eb2d683031', post_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c0cef953-517c-4a18-9705-81eb2d683031', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, veteran_incident_commanders).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, post_incident_consultancies).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, frontline_operators_between_incidents).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, simulation_program_staff).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, organizations_awaiting_disaster_to_validate).
narrative_ontology:constraint_vindicates(competence_exercise_validity__real_catastrophe_only, hard_won_experience_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Having personally commanded through a real catastrophic event, they hold institutional authority premised on the claim that only lived catastrophe proves competence. This doctrine elevates their status above colleagues who trained only in simulation, and they administer promotion, certification, and post-incident review boards accordingly. Their standing is portable across organizations that share the doctrine — arbitrage exit if any single employer challenges the premise.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, veteran_incident_commanders, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, veteran_incident_commanders, agenda_setter).

% Firms specializing in after-action reviews and 'lessons learned' consulting profit directly from the doctrine's premise that real events are the only valid teacher: their entire business model requires organizations to believe that simulation cannot substitute, generating recurring demand for their services after every incident.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, post_incident_consultancies, beneficiary,
    organized, biographical, mobile, national).

% Operators who have not yet lived through a qualifying catastrophe are treated as having unproven, effectively unverifiable competence no matter how extensively they drill. They bear career stagnation, second-guessing under audit, and exclusion from senior roles until an event happens to them — a status they cannot earn their way out of by effort alone.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, frontline_operators_between_incidents, payer,
    moderate, biographical, constrained, regional).

% Designers and instructors running high-fidelity drills and tabletop exercises find their work structurally devalued by the doctrine: budgets shrink, their certifications are treated as second-tier, and their professional judgment about training adequacy is routinely overridden by 'wait until the real thing happens' logic from senior leadership.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_program_staff, payer,
    moderate, biographical, constrained, national).

% Entire organizations operating under this doctrine cannot certify their own readiness through any peacetime means; they must either wait for a real catastrophe to test themselves or accept permanent uncertified status. Regulators and boards demand assurance the doctrine says cannot be given short of disaster, producing chronic compliance anxiety with no resolution path.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, organizations_awaiting_disaster_to_validate, payer,
    institutional, generational, trapped, national).

% Regulatory bodies must certify organizational readiness against a doctrine that denies simulation can ever demonstrate it, forcing them to either accept unfalsifiable claims of untested competence or quietly substitute simulation-based certification anyway, undermining the doctrine's stated logic in practice.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, safety_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__real_catastrophe_only, veteran_incident_commanders).
narrative_ontology:fixing_cost_class(competence_exercise_validity__real_catastrophe_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The doctrine coordinates deference toward those who have survived real catastrophic command, concentrating institutional trust and decision authority in demonstrated survivors rather than distributing it evenly across untested personnel.
% TRANSFER_FUNCTION: Moves career advancement, certification authority, consulting revenue, and organizational prestige toward veterans of real incidents and the firms that service post-incident review, and away from simulation-trained staff and organizations that have not yet suffered a qualifying event.
% ABSENT_VOICES: Frontline operators who drill extensively but have no real catastrophe on their record have no forum to contest the premise that their competence is unproven; simulation designers whose exercises are dismissed as inadequate have no seat on the boards that certify readiness using the real-catastrophe standard.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, certification boards would have to accept simulation-based evidence of competence as sufficient, veteran commanders would lose their exclusive claim to proven readiness, consultancies would lose the recurring 'you haven't really been tested' sales pitch, and promotion pathways would open to simulation-trained staff currently locked out.
% FOUNDING_PROBLEM: Early safety-critical industries observed cases where personnel who performed flawlessly in drills froze, panicked, or made fatal errors under real catastrophic stress, suggesting simulation fidelity was insufficient to predict real performance — the doctrine was built to name that gap.
% FOUNDING_PROBLEM_CORROBORATION: Veteran commanders and post-incident consultancies attest the gap remains real and cite specific freeze/panic case studies. Independent human-factors researchers and simulation-fidelity engineers outside the beneficiary set argue the historical gap was largely a fidelity and stress-inoculation failure of 1970s-era simulation technology, not evidence that simulation is categorically incapable of exercising competence — modern high-fidelity, stress-loaded simulation closes much of the gap, which the doctrine's proponents do not incorporate.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and theater ratio (0.62) are both moderately high and rising: the doctrine increasingly functions as a status-and-revenue allocation mechanism (who gets called 'truly tested') rather than a genuine safety-improvement mechanism, since by its own terms it offers no actionable peacetime remedy — you cannot drill your way to validated competence, you can only wait. Suppression (0.52) reflects the doctrine's active discouragement of simulation-based certification claims, enforced through promotion boards and review panels controlled by veteran commanders. Accessibility collapse is only moderate (0.4) because the doctrine has not fully displaced simulation practice — it coexists uneasily with it, contested rather than totalizing.
 *
 * PERSPECTIVAL GAP:
 *   From the veteran-commander seat, the doctrine looks like earned epistemic humility — hard experience really did teach things simulation could not. From the frontline-operator seat, the identical doctrine looks like an unfalsifiable gatekeeping device that can never be satisfied by effort, only by the arrival of misfortune. The engine's per-seat computation should surface this divergence directly from the power/exit asymmetry rather than requiring either seat's narrative to be adjudicated as simply true or false.
 *
 * DIRECTIONALITY LOGIC:
 *   Veteran commanders and consultancies sit near the beneficiary end: the doctrine's unfalsifiability is the source of their differentiated status and revenue, and they have mobile/arbitrage exit because their authority travels across organizations that share the doctrine. Frontline operators and simulation staff sit near the target end: they bear the doctrine's costs (blocked advancement, devalued expertise) with constrained exit, since leaving the industry forfeits their accumulated non-portable expertise. Organizations awaiting disaster are institutionally trapped — they cannot exit the doctrine without abandoning the safety-culture framework their regulators and insurers expect them to hold.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that mid-century simulation technology sometimes failed to predict real performance under acute stress — was real and is corroborated even by sources outside the beneficiary set. But the doctrine, as currently operated, treats that historically-grounded gap as a permanent categorical truth rather than a technology-and-fidelity-dependent one, which is where mandatrophy risk concentrates: the founding problem is 'contested' rather than cleanly 'dead' because modern high-fidelity, physiologically-loaded simulation has narrowed but not eliminated the original gap, and no consensus exists on how much of the gap remains. Classifying this as tangled_rope rather than snare preserves that the doctrine's coordination function (screening for genuine freeze/panic failure modes under stress) is not pure fiction — it just now operates through an asymmetric extraction structure that a cleaner freshness-limited claim would not require.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is ''real_catastrophe_only'' the correct reading of the competence_exercise_validity kernel, or does the historical case record actually support ''continuous_refresh_hybrid'' (simulation necessary-but-insufficient) or even ''simulation_as_proxy''?',
    'Systematic comparison of post-incident performance data between personnel with only real-catastrophe exposure, personnel with only high-fidelity continuous simulation exposure, and personnel with both, controlling for domain and incident severity.',
    'If continuous high-fidelity simulation performance predicts real-event performance as well as prior real-event exposure does, this reading''s core premise fails and the doctrine''s authority-concentration function loses its stated justification, reclassifying the constraint toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, empirical, 'Whether the real-catastrophe-only reading is empirically the correct reading of the kernel among the three declared readings.').

omega_variable(
    luck_vs_competence_confound,
    'When organizations under this doctrine have good safety records without a qualifying catastrophic test, is that because underlying competence is genuinely unproven-but-adequate, or because redundant engineering and low base-rate exposure mask an actual competence deficit that would surface under real stress?',
    'Near-miss and precursor-event analysis: examine whether organizations without catastrophic-event history show degraded performance in high-fidelity unannounced drills that approximate real stress conditions.',
    'If near-miss data shows performance degradation consistent with the doctrine''s prediction, the doctrine''s core empirical claim is partially vindicated even though its extraction structure remains; if performance holds up, the doctrine is exposed as pure status allocation with no predictive validity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(luck_vs_competence_confound, empirical, 'Whether safety records under this doctrine reflect proven competence, luck, or masked deficit.').

omega_variable(
    cs_framing_alternative_kernel_layer,
    'Is the contested kernel here ''what validates competence'' (the framing used), or is there a deeper contested kernel — ''what counts as legitimate authority to certify readiness'' — with the real-catastrophe doctrine as one instantiated authority-grounding claim layered above the certification institution itself?',
    'Trace whether disputes in this domain center on evidentiary standards (kernel as authored) or on who has standing to adjudicate readiness (a governance-layer kernel one level up).',
    'If the deeper governance-layer kernel is the real site of contest, the reading_relations authored here (about evidentiary standards) would need to be re-derived one layer up, changing which axioms are foundational versus secondary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_alternative_kernel_layer, conceptual, 'Whether the authored kernel level is the evidentiary-standard layer or a deeper certification-authority layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__real_catastrophe_only, theater_ratio, 8, 0.47).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__real_catastrophe_only, theater_ratio, 16, 0.52).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__real_catastrophe_only, theater_ratio, 24, 0.57).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_validity__real_catastrophe_only, theater_ratio, 32, 0.6).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__real_catastrophe_only, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(comp_be_t32, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.36).
narrative_ontology:measurement(comp_su_t8, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(comp_su_t16, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(comp_su_t24, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(comp_su_t32, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(comp_su_t40, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__real_catastrophe_only, 0.1).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'what validates competence exercise' per the ε-invariance principle: 'real_catastrophe_only' (this story, tangled_rope, extraction concentrated in veteran-authority and consultancy seats), 'simulation_as_proxy' (lower extraction, rope-leaning, offers a peacetime remedy path), and 'continuous_refresh_hybrid' (scaffold-leaning, built around a refresh/re-certification cycle rather than a one-time or unattainable validation event). Each carries its own ε and stakeholder structure; they are linked here rather than merged because measuring 'competence validity' via the real-catastrophe observable versus the continuous-drill observable yields structurally different extraction profiles, satisfying the decomposition trigger.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
