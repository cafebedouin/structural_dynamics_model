% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__husk_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: preparedness_commitment__husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: institutional/emergency-management
 *
 * SUMMARY:
 *   This story instantiates the husk_reading of the preparedness_commitment
 *   kernel: an emergency-management agency, several decades into a mature
 *   drill-and-certification program, whose exercises now feel like
 *   institutional memory but no longer build operational competence.
 *   ASSUMPTIONS: the scenario is a composite of documented public-agency
 *   patterns (scripted annual exercises scored on completion, certification
 *   cycles tied to advancement, after-action recommendations recurring nearly
 *   verbatim across decades); no single real agency is named, and the 40-year
 *   interval is a stylized program lifetime. The claim/metric gap is
 *   deliberate: the constraint is CLAIMED as piton from this reading's
 *   analytic seat while the metrics are authored independently as descriptive
 *   facts — the engine computes per-seat classifications from the structural
 *   data, and divergence between claim and computed type is the measurement
 *   the corpus exists to take. Per the epsilon-invariance principle, the
 *   sibling readings (competence_reading, hybrid_reading) are separate
 *   constraints in separate files with their own epsilon values; this file
 *   authors only the husk instantiation and links to them via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   emergency_management_directorate: Agenda-setter
 *   (institutional/identity_locked) — administers the program, could change
 *   it, is bound to it by custodial identity -
 *   compliance_and_accreditation_office: Beneficiary (organized/mobile) —
 *   collects budget and headcount scaled to mandated activity -
 *   executive_leadership_and_board: Beneficiary (powerful/arbitrage) —
 *   collects reputational insurance, rotates out before consequences arrive -
 *   frontline_responders: Primary payer (moderate/constrained) — supplies the
 *   time and labor the ceremonies consume - disaster_affected_public:
 *   Ultimate payer (powerless/trapped) — bears the tail cost of the
 *   capability gap - retired_veteran_operators: Excluded voice
 *   (moderate/constrained) — holds the counter-memory, lacks standing -
 *   post_incident_review_commissions: Analytical observer
 *   (institutional/analytical) — documents the gap recurrently
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.62).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.55).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, piton).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "institutional/emergency-management").

domain_priors:requires_active_enforcement(preparedness_commitment__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, '44f8d5f0-3dd6-4847-9406-85e58137af2b').
narrative_ontology:cs_kernel_codification('44f8d5f0-3dd6-4847-9406-85e58137af2b', formalized).
narrative_ontology:cs_authority_grounding('44f8d5f0-3dd6-4847-9406-85e58137af2b', extraction).
narrative_ontology:cs_interpretation_layer_present('44f8d5f0-3dd6-4847-9406-85e58137af2b').
narrative_ontology:cs_reading_relation('44f8d5f0-3dd6-4847-9406-85e58137af2b', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('44f8d5f0-3dd6-4847-9406-85e58137af2b', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('44f8d5f0-3dd6-4847-9406-85e58137af2b', foundational, ritual_compliance_is_not_retention).
narrative_ontology:cs_axiom_status(ritual_compliance_is_not_retention, holdable).
narrative_ontology:cs_axiom_grounding('44f8d5f0-3dd6-4847-9406-85e58137af2b', ritual_compliance_is_not_retention, empirically_contingent).
narrative_ontology:cs_axiom('44f8d5f0-3dd6-4847-9406-85e58137af2b', secondary, assurance_artifacts_mask_capability).
narrative_ontology:cs_axiom_status(assurance_artifacts_mask_capability, holdable).
narrative_ontology:cs_axiom_grounding('44f8d5f0-3dd6-4847-9406-85e58137af2b', assurance_artifacts_mask_capability, empirically_contingent).
narrative_ontology:cs_reference_frame('44f8d5f0-3dd6-4847-9406-85e58137af2b', inherited_memorial_routine).
narrative_ontology:cs_drift_state('44f8d5f0-3dd6-4847-9406-85e58137af2b', contemporary_post_failure_review_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('44f8d5f0-3dd6-4847-9406-85e58137af2b', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, compliance_and_accreditation_office).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, executive_leadership_and_board).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, disaster_affected_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the annual exercise calendar, owns the certification standard, and signs the readiness attestations presented to overseers. Its senior members inherited the program from mentors and built careers as custodians of institutional memory; redesigning the program would require declaring their predecessors' and their own earlier attestations hollow. Moving to another agency means abandoning that custodial identity.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, emergency_management_directorate, agenda_setter,
    institutional, biographical, identity_locked, national).

% Staffs inspections, scores exercise completion, issues certificates, and maintains the documentation archive. Its headcount and budget scale with the volume of mandated activity. Its auditing and documentation skills transfer readily to other regulated domains, so individual members can leave, though the office itself persists.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, compliance_and_accreditation_office, beneficiary,
    organized, biographical, mobile, national).

% Presents readiness metrics to political overseers, insurers, and accreditation bodies, and collects reputational insurance from clean certificates. Members rotate on appointment cycles shorter than the interval between major disasters, so the consequences of any gap between certified and actual capability land on successors and on responders rather than on them.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, executive_leadership_and_board, beneficiary,
    powerful, immediate, arbitrage, national).

% Execute the scripted exercises, complete the associated documentation, and staff the evaluation positions. Many know precisely where the scripts diverge from actual equipment, staffing, and mutual-aid realities, and must perform the scripts anyway. Transferring to a peer agency reproduces the same regime; leaving the profession forfeits pension credit and professional identity.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, frontline_responders, payer,
    moderate, biographical, constrained, national).

% Relies on the agency's advertised readiness and cannot directly observe exercise quality. Bears the difference between certified and delivered response when a novel event arrives, in evacuated neighborhoods, delayed ambulances, and failed evacuations. Relocating away from the hazard jurisdiction is unaffordable for most households, and the cost is invisible until the event occurs.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, disaster_affected_public, payer,
    powerless, biographical, trapped, regional).

% Carry living memory of the era when exercises were unscripted, failure-tolerant, and scored on adaptation rather than completion. Would testify that the current ceremonies replaced those practices rather than extending them. Hold no standing in current planning cycles and reach the conversation only through memoirs, oral histories, and occasional advisory invitations that rarely alter the program.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, retired_veteran_operators, excluded,
    moderate, generational, constrained, national).

% Convene after major failures, compare certified readiness against field performance, and issue recommendations. Their findings recur with strikingly similar wording across decades, which makes the commissions a running record of the gap rather than a lever that closes it.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, post_incident_review_commissions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The exercise-and-certification cycle synchronizes a rotating, multi-shift, multi-agency workforce around common procedures, a shared vocabulary, and a fixed calendar, and produces auditable records that satisfy oversight bodies, insurers, and accreditors. It coordinates the form of joint action and the paper trail of assurance.
% TRANSFER_FUNCTION: Moves responder time and operating budget out of capability-building and into scheduled ceremony and documentation; moves assurance artifacts (completion scores, certificates, readiness reports) upward to executives and overseers; and, when a novel event arrives, moves the unpriced cost of the capability gap onto the affected public.
% ABSENT_VOICES: Retired veteran operators who remember competence-based training are outside the planning cycle entirely; rank-and-file responders see the script-versus-reality gaps but submit sanitized evaluation forms through channels their managers read; the not-yet-affected public is present only as aggregate statistics. Post-disaster survivors enter retrospectively, through after-action processes whose recommendations decay before the next cycle completes.
% DISAPPEARANCE_RATIONALE: If the exercise-and-certification regime vanished overnight, the compliance office would have no mandate and dissolve, thousands of responder-hours per year would return to line operations, and executives would lose the assurance artifacts they present upward, forcing them either to commission real capability-building or to declare unreadiness openly. Budgets, careers, and oversight relationships would all reorganize around whatever replaced the certificates.
% FOUNDING_PROBLEM: Mid-century disaster responses repeatedly exposed two failures: workforce turnover eroding hard-won tactical knowledge between generations, and ad hoc multi-agency coordination collapsing under stress. Recurring exercises were built to transmit live skill across generations and to give commanders a practiced interface with counterpart agencies.
% FOUNDING_PROBLEM_CORROBORATION: The underlying problem remains live and is attested from outside the benefiting parties: published after-action commissions across multiple decades document the same capability gaps, peer-reviewed training-transfer research shows scripted completion-based drills do not preserve adaptive skill, and union grievance records and veteran testimony corroborate that exercise time displaced rather than built capability. The benefiting parties' own readiness reports assert the opposite and are discounted as in-seat claims.
narrative_ontology:disappearance_verdict(preparedness_commitment__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__husk_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the referent is the standing drill-and-certification arrangement as the husk reading assesses it — it consumes substantial responder time and operating budget and returns form-compliance rather than capability. It is not higher because the ceremony does preserve thin real goods: procedural familiarity, paperwork fluency, and a common vocabulary. Suppression 0.55: participation is compelled through certification gates and career consequences, and open skepticism about the program's value is career-limiting; the enforcement machinery visibly hardened over the interval (see the rising suppression_requirement series), which is why this dynamic is tracked temporally rather than left as a static scalar. Suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater_ratio 0.78: the majority of exercise hours are scripted to succeed, deviations are penalized in evaluations, and after-action findings repeat across decades — the signature of performance displacing function. Accessibility_collapse 0.45: alternatives (no-notice functional exercises, failure-tolerant drills, apprenticeship rotations) are known, periodically proposed, and never suppressed outright; they lose budget competitions and risk-exposure arguments. Understanding the regime's hollowness opens no individual exit — compliance stays mandatory — but systemic alternatives remain on the table, so collapse is partial. Resistance 0.38: episodic rather than sustained — spikes after visible failures (reviews, grievances, leaks) and decays as attention passes; the decay is itself part of the maintenance cycle, since each review concludes in 'lessons learned' folded back into the same forms. The three temporal series run on one shared nine-point grid (T=0..40, step 5) so every tracked metric is authored at every examined time point; endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats should compute very differently. From the compliance office and the executive board, the arrangement is assurance production: certificates, clean audits, defensible attestations — a low-burden, career-positive structure. From the frontline responders it is uncompensated labor under scripts they know to be false, and from the affected public it is an invisible mortgage on their safety, payable only in catastrophe. The directorate occupies the pivot: it experiences the program as stewardship and continuity, and its identity lock makes the payer-side reading of its own program nearly unavailable to it. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (compliance_and_accreditation_office, executive_leadership_and_board) derive directionality near the beneficiary end — the office materially, the board reputationally — amplified toward full subsidy by the board's arbitrage exit. Declared victims (frontline_responders, disaster_affected_public) derive directionality near the full-target end, with the public pushed furthest by its trapped exit and inability to perceive the arrangement before it fails. The directorate declares no collection and no payment: it administers, and its seat is placed by its own atoms (institutional power, identity-locked exit), which the commentary treats as ambivalent rather than forcing it into either declaration. Observers are exempt from the extraction computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate — maintain readiness — has outlived the function that justified it — transmit operational competence across generations. The function atrophied while the mandate's ceremonies expanded, which is the defining movement of this reading. The classification discipline matters in both directions: this is NOT a snare, because no seat captures the extracted resources — the bulk of extracted time and budget is consumed by the performances themselves, with residual capture split between the compliance office (salaries, headcount) and the board (reputation), neither dominant, and no active predation is required to sustain the regime, only administration. And it is NOT a rope, because coordinating the form of joint action is not coordinating its function: the collective-action problem the regime nominally solves — capability maintenance under turnover — is unsolved and worsening. The piton cost-asymmetry test holds: the directorate could redesign the program, but the cost to it (self-indictment, dismantling its custodial identity, owning transition failures) exceeds anything it bears from continuation, while the public bears a catastrophic but diffuse and unpredictable tail cost that is structurally unable to organize ex ante. Hence gain_flow is authored as 'diffuse' — an affirmative claim made after checking every named seat — and fixing_cost as 'prohibitive'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Is the husk instantiation the right cut of the preparedness_commitment kernel, or do the same observable routines belong to the competence_reading (routines maintain capacity) or the hybrid_reading (layered system), yielding a different epsilon and type for the same drill archive?',
    'Comparative no-notice exercise trials with novel-scenario injection across comparable agencies, scored on adaptive performance rather than completion, paired with longitudinal skill-decay measurement across turnover cohorts.',
    'If the competence reading fits, epsilon falls toward the coordination-cost floor and the type shifts toward rope; if the hybrid reading fits, extraction is intermediate and the memorial layer earns partial credit as commitment stabilization. Only if both fail does the husk instantiation stand as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the preparedness kernel correctly carves the observed drill regime.').

omega_variable(
    tacit_skill_residual,
    'Does unscripted tactical skill survive beneath the ceremonial layer — seasoned staff improvising competently despite hollow drills — or has the capability genuinely atrophied?',
    'No-notice functional exercises scored on deviation-handling and novel-problem response rather than script compliance, disaggregated by cohort exposure to the scripted era.',
    'Residual skill would mean the regime retains partial function and profiles closer to a hybrid coordination-extraction structure with lower effective extraction for experienced seats; no residual confirms the husk diagnosis and the piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_skill_residual, empirical, 'Whether live capability persists under the memorial performance.').

omega_variable(
    ritual_sincerity_vs_coercion,
    'Is participation in the ceremonies sustained by career enforcement or by sincere belief among staff that the rituals preserve readiness?',
    'Separation experiment: offer penalty-free opt-out from non-mandatory exercise components and track uptake, plus attrition-interview coding of stated versus revealed reasons for compliance.',
    'If belief carries participation, measured suppression overstates the enforcement burden and reform must target belief change, not just enforcement removal; if coercion carries it, removing enforcement collapses participation quickly and the regime''s persistence is more fragile than its forty-year record suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_sincerity_vs_coercion, empirical, 'Internalized belief versus structural coercion as the binding mechanism.').

omega_variable(
    passive_inertia_vs_active_defense,
    'Does the compliance apparatus merely administer the inherited program, or does it actively defend and expand the mandate against revision?',
    'Budget-cycle behavior analysis, mandate-expansion history, and records of the office''s positions on proposed exercise reform, distinguishing passive administration from active lobbying and scope growth.',
    'Active defense with captured gains would push the classification toward snare or tangled_rope; passive administration with diffuse residual capture confirms the piton reading and its cost-asymmetry account of persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_inertia_vs_active_defense, empirical, 'Whether persistence is inertial or actively defended.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__husk_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__husk_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__husk_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__husk_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(prep_tr_t25, preparedness_commitment__husk_reading, theater_ratio, 25, 0.6).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__husk_reading, theater_ratio, 30, 0.67).
narrative_ontology:measurement(prep_tr_t35, preparedness_commitment__husk_reading, theater_ratio, 35, 0.73).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__husk_reading, theater_ratio, 40, 0.78).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__husk_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__husk_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__husk_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__husk_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(prep_be_t25, preparedness_commitment__husk_reading, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__husk_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(prep_be_t35, preparedness_commitment__husk_reading, base_extractiveness, 35, 0.59).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__husk_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__husk_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(prep_su_t5, preparedness_commitment__husk_reading, suppression_requirement, 5, 0.08).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__husk_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(prep_su_t15, preparedness_commitment__husk_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__husk_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(prep_su_t25, preparedness_commitment__husk_reading, suppression_requirement, 25, 0.37).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__husk_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(prep_su_t35, preparedness_commitment__husk_reading, suppression_requirement, 35, 0.5).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__husk_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'organizational preparedness' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of the preparedness_commitment kernel. competence_reading is the upstream member (historically prior, higher empirical confidence in the original function); this husk_reading is the degraded descendant — the same routines assessed after their function atrophied, hence a far higher epsilon and a piton claim where the upstream reading supports a rope-like profile. hybrid_reading mediates: it treats the memorial layer as commitment-stabilizing and the competence layer as function-bearing, and this reading's collapse evidence is the main empirical pressure shaping it. Every family member links the others via affects_constraints; no single file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
