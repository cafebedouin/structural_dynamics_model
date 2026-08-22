% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real Catastrophic Incident Necessity for Competence Occupation
 *   domain: organizational/safety
 *
 * SUMMARY:
 *   This constraint instantiates the real_incident_necessity reading of the
 *   competence_occupation kernel: the claim that only actual catastrophic
 *   incidents provide the authentic conditions necessary to occupy the
 *   competence kernel in high-reliability organizations. The constraint
 *   asserts that simulations, refresher training, and procedural compliance
 *   cannot certify that personnel possess authentic competence under extreme
 *   pressure — only real catastrophes can. This reading structures competence
 *   maintenance in safety-critical domains (aviation, nuclear operations,
 *   emergency medicine, disaster response) by declaring that the founding
 *   epistemological problem — how to verify competence when authentic
 *   conditions occur rarely or never — is unsolvable except through
 *   catastrophe. The constraint becomes the mechanism by which catastrophes
 *   are normalized as necessary for safety assurance, creating a perverse
 *   incentive structure: organizations maintain expensive simulation and
 *   training regimes (declared insufficient) while catastrophes provide the
 *   only authentic test, making catastrophe tolerance an embedded feature of
 *   competence-occupation doctrine.
 *
 * KEY AGENTS:
 *   - incident_survivors — catastrophe victims whose harm provides the authentic test condition
 *   - organizational_leadership — institutional actor that must maintain competence via a mechanism (real incidents) they cannot ethically accept
 *   - front_line_personnel — occupants of the competence kernel whose authentic readiness remains forever unverified until failure
 *   - simulation_operators — beneficiaries of the constraint's assertion that simulations are insufficient
 *   - regulatory_bodies — institutional payers forced to certify competence through proxy measures while the constraint declares them inadequate
 *   - public_exposed_to_failure_states — non-consenting, powerless exposure to the incidents that test competence
 *   - incident_researchers — analytical observers who reconstruct competence failure post-hoc but cannot inform prospective certification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.88).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.71).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.88).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, snare).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real Catastrophic Incident Necessity for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational/safety").

domain_priors:requires_active_enforcement(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '2dfa89c2-cd53-4c03-acbf-398cd05ad9de').
narrative_ontology:cs_kernel_codification('2dfa89c2-cd53-4c03-acbf-398cd05ad9de', distributed).
narrative_ontology:cs_authority_grounding('2dfa89c2-cd53-4c03-acbf-398cd05ad9de', extraction).
narrative_ontology:cs_interpretation_layer_present('2dfa89c2-cd53-4c03-acbf-398cd05ad9de').
narrative_ontology:cs_reading_relation('2dfa89c2-cd53-4c03-acbf-398cd05ad9de', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('2dfa89c2-cd53-4c03-acbf-398cd05ad9de', competence_occupation__hybrid_occupation, coexists_with).
narrative_ontology:cs_axiom('2dfa89c2-cd53-4c03-acbf-398cd05ad9de', foundational, only_real_catastrophe_authentic).
narrative_ontology:cs_axiom_status(only_real_catastrophe_authentic, holdable).
narrative_ontology:cs_axiom_grounding('2dfa89c2-cd53-4c03-acbf-398cd05ad9de', only_real_catastrophe_authentic, empirically_contingent).
narrative_ontology:cs_axiom('2dfa89c2-cd53-4c03-acbf-398cd05ad9de', secondary, simulation_necessarily_insufficient).
narrative_ontology:cs_axiom_status(simulation_necessarily_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('2dfa89c2-cd53-4c03-acbf-398cd05ad9de', simulation_necessarily_insufficient, deontological).
narrative_ontology:cs_reference_frame('2dfa89c2-cd53-4c03-acbf-398cd05ad9de', authentic_competence_gate).
narrative_ontology:cs_drift_state('2dfa89c2-cd53-4c03-acbf-398cd05ad9de', evidence_based_hybrid_emergence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2dfa89c2-cd53-4c03-acbf-398cd05ad9de', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, incident_survivors).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, organizational_leadership).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, regulatory_bodies).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, public_exposed_to_failure_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, simulation_operators).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, front_line_personnel).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, competence_requires_authentic_challenge).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, simulation_is_insufficient_proxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience the catastrophic failure directly. Bear the immediate costs — injury, trauma, death, or operational loss — as the only mechanism by which organizational personnel can access authentic competence-testing conditions. Their survival or non-survival becomes the evidentiary substrate the constraint depends on.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_survivors, payer,
    powerless, immediate, trapped, local).

% Operates under the competence-occupation rule: they must accept that their personnel's authentic competence can only be verified through real catastrophic incidents, not simulations. They face an unresolvable mandate — maintain competence without the only authentic testing mechanism, or accept incidents as cost of occupation. Cannot exit without abandoning safety responsibility.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, organizational_leadership, payer,
    institutional, generational, constrained, national).

% Professional identity is constituted by occupying the competence kernel in high-reliability domains (pilots, surgeons, nuclear operators, disaster responders). They cannot know whether their trained competence is authentic until a real incident occurs. Simulation provides no real test; their readiness is perpetually uncertain. Exit means abandoning professional identity.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, front_line_personnel, payer,
    moderate, biographical, identity_locked, local).

% Maintain and expand simulation infrastructure while the constraint declares it insufficient. The constraint vindicates their expense (simulations are needed even though inadequate) and protects their institutional role from optimization pressure: no simulation regime can be certified as sufficient, so the program persists and expands regardless of measured competence outcomes.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_operators, beneficiary,
    organized, biographical, mobile, regional).

% Must certify personnel and organizations as competent without access to the only authentic test. Forced to rely on proxy measures (simulation completion, procedural compliance, incident post-mortems) while the constraint asserts these are insufficient. Accept catastrophes as inevitable cost of the system or mandate changes that contradict the constraint.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, regulatory_bodies, payer,
    institutional, generational, constrained, national).

% Non-consenting, non-organized exposure to the failure states that test competence. They depend on systems (aviation, healthcare, nuclear, disaster response) certified as competent through a regime the constraint asserts as insufficient. Catastrophes generate incidents they are exposed to without choice or representation.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, public_exposed_to_failure_states, payer,
    powerless, immediate, trapped, universal).

% Analyze catastrophic incidents to reconstruct competence failure. Their analyses are the primary source of evidence that competence was or was not occupied at the moment of failure — but this evidence arrives only after the fact, making it useless for competence certification before incident. Occupies the analytical seat: can document the constraint's operation but not participate in its enforcement.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__real_incident_necessity, simulation_operators).
narrative_ontology:fixing_cost_class(competence_occupation__real_incident_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint structures how high-reliability organizations approach competence maintenance: it asserts a specific epistemological principle — that authentic occupancy of the competence kernel requires conditions only catastrophic incidents provide. This coordinates a safety doctrine around the principle that simulated environments, procedural exercises, and refresher training cannot substitute for real-world authenticity under extreme pressure.
% TRANSFER_FUNCTION: Transfers the cost of competence verification from organizational resources (simulation, training, continuous assessment) to front-line personnel and public exposure: the constraint makes authentic testing inseparable from actual catastrophe, so competence occupation remains perpetually unverified until an incident occurs. Organizations cannot pay simulation costs and achieve the same evidentiary outcome; survivors and exposed publics bear the testing cost.
% ABSENT_VOICES: Alternative competence-occupation frameworks that could adjudicate whether simulation, hybrid approaches, or other mechanisms suffice are structurally excluded by the constraint itself. Designers, researchers, and practitioners who argue simulation CAN occupy the competence kernel are not represented in competence-certification proceedings — their evidence is treated as insufficient by the constraint's own terms. The constraint pre-selects the evidence class (real catastrophes only) that can count.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, organizations would shift to hybrid or simulation-primary competence-certification regimes within months. Incident frequency would become measurable against simulation outcomes, training programs would be optimized by evidence rather than stuck at 'simulations are insufficient,' and competence certification would become a continuous, testable process rather than waiting for catastrophes. The entire safety training infrastructure would reorganize around what evidence is actually actionable rather than what the constraint declares authentic.
% FOUNDING_PROBLEM: High-reliability organizations need to maintain authentic competence in personnel facing rare, high-consequence scenarios (aviation, nuclear, medicine, disaster response). Early approaches relied on live exercises and incident post-mortems. The founding problem: how to certify competence in systems where authentic conditions (catastrophic failure modes) occur rarely or never during a career, making continuous competence verification extremely difficult.
% FOUNDING_PROBLEM_CORROBORATION: The constraint's beneficiaries (simulation operators, organizations seeking justification for expensive training regimes) attest the founding problem is live and unresolved. Front-line personnel and regulatory bodies attest the problem is partially addressed by modern hybrid approaches, and that treating only catastrophic incidents as authentic competence tests creates unacceptable public exposure. Independent safety researchers (NASA, FAA, medical boards) publish evidence that simulation supplemented by refresher protocols and procedural audits produces measurable competence maintenance; this evidence is outside the benefiting parties and directly contradicts the constraint's core premise.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.88) and rising because the constraint makes catastrophes structurally necessary for organizational safety assurance, transferring the cost of competence verification to incident victims and exposed publics. Suppression is substantial (0.71) because alternative theories of competence occupation — simulation sufficiency, hybrid approaches, continuous evidence-based certification — must be actively suppressed in favor of the 'only real incidents count' doctrine. Theater is elevated (0.62) because organizations perform elaborate simulation and training regimes (the constraint vindicates their expense) while simultaneously asserting these regimes cannot achieve their stated goal; the procedural performance masks the underlying mandate that competence remains unverified. Resistance is high (0.73) from regulatory bodies, alternative practitioners, and safety researchers who argue the constraint is unfalsifiable and unethical — yet the constraint persists because it operates through epistemological control, not institutional enforcement. The temporal measurements show slow extraction accumulation (0.72→0.88 over 40 time units) as organizations normalize catastrophe tolerance and theater expands (0.48→0.62) as simulation regimes become more elaborate while remaining declared insufficient. The suppression_requirement plateaus (0.58→0.71 then flat) because active suppression of alternative frameworks has hardened into institutional practice.
 *
 * PERSPECTIVAL GAP:
 *   From the simulation_operators' seat: the constraint is coordination (it justifies their infrastructure, their expansion, their resource allocation in perpetuity — the doctrine that simulations can never be sufficient legitimizes continuous investment). From organizational_leadership's seat: the constraint is a trap (they must certify competence without the only authentic test, or accept catastrophes; they have no ethical exit). From regulatory_bodies' seat: the constraint is an unfalsifiable doctrine they are forced to operate under while accumulating liability. The engine computes per-seat types from the structural data; these perspectives should diverge sharply — simulation_operators may compute as rope (coordination benefit, low d, stable), while organizational_leadership computes as snare (constrained, high d, no exit). This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint exploits an epistemological asymmetry: it privileges one class of evidence (real catastrophes) as the only authentic test, while declaring all other evidence classes (simulation, training, procedural compliance) as insufficient by the constraint's own terms. This makes the constraint unfalsifiable from within organizational practice — any simulation outcome can be dismissed as 'not authentic,' any procedural success can be reinterpreted as 'unverified until real test,' making catastrophes the only possible counter-evidence. Front-line personnel cannot know they are truly competent until catastrophe, which means their competence is perpetually unverified — a high-d, trapped-exit situation. Organizational leadership must accept catastrophes as acceptable (the only authentic test) or reject the competence-occupation doctrine entirely (which has no viable alternative in safety-critical domains). Regulatory bodies operate under the constraint while their core mandate (certify competence, prevent failures) is made impossible by the constraint's own logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to verify competence in rare-event domains) was live at the constraint's origin. Modern evidence-based safety research (NASA, FAA, medical boards) demonstrates that hybrid competence-occupation frameworks (simulation + refresher + procedural audits + continuous assessment) measurably reduce incident rates and improve outcomes. The founding problem is substantially dead — it has been solved by hybrid approaches that produce better safety outcomes than incident-based certification. Yet the constraint persists because its authority structure (the doctrine that 'only real incidents count') does not engage with empirical evidence of hybrid effectiveness. The constraint exhibits classic mandatrophy: it vindicates simulation operators (who perform well under the constraint) while becoming increasingly disconnected from the founding problem it was meant to solve. The temporal measurements show base extractiveness rising even as regulatory evidence accumulates that the constraint is counterproductive to its stated goal. This is mandatrophy: the constraint's persistence is sustained by institutional inertia, simulation operators' interest in perpetual insufficiency, and the logical unfalsifiability of the doctrine, not by any active coordination or safety function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_criterion_definition,
    'What makes a competence test ''authentic'' — is authenticity a property of the test conditions themselves (high-fidelity realism), the stakes (real consequences), the pressure environment (psychological and operational stress), or the rarity of the scenario?',
    'Empirical comparison of competence outcomes between groups trained under different conditions (high-fidelity simulation, lower-fidelity simulation, hybrid, incident-post-hoc learning) controlling for personnel selection, experience, and domain. If equivalent or higher competence measures arise from hybrid approaches, authenticity is not synonymous with real incidents.',
    'If authenticity is defined by realism and stress rather than real consequences, simulation (sufficiently high-fidelity) could occupy the competence kernel. This would dissolve the constraint''s core premise and reclassify it from snare to piton (inertial performance of a doctrine whose founding problem is solved).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authenticity_criterion_definition, empirical, 'Whether ''authentic'' competence-occupation requires real incidents or high-fidelity conditions more broadly.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of alternative competence-occupation frameworks (simulation sufficiency, hybrid approaches) structural (institutional gatekeeping, credentialing rules, regulatory exclusion) or internalized (personnel and organizations genuinely believe only real incidents count)?',
    'Longitudinal observation of organizations or jurisdictions that introduce evidence-based hybrid frameworks: if suppression persists after structural barriers are removed, the suppression is partially internalized and persists as belief rather than coercive mechanism.',
    'If suppression is largely internalized, the constraint persists through cognitive capture rather than institutional enforcement — personnel and leadership genuinely believe the constraint''s doctrine even when it contradicts their own interests and safety data. This raises the cost of escape and shifts the classification toward deeper snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative frameworks is structural enforcement or internalized belief.').

omega_variable(
    unfalsifiability_structural,
    'Is the constraint structurally unfalsifiable (any non-catastrophic outcome can be dismissed as ''unverified,'' any simulation success can be labeled ''not authentic''), making it immune to empirical disproof?',
    'Logical analysis of what evidence would count as counter-proof under the constraint''s own terms. If no possible evidence can falsify the constraint, it is unfalsifiable by construction.',
    'If unfalsifiable, the constraint is not subject to empirical resolution — it persists as doctrine regardless of safety outcomes. This makes it a pure knowledge-control mechanism (epistemological snare) rather than a disputed empirical claim. Regulatory bodies cannot defeat it with evidence; they can only escape by rejecting the constraint''s authority structure wholesale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unfalsifiability_structural, conceptual, 'Whether the constraint is logically unfalsifiable or empirically disprovable.').

omega_variable(
    kernel_reading_alternative_simulation_sufficiency,
    'Is this reading (real_incident_necessity) in genuine logical conflict with the simulation_sufficiency reading (that simulation-based drills constitute sufficient exercise), or can both readings coexist by reference to different operative definitions of ''sufficiency''?',
    'Clarify the definition of ''sufficient competence-occupation'' in each reading: if real_incident_necessity means ''sufficient for certification to authorities'' and simulation_sufficiency means ''sufficient for functional job performance,'' the readings address different questions and can coexist. If both address the same question, they foreclose.',
    'If they foreclose (address the same question with opposite answers), this reading''s classification is stronger — it is not merely asserting one option among viable alternatives but making an exclusive claim. If they coexist (address different questions), this reading becomes more obviously a contingent choice among frameworks rather than a structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_simulation_sufficiency, conceptual, 'Whether real_incident_necessity and simulation_sufficiency foreclose or coexist as alternative readings of the competence kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__real_incident_necessity, theater_ratio, 5, 0.51).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__real_incident_necessity, theater_ratio, 10, 0.54).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__real_incident_necessity, theater_ratio, 15, 0.57).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__real_incident_necessity, theater_ratio, 20, 0.59).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__real_incident_necessity, theater_ratio, 25, 0.6).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__real_incident_necessity, theater_ratio, 30, 0.61).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_occupation__real_incident_necessity, base_extractiveness, 5, 0.75).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_occupation__real_incident_necessity, base_extractiveness, 10, 0.78).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_occupation__real_incident_necessity, base_extractiveness, 15, 0.81).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_occupation__real_incident_necessity, base_extractiveness, 20, 0.84).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t25, competence_occupation__real_incident_necessity, base_extractiveness, 25, 0.86).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t30, competence_occupation__real_incident_necessity, base_extractiveness, 30, 0.87).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.88).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_occupation__real_incident_necessity, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_occupation__real_incident_necessity, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_occupation__real_incident_necessity, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_occupation__real_incident_necessity, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t25, competence_occupation__real_incident_necessity, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t30, competence_occupation__real_incident_necessity, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t40, competence_occupation__real_incident_necessity, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(comp_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__real_incident_necessity, 0.12).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% Three readings of the competence_occupation kernel constitute a constraint family: real_incident_necessity (this story) asserts that only catastrophes provide authentic testing; simulation_sufficiency asserts simulation drills are sufficient; hybrid_occupation asserts continuous multi-mechanism exercise without settled optimal configuration. The readings share a referent (competence-occupation in high-reliability organizations) but differ fundamentally in their epistemological criterion for authenticity and sufficiency. Each reading instantiates a different constraint with different extracted values, beneficiary structures, and persistence mechanisms. This reading (real_incident_necessity) forecloses simulation_sufficiency at the core premise level (if only real incidents are authentic, simulations cannot be sufficient) but coexists_with hybrid_occupation (which disputes authenticity criteria while accepting multi-mechanism involvement). The three stories are linked via network.affects_constraints for constraint-family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, powerful, 0.72).
constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
