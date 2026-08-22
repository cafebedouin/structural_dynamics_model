% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__simulation_sufficiency_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Drill-Mandate Regime as Sufficient Competence Maintenance (Simulation-Sufficiency Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This story instantiates the simulation_sufficiency_reading of the kernel
 *   exercise_as_competence_maintenance. The standing arrangement under
 *   contest is the regulatory drill-mandate regime: recurring mandated
 *   catastrophe simulations across safety-critical sectors, audited through
 *   completion artifacts and scored by simulator performance metrics, and
 *   treated by the regime as sufficient maintenance of crisis-response
 *   competence. Per the epsilon-referent rule, extractiveness is assessed on
 *   THAT standing arrangement as this reading sees it — never on the
 *   higher-fidelity program this reading would endorse if pressed. The
 *   reading's own structure locates extraction precisely: simulation
 *   genuinely exercises competence (so the arrangement is not pure
 *   extraction), fidelity determines retention (so protection is conditional
 *   on a variable the mandate does not itself guarantee), and the victim set
 *   is confined to those harmed when real events exceed inadequately faithful
 *   rehearsal. Per the epsilon-invariance principle this is one file of a
 *   three-file family: the sibling readings author separate constraints over
 *   the same arrangement with different epsilon values — the
 *   lived-catastrophe sibling assesses the same regime from premises under
 *   which simulation contributes almost nothing, so its epsilon is far
 *   higher; the hybrid sibling sits between. This file's epsilon covers only
 *   this reading's assessment. Claim and metrics are authored independently:
 *   claimed_type tangled_rope reflects the structural belief that genuine
 *   coordination and asymmetric extraction coexist in one enforced structure;
 *   the metric values reflect descriptive judgment about how the regime
 *   actually operates.
 *
 * KEY AGENTS:
 *   - - safety_regulators: agenda_setter (institutional/mobile) — writes and audits the mandate; collects oversight legitimacy
 *   - - regulated_operators: payer with beneficiary secondary_role (powerful/constrained) — bears compliance cost, collects the liability-shield record
 *   - - simulator_training_vendors: beneficiary (organized/arbitrage) — sells into demand the mandate itself creates
 *   - - frontline_response_teams: payer (organized/constrained) — staff both the drills and the real events; first-harmed when fidelity fails
 *   - - catastrophe_exposed_public: payer (powerless/trapped) — bears residual fidelity-gap risk, learns of it retrospectively
 *   - - accident_investigation_bodies: observer (institutional/analytical) — produce the only systematic simulated-versus-live comparison
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.58).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.5).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Drill-Mandate Regime as Sufficient Competence Maintenance (Simulation-Sufficiency Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, '757e56ca-4b05-4a17-95b4-3834a546091f').
narrative_ontology:cs_kernel_codification('757e56ca-4b05-4a17-95b4-3834a546091f', formalized).
narrative_ontology:cs_authority_grounding('757e56ca-4b05-4a17-95b4-3834a546091f', expertise).
narrative_ontology:cs_interpretation_layer_present('757e56ca-4b05-4a17-95b4-3834a546091f').
narrative_ontology:cs_reading_relation('757e56ca-4b05-4a17-95b4-3834a546091f', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('757e56ca-4b05-4a17-95b4-3834a546091f', exercise_as_competence_maintenance__hybrid_decay_reading, forecloses).
narrative_ontology:cs_axiom('757e56ca-4b05-4a17-95b4-3834a546091f', foundational, simulated_catastrophe_is_genuine_kernel_exercise).
narrative_ontology:cs_axiom_status(simulated_catastrophe_is_genuine_kernel_exercise, holdable).
narrative_ontology:cs_axiom_grounding('757e56ca-4b05-4a17-95b4-3834a546091f', simulated_catastrophe_is_genuine_kernel_exercise, empirically_contingent).
narrative_ontology:cs_axiom('757e56ca-4b05-4a17-95b4-3834a546091f', foundational, retention_effectiveness_tracks_simulation_fidelity).
narrative_ontology:cs_axiom_status(retention_effectiveness_tracks_simulation_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('757e56ca-4b05-4a17-95b4-3834a546091f', retention_effectiveness_tracks_simulation_fidelity, empirically_contingent).
narrative_ontology:cs_axiom('757e56ca-4b05-4a17-95b4-3834a546091f', secondary, mandated_drill_completion_constitutes_readiness_evidence).
narrative_ontology:cs_axiom_status(mandated_drill_completion_constitutes_readiness_evidence, holdable).
narrative_ontology:cs_axiom_grounding('757e56ca-4b05-4a17-95b4-3834a546091f', mandated_drill_completion_constitutes_readiness_evidence, conventional).
narrative_ontology:cs_reference_frame('757e56ca-4b05-4a17-95b4-3834a546091f', simulation_exercise_equivalence).
narrative_ontology:cs_drift_state('757e56ca-4b05-4a17-95b4-3834a546091f', contemporary_post_accident_inquiry_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('757e56ca-4b05-4a17-95b4-3834a546091f', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_regulators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulator_training_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulated_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, catastrophe_exposed_public).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_response_teams).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulated_operators).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_transfer_doctrine).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_metric_objectivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes drill-frequency and participation requirements into safety codes, audits completion records, and treats simulator performance scores as the measure of maintained competence. Collects demonstrable oversight: completed drill calendars, score trends, and the ability to show the public that preparedness is being verified. Can revise frequency or scoring rules but has inherited the metric apparatus its predecessors built and staffed careers around.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_regulators, agenda_setter,
    institutional, generational, mobile, national).

% Runs the mandated exercises and bears their direct costs — staff time, simulator fees, production downtime, audit preparation. Receives the compliance record in return: regulatory sign-off, insurance-rating input, and litigation defense whose protective value frequently rivals or exceeds what the exercises cost. Cannot leave the regulatory perimeter; may exceed mandates voluntarily when reputation or board attention favors it.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulated_operators, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulated_operators, beneficiary).

% Sells mandated simulator hours, scenario libraries, and performance dashboards into demand that exists because the mandates exist. Revenue tracks mandate stringency and audit cycles more closely than it tracks measured improvement in live performance. Operates across sectors and jurisdictions and can shift sales effort toward whichever regime tightens next.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulator_training_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Staff the exercises and staff the real events. Their proficiency is what the entire apparatus claims to maintain. When a live event outruns what scheduled rehearsal preserved, they absorb the first harm — injuries, deaths, lasting moral injury — and they hold no seat in deciding what counts as adequate fidelity. Professional associations give them a voice in commentary but not in mandate design.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_response_teams, payer,
    organized, biographical, constrained, regional).

% Depends on crisis response it cannot inspect. Carries the residual risk left by the gap between rehearsed and required performance, and typically learns the size of that gap only afterward, from investigation reports. Cannot exit the safety regime of the jurisdiction it lives in, and its harm arrives dispersed across events and decades, which frustrates organized response.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, catastrophe_exposed_public, payer,
    powerless, generational, trapped, national).

% Reconstruct actual events against the drill record, producing the only systematic comparison of simulated versus live performance available to anyone. Findings feed mandate revisions after each revealing event; the bodies themselves command no enforcement machinery and depend on the regulator to act on what they find.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, accident_investigation_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulated_operators).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__simulation_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem in rare-event preparedness: individual organizations cannot safely learn catastrophe response from live events, and society cannot verify readiness by waiting for disasters. Recurring mandated simulation gives many organizations a shared rehearsal technology, standardizes inter-team interfaces, and produces an auditable readiness signal.
% TRANSFER_FUNCTION: Moves compliance effort, staff time, and training expenditure from regulated operators into the exercise-and-simulation economy (vendors, scenario libraries, auditor time), and moves residual readiness risk — the gap between rehearsed and required performance — onto whoever depends on crisis response when a real event exceeds the rehearsed envelope.
% ABSENT_VOICES: Casualties and bereaved families of past events where live performance fell short of drilled performance are absent from mandate design; independent human-factors researchers on training transfer sit outside the regulatory process; frontline responders participate in drills but hold no seat in setting what counts as adequate fidelity.
% DISAPPEARANCE_RATIONALE: If the mandate regime vanished overnight, rehearsal would become voluntary and unevenly funded, insurers would lose their readiness-rating input, the simulator market would contract sharply, and readiness would diverge across operators according to margin and culture — the preparedness economy built around the mandate would visibly rearrange.
% FOUNDING_PROBLEM: Rare catastrophic events cannot be used as training: no organization can rehearse a reactor accident, mass-casualty event, or aircraft emergency live, and regulators had no verifiable way to require and audit preparedness without waiting for disasters to reveal its absence.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: accident investigation boards document that unrehearsed interfaces fail in actual events; the human-factors literature on skill decay and transfer of training independently establishes that rare skills atrophy without rehearsal; the pre-mandate historical record of industrial and transport disasters attests the original problem. No corroborating source outside the benefiting parties attests that current mandated fidelity levels suffice — that question remains open, and its openness is itself signal.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: genuine rehearsal value is real but partial — the mandate buys procedural fluency and interface coordination while leaving protection conditional on fidelity, and layers compliance rents (vendor revenue, audit labor, artifact value) on top; the referent is the standing mandate regime, not an idealized program. Suppression 0.50, unscaled by construction: audit sanctions and procurement consequences suppress overt challenge to drill adequacy, and drill-success confidence suppresses it internally; suppression is a raw structural property and the engine scales only extractiveness. Theater_ratio 0.52: scenario selection favors winnable scenarios, exercises are scheduled and announced, after-action reports are curated for the audit file — a share of exercise activity approaching half now defends the metric rather than the competence. Accessibility_collapse 0.40: superior alternatives (no-notice drills, red-team adversaries, instrumented transfer measurement) remain visible and occasionally adopted, but compliance-shaped exercise crowds them out because the audit recognizes only mandated forms. Resistance 0.50: investigation bodies and human-factors researchers contest adequacy after every revealing event, and professional bodies push back on mandate expansions, without ever assembling a coalition able to replace the metric system — the harmed public is diffuse and learns of the gap only retrospectively, which blunts coalition formation. The measurement series share one grid (points 0-36, step 6): base_extractiveness and theater_ratio rise together as metric substitution compounds, and suppression_requirement rises as the audit ratchet hardens — enforcement capacity grew over the interval, which is why suppression_requirement is tracked rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator seat the regime computes as successful oversight: mandates met, metrics improving, readiness demonstrable on paper. From the operator seat it computes as a managed cost center that doubles as legal armor — a near-symmetric exchange. From the vendor seat it is simply a market. From the responder and public seats the same structure computes as extraction: effort and risk flow toward them while the protective promise stays conditional on a fidelity variable no seat but the vendor profitably attends to. The engine derives these per-seat classifications from the structural data; the divergence between the regulator's coordination experience and the public's extraction experience is the perspectival fact the corpus exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. safety_regulators and simulator_training_vendors sit at the beneficiary pole: the regime subsidizes their oversight legitimacy and revenue respectively, and both hold mobile or arbitrage exit. catastrophe_exposed_public sits nearest the target pole: trapped, powerless, bearing the residual risk the regime's conditional protection leaves behind. frontline_response_teams sit high-target: they supply the exercised competence and absorb first-order harm when fidelity fails, with constrained exit. regulated_operators are the deliberate correction case: their primary role is payer and their exit is constrained, which the structural derivation alone would push toward the target pole, but their situation documents capture of the regime's principal private gain — the compliance record functioning as liability shield and insurance input — placing their net position near symmetric; the directionality_overrides entry sets the powerful atom to 0.5 for exactly this reason, and omega operator_net_position_ambiguity marks the residual uncertainty. accident_investigation_bodies hold the analytical seat and take no directional position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — safe rehearsal of unrepeatable catastrophe and auditable readiness — is live, and the arrangement still performs its coordination function, so the R5 mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag. Mandatrophy risk here is subtler than obsolescence: the mandate's function is migrating from producing readiness to producing evidence of readiness, visible in the theater_ratio trajectory crossing 0.5 late in the interval. Classification guards both mislabels: calling the regime a rope ignores the identifiable victims of the fidelity gap and the enforced crowding-out of higher-fidelity alternatives; calling it a snare erases the genuine procedural competence the exercises demonstrably build. Tangled_rope holds both facts in one structure: coordination that works, extraction riding on it, active enforcement required to keep the artifact economy closed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the simulation_sufficiency_reading of the kernel exercise_as_competence_maintenance; what structural changes follow if either sibling reading — lived_catastrophe_necessity_reading or hybrid_decay_reading — is adopted instead? The disagreement is located in two elements: whether simulation reaches the competence kernel at all, and whether the kernel is unitary or composed of separately-exercised components.',
    'Comparative classification across the three reading stories in the family: each reading authors its own beneficiaries, victims, and epsilon over the same standing drill-mandate arrangement; divergence in computed types localizes which structural element the readings actually dispute.',
    'Under lived_catastrophe_necessity_reading the victim set expands to everyone relying on competence that never received real-stakes activation and epsilon rises sharply, since nearly the whole protective promise reads as vacuous; under hybrid_decay_reading victims include those harmed specifically by the unexercised judgment component and the mandate''s coordination claim narrows to procedural competence only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three readings of the exercise-as-competence-maintenance kernel; sibling adoption changes victim sets and epsilon.').

omega_variable(
    fidelity_threshold_location,
    'What minimum simulation fidelity does retention of crisis-response competence actually require, and do currently mandated exercises meet it?',
    'Instrumented transfer studies correlating graded simulator fidelity (stress inoculation, no-notice conditions, decision ambiguity) with live-event performance outcomes across regulated sectors.',
    'If mandated exercises sit well below the threshold, the arrangement''s protective function is largely nominal, effective extraction rises sharply, and the classification trends snare-ward; if mandates meet the threshold, the coordination reading strengthens and extraction compresses toward coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_threshold_location, empirical, 'Location of the fidelity threshold relative to mandated exercise design.').

omega_variable(
    pass_rate_metric_substitution,
    'Have simulator performance metrics become targets detached from live readiness, and how large is the resulting divergence?',
    'Systematic comparison of drill scores and after-action reports against performance in actual events for the same teams and comparable scenarios.',
    'Large divergence means theater_ratio is understated and the mandate''s evidentiary function is hollow; classification shifts toward piton (theatrical maintenance of a spent function) or snare (artifact rents without delivered protection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pass_rate_metric_substitution, empirical, 'Divergence between simulator performance metrics and live readiness.').

omega_variable(
    operator_net_position_ambiguity,
    'Is the regulated operator a net payer or net beneficiary once the liability shield, insurance-rating effects, and litigation-defense value of the compliance record are priced against total drill cost?',
    'Actuarial and legal-cost analysis comparing operators'' exercise expenditure with realized liability reductions attributable to documented drill programs.',
    'Net-beneficiary status pulls the operator''s directionality below symmetric and weakens the extraction-asymmetry claim; net-payer status restores it toward the target pole and strengthens the tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operator_net_position_ambiguity, empirical, 'Whether operator compliance cost exceeds the protective value of the compliance record.').

omega_variable(
    complacency_internalization,
    'Is the suppressed skepticism about drill adequacy carried structurally (career and procurement risk for challengers inside the audit regime) or internalized (drill-success confidence displacing doubt among trained staff)?',
    'Post-event debrief and survey data: whether teams with strong drill records show premature certainty in live events, and whether internal critics of exercise design exist and what happens to them.',
    'If internalized, effective suppression exceeds the structural measure — the fidelity gap persists even where formal challenge channels open — and remediation requires reframing drill success rather than only loosening audit incentives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complacency_internalization, empirical, 'Structural versus internalized suppression of adequacy skepticism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(escm_ssr_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(escm_ssr_tr_t0, observed).
narrative_ontology:measurement(escm_ssr_tr_t6, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(escm_ssr_tr_t6, observed).
narrative_ontology:measurement(escm_ssr_tr_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(escm_ssr_tr_t12, observed).
narrative_ontology:measurement(escm_ssr_tr_t18, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 18, 0.37).
narrative_ontology:measurement_basis(escm_ssr_tr_t18, observed).
narrative_ontology:measurement(escm_ssr_tr_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement_basis(escm_ssr_tr_t24, observed).
narrative_ontology:measurement(escm_ssr_tr_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(escm_ssr_tr_t30, observed).
narrative_ontology:measurement(escm_ssr_tr_t36, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 36, 0.52).
narrative_ontology:measurement_basis(escm_ssr_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(escm_ssr_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(escm_ssr_be_t0, observed).
narrative_ontology:measurement(escm_ssr_be_t6, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement_basis(escm_ssr_be_t6, observed).
narrative_ontology:measurement(escm_ssr_be_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement_basis(escm_ssr_be_t12, observed).
narrative_ontology:measurement(escm_ssr_be_t18, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 18, 0.51).
narrative_ontology:measurement_basis(escm_ssr_be_t18, observed).
narrative_ontology:measurement(escm_ssr_be_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement_basis(escm_ssr_be_t24, observed).
narrative_ontology:measurement(escm_ssr_be_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(escm_ssr_be_t30, observed).
narrative_ontology:measurement(escm_ssr_be_t36, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 36, 0.58).
narrative_ontology:measurement_basis(escm_ssr_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(escm_ssr_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(escm_ssr_su_t0, observed).
narrative_ontology:measurement(escm_ssr_su_t6, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(escm_ssr_su_t6, observed).
narrative_ontology:measurement(escm_ssr_su_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement_basis(escm_ssr_su_t12, observed).
narrative_ontology:measurement(escm_ssr_su_t18, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 18, 0.44).
narrative_ontology:measurement_basis(escm_ssr_su_t18, observed).
narrative_ontology:measurement(escm_ssr_su_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 24, 0.46).
narrative_ontology:measurement_basis(escm_ssr_su_t24, observed).
narrative_ontology:measurement(escm_ssr_su_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(escm_ssr_su_t30, observed).
narrative_ontology:measurement(escm_ssr_su_t36, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 36, 0.5).
narrative_ontology:measurement_basis(escm_ssr_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'exercise maintains competence' decomposes per the epsilon-invariance principle into three structurally distinct readings of one kernel: simulation sufficiency (this file), lived-catastrophe necessity, and hybrid decay. Each authors its own epsilon, victim set, and type over the same standing drill-mandate arrangement; the epsilons differ because each reading assesses the identical referent from its own premises, not because the observables differ. Structural coupling: the mandate regime (this reading's operational home) funds and legitimates the simulation research base the hybrid reading draws upon, while the lived-catastrophe reading stands outside the mandate economy entirely — contamination propagates from this node toward the hybrid sibling and only weakly toward the lived-catastrophe sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, powerful, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
