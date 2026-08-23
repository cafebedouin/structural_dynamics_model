% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real-Incident Necessity Doctrine for Competence Occupation
 *   domain: organizational/safety-science
 *
 * SUMMARY:
 *   This story instantiates the real_incident_necessity reading of the
 *   competence_occupation kernel: the claim that only actual catastrophic
 *   incidents provide the authentic conditions under which
 *   catastrophic-response competence can be occupied. Taken as the operative
 *   standard, the claim makes rare real incidents the only admissible
 *   observable, confronts high-reliability organizations with an unresolvable
 *   maintenance problem (the training condition cannot be ethically
 *   manufactured, and the competence cannot be maintained without it), and
 *   yields no viable beneficiary structure — catastrophes are unacceptable,
 *   so no seat can be coherently declared a beneficiary of the arrangement.
 *   The epsilon referent is the standing arrangement under contest: the
 *   incident-as-curriculum regime in which each catastrophe is converted into
 *   findings, rules, and recertification, assessed by this reading's own
 *   lights. KEY AGENTS (by structural relationship): see key_agents. The
 *   claim and the metrics are authored independently: the claimed type is
 *   mountain because the necessity claim presents as a natural limit on skill
 *   acquisition under consequence — it would persist regardless of who
 *   defends it, and no party collects from its operation by design — while
 *   the metrics describe the doctrine's institutional shadow, which is
 *   contested, resisted, partly theatrical, and moderately costly. That
 *   tension is the reading's actual condition, not an authoring error.
 *
 * KEY AGENTS:
 *   - incident_investigation_regimes: Agenda-setting administrator (institutional/constrained) — converts each catastrophe into curriculum, findings, and mandates; budget and legitimacy accrue to it with every event
 *   - frontline_response_teams: Primary operational target (organized/constrained) — must execute under authentic conditions carrying whatever preparation the regime permitted
 *   - catastrophe_exposed_publics: Bearing population (powerless/trapped) — their harm constitutes the doctrine's 'authentic conditions'; never consented to be the curriculum
 *   - junior_practitioners: Gated trainees (moderate/constrained) — development indexed to events they cannot schedule
 *   - simulation_training_advocates: Excluded alternative (organized/mobile) — their core claim is ruled categorically insufficient wherever the doctrine governs certification
 *   - regulatory_certification_authorities: Adjudicating observer (institutional/analytical) — decide how much simulator credit substitutes for lived events
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.56).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.52).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.66).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.56).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.66).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real-Incident Necessity Doctrine for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational/safety-science").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, 'e6e51e27-daca-4ece-8a46-8f220e0bc6a1').
narrative_ontology:cs_kernel_codification('e6e51e27-daca-4ece-8a46-8f220e0bc6a1', distributed).
narrative_ontology:cs_authority_grounding('e6e51e27-daca-4ece-8a46-8f220e0bc6a1', practice).
narrative_ontology:cs_interpretation_layer_present('e6e51e27-daca-4ece-8a46-8f220e0bc6a1').
narrative_ontology:cs_reading_relation('e6e51e27-daca-4ece-8a46-8f220e0bc6a1', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('e6e51e27-daca-4ece-8a46-8f220e0bc6a1', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('e6e51e27-daca-4ece-8a46-8f220e0bc6a1', foundational, authentic_kernel_occupation_requires_lived_catastrophe).
narrative_ontology:cs_axiom_status(authentic_kernel_occupation_requires_lived_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('e6e51e27-daca-4ece-8a46-8f220e0bc6a1', authentic_kernel_occupation_requires_lived_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('e6e51e27-daca-4ece-8a46-8f220e0bc6a1', secondary, simulated_stress_cannot_inoculate_against_real_catastrophe).
narrative_ontology:cs_axiom_status(simulated_stress_cannot_inoculate_against_real_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('e6e51e27-daca-4ece-8a46-8f220e0bc6a1', simulated_stress_cannot_inoculate_against_real_catastrophe, empirically_contingent).
narrative_ontology:cs_reference_frame('e6e51e27-daca-4ece-8a46-8f220e0bc6a1', consequence_authentication_standard).
narrative_ontology:cs_drift_state('e6e51e27-daca-4ece-8a46-8f220e0bc6a1', contemporary_high_fidelity_simulation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e6e51e27-daca-4ece-8a46-8f220e0bc6a1', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, frontline_response_teams).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, catastrophe_exposed_publics).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, junior_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the standing apparatus that converts each catastrophe into findings, recommendations, and revised mandates — accident boards, sentinel-event review panels, internal post-incident review offices. Their budget, staffing, and statutory authority expand with each event the doctrine designates as curriculum; between events they maintain the archive and await the next one. Exit is constrained: the apparatus exists only so long as events occur, yet dismantling it after any tragedy is politically impossible.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_investigation_regimes, agenda_setter,
    institutional, generational, constrained, global).

% Cockpit crews, surgical teams, reactor shift crews, incident-command units. When the rare event arrives they execute under the authentic conditions the doctrine says cannot be rehearsed, carrying whatever preparation the regime permitted. Professional associations give them collective voice, but no amount of organizing schedules the experience their qualification is indexed to.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, frontline_response_teams, payer,
    organized, biographical, constrained, national).

% Passengers, patients, plant neighbors, building occupants — the people inside the event. Their harm is the raw material the doctrine calls authentic conditions; they did not consent to serve as the curriculum and cannot audit the trade their injury purchases. Abstaining from flight, hospital care, or urban life is not a live alternative, so exit is effectively nil.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, catastrophe_exposed_publics, payer,
    powerless, biographical, trapped, regional).

% Trainees and early-career operators whose advancement criteria weight real-event participation. Their development timeline is indexed to occurrences they cannot cause, hasten, or schedule; careers advance on lottery draws of posting and luck, and the doctrine dismisses the simulator hours they log as categorically second-best.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, junior_practitioners, payer,
    moderate, biographical, constrained, national).

% HRO researchers, simulator vendors, training scientists, and the professional societies advancing rehearsal-based readiness. Wherever the doctrine governs certification their core product is ruled categorically insufficient; they publish, sell, and lobby from outside the room where qualification standards are set, and their mobility lies in serving jurisdictions and industries that have not adopted the doctrine.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_training_advocates, excluded,
    organized, biographical, mobile, global).

% Civil aviation authorities, medical boards, nuclear regulators: the seats that decide how much simulator credit substitutes for lived event experience. They commission comparative studies, hear both sides' testimony, and periodically re-weight the substitution ratio — the lever by which the contest over competence standards is actually settled, jurisdiction by jurisdiction.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, regulatory_certification_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__real_incident_necessity, incident_investigation_regimes).
narrative_ontology:fixing_cost_class(competence_occupation__real_incident_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single shared criterion for what counts as genuine qualification for catastrophic-event response — resolving disputes about training adequacy by admitting only one form of evidence, the survived real event, so that boards, regulators, and operators coordinate on a common (if austere) standard of authenticity.
% TRANSFER_FUNCTION: Moves harm and firsthand knowledge from catastrophe-exposed populations and frontline teams into the institutional record — investigation findings, revised procedures, recertification requirements — and moves status and authority toward incident-veterans and the investigation apparatus that curates the record.
% ABSENT_VOICES: The dead and injured of each event cannot testify in the investigations their harm convenes; families and survivor advocates stand outside the technical proceedings. Simulation-training advocates are present in the field but excluded from certification decisions wherever the doctrine sets the standard.
% DISAPPEARANCE_RATIONALE: Certification frameworks would re-weight simulator credit immediately, investigation mandates would lose their doctrinal justification and shrink to compliance functions, incident-veteran premium status would deflate, and training budgets would migrate toward the rehearsal-based regimes the doctrine currently rules insufficient — the competence-maintenance economy reorganizes around whichever criterion replaces authenticity-through-consequence.
% FOUNDING_PROBLEM: Industries facing rare, high-consequence events found that pre-event training could not reproduce the conditions under which the competence would be used, and that the same failures recurred until each catastrophe forced a rewrite — the doctrine crystallized as both explanation ('the unrehearsable can only be learned by living it') and justification for rulemaking written in the casualties of the last event.
% FOUNDING_PROBLEM_CORROBORATION: Investigation regimes attest the problem's liveness self-interestedly, since each affirmation of liveness funds the apparatus. Corroboration from outside that circle: the academic HRO and skill-decay literature (which documents both genuine simulation-transfer limits and genuine rehearsal gains), regulator-commissioned simulator-credit studies, and survivor-family advocacy testimony disputing that any pedagogic yield redeems the price. No party outside the doctrine's orbit attests that the founding problem is closed.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, ExtMetricName, E),
    domain_priors:suppression_score(competence_occupation__real_incident_necessity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_occupation__real_incident_necessity),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.56: where the doctrine governs, it channels real costs — publics bear the harm that becomes curriculum, frontline teams execute under-prepared, simulation investment is discounted — while the post-incident learning it produces is genuine, so net extraction is moderate rather than severe. Suppression 0.52: the doctrine suppresses alternatives by dismissal and defunding rather than coercion; it is a raw structural property and is not scaled by power or scope in the engine's computation. Theater_ratio 0.66 and rising: the doctrine's institutional life is increasingly ritual — 'lessons learned' reports recycle, recommendations are filed and re-filed, and organizations perform readiness while knowing the kernel is vacant; the secular rise models Goodhart drift on the post-incident process. Accessibility_collapse 0.82: for anyone who accepts the necessity claim, the alternative space collapses nearly completely — simulation is categorically insufficient, and the choice reduces to accepting catastrophe as tuition or accepting permanent kernel vacancy; the residual 0.18 reflects the live empirical contest. Resistance 0.60: unusually high for a claimed mountain, because the claim is contested — the simulation-training complex, the HRO research community, and regulator simulator-credit policies actively resist it; a certified natural law would meet near-zero resistance, and this divergence is exactly the signal the false-summit machinery exists to examine. Temporal structure: all three series share one grid (t=0..30, step 3). Extractiveness and suppression cycle with roughly period 12 — surge after each catastrophe (t≈3, 15, 27), decay between events — while theater rises monotonically underneath. The cycle is plausibly intermittent reinforcement: organizations defer safety investment until fresh harm restores salience (see omega intermittent_reinforcement_cycle). Base_properties values correspond to t=30, on the decaying limb just past the last spike. Suppression_requirement is authored because the story specifically tracks enforcement-capacity change: mandatory incident-experience requirements and post-accident rulemaking surge after events and attrit between them, while the necessity claim itself requires no enforcement to persist — the flag requires_active_enforcement is false for the claim, and the cycling series describes the regime built around it.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the investigation regime's position the doctrine is sacred pedagogy — the only honest teacher, administered with rigor. From the catastrophe-exposed publics' position the same structure is being spent as tuition without consent. From the simulation advocates' position it is dogma that blocks a solvable problem. From the frontline teams' position it is a promise that their ordeal will mean something. The engine computes per-seat classifications from the structural data; this story authors the data and does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared in base_properties, following the reading's expected structural delta: catastrophes are unacceptable, so no viable beneficiary structure exists, and declaring one would smuggle in an endorsement the reading cannot support. With no beneficiary/victim declarations, the derivation chain falls back to per-power-atom defaults; qualitatively, the investigation regimes sit nearest the beneficiary end (they collect mandate and budget, though receipt is not role — they administer rather than merely collect), the catastrophe-exposed publics sit nearest the full-target end (they bear the harm outright, with no exit), frontline teams and junior practitioners sit high-target (they bear operational and career costs with constrained exit), simulation advocates are suppressed targets with arbitrage-grade mobility, and the certification authorities sit near symmetric as adjudicators. Directionality overrides are deliberately omitted: the override mechanism is keyed by power atom and would function as a substitute for the undeclared structural data, which the authoring rules discourage; the no-beneficiary structure is itself the finding, and omega no_beneficiary_structure_framing documents the ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — pre-event training cannot reproduce authentic conditions, so each catastrophe forces the rewrite — is contested rather than dead: simulation advances genuinely erode the claim's domain while catastrophic events continue to anchor major reforms. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges, which produces no capture/zombie flag; the arrangement's persistence tracks a live dispute, not an abandoned mandate. The mandatrophy discipline prevents two opposite mislabels: reading the doctrine's heavy investigative apparatus as pure rent-seeking coordination (which would ignore the genuine epistemic function of incident learning) and reading it as pure natural necessity (which would ignore the rising theater ratio and the concentrated receipt of gains in the investigation complex). If simulation-transfer evidence matures and the doctrine survives only as investigation ritual, mandatrophy resolves and the computed type should drift toward piton — theatrical maintenance of an atrophied necessity claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the competence_occupation kernel; which structural features of this story are artifacts of the real_incident_necessity reading rather than of the underlying phenomenon?',
    'Cross-reading comparison: regenerate the story under the simulation_sufficiency and hybrid_occupation readings and diff the derived structures; features stable across readings belong to the phenomenon, the rest to the reading.',
    'Under simulation_sufficiency the unresolvable competence-maintenance problem dissolves and the mountain character evaporates; under hybrid_occupation the question becomes a portfolio-configuration problem with ordinary coordination morphology. Classification is reading-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story''s classification is indexed to the real_incident_necessity reading of the competence_occupation kernel.').

omega_variable(
    simulation_transfer_ceiling,
    'What fraction of authentic-condition competence can high-fidelity simulation actually transfer, and does an irreducible residue exist that only lived catastrophe can occupy?',
    'Longitudinal cohort studies comparing simulator-trained versus incident-experienced crews on subsequent real-event performance, with matched baselines and blind scoring.',
    'A high transfer ceiling with negligible residue falsifies the necessity axiom and collapses this reading toward simulation_sufficiency; a large irreducible residue confirms the mountain claim and hardens the unresolvable-maintenance problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_ceiling, empirical, 'Empirical test of the necessity claim''s core factual premise.').

omega_variable(
    no_beneficiary_structure_framing,
    'Is the absence of declared beneficiaries a structural fact of this constraint, or an artifact of the moral rule that catastrophe-beneficiaries may not be named?',
    'Counterfactual welfare accounting separating descriptive rents (investigation budgets, veteran status premiums) from endorsed welfare flows, audited against the false-summit signature detector.',
    'If descriptive beneficiaries are admitted, the story becomes an FSM candidate and the computed type shifts toward tangled_rope; if the no-beneficiary structure holds, the mountain claim stands unchallenged on that axis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(no_beneficiary_structure_framing, conceptual, 'Whether the no-beneficiary morphology is structural or framing-induced.').

omega_variable(
    intermittent_reinforcement_cycle,
    'Is the cyclic intensity of the doctrine (surge after each catastrophe, decay between events) itself an extraction mechanism — intermittent reinforcement that lets organizations defer safety investment until fresh blood restores salience?',
    'Panel data linking training-investment and enforcement cycles to incident dates across industries; test whether investment troughs systematically precede events.',
    'If the cycle is the mechanism, effective extraction exceeds the scalar series and the drift trajectory bends toward snare; if it is exogenous shock response, the cycle is noise around a stable regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermittent_reinforcement_cycle, empirical, 'Whether the oscillation is an extraction mechanism or exogenous shock response.').

omega_variable(
    veteran_identity_lock,
    'Is the doctrine''s persistence partly carried by identity fusion — experts whose professional selfhood is constituted by having lived the event (''you had to be there'') — rather than by the claim''s evidential force?',
    'Post-refutation persistence study: track whether veteran-authority endorsement of the necessity claim tracks maturing simulation-transfer evidence or biographical investment; persistent endorsement against evidence indicates identity-carried maintenance.',
    'Identity-carried persistence raises effective suppression above the structural measure and predicts piton-style theatrical maintenance after the claim loses evidential force; evidence-carried persistence predicts clean retirement of the doctrine if refuted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veteran_identity_lock, empirical, 'Identity-lock contribution to the doctrine''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.4).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t3, competence_occupation__real_incident_necessity, theater_ratio, 3, 0.46).
narrative_ontology:measurement_basis(comp_tr_t3, observed).
narrative_ontology:measurement(comp_tr_t6, competence_occupation__real_incident_necessity, theater_ratio, 6, 0.44).
narrative_ontology:measurement_basis(comp_tr_t6, observed).
narrative_ontology:measurement(comp_tr_t9, competence_occupation__real_incident_necessity, theater_ratio, 9, 0.47).
narrative_ontology:measurement_basis(comp_tr_t9, observed).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__real_incident_necessity, theater_ratio, 12, 0.5).
narrative_ontology:measurement_basis(comp_tr_t12, observed).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__real_incident_necessity, theater_ratio, 15, 0.54).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t18, competence_occupation__real_incident_necessity, theater_ratio, 18, 0.53).
narrative_ontology:measurement_basis(comp_tr_t18, observed).
narrative_ontology:measurement(comp_tr_t21, competence_occupation__real_incident_necessity, theater_ratio, 21, 0.56).
narrative_ontology:measurement_basis(comp_tr_t21, observed).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__real_incident_necessity, theater_ratio, 24, 0.58).
narrative_ontology:measurement_basis(comp_tr_t24, observed).
narrative_ontology:measurement(comp_tr_t27, competence_occupation__real_incident_necessity, theater_ratio, 27, 0.63).
narrative_ontology:measurement_basis(comp_tr_t27, observed).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__real_incident_necessity, theater_ratio, 30, 0.66).
narrative_ontology:measurement_basis(comp_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t3, competence_occupation__real_incident_necessity, base_extractiveness, 3, 0.62).
narrative_ontology:measurement_basis(comp_be_t3, observed).
narrative_ontology:measurement(comp_be_t6, competence_occupation__real_incident_necessity, base_extractiveness, 6, 0.55).
narrative_ontology:measurement_basis(comp_be_t6, observed).
narrative_ontology:measurement(comp_be_t9, competence_occupation__real_incident_necessity, base_extractiveness, 9, 0.48).
narrative_ontology:measurement_basis(comp_be_t9, observed).
narrative_ontology:measurement(comp_be_t12, competence_occupation__real_incident_necessity, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(comp_be_t12, observed).
narrative_ontology:measurement(comp_be_t15, competence_occupation__real_incident_necessity, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t18, competence_occupation__real_incident_necessity, base_extractiveness, 18, 0.52).
narrative_ontology:measurement_basis(comp_be_t18, observed).
narrative_ontology:measurement(comp_be_t21, competence_occupation__real_incident_necessity, base_extractiveness, 21, 0.47).
narrative_ontology:measurement_basis(comp_be_t21, observed).
narrative_ontology:measurement(comp_be_t24, competence_occupation__real_incident_necessity, base_extractiveness, 24, 0.44).
narrative_ontology:measurement_basis(comp_be_t24, observed).
narrative_ontology:measurement(comp_be_t27, competence_occupation__real_incident_necessity, base_extractiveness, 27, 0.58).
narrative_ontology:measurement_basis(comp_be_t27, observed).
narrative_ontology:measurement(comp_be_t30, competence_occupation__real_incident_necessity, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(comp_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t3, competence_occupation__real_incident_necessity, suppression_requirement, 3, 0.55).
narrative_ontology:measurement_basis(comp_su_t3, observed).
narrative_ontology:measurement(comp_su_t6, competence_occupation__real_incident_necessity, suppression_requirement, 6, 0.42).
narrative_ontology:measurement_basis(comp_su_t6, observed).
narrative_ontology:measurement(comp_su_t9, competence_occupation__real_incident_necessity, suppression_requirement, 9, 0.34).
narrative_ontology:measurement_basis(comp_su_t9, observed).
narrative_ontology:measurement(comp_su_t12, competence_occupation__real_incident_necessity, suppression_requirement, 12, 0.3).
narrative_ontology:measurement_basis(comp_su_t12, observed).
narrative_ontology:measurement(comp_su_t15, competence_occupation__real_incident_necessity, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t18, competence_occupation__real_incident_necessity, suppression_requirement, 18, 0.4).
narrative_ontology:measurement_basis(comp_su_t18, observed).
narrative_ontology:measurement(comp_su_t21, competence_occupation__real_incident_necessity, suppression_requirement, 21, 0.33).
narrative_ontology:measurement_basis(comp_su_t21, observed).
narrative_ontology:measurement(comp_su_t24, competence_occupation__real_incident_necessity, suppression_requirement, 24, 0.29).
narrative_ontology:measurement_basis(comp_su_t24, observed).
narrative_ontology:measurement(comp_su_t27, competence_occupation__real_incident_necessity, suppression_requirement, 27, 0.5).
narrative_ontology:measurement_basis(comp_su_t27, observed).
narrative_ontology:measurement(comp_su_t30, competence_occupation__real_incident_necessity, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(comp_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, identity_coordination).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% The colloquial label 'competence occupation for catastrophic events' decomposes into a three-story constraint family per the epsilon-invariance principle: real_incident_necessity (this story — only lived catastrophe authenticates), simulation_sufficiency (drills suffice), and hybrid_occupation (continuous multi-mechanism portfolios). Each member carries its own epsilon, beneficiary structure, and classification; they are linked here because the upstream empirical question (simulation transfer ceiling) is cited as evidence within the sibling stories, and because certification policy trades the readings off against each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
