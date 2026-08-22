% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: Simulator Adequacy Standard for Competence Maintenance (High-Fidelity Simulation Plus Debriefing Constitutes Sufficient Exercise)
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The arrangement under examination is the codified standard by which
 *   high-reliability industries — instantiated here primarily in aviation,
 *   with direct analogues in nuclear operations and proceduralized medicine —
 *   discharge the duty to keep operator competence exercised. Under this
 *   reading, a scheduled cycle of certified high-fidelity simulation with
 *   structured debriefing constitutes sufficient exercise of the competence
 *   kernel; nothing further is owed. The standard solves a genuine and severe
 *   problem (live rehearsal of catastrophe kills people) while simultaneously
 *   licensing a large, permanent substitution of synthetic hours for real
 *   ones whose savings concentrate on operators and whose residual risk
 *   diffuses onto the public. KEY AGENTS (by structural relationship): -
 *   aviation_regulators: agenda-setter (institutional/constrained) — codifies
 *   the cycle, audits compliance, collects oversight tractability -
 *   airline_operators: primary beneficiary (institutional/mobile) — books the
 *   saved real-aircraft cost - simulator_training_industry: secondary
 *   beneficiary (institutional/mobile) — sells the mandated cycles -
 *   line_pilots: dual-positioned (organized/constrained) — receives
 *   jeopardy-free training, bears skill-atrophy and first-seat exposure -
 *   flying_public: primary target (powerless/trapped) — carries the unpriced
 *   tail risk - safety_science_community: excluded voice
 *   (moderate/analytical) — argues the hybrid alternative, holds no standing
 *   seat - accident_investigation_bodies: observer (institutional/analytical)
 *   — sees the failure population the standard predicted should not exist.
 *   This file instantiates ONE reading of the competence_exercise_requirement
 *   kernel; the sibling readings are separate constraints linked through the
 *   network section, and epsilon here refers to the standing sim-adequacy
 *   arrangement as this reading holds it — not to the hybrid regime this
 *   reading declines to require.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.58).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.5).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "Simulator Adequacy Standard for Competence Maintenance (High-Fidelity Simulation Plus Debriefing Constitutes Sufficient Exercise)").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, '51ef260b-7d3f-4824-8fb8-2f06ae727a0c').
narrative_ontology:cs_kernel_codification('51ef260b-7d3f-4824-8fb8-2f06ae727a0c', formalized).
narrative_ontology:cs_authority_grounding('51ef260b-7d3f-4824-8fb8-2f06ae727a0c', expertise).
narrative_ontology:cs_interpretation_layer_present('51ef260b-7d3f-4824-8fb8-2f06ae727a0c').
narrative_ontology:cs_reading_relation('51ef260b-7d3f-4824-8fb8-2f06ae727a0c', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('51ef260b-7d3f-4824-8fb8-2f06ae727a0c', competence_exercise_requirement__hybrid_dependency, forecloses).
narrative_ontology:cs_axiom('51ef260b-7d3f-4824-8fb8-2f06ae727a0c', foundational, certified_fidelity_transfer_sufficiency).
narrative_ontology:cs_axiom_status(certified_fidelity_transfer_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('51ef260b-7d3f-4824-8fb8-2f06ae727a0c', certified_fidelity_transfer_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('51ef260b-7d3f-4824-8fb8-2f06ae727a0c', foundational, structured_debrief_experiential_equivalence).
narrative_ontology:cs_axiom_status(structured_debrief_experiential_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('51ef260b-7d3f-4824-8fb8-2f06ae727a0c', structured_debrief_experiential_equivalence, empirically_contingent).
narrative_ontology:cs_reference_frame('51ef260b-7d3f-4824-8fb8-2f06ae727a0c', certified_fidelity_sufficiency_baseline).
narrative_ontology:cs_drift_state('51ef260b-7d3f-4824-8fb8-2f06ae727a0c', contemporary_post_accident_review_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('51ef260b-7d3f-4824-8fb8-2f06ae727a0c', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, simulator_training_industry).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, aviation_regulators).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, flying_public).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, crews_facing_unrehearsed_tail_events).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, line_pilots).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, line_pilots).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certifies simulator fidelity levels, prescribes recurrent training cycle content, and audits operator compliance records. The codified cycle gives them a finite, inspectable artifact — a logbook entry — in place of open-ended judgment about whether a workforce is genuinely exercised for rare events. Revisiting the adequacy question would reopen harmonized international standards their own precedents anchor, so revision is institutionally expensive even when accident data prompts it.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, aviation_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, aviation_regulators, beneficiary).

% Run scheduled simulator cycles instead of consuming real aircraft hours, fuel, maintenance slots, and insurance exposure for training. Each hour moved from the line into the simulator is a direct, booked cost saving, and the adequacy standard is what licenses the substitution. They participate actively in standards bodies and can shift fleets between registries if one jurisdiction tightens requirements faster than competitors'.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators, beneficiary,
    institutional, biographical, mobile, global).

% Builds and operates the full-flight simulators and training centers that the mandated cycles purchase. Every expansion of the required cycle count, and every narrowing of real-aircraft alternatives, enlarges their addressable market. Their commercial interest aligns with the adequacy claim being read broadly.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, simulator_training_industry, beneficiary,
    institutional, generational, mobile, global).

% Train in high-fidelity devices without jeopardy — no license risk, no physical danger, repeatable scenarios — which they genuinely value. The same regime means their manual handling and startle-exposure accumulate mainly in synthetic conditions while line flying grows more automated; when a genuinely novel event arrives, they are the ones in the seat, carrying both the personal danger and the legal accountability for an encounter their training never reproduced.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, line_pilots, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, line_pilots, payer).

% Board aircraft operated by crews whose competence assurance was discharged entirely in synthetic conditions. They bear the residual probability that a rare, compound, off-envelope event exceeds what the simulated repertoire covered — a risk they did not price, did not consent to, and cannot opt out of short of never flying. No seat exists for them in the working groups that set cycle content.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, flying_public, payer,
    powerless, immediate, trapped, global).

% Human-factors researchers and veteran instructors who argue that startle response, consequence salience, and genuine novelty have no full synthetic substitute, and who propose hybrid regimes pairing simulation with periodic real-aircraft anchoring. They publish, testify after accidents, and are then consulted episodically; they hold no standing vote in cycle-content working groups, and their objections recede between events.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, safety_science_community, excluded,
    moderate, civilizational, analytical, global).

% Reconstruct the small population of events the adequacy standard predicted should be handled: loss-of-control following automation surprise, manual-handling breakdown after years of synthetic-dominant practice. Their findings arrive after the fact, enter the record as recommendations, and exert pressure on cycle content only until the next harmonization round settles the question again.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, accident_investigation_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective problem: crews cannot rehearse catastrophic emergencies in live operation without killing people, so competence for rare events must be exercised somewhere safe. High-fidelity simulation with structured debriefing provides a standardized, repeatable, schedulable, auditable exercise venue shared across thousands of crews and dozens of jurisdictions.
% TRANSFER_FUNCTION: Moves training hours and their associated cost out of real aircraft and into purchased simulator cycles — concentrating the saved operating cost on operators and the simulator industry — while relocating the residual risk of rare-event mishandling from the training environment onto the traveling public, who carry it unpriced. Revenue flows to simulator providers; assurance artifacts flow to regulators.
% ABSENT_VOICES: The traveling public has no representative in training-rule working groups; the safety-science proponents of hybrid anchoring are heard episodically after accidents rather than seated continuously; accident investigators hold a microphone that only functions retrospectively. Unanimity around the adequacy standard arises in rooms from which the parties bearing its tail risk were never present.
% DISAPPEARANCE_RATIONALE: If the sim-adequacy standard vanished overnight, every operator's compliance program would be void, training schedules worldwide would halt pending reconstruction, simulator capacity contracts would be renegotiated, and jurisdictions would race to rebuild either hybrid real-aircraft anchoring or fragmented national substitutes — a multi-year reorganization of how an entire industry discharges its competence duty.
% FOUNDING_PROBLEM: Early aviation trained crews in real aircraft under real risk, and rehearsing emergencies live killed trainees and bystanders. The founding problem was how to exercise competence for events too dangerous to practice — how to give crews genuine encounter with failure modes without paying for it in hulls and lives.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation reports (loss-of-control and startle-related mishaps involving manually degraded crews), peer-reviewed human-factors literature on stress inoculation, and union testimony on manual-handling decline all attest, from outside the operator and regulator beneficiary set, that the underlying problem — exercising rare-event competence safely and adequately — remains unsolved at the margin the standard governs.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.58 because the arrangement's surplus is real but split: the coordination core (safe, scalable, standardized rehearsal) is a genuine achievement that transformed an entire industry's safety profile, while the margin above genuine cost — the portion of the substitution justified by the adequacy claim rather than by demonstrated transfer — accrues as concentrated savings against diffuse, unconsented risk. Suppression is 0.50 and operates definitionally rather than coercively: once adequacy is codified, compliance becomes the complete answer to the competence question, and demanding more than compliance marks an operator as anomalous; the alternative (hybrid anchoring) is not banned, it is made institutionally expensive. Theater ratio is 0.25 — the training function is overwhelmingly real, with checkbox drift concentrated in debrief quality and scripted scenario familiarity. Accessibility collapse is 0.60: codification closes most practical alternatives, though voluntary hybrid programs persist at operators willing to pay above the floor. Resistance is 0.35: episodic, post-accident surges (manual-handling findings, startle research) that recede at each harmonization round. All three temporal series run on one shared grid (t=0..42, step 6); the monotonic rise in every series models the codification ratchet — each successive standards round narrows real-aircraft requirements further, hardens the audit surface, and layers compliance formality onto the original exercise function. Post-accident perturbations temporarily bend these curves before the ratchet resumes; the net trajectory is upward.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and operator seats, the arrangement presents as certified assurance: a finite, inspectable artifact demonstrating the duty was discharged, backed by decades without the catastrophes the training targets. From the line-pilot seat it presents doubly — as the safest training environment ever built and as the reason their hands are rusty when the automation quits. From the passenger seat it presents as nothing at all: a risk carried invisibly, consented to by no one, represented in no working group. The accident investigator sees the small population of events the adequacy formula predicted should have been handled. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Airline operators and the simulator industry sit near the beneficiary pole: the standard subsidizes their cost structure and market respectively, with mobile exit (registry shopping, diversified product lines) damping any residual cost they bear. Regulators sit beneficiary-side as well — the codified cycle converts an unbounded judgment problem into an auditable artifact — though their agenda-setter role means they also absorb reputational cost when the tail event arrives. Line pilots derive near-symmetric directionality: genuine benefit (jeopardy-free training) nearly cancelling personal exposure and skill atrophy. The flying public sits at the full-target pole: powerless, trapped (no exit short of abstaining from flight), bearing the exact residual the adequacy claim externalizes. Crews facing unrehearsed tail events share that pole in the moments that matter. Beneficiary and victim declarations map onto these positions directly; no directionality override was needed because the derivation from declared structure reproduces these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — exercising catastrophic-event competence without killing anyone — is live, not dead: the need it addressed has not dissolved, and the corroborating sources outside the beneficiary set (investigation reports, human-factors literature, union testimony) attest continued occurrence of exactly the failures adequate exercise should prevent. Mandatrophy is therefore not resolved, and the classification guards against both symmetrical errors: reading the arrangement as pure coordination would erase the uncompensated transfer from an unrepresented public to organized beneficiaries; reading it as pure extraction would erase the single largest safety achievement in the domain's history and license abandoning simulation altogether. The tangled-rope structure holds both facts. The forward risk is not obsolescence but drift — debrief quality decaying into completion signaling while the audit surface grows — which the theater_ratio series tracks and the debrief-quality omega interrogates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_resolution,
    'This constraint is one reading of the competence_exercise_requirement kernel — the reading that certified high-fidelity simulation with debriefing constitutes sufficient exercise. Would the sibling readings (catastrophe_as_necessary_anchor: only real catastrophic events or near-misses provide irreducible exercise; hybrid_dependency: simulation necessary but requiring periodic real-world anchoring) better characterize what actually maintains the competence kernel?',
    'Longitudinal outcome comparison across jurisdictions that differ in real-aircraft anchoring requirements — cohorts trained sim-only versus hybrid, followed on rare-event and startle-related incident rates over decades.',
    'If a sibling reading wins, the surplus this arrangement concentrates on operators stops being justified by adequacy and reads as cost avoidance licensed by definition — shifting classification toward the purely extractive end; if this reading survives, the measured extraction collapses toward coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, empirical, 'Which reading of the competence-exercise kernel is structurally correct.').

omega_variable(
    tail_risk_externalization_magnitude,
    'How much residual tail risk does the sim-only standard actually relocate onto the traveling public, relative to the operating-cost savings it concentrates on operators?',
    'Quantified precursor analysis — loss-of-control, automation-surprise, and manual-handling event rates normalized against fleet exposure — compared across training regimes of differing synthetic share.',
    'Near-zero externalization trends the arrangement toward pure coordination; substantial externalization establishes an uncompensated transfer from an unrepresented party to organized beneficiaries and raises effective extraction sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_risk_externalization_magnitude, empirical, 'Magnitude of the uncompensated risk transfer embedded in the adequacy claim.').

omega_variable(
    fidelity_ceiling_fundamentality,
    'Is the gap between simulated and lived encounter — startle response, consequence salience, genuine novelty — an engineering deficit that rising fidelity will close, or a structural ceiling inherent to knowing the event is not real?',
    'Psychophysiological stress-inoculation research comparing crew responses in certified simulators against recorded responses in actual line events, tracked as fidelity generations advance.',
    'A structural ceiling invalidates the reading''s transfer axiom permanently and strengthens the hybrid sibling; a closable gap lets this reading converge on truth as technology improves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_ceiling_fundamentality, conceptual, 'Whether the simulation-to-line transfer gap is fundamental or merely current-engineering.').

omega_variable(
    debrief_quality_goodhart_drift,
    'Does structured debriefing retain its diagnostic function as compliance auditing intensifies, or does it degrade into completion signaling — scenarios flown to script, debriefs logged rather than conducted?',
    'Blinded scoring of recorded debrief sessions against declared scenario learning objectives, sampled across operators and years.',
    'Sustained degradation would drive theater_ratio upward, indicate proxy displacement of the training function, and push the arrangement toward inertial persistence maintained by paperwork.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(debrief_quality_goodhart_drift, empirical, 'Whether the debriefing half of the adequacy formula is drifting theatrical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comp_tr_t6, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 6, 0.13).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 12, 0.16).
narrative_ontology:measurement(comp_tr_t18, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 18, 0.18).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 24, 0.21).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 30, 0.23).
narrative_ontology:measurement(comp_tr_t36, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 36, 0.24).
narrative_ontology:measurement(comp_tr_t42, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 42, 0.25).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(comp_be_t6, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(comp_be_t12, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(comp_be_t18, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 18, 0.44).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(comp_be_t30, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(comp_be_t36, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 36, 0.56).
narrative_ontology:measurement(comp_be_t42, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 42, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(comp_su_t6, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 6, 0.33).
narrative_ontology:measurement(comp_su_t12, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(comp_su_t18, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 18, 0.41).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 24, 0.44).
narrative_ontology:measurement(comp_su_t30, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(comp_su_t36, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 36, 0.49).
narrative_ontology:measurement(comp_su_t42, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 42, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, hybrid_dependency).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, automation_manual_skill_decay).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'keeping crews competent' decomposes into three structurally distinct readings of one kernel. This file (simulation_as_adequate_exercise) authors the sufficiency claim; catastrophe_as_necessary_anchor authors the irreducibility-of-real-events claim; hybrid_dependency authors the two-component claim. Epsilon differs across the family because the referent arrangements differ: this reading's standing arrangement (codified sim-only cycles) carries the cost-transfer structure measured here, while the hybrid sibling's arrangement would internalize real-aircraft anchoring costs back onto operators. The upstream/downstream edge runs from this reading to automation_manual_skill_decay because the adequacy claim is cited as licensing the synthetic-dominant practice whose skill-decay consequences that constraint tracks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
