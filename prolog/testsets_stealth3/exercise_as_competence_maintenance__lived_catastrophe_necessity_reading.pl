% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Exercise-Certified Readiness Regime (Lived-Catastrophe Necessity Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   A standing international arrangement requires operators of hazardous
 *   systems — nuclear stations, airlines, hospital networks, chemical
 *   complexes, emergency services — to demonstrate crisis readiness chiefly
 *   through scheduled, documented simulation exercises, and accepts those
 *   records as admissible evidence when licenses, accreditations, and
 *   insurance terms are issued. This file instantiates the
 *   lived_catastrophe_necessity_reading of the
 *   exercise_as_competence_maintenance kernel: on this reading, only actual
 *   catastrophe activates the full competence kernel (judgment under real
 *   stakes), simulation functions as rehearsal of procedure rather than
 *   maintenance of the thing itself, and competence decays covertly between
 *   real events. Measured by that reading's lights, the standing regime
 *   collects a large assurance premium — licenses, insurance pricing,
 *   accreditation status, public confidence — from evidence it cannot in
 *   principle supply, while transferring untested-judgment risk to everyone
 *   exposed to certified operators. Epsilon's referent is the standing
 *   simulation-certified preparedness arrangement as this reading assesses
 *   it, never the apprenticeship-heavy alternative this reading would prefer.
 *   Interval 0–30 maps approximately to the mid-1990s through mid-2020s
 *   maturation of exercise-based certification. This story belongs to a
 *   three-member constraint family; see network.dual_formulation_note and
 *   commentary.kernel_context. KEY AGENTS (by structural relationship): -
 *   safety_certification_agencies: Agenda setter (institutional/constrained)
 *   — administers the regime and adjudicates what counts as readiness
 *   evidence - licensed_hazardous_operations_firms: Primary beneficiary
 *   (institutional/arbitrage) — converts drill compliance into licensing,
 *   premiums, and continuity - simulation_vendor_industry: Secondary
 *   beneficiary (organized/mobile) — supplies the exercise apparatus -
 *   catastrophe_exposed_publics: Primary target (powerless/trapped) — absorbs
 *   outcomes when rehearsed response fails real conditions -
 *   untested_frontline_operators: Target with dual receipt
 *   (organized/identity_locked) — carries the false-confidence exposure
 *   personally while receiving genuine procedural training -
 *   veteran_real_stakes_advocates: Excluded voice (organized/constrained) —
 *   argues real consequences are irreplaceable, holds no rule-making seat -
 *   independent_skill_decay_researchers: Analytical observer
 *   (analytical/analytical) — produces the retention and transfer literature
 *   both sides cite
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.66).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.55).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Exercise-Certified Readiness Regime (Lived-Catastrophe Necessity Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'f261ca2b-e1d0-4b40-ac53-511143681fda').
narrative_ontology:cs_kernel_codification('f261ca2b-e1d0-4b40-ac53-511143681fda', distributed).
narrative_ontology:cs_authority_grounding('f261ca2b-e1d0-4b40-ac53-511143681fda', practice).
narrative_ontology:cs_interpretation_layer_present('f261ca2b-e1d0-4b40-ac53-511143681fda').
narrative_ontology:cs_reading_relation('f261ca2b-e1d0-4b40-ac53-511143681fda', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('f261ca2b-e1d0-4b40-ac53-511143681fda', exercise_as_competence_maintenance__hybrid_decay_reading, coexists_with).
narrative_ontology:cs_axiom('f261ca2b-e1d0-4b40-ac53-511143681fda', foundational, real_stakes_sole_kernel_activator).
narrative_ontology:cs_axiom_status(real_stakes_sole_kernel_activator, holdable).
narrative_ontology:cs_axiom_grounding('f261ca2b-e1d0-4b40-ac53-511143681fda', real_stakes_sole_kernel_activator, empirically_contingent).
narrative_ontology:cs_axiom('f261ca2b-e1d0-4b40-ac53-511143681fda', secondary, covert_decay_creates_latent_exposure).
narrative_ontology:cs_axiom_status(covert_decay_creates_latent_exposure, holdable).
narrative_ontology:cs_axiom_grounding('f261ca2b-e1d0-4b40-ac53-511143681fda', covert_decay_creates_latent_exposure, empirically_contingent).
narrative_ontology:cs_reference_frame('f261ca2b-e1d0-4b40-ac53-511143681fda', real_event_validated_competence).
narrative_ontology:cs_drift_state('f261ca2b-e1d0-4b40-ac53-511143681fda', contemporary_prevention_success_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f261ca2b-e1d0-4b40-ac53-511143681fda', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, licensed_hazardous_operations_firms).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_certification_agencies).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_vendor_industry).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, catastrophe_exposed_publics).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, untested_frontline_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, untested_frontline_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, licensed_hazardous_operations_firms).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, drill_compliance_evidences_readiness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate minimum drill hours, scenario frequencies, and exercise formats for licensed hazardous operations, and accept documented exercise performance as the admissible evidence of readiness when issuing or renewing licenses. They audit exercise records, publish pass criteria, and defend the arrangement in hearings after incidents. Their alternatives are narrow: conceding that scheduled exercises cannot establish real-event readiness would undercut the licensing framework they administer, so their exit runs through redesigning the evidence standard they themselves policed into place.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_certification_agencies, agenda_setter,
    institutional, generational, constrained, global).

% Run nuclear plants, airlines, hospital systems, and chemical complexes whose licenses require documented exercise compliance. They fund the drill program, purchase vendor services, and file the records that keep insurance premiums low and licenses current. Because exercise performance is the accepted currency of readiness, they can meet the obligation at predictable cost and on their own calendar, without unscheduled disruption to operations; jurisdiction shopping and regulatory negotiation give them room to soften requirements that bite.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, licensed_hazardous_operations_firms, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, licensed_hazardous_operations_firms, payer).

% Builds and sells simulator hardware, scenario software, and exercise facilitation services; revenue tracks mandated exercise volumes and fidelity upgrade cycles. Customers span industries and borders, so when one regulator loosens, sales follow the next one that tightens.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_vendor_industry, beneficiary,
    organized, biographical, mobile, global).

% Live downwind, downstream, or aboard the systems operated under this regime — plant corridors, chemical valleys, flight paths, the catchment areas of major trauma centers. They absorb the consequences when rehearsed-but-untested response meets real conditions, and relocating away from the hazard is not a realistic option at ordinary household cost. Their principal lever is collective pressure, and it historically arrives only after an event has already exposed the gap.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, catastrophe_exposed_publics, payer,
    powerless, generational, trapped, regional).

% Crew the plants, wards, cockpits, and firegrounds. They complete the mandated exercises, take the passing scores as evidence of personal readiness, and are first to meet the difference between the rehearsed scenario and the real one. Union membership, licensure, and seniority tie income and standing to remaining in the role; leaving the profession would forfeit the livelihood and the identity built around being the person who handles the emergency.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, untested_frontline_operators, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, untested_frontline_operators, beneficiary).

% Retired incident commanders, senior clinicians, test pilots, and survivors of real events who hold that judgment under stakes forms only in contact with real consequences. They testify to inquiries, publish accounts, and press for apprenticeship-style exposure pipelines, but hold no seat in the proceedings that define admissible readiness evidence, and their proposals are routinely dismissed as impractical because real events cannot be scheduled.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, veteran_real_stakes_advocates, excluded,
    organized, generational, constrained, continental).

% Study retention curves, simulation-to-performance transfer, and post-incident behavior across industries. They produce the decay and transfer literature that all camps cite, collect no drill spending, and can compare regimes across jurisdictions that the regulated parties cannot freely leave.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, independent_skill_decay_researchers, observer,
    analytical, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, licensed_hazardous_operations_firms).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives distributed response teams a recurring, schedulable setting in which to practice procedures, coordinate roles, and surface equipment and communication failures at low cost — solving the problem that real catastrophes arrive rarely, dangerously, and cannot be summoned for practice.
% TRANSFER_FUNCTION: Moves assurance goods — license continuance, insurance pricing, accreditation status, public reassurance — to operating firms in exchange for documented exercise compliance; moves drill fees and audit business to vendors and agencies; leaves the risk that rehearsed procedure will not survive contact with real conditions resting on exposed publics and frontline crews.
% ABSENT_VOICES: Veteran practitioners and event survivors who hold that judgment-under-stakes forms only through real consequences are outside the rule-making rooms, as are residents' panels from hazard corridors, typically consulted after drafts close. The conversation that defines what counts as readiness evidence is conducted among agencies, firms, and vendors — parties who all need the evidence to be schedulable.
% DISAPPEARANCE_RATIONALE: Licenses, insurance contracts, accreditation surveys, and mutual-aid agreements all price readiness off exercise records. Remove the arrangement overnight and the licensing frameworks lose their evidence base: firms would face either open admission that readiness levels are unknown or rapid construction of apprenticeship-style exposure pipelines, insurers would reprice or withdraw, and the assurance economy built on drill documentation would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: Real crises are too rare, too dangerous, and too expensive to serve as routine training, so organizations needed a repeatable way to practice crisis response, preserve procedural memory between events, and demonstrate diligence to regulators and the public.
% FOUNDING_PROBLEM_CORROBORATION: Peer-reviewed skill-decay and retention research, accident-investigation findings that crews executed rehearsed steps but misjudged unrehearsed conditions, and frontline union submissions all attest, from outside the benefiting parties, that the underlying problem — competence fades between real events — remains live. No source outside the beneficiary set attests that the problem is solved.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.66 because the regime's currency — documented exercise performance — is accepted as readiness evidence that this reading holds to be category-insufficient, so assurance value is collected without the corresponding good being delivered; the figure is tempered by the rehearsal utility this reading itself concedes ('necessary but insufficient'). Suppression is 0.55 and structural: licensing, accreditation, and insurance all condition on drill records, career progression ties to exercise participation, and the apprenticeship alternative is only partly available because real catastrophes cannot be scheduled. Suppression is authored as a raw structural property — the engine, not this file, scales anything. Theater ratio is 0.62: scenarios are announced in advance, evaluated against published rubrics, and passed at near-universal rates, and after-action reporting is increasingly optimized for audit rather than correction. Accessibility_collapse is 0.48 — understanding the regime does not close the exit to real stakes, because that exit is not schedulable; apprenticeship channels persist in firefighting, residency medicine, and line flying. Resistance is 0.45: inquiry recommendations, union campaigns, and researcher critique recur episodically and are absorbed between disasters. All three tracked series run on one shared grid ({0,6,12,18,24,30}); suppression_requirement is tracked because enforcement capacity visibly ratcheted over the interval (mandatory hour floors, force-on-force programs, accreditation conditionality following signal events), not merely drifted. Extraction rises as certification stakes grew and real-stakes opportunities shrank; theater climbs as exercises formalized into auditable ritual. Claimed_type is tangled_rope from structure — genuine rehearsal coordination, asymmetric assurance capture, active enforcement — authored independently of these metric values; where the engine's per-seat computation disagrees with the claim, that divergence is the datum. Fixing_cost is prohibitive because replacing certification with real-stakes validation would require admitting currently unknowable readiness levels, rebuilding apprenticeship pipelines at scale, and repricing every license and premium — a cost far exceeding any single actor's benefit from honesty.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the certification-agency seat the regime is diligent governance infrastructure — schedulable, auditable, defensible in hearings — and likely computes coordination-forward. From the operating-firm seat it is a manageable compliance cost that purchases an assurance asset; the beneficiary-side arithmetic damps effective burden toward subsidy. From the frontline-operator seat the same structure delivers real procedural training while asking the operator to stake their life on competence the exercises never touched — the dual-role seat sits between. From the exposed-public seat only outcomes are visible: the entire apparatus appears as an assurance machine whose failure mode lands on people who never entered the room. The engine computes these per-seat classifications from the structural data; this file does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the low-directionality end: firms convert compliance into licensing continuity and premium relief and hold arbitrage-grade exit (jurisdiction shopping, negotiated requirements); vendors are mobile across customers and borders; agencies collect authority and mandate-scope rather than cash rents, placing them mildly beneficiary-side rather than captured targets. Victims sit high: exposed publics are trapped (relocation away from hazard corridors is not a realistic exit) and individually powerless, with coalition leverage available mainly after an event has already demonstrated the gap — hence class-level mobilization potential noted but not individual exit. Frontline operators derive high directionality despite their secondary training receipt because identity_locked exit amplifies their position toward the full-target end: professional identity is fused with being the person who handles the emergency, so the miscalibration cannot be exited without exiting the self. The regime operates at national-to-global scope while harms localize regionally, and scope-driven verification difficulty is the engine's arithmetic, not this file's. No directionality overrides were needed: beneficiary/victim declarations plus exit atoms already differentiate the seats, and a power-atom-keyed override would collide across the two institutional seats rather than separate them.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against both mislabelings. A pure-coordination reading would wash out the transferred risk — the regime's defining asymmetry is that assurance is collected here and consequences land elsewhere. A pure-extraction reading would ignore what even this reading concedes: rehearsal is necessary, real catastrophes cannot be scheduled, and abolishing the only available practice channel would leave procedural memory to atrophy with nothing in its place. The founding problem — practicing for events too rare and dangerous to train on live — remains live (corroborated outside the beneficiary set by decay research, investigation findings, and union testimony), so mandatrophy is not resolved and no sunset structure applies. The live hinge is omega simulation_to_real_transfer_rate: verified positive transfer would migrate this arrangement toward ordinary coordination cost; confirmed null transfer hardens it toward the extractive end of its range.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the exercise_as_competence_maintenance kernel — the lived_catastrophe_necessity_reading. How would classification and victim scope shift if a sibling reading (simulation_sufficiency_reading or hybrid_decay_reading) were instantiated instead?',
    'Compile and classify the sibling stories (exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__hybrid_decay_reading) and compare per-seat verdicts across the family; the divergence localizes what the reading choice changes structurally.',
    'Under the sufficiency reading the victim set shrinks to fidelity-lag cases and extraction falls toward coordination-cost levels; under the hybrid reading victims split into procedural-decay and judgment-decay classes and extraction lands between this file and the sufficiency file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one-of-three kernel readings; sibling choice changes victim scope and epsilon.').

omega_variable(
    simulation_to_real_transfer_rate,
    'Does simulated catastrophe transfer to real-event judgment under stakes at any rate above zero?',
    'Outcome comparison of real incidents involving simulation-only-prepared versus mixed-preparation crews and commands, controlling for event severity, staffing, and tenure.',
    'A reliably positive transfer rate would erode the categorical-necessity premise and pull epsilon toward the hybrid or sufficiency readings; a null rate confirms this file''s high-extraction estimate and widens the justified victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_to_real_transfer_rate, empirical, 'Whether the reading''s core premise (zero kernel exercise via simulation) survives transfer evidence.').

omega_variable(
    covert_decay_observability,
    'How far has any given operator corps'' real-stakes competence decayed at certification time, given that decay reveals itself decisively only upon real activation?',
    'Latent-variable modeling against rare real activations, near-miss telemetry, and degraded-condition audits; cannot be fully closed because real activation is the only decisive probe — the quantity is partially unobservable in principle.',
    'The less observable the decay, the wider the gap between certified and actual readiness and the higher the burden shifted onto exposed publics; near-perfect observability would push the arrangement toward honest uncertainty labeling and lower effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covert_decay_observability, empirical, 'Irreducible partial unobservability of the decay this reading asserts.').

omega_variable(
    false_confidence_internalization,
    'Is frontline operators'' reliance on passing scores a structural product of the certification regime, or an internalized belief that would persist if the regime were removed?',
    'Track self-assessed readiness and risk-taking among operators who move across employers or jurisdictions with materially different exercise regimes; persistence of inflated self-assessment after regime change indicates internalization.',
    'If internalized, miscalibrated confidence travels with the operator after reform and the exposed-public burden stays large even under a restructured regime; if purely structural, reform restores calibration quickly and the victim set contracts with the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_confidence_internalization, empirical, 'Structural versus internalized component of operator miscalibration (suppression-mechanism ambiguity).').

omega_variable(
    certification_legitimacy_layer_framing,
    'Should the standing arrangement be framed as the drill-and-exercise regime itself, or as the certification-legitimacy layer that converts drill records into licenses, premiums, and public assurances?',
    'Apply the epsilon-invariance test to both framings: if the two framings assign different epsilon to the same observable set, split into two linked stories — one for the training apparatus, one for the conversion of exercise records into institutional assurance.',
    'The legitimacy-layer framing isolates the rent-collecting conversion step and raises measured extraction on the certification seat while removing the training function from the ledger; the unified framing blends them into the current single estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_legitimacy_layer_framing, conceptual, 'CS-framing under-determination: regime-versus-legitimacy-layer framing choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(exer_tr_t0, observed).
narrative_ontology:measurement(exer_tr_t6, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement_basis(exer_tr_t6, observed).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement_basis(exer_tr_t12, observed).
narrative_ontology:measurement(exer_tr_t18, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 18, 0.51).
narrative_ontology:measurement_basis(exer_tr_t18, observed).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 24, 0.57).
narrative_ontology:measurement_basis(exer_tr_t24, observed).
narrative_ontology:measurement(exer_tr_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement_basis(exer_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(exer_be_t0, observed).
narrative_ontology:measurement(exer_be_t6, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(exer_be_t6, observed).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement_basis(exer_be_t12, observed).
narrative_ontology:measurement(exer_be_t18, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 18, 0.59).
narrative_ontology:measurement_basis(exer_be_t18, observed).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement_basis(exer_be_t24, observed).
narrative_ontology:measurement(exer_be_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(exer_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(exer_su_t0, observed).
narrative_ontology:measurement(exer_su_t6, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement_basis(exer_su_t6, observed).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement_basis(exer_su_t12, observed).
narrative_ontology:measurement(exer_su_t18, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(exer_su_t18, observed).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement_basis(exer_su_t24, observed).
narrative_ontology:measurement(exer_su_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(exer_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'exercise maintains crisis competence' decomposes, per the epsilon-invariance principle, into three structurally distinct claims with distinct epsilon and distinct victim sets: this file (lived_catastrophe_necessity_reading — categorical necessity of real stakes; widest victim set, including all exposed to untested operators; epsilon 0.66), exercise_as_competence_maintenance__simulation_sufficiency_reading (simulation is genuine exercise; epsilon near coordination cost; victims limited to fidelity-lag cases), and exercise_as_competence_maintenance__hybrid_decay_reading (split kernel; intermediate epsilon and a two-class victim set). Family members cite one another as evidence: the sufficiency reading's transfer findings are deployed against this file's necessity premise and vice versa. All three link mutually through affects_constraints; no member stands alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
