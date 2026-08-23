% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe-Anchored Competence Doctrine (Real-Event Exercise Floor)
 *   domain: safety engineering/organizational learning/high-reliability organizations
 *
 * SUMMARY:
 *   In high-reliability organizations — airline flight departments, nuclear
 *   control rooms, wildland and urban fire services — a durable doctrine
 *   governs how readiness for rare, high-consequence events is maintained:
 *   that only real catastrophic events or their near-misses supply the
 *   exercise that keeps complex emergency competence alive, and that
 *   classroom and simulator work, however sophisticated, leaves a gap no
 *   rehearsal short of jeopardy can close. The doctrine organizes concrete
 *   institutional choices: how much capital reaches full-fidelity simulators
 *   versus fleet growth, how near-miss reports are weighed, how veterans'
 *   authority is earned, and what may be said after a disaster that training
 *   should have prevented. This file instantiates ONE reading of the
 *   contested competence-exercise kernel — the
 *   catastrophe-as-necessary-anchor reading — as a clean, epsilon-invariant
 *   constraint: the standing arrangement under assessment is the practice
 *   regime organized around that premise (simulation treated as partial; real
 *   and near-miss events as the load-bearing exercise), assessed by the
 *   reading's own lights. Sibling readings are separate constraints linked
 *   through the network block. The claim/metrics split is deliberate: the
 *   type claim is authored from structure, the metrics from observed
 *   operation, and any divergence between them is the measurement the corpus
 *   exists to take. KEY AGENTS (by structural relationship): -
 *   hro_training_establishments: agenda-setting administrator
 *   (institutional/constrained) — runs the curricula and standards embodying
 *   the doctrine - deferred_simulator_budget_holders: primary beneficiary
 *   (powerful/arbitrage) — capital spared by the doctrine's demotion of
 *   simulation - post_event_accountability_managers: beneficiary with payer
 *   exposure (powerful/mobile) — converts failures into necessary-tuition
 *   narratives - frontline_crews: primary payer with secondary beneficiary
 *   position (organized/identity_locked) — meets the first unrehearsed event
 *   with whatever readiness survived - exposed_publics: payer
 *   (powerless/constrained) — absorbs residual casualty risk -
 *   liability_insurers: institutional payer (institutional/arbitrage) — pays
 *   claims and pushes back through pricing and audits -
 *   simulation_training_scientists: excluded voice (moderate/mobile) —
 *   transfer evidence ruled artificial by the doctrine's custodians -
 *   high_reliability_researchers: analytical observer (analytical/analytical)
 *   — maps the structure without belonging to it
 *
 * KEY AGENTS:
 *   - - hro_training_establishments: agenda-setting administrator (institutional/constrained) — administers curricula and standards premised on the doctrine
 *   - - deferred_simulator_budget_holders: primary beneficiary (powerful/arbitrage) — receives recurring budget relief from the doctrine's claim that the decisive exercise cannot be purchased
 *   - - post_event_accountability_managers: beneficiary, secondarily payer (powerful/mobile) — deploys the exculpation script after failure while personally wearing each failure
 *   - - frontline_crews: primary payer, secondarily beneficiary (organized/identity_locked) — bears first-contact risk; professional authority fused with real-event veteranhood
 *   - - exposed_publics: payer (powerless/constrained) — carries the aggregate residual casualty risk
 *   - - liability_insurers: payer (institutional/arbitrage) — pays claims; organized counterweight pressing for simulator investment
 *   - - simulation_training_scientists: excluded voice (moderate/mobile) — produces transfer and fidelity evidence the operative conversation dismisses
 *   - - high_reliability_researchers: analytical observer (analytical/analytical) — scholarly and investigative mapping of decay, drift, and mindfulness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.62).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.55).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe-Anchored Competence Doctrine (Real-Event Exercise Floor)").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety engineering/organizational learning/high-reliability organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'fb8c304b-9b46-4003-9f0d-602e7e19bc7e').
narrative_ontology:cs_kernel_codification('fb8c304b-9b46-4003-9f0d-602e7e19bc7e', distributed).
narrative_ontology:cs_authority_grounding('fb8c304b-9b46-4003-9f0d-602e7e19bc7e', practice).
narrative_ontology:cs_interpretation_layer_present('fb8c304b-9b46-4003-9f0d-602e7e19bc7e').
narrative_ontology:cs_reading_relation('fb8c304b-9b46-4003-9f0d-602e7e19bc7e', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('fb8c304b-9b46-4003-9f0d-602e7e19bc7e', competence_exercise_requirement__hybrid_dependency, forecloses).
narrative_ontology:cs_axiom('fb8c304b-9b46-4003-9f0d-602e7e19bc7e', foundational, real_event_exposure_irreplaceable).
narrative_ontology:cs_axiom_status(real_event_exposure_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('fb8c304b-9b46-4003-9f0d-602e7e19bc7e', real_event_exposure_irreplaceable, empirically_contingent).
narrative_ontology:cs_axiom('fb8c304b-9b46-4003-9f0d-602e7e19bc7e', foundational, jeopardy_arousal_constitutive_of_exercise).
narrative_ontology:cs_axiom_status(jeopardy_arousal_constitutive_of_exercise, holdable).
narrative_ontology:cs_axiom_grounding('fb8c304b-9b46-4003-9f0d-602e7e19bc7e', jeopardy_arousal_constitutive_of_exercise, empirically_contingent).
narrative_ontology:cs_reference_frame('fb8c304b-9b46-4003-9f0d-602e7e19bc7e', real_event_exercise_floor).
narrative_ontology:cs_drift_state('fb8c304b-9b46-4003-9f0d-602e7e19bc7e', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fb8c304b-9b46-4003-9f0d-602e7e19bc7e', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, deferred_simulator_budget_holders).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, post_event_accountability_managers).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_crews).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, exposed_publics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_crews).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, post_event_accountability_managers).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, liability_insurers).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_decays_in_catastrophe_free_intervals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Airline flight-department training hierarchies, nuclear-operator training academies, and the standards committees that certify recurrent training. They administer curricula premised on the doctrine: they schedule simulator hours, weigh near-miss reports against line experience, decide what counts as qualifying exposure, and interpret each new incident into updated standards. Abandoning the premise would mean refounding curricula against sunk training infrastructure and veteran-instructor resistance, so their exit from the doctrine is constrained.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, hro_training_establishments, agenda_setter,
    institutional, generational, constrained, global).

% Chief financial officers and fleet planners whose capital plans are relieved by the doctrine's central claim: that the decisive exercise cannot be purchased, so full-fidelity simulator fleets and fidelity upgrades are perpetually deferrable. The budget relief recurs annually and is redeployed into fleet growth, dividends, or debt reduction; their mobility within capital allocation means the relief costs them nothing they notice.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, deferred_simulator_budget_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Senior executives who, when a preventable failure arrives, invoke the doctrine's script: no training could have prepared anyone for the first real occurrence, the event was tuition the organization had to pay. The script converts liability inquiries into learning narratives. They also personally wear each failure — reputation, board standing, career — so they carry real exposure on the same structure that protects them narratively; their mobility between firms and boards softens that exposure.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, post_event_accountability_managers, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, post_event_accountability_managers, payer).

% Flight crews, reactor operators, fireground commanders: they meet the first unrehearsed real event with whatever readiness survived the quiet years, and they are the bodies at risk when it has not survived. Their near-miss reports feed the exercise economy and gain weight under the doctrine. Their professional identity is fused with real-event veteranhood — authority in the cockpit, the control room, and the fireground is constituted through having been there — so leaving the role or repudiating its credential economy is not a live option for most. When readiness is genuine they collect its protective value firsthand.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_crews, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_crews, beneficiary).

% Passengers, plant neighbors, and the public downrange of industrial processes: they absorb the residual casualty risk created by readiness gaps, aggregated across millions of operations. They cannot observe or price operator-specific readiness, their route and residence choices move the risk only marginally, and they enter the conversation only as casualty statistics after a failure.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, exposed_publics, payer,
    powerless, biographical, constrained, global).

% Hull, casualty, and liability underwriters who pay the claims when readiness fails. They finance readiness audits, press for simulator investment and recurrent-training mandates, and reprice premiums against operators with thin exercise records. Their exit is portfolio-level: withdraw capacity or reprice a book, not exit the risk class.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, liability_insurers, payer,
    institutional, biographical, arbitrage, global).

% Human-factors researchers producing transfer, fidelity, and skill-retention evidence. Their findings are ruled artificial by the doctrine's custodians — a simulator is not the event — and they are heard as vendors rather than witnesses. They publish in journals and conferences outside the standards committees, instructor hierarchies, and investigation boards where the doctrine is administered, and they can move institutions, though not into the operative conversation.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_training_scientists, excluded,
    moderate, biographical, mobile, global).

% Organizational scholars of high-reliability practice and independent accident-board analysts who map mindfulness, drift, and decay across organizations. They take testimony from every seat, neither collect from the arrangement nor pay into it, and their analyses occasionally reshape the standards conversation without belonging to it.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, high_reliability_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__catastrophe_as_necessary_anchor, deferred_simulator_budget_holders).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__catastrophe_as_necessary_anchor, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real attention-allocation problem: an organization cannot verify readiness for events that almost never happen, and the doctrine supplies a shared rule — treat real events and near-misses as the only fully trustworthy exercise, weight near-miss reporting accordingly, and discount rehearsal-based assurance accordingly.
% TRANSFER_FUNCTION: Moves residual casualty risk onto whoever is present at the first unrehearsed real event (crews, passengers, neighbors); moves accountability away from decision-makers after failure by reframing it as unavoidable tuition; moves capital away from simulator programs into other uses.
% ABSENT_VOICES: Training-science and simulation researchers would object that their transfer evidence is dismissed as artificial; the prospective casualties of the next first event are absent by definition until they become statistics; simulator manufacturers are present as vendors, not as witnesses. All sit outside the committees, hierarchies, and boards where the doctrine is administered.
% DISAPPEARANCE_RATIONALE: Overnight removal would reroute training capital toward simulation and hybrid regimes within a planning cycle, reprice the accountability conversation after every subsequent failure, and dissolve the veteran-prestige economy that anchors operational authority — the practice regime visibly depends on the doctrine's operative force. Any underlying decay phenomenon, if real, would persist; what rearranges is the constructed regime built on the doctrine, which is the arrangement this story models.
% FOUNDING_PROBLEM: Early jet-age and nuclear-era operators watched meticulously trained crews falter in first live emergencies and needed an account of why rehearsal did not transfer — and a rule for where scarce preparation effort should go.
% FOUNDING_PROBLEM_CORROBORATION: Independent human-factors skill-decay research and accident-board findings documenting performance gaps at first real events corroborate the founding problem from outside the doctrine's beneficiaries. Nothing outside the beneficiary set corroborates the remedy step — that catastrophe specifically is the irreplaceable exercise; simulation researchers actively dispute that step, and the dispute is recorded as signal rather than smoothed over.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores describe the doctrine's operative regime, not the underlying decay phenomenon. Extractiveness 0.62: the regime moves real costs — deferred readiness surfacing as casualties at first unrehearsed events, and post-hoc conversion of preventable failure into tuition — while returning budget relief and narrative protection to identifiable seats; the coordination core (honest attention to decay, a functioning near-miss economy) is genuine but partial. Suppression 0.55: epistemic and budgetary rather than physical — transfer evidence is ruled artificial, fidelity investment is argued down year over year, and veteran prestige anchors dissent; the mechanism is structural (funding gates, standards control, investigation-framing power) with a substantial internalized component (professional self-concept fused with real-event veteranhood), which persists even where formal barriers fall. Theater 0.31: near-miss investigation does real work, but lessons-learned ceremonies increasingly perform learning without altering the next capital plan. Accessibility collapse 0.45: alternatives — hybrid regimes, high-fidelity simulation — remain visible and are practiced elsewhere, so understanding the doctrine does not close the option space. Resistance 0.60: training scientists, simulator manufacturers, insurers, and hybrid-practicing operators actively contest the doctrine. Temporal design: one shared ten-point grid across all tracked metrics. Extractiveness and theater oscillate on a catastrophe cadence — each real event opens a reform window (investment surges, accountability tightens, measured extraction dips) followed by quiet-period relaxation and re-entrenchment — while both peaks and troughs ascend: a secular extraction accumulation rides the cycle. The oscillation is partly the doctrine's own mechanism (quiet-period decay validates its warning, intermittently reinforcing it) and partly external Poisson shock timing; reform windows characteristically close before completing. Suppression_requirement rises monotonically 0.44 to 0.55 across the interval: as simulator fidelity improves, the doctrine's custodians must defend the not-real boundary harder, so the enforcement effort sustaining the constraint intensifies even as its factual grip erodes — that enforcement-capacity dynamic is what the series tracks, and the base_properties suppression scalar (0.55) is the interval-end state of it. Coalition note: the payer seats are not condemned to isolation — crews' unions, passenger advocates, and liability insurers hold overlapping interests, insurers already act as an organized counterweight, and a crew-insurer-regulator coalition is the credible path that converts fixing from prohibitive to affordable. Identity lock: frontline crews' exit is identity_locked because captaincy and operator authority are constituted through real-event veteranhood; if prestige attached instead to demonstrated simulation mastery, the doctrine would lose its enforcement backbone from inside the profession. Claim/metric independence: claimed_type tangled_rope is authored from structure (genuine coordination function plus asymmetric, actively defended extraction); the metrics are authored from observed operation; neither was tuned toward the other or toward a predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the training establishment the doctrine is stewardship — a sober refusal to mistake rehearsed competence for ready competence. From the budget-holder seat it is prudence — capital deferred against a claim that the decisive exercise cannot be purchased anyway. From the crew seat the same arrangement is uncompensated risk exposure plus an identity bind: the credential that commands authority is the very experience the regime declines to manufacture safely. From the insurer's seat it is a priced tail risk to be pushed back on through audits and premiums. Same-level divergence: budget holders and insurers are comparably resourced institutional actors whose opposed positions arise not from power asymmetry but from position — the former receives the doctrine's budget relief, the latter pays its claims — with exit differentiated by what arbitrage means in each seat (reallocating capital internally versus repricing or withdrawing capacity across the book). The excluded scientists' testimony explains how consensus around the doctrine was assembled, but as authored absence it stays commentary-grade and never drives a classification override.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive d, and exit modulates it. deferred_simulator_budget_holders sit nearest the beneficiary end: the doctrine subsidizes their capital plans and their arbitrage-grade mobility within allocation damps their experienced burden toward subsidy. post_event_accountability_managers derive low-to-mid d from the beneficiary role, pulled upward by their payer secondary role — they collect the exculpation script yet personally wear each failure. frontline_crews derive d near the full-target end: they bear the transfer, and identity_locked exit holds them nearer the full-target end than their organized power alone would suggest; the beneficiary secondary role (protective value of genuine readiness, weighted near-miss reports) pulls them slightly off the extreme. exposed_publics sit near full target with constrained exit amplifying their exposure. liability_insurers are declared payers whose arbitrage exit moderates, but does not invert, their d. The excluded scientists and analytical observers take no directional position: exclusion feeds the consensus-provenance check, never d. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, inside the engine.
 *
 * MANDATROPHY ANALYSIS:
 *   The category discipline prevents two symmetrical mislabels. Read as a mountain (the physics of skill decay, immune to scrutiny), the doctrine's beneficiaries — budget relief, exculpation — would vanish into natural law; the tangled-rope frame keeps the genuine decay phenomenon visible while forcing the extraction layer to name its payers. Read as a snare, the doctrine's real coordination work — a functioning near-miss economy, institutional honesty about the limits of rehearsal — would be erased, and reformers would discard the decay warning along with the rent-seeking. On mandate: the founding problem, preserving readiness for events too rare to practice routinely, is emphatically live — novel automation, new fleet types, and climate-driven extremes mint new rare events faster than careers can absorb them — so no dead-mandate declaration is authored. The drift to watch is functional rather than mandatorial: the ascending theater_ratio envelope marks lessons-learned ritualization, the characteristic pre-atrophy symptom. The piton cell would open only if a successor regime kept the ceremonies after abandoning both the decay warning and the budget relief — ceremony without function and without a capturer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the catastrophe_as_necessary_anchor reading of the competence_exercise_requirement kernel; what structural changes would the sibling readings impose if adopted?',
    'Longitudinal cohort studies separating first-live-event performance across simulation-trained, hybrid-trained, and experience-anchored operators, adjudicating among the three readings on transfer outcomes.',
    'Under simulation_as_adequate_exercise the casualty victim set collapses to misallocated training spend; under hybrid_dependency victims split between simulation-starved foundations and anchor-starved readiness; this reading''s regime concentrates casualties at the first unrehearsed event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three rival readings of the competence-exercise kernel; sibling adoption restructures the victim set and the extraction surface.').

omega_variable(
    decay_measurement_confound,
    'Is the observed competence decay a natural law of skill retention, or partly manufactured by the doctrine itself — organizations convinced simulation is unreal under-invest in fidelity, creating the very decay the doctrine predicts?',
    'Compare decay rates across operators matched on event history but differing in simulator investment and doctrine adherence; if decay tracks doctrine adherence at equal fidelity, the confound is real.',
    'If partly self-fulfilling, the constraint loses mountain-side naturality and its effective extraction exceeds what decay alone would justify; if decay persists at maximal fidelity, the doctrine''s coordination core is strengthened and its naturality claim gains footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_measurement_confound, empirical, 'Natural-law versus self-fulfilling ambiguity in the decay evidence underpinning the doctrine.').

omega_variable(
    arousal_transfer_irreducibility,
    'Is the irreducible exercise component the stress-arousal calibration of real jeopardy (the muscle-memory-versus-knowing-about gap), or knowledge organization that rising simulation fidelity progressively replicates?',
    'Psychophysiological transfer research: startle-response and cortisol studies comparing simulator, immersive stress-inoculation, and live-event cohorts on first-real-event performance.',
    'If arousal calibration is constitutive and non-substitutable, the reading''s foundational axioms hold and the hybrid reading converges toward it; if fidelity closes the gap, this reading forecloses on an empty premise and the sibling readings dominate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arousal_transfer_irreducibility, empirical, 'Whether the jeopardy-arousal gap is constitutive of exercise or an artifact of current simulator fidelity.').

omega_variable(
    near_miss_substitution_adequacy,
    'Can accumulated near-misses substitute for catastrophes as the anchoring exercise, dissolving the doctrine''s insistence on catastrophe-class events?',
    'Dose-response analysis of readiness indicators against near-miss investigation depth across comparable high-reliability organizations.',
    'If near-misses suffice, the victim set shrinks sharply — no one waits for a mass casualty to exercise competence — and the constraint migrates toward rope; if not, the doctrine retains a genuine tragic core that no reporting program can replace.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_substitution_adequacy, empirical, 'Whether the near-miss tier of the doctrine can carry the whole anchoring load without catastrophe-class events.').

omega_variable(
    exculpation_moral_hazard,
    'Does the doctrine''s exculpation function causally suppress preventive investment — leaders anticipating the tuition framing skimp on readiness capex — or is investment governed independently by insurance pressure and regulation?',
    'Natural experiments comparing training capital expenditure in jurisdictions or eras where exculpatory framing was legally foreclosed (strict liability, mandated readiness disclosure) against regimes where it remained tolerated.',
    'A causal moral-hazard channel raises this reading''s effective extraction well above its coordination floor and sharpens payer-seat classifications; absence of the channel confines the extraction to straightforward budget substitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exculpation_moral_hazard, empirical, 'Whether the accountability-deflection benefit of the doctrine feeds back into reduced preventive investment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.26).
narrative_ontology:measurement(comp_tr_t4, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 4, 0.22).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 8, 0.24).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 12, 0.27).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 16, 0.23).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 20, 0.26).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 24, 0.29).
narrative_ontology:measurement(comp_tr_t28, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 28, 0.25).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 32, 0.28).
narrative_ontology:measurement(comp_tr_t36, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 36, 0.31).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comp_be_t4, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(comp_be_t12, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(comp_be_t28, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 28, 0.54).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(comp_be_t36, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 36, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(comp_su_t4, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(comp_su_t12, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(comp_su_t28, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 28, 0.53).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 32, 0.54).
narrative_ontology:measurement(comp_su_t36, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 36, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__hybrid_dependency).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__simulation_as_adequate_exercise).

% DUAL FORMULATION NOTE:
% The colloquial question 'what maintains competence in high-reliability organizations?' decomposes per the epsilon-invariance principle into three structurally distinct claims, each with its own epsilon, beneficiary/victim structure, and classification: this story (catastrophe_as_necessary_anchor), competence_exercise_requirement__hybrid_dependency, and competence_exercise_requirement__simulation_as_adequate_exercise. They form one constraint family and are mutually linked through network.affects_constraints. This reading sits upstream of hybrid_dependency, which cites real-event anchoring evidence while broadening what counts as anchoring, and stands opposed to simulation_as_adequate_exercise, whose advocates must answer the first-real-event decay record this reading organizes. The family exists because the single label 'competence exercise requirement' conflates claims with materially different epsilon values; forcing them into one story would make epsilon observer-dependent, violating DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
