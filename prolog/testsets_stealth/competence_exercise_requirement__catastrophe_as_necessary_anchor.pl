% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe-as-Necessary-Anchor Doctrine of Competence Maintenance
 *   domain: organizational/safety-engineering
 *
 * SUMMARY:
 *   In high-reliability industries — aviation, nuclear power, petrochemicals,
 *   surgery — a durable doctrine holds that only real catastrophic events or
 *   near-misses provide the exercise that maintains crisis competence:
 *   simulation trains knowing-about, never the stress-encoded muscle memory
 *   that fails under load. The doctrine presents itself as natural law about
 *   skill acquisition, and this story instantiates that presentation: it is
 *   the catastrophe_as_necessary_anchor reading of the
 *   competence_exercise_requirement kernel, claimed as mountain with
 *   emerges_naturally true. The claim and the metrics are independent
 *   authored facts. The metrics describe a contested, extractively operated
 *   construct: identifiable parties collect premiums from treating the
 *   exclusivity claim as settled (the experiential elite whose authority it
 *   grounds, an investigation apparatus funded by event flow, regulators
 *   whose retrospective model it fits, managers whose training deferral it
 *   excuses), while the risk of each exercise falls on frontline crews,
 *   novices, and the public who never consented to be the curriculum. Because
 *   beneficiaries are declared on a mountain claim, the false-summit
 *   signature applies and the natural-law-versus-constructed ambiguity is
 *   carried in the omegas. Constraint family: this reading links to its
 *   siblings simulation_as_adequate_exercise (lowest epsilon — no catastrophe
 *   dependency) and hybrid_dependency (intermediate epsilon — simulation
 *   necessary but anchored periodically); the corroborated decay observation
 *   is the upstream member of the family and the exclusivity claim is its
 *   downstream, most extractive extension. The epsilon referent throughout is
 *   the standing arrangement — event-driven competence maintenance as
 *   actually operated — assessed by this reading's own lights, not the hybrid
 *   or simulation arrangements the siblings would install. Interval
 *   semantics: time points 0-50 represent approximately 1975-2025, five
 *   decades of doctrine entrenchment.
 *
 * KEY AGENTS:
 *   - veteran_operators_experiential_elite: Primary beneficiary and doctrinal agenda-setter (powerful/identity_locked) — collects the authority premium the doctrine grounds
 *   - incident_investigation_apparatus: Secondary beneficiary (organized/constrained) — mandate and funding scale with event flow
 *   - regulatory_oversight_bodies: Beneficiary and co-agenda-setter (institutional/constrained) — retrospective enforcement model fits the doctrine
 *   - cost_constrained_operations_management: Agenda-setter and indirect beneficiary (powerful/mobile) — defers training spend under doctrinal cover
 *   - frontline_operators_exposed_to_live_events: Primary payer (organized/constrained) — bears acute risk of the exercise
 *   - catastrophe_victims_and_survivors: Primary payer (powerless/trapped) — harmed before any lesson is extracted
 *   - novice_staff_deferred_to_event_learning: Payer (powerless/constrained) — development gated on catastrophe frequency
 *   - simulation_advocates_and_training_scientists: Excluded challenger (moderate/mobile) — evidence dismissed categorically
 *   - hro_research_community: Analytical observer (analytical/analytical) — maps which parts of the doctrine survive scrutiny
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.66).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.58).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.66).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.34).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, mountain).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe-as-Necessary-Anchor Doctrine of Competence Maintenance").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "organizational/safety-engineering").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).
domain_priors:emerges_naturally(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, '0a8334bd-ec58-413a-baf5-40cf5f227fdb').
narrative_ontology:cs_kernel_codification('0a8334bd-ec58-413a-baf5-40cf5f227fdb', distributed).
narrative_ontology:cs_authority_grounding('0a8334bd-ec58-413a-baf5-40cf5f227fdb', practice).
narrative_ontology:cs_interpretation_layer_present('0a8334bd-ec58-413a-baf5-40cf5f227fdb').
narrative_ontology:cs_reading_relation('0a8334bd-ec58-413a-baf5-40cf5f227fdb', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('0a8334bd-ec58-413a-baf5-40cf5f227fdb', competence_exercise_requirement__hybrid_dependency, forecloses).
narrative_ontology:cs_axiom('0a8334bd-ec58-413a-baf5-40cf5f227fdb', foundational, crisis_competence_requires_live_event_exposure).
narrative_ontology:cs_axiom_status(crisis_competence_requires_live_event_exposure, holdable).
narrative_ontology:cs_axiom_grounding('0a8334bd-ec58-413a-baf5-40cf5f227fdb', crisis_competence_requires_live_event_exposure, empirically_contingent).
narrative_ontology:cs_axiom('0a8334bd-ec58-413a-baf5-40cf5f227fdb', foundational, stress_encoded_performance_not_simulable).
narrative_ontology:cs_axiom_status(stress_encoded_performance_not_simulable, holdable).
narrative_ontology:cs_axiom_grounding('0a8334bd-ec58-413a-baf5-40cf5f227fdb', stress_encoded_performance_not_simulable, empirically_contingent).
narrative_ontology:cs_reference_frame('0a8334bd-ec58-413a-baf5-40cf5f227fdb', catastrophe_necessity_canon).
narrative_ontology:cs_drift_state('0a8334bd-ec58-413a-baf5-40cf5f227fdb', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0a8334bd-ec58-413a-baf5-40cf5f227fdb', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_operators_experiential_elite).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, incident_investigation_apparatus).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_oversight_bodies).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, cost_constrained_operations_management).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_exposed_to_live_events).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, catastrophe_victims_and_survivors).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, novice_staff_deferred_to_event_learning).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, skill_decay_without_live_exposure).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, experiential_learning_supremacy).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__catastrophe_as_necessary_anchor, stress_encoded_performance_not_simulable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior pilots, plant operators, and incident commanders whose authority, pay premiums, and board seats derive from having commanded through real emergencies. They teach, certify, and select successors, weighting candidates by event exposure. Conceding the doctrine would mean conceding their distinguishing asset is reproducible in simulation; their professional self-concept is built on having been there.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_operators_experiential_elite, beneficiary,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_operators_experiential_elite, agenda_setter).

% Accident investigation boards, forensic safety consultancies, and litigation-support experts whose mandate, staffing, and funding scale with the flow of real events. Their methods presuppose wreckage, recordings, and casualties; a world of preventive simulation would shrink their jurisdiction. Exit means retraining into disciplines their event-derived expertise does not cover.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, incident_investigation_apparatus, beneficiary,
    organized, generational, constrained, national).

% Aviation, nuclear, chemical, and healthcare regulators whose enforcement model is retrospective: they certify operators partly on demonstrated response to real events and codify lessons into rules after harm occurs. Their legitimacy narrative — we learn from every accident — depends on accidents arriving to be learned from. Statutory design makes proactive simulation-first oversight a structural stretch.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_oversight_bodies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_oversight_bodies, agenda_setter).

% Operations executives who allocate training budgets and set risk tolerances. The doctrine lets them defer expensive simulation programs and full-scale drills until events force the issue, booking the difference as margin; when catastrophe arrives, the same doctrine supplies the remediation playbook. They move between firms carrying the doctrine with them.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, cost_constrained_operations_management, agenda_setter,
    powerful, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__catastrophe_as_necessary_anchor, cost_constrained_operations_management, beneficiary).

% Shift crews, cockpit crews, and unit nursing staff who operate inside the systems where the irreducible exercise occurs. They absorb the acute risk of degraded collective competence during long quiet periods and the trauma of the events themselves. Union representation gives voice but not exit: seniority, licensure, and geography tie them to the industry.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, frontline_operators_exposed_to_live_events, payer,
    organized, biographical, constrained, national).

% Passengers, residents, and patients who are inside the event when it arrives. They neither chose nor benefited from being the exercise; harm lands before any lesson is extracted. Their only structural role is post-hoc: testimony, litigation, memorialization.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, catastrophe_victims_and_survivors, payer,
    powerless, immediate, trapped, local).

% Junior operators told that real competence comes only from living through events, so their development is gated on catastrophe frequency they do not control. Career progression stalls in quiet decades; they inherit the risk exposure of under-drilled rosters while awaiting their exercise.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, novice_staff_deferred_to_event_learning, payer,
    powerless, biographical, constrained, national).

% Training researchers and simulator developers producing evidence that high-fidelity simulation with stress inoculation transfers to real-event performance. The doctrine dismisses their output categorically — the simulator always knows you are safe — so their findings reach procurement decisions mainly after disasters legitimate them. They publish from outside the operational hierarchy.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_advocates_and_training_scientists, excluded,
    moderate, generational, mobile, global).

% High-reliability organization scholars studying how institutions learn from near-misses and crises. They document both the reality of skill decay and the contingency of the exclusivity claim, mapping which portions of the doctrine survive scrutiny and which serve other functions.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, hro_research_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_operators_experiential_elite).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__catastrophe_as_necessary_anchor, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates scarce organizational learning effort on rare, high-information events; provides a shared criterion — has commanded through a real event — for selecting and trusting crisis leaders; sequences careers so that authority tracks demonstrated event exposure.
% TRANSFER_FUNCTION: Moves acute physical risk and developmental gating onto whoever occupies the operation when an event arrives (frontline crews, the surrounding public), and moves authority, pay premiums, investigation funding, and training-budget discretion upward to veterans, investigators, regulators, and management.
% ABSENT_VOICES: Future victims of the next exercise are definitionally absent whenever the doctrine is reaffirmed; novice staff lack standing in the certification bodies that gate their advancement; simulation researchers are answered categorically rather than engaged. The apparent unanimity behind the doctrine arises partly because its costs fall on seats that are not in the room.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, training investment would migrate toward validated simulation and hybrid regimens; certification criteria would decouple from event exposure; investigation funding would contract toward audit functions; the experiential authority premium would collapse; and risk-tolerance norms would tighten, because events would no longer be rationalized in advance as necessary pedagogy.
% FOUNDING_PROBLEM: Mid-century high-risk industries repeatedly found that emergency-handling skills decayed without exercise and that classroom instruction failed to produce crisis performance. The doctrine crystallized to explain why only real events seemed to restore competence and to organize learning, careers, and oversight around that observation.
% FOUNDING_PROBLEM_CORROBORATION: The decay half of the founding problem is corroborated from outside the beneficiary set by motor-skills-decay literature and regulator-commissioned training studies. The exclusivity half is not: no source outside the parties who collect the doctrine's premiums attests that only catastrophe suffices, and simulation-transfer research actively disputes it. Corroboration is therefore split by claim — corroborated for decay, uncorroborated for exclusivity.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, ExtMetricName, E),
    domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_exercise_requirement__catastrophe_as_necessary_anchor),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: even granted the reading's own premise, the arrangement purchases organizational competence with risk borne involuntarily by people who collect none of the premium; the exercise is administered on seats that did not enroll. Suppression 0.58: the doctrine is enforced through certification rules that weight event exposure, budget governance that defers simulation investment, and categorical dismissal of contrary evidence — discursive and structural rather than prohibitive, since hybrid regimes remain legally operable, which keeps suppression below snare grade. Theater_ratio 0.32: post-event learning is genuinely functional, but a growing share of activity is ritualized (lessons-learned artifacts, anniversary reviews, blame-allocating hearings) — the rising series shows Goodhart drift. Accessibility_collapse 0.34: alternatives remain live — the sibling readings are held by working professionals and hybrid regimens operate in some fleets and plants — which is inconsistent with a genuine natural law and consistent with a contested construct defended as one. Resistance 0.58: simulation scientists, HRO researchers, unions negotiating consent-based risk allocation, and victims' litigants actively contest the doctrine. The measurement series share one grid (points 0,10,20,30,40,50) so no metric is sampled against another's end-state. The dominant temporal dynamic is quiet-period accumulation: as catastrophe-free stretches lengthen, deferred training compounds and extractiveness rises monotonically — the first real event then reveals the accumulated decay, which the doctrine cites as proof of itself.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats the arrangement is prudence: no one can simulate the real thing, so readiness must be bought with exposure, and the premiums flowing to experience are earned. From the payer seats the identical structure is uncompensated risk transfer: they are enrolled in an exercise they did not consent to, gated out of advancement by its scarcity, and harmed by its delivery. From the observer seat it is an open empirical question being administered as settled. The engine computes these divergent per-seat classifications from the structural data; the authored mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: the veteran elite sits nearest the subsidy end (d near 0.1) amplified by identity_locked exit — the doctrine constitutes their professional selves, so they cannot arbitrage away from it; investigators and regulators sit slightly higher (d near 0.15-0.2) because their benefit is jurisdictional rather than constitutive; management's dual position (sets the arrangement, saves the budget) places it near 0.25 despite mobility. Declared victims map to high directionality: catastrophe victims are full targets (d near 0.95) — trapped by harm already delivered, zero exit; frontline crews and novices sit near 0.8, constrained by licensure and sunk career capital. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation. No directionality overrides are needed: beneficiary/victim declarations plus exit options reproduce the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — skill decay without exercise — is live and externally corroborated; the exclusivity remedy is contested. Classification prevents mislabeling in both directions: calling the arrangement a snare would erase its genuine coordination function (event-driven learning does extract real lessons and does select capable crisis leaders); calling it a rope or accepting the mountain claim at face value would erase the involuntary risk transfer and the premium concentration that ride on the same structure. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no zombie flag fires while the dispute is live. If future evidence kills the exclusivity claim while the arrangements persist unchanged, status flips to dead against a rearranging world and the capture/zombie flag fires, cross-checked against the rising theater_ratio series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the exclusivity claim a genuine structural feature of skill acquisition — a natural law that simulation cannot reach stress-encoded competence — or a constructed doctrine that persists because identifiable parties collect premiums from treating it as settled?',
    'Longitudinal head-to-head cohort studies comparing event-anchored versus high-fidelity-simulation-trained operators on blind-scored real-event performance, combined with forensic tracing of who funds and propagates the doctrine.',
    'If constructed, the false-summit reclassification stands and the arrangement reads as extraction riding on a real decay problem; if natural law, the measured extraction is the unavoidable price of competence and the mountain claim survives with beneficiaries as incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, empirical, 'Whether the catastrophe-exclusivity claim is natural law or interested doctrine.').

omega_variable(
    near_miss_substitution_ambiguity,
    'Do near-misses deliver the same irreducible exercise as full catastrophes — making the exercise obtainable without mass harm — or does only full-scale catastrophe carry the signal?',
    'Comparative performance analysis of organizations whose recent history contains near-misses versus matched controls with comparable simulation exposure, controlling for reporting culture.',
    'If near-misses substitute, the doctrine''s victim set shrinks drastically and effective extraction drops toward the hybrid reading''s profile; if not, the doctrine in effect schedules catastrophes and the extraction is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_substitution_ambiguity, empirical, 'Whether the exercise can be harvested without the catastrophe.').

omega_variable(
    survivorship_bias_in_experiential_authority,
    'Is the observed association between event experience and crisis competence causal (the exercise built the competence) or selection effects (those who already performed well were promoted into event-rich roles and survived to tell)?',
    'Prospective cohort designs tracking competence trajectories against event exposure with explicit selection controls, rather than retrospective hero narratives.',
    'If selection dominates, the experiential elite''s claim to the competence premium collapses, the doctrine loses its main evidentiary prop, and the beneficiary structure driving extraction dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_bias_in_experiential_authority, empirical, 'Causal versus selection account of the experience-competence correlation.').

omega_variable(
    kernel_quantifier_disagreement_location,
    'This constraint is one reading of the competence_exercise_requirement kernel; the sibling readings relocate or dissolve the exclusivity quantifier. Is the framing chosen here — crisis competence as a distinct stress-encoded faculty reachable only by live events — the only defensible one?',
    'Conceptual analysis of what the competence kernel denotes: if the kernel is defined as declarative-plus-procedural mastery, the simulation reading absorbs this one; if defined as stress-state performance, this reading''s framing holds and the hybrid reading marks the true boundary. Signals guiding the current choice: persistent post-event performance gaps documented in several industries despite high simulation adoption, and the recurring first-real-event decay reveal.',
    'Under the alternative framing (kernel as trainable mastery), this constraint''s epsilon collapses toward the simulation reading''s and the victim structure thins to near-benign; under the current framing, the extraction measured here is real and the siblings understate it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_quantifier_disagreement_location, conceptual, 'Committer-frame omega: reading-choice under-determination within the competence kernel.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of simulation investment structural (certification rules, budget governance, procurement gates) or internalized (practitioners'' identity fusion with event experience making simulation feel categorically illegitimate)?',
    'Natural experiments where certification rules decouple from event exposure: if simulation investment rises once structural barriers drop, suppression was structural; if attitudes and budget requests stay frozen, the internalized component dominates.',
    'If largely internalized, removing structural barriers will not restore alternatives — the veteran elite carries the doctrine with them across reforms, and effective suppression exceeds the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of the doctrine''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(comp_tr_t40, observed).
narrative_ontology:measurement(comp_tr_t50, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(comp_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 30, 0.59).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 40, 0.63).
narrative_ontology:measurement_basis(comp_be_t40, observed).
narrative_ontology:measurement(comp_be_t50, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 50, 0.66).
narrative_ontology:measurement_basis(comp_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.36).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t30, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 30, 0.51).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(comp_su_t40, observed).
narrative_ontology:measurement(comp_su_t50, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(comp_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__hybrid_dependency).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, near_miss_reporting_regimes).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'competence needs real events' decomposes into three structurally distinct claims with different epsilon values. Upstream: the corroborated observation that skills decay without exercise (shared ground, low extraction). Downstream extensions diverge: simulation_as_adequate_exercise (epsilon near zero on the catastrophe axis — no dependency on harm), hybrid_dependency (moderate — retains a real-anchoring dependency), and this story, catastrophe_as_necessary_anchor (highest epsilon — the exclusivity claim externalizes the cost of competence maintenance onto whoever occupies the system when events arrive). The upstream decay claim is routinely cited as evidence for the downstream exclusivity claim; the family links make that inferential coupling visible to contamination analysis. The edge to near_miss_reporting_regimes records a structural influence: this reading determines whether near-misses count as full-value exercise, which sets the reporting system's stakes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
