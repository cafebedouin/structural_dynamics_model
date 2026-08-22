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
 *   human_readable: Real-Catastrophe Necessity Doctrine (Lived Catastrophe Reading)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel. The colloquial
 *   commitment — 'operators stay sharp only through real emergencies; drills
 *   help but are not the real thing' — decomposes into three structurally
 *   distinct constraints per the epsilon-invariance principle. This file
 *   authors the lived_catastrophe_necessity_reading: the competence kernel is
 *   unitary, only actual catastrophe exercises it, simulation is rehearsal
 *   rather than exercise, and decay between real activations is covert. Its
 *   siblings — simulation_sufficiency_reading (simulated catastrophe
 *   genuinely exercises the kernel; fidelity governs retention) and
 *   hybrid_decay_reading (a two-component kernel: simulation exercises
 *   procedure, only real stakes exercise judgment-under-stakes) — are
 *   separate constraint files linked through network.affects_constraints. The
 *   epsilon values differ by construction: this reading's referent is the
 *   standing arrangement in which real-event proof governs credentialing, and
 *   its own lights register that arrangement as substantially costly —
 *   simulation admitted as necessary but insufficient, decay assumed
 *   invisible, and the victim set widened to everyone served by operators
 *   awaiting a real event. The arrangement retains a genuine coordination
 *   function: it guards professions against certifying readiness on exercises
 *   whose outcomes are reversible and known to participants, a failure mode
 *   accident investigations document repeatedly. Around that function sits
 *   asymmetric extraction: status and adjudication authority concentrate in
 *   crisis-experienced operators and the boards that certify them, while the
 *   residual risk of unactivated competence lands on publics, career costs
 *   land on juniors, and an unsatisfiable proof standard lands on rare-event
 *   industries. KEY AGENTS (by structural relationship):
 *   crisis_veteran_operators — primary beneficiary
 *   (organized/identity_locked), status premium from scarcity of real-event
 *   proof; high_event_rate_institutions — secondary beneficiary
 *   (institutional/arbitrage), converts event tempo into verified-competence
 *   legitimacy; licensing_and_promotion_authorities — agenda setter
 *   (institutional/constrained), administers the standard and collects
 *   adjudication authority; publics_served_by_untested_operators — primary
 *   target (powerless/trapped), bears residual risk of unactivated
 *   competence; junior_practitioners_awaiting_crisis — secondary target
 *   (moderate/constrained), careers gated on unschedulable experience;
 *   rare_event_industry_operators — structural target
 *   (institutional/trapped), permanently unable to satisfy the proof
 *   standard; simulation_training_departments — dual-positioned
 *   (moderate/constrained), funded as necessary and discounted as
 *   insufficient; training_research_community — excluded voice
 *   (moderate/mobile), holds transfer evidence with no seat in credentialing.
 *
 * KEY AGENTS:
 *   - crisis_veteran_operators: Primary beneficiary (organized/identity_locked) — status premium from scarcity of real-event proof
 *   - high_event_rate_institutions: Secondary beneficiary (institutional/arbitrage) — converts event tempo into verified-competence legitimacy
 *   - licensing_and_promotion_authorities: Agenda setter (institutional/constrained) — administers the standard; collects adjudication authority
 *   - publics_served_by_untested_operators: Primary target (powerless/trapped) — bears residual risk of unactivated competence
 *   - junior_practitioners_awaiting_crisis: Secondary target (moderate/constrained) — careers gated on unschedulable experience
 *   - rare_event_industry_operators: Structural target (institutional/trapped) — permanent inability to satisfy the proof standard
 *   - simulation_training_departments: Dual-positioned (moderate/constrained) — funded as necessary, discounted as insufficient
 *   - training_research_community: Excluded voice (moderate/mobile) — holds transfer evidence, no seat in credentialing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.62).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.55).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Real-Catastrophe Necessity Doctrine (Lived Catastrophe Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'c86bbdfa-5b6a-44d2-9943-fd06847a6926').
narrative_ontology:cs_kernel_codification('c86bbdfa-5b6a-44d2-9943-fd06847a6926', distributed).
narrative_ontology:cs_authority_grounding('c86bbdfa-5b6a-44d2-9943-fd06847a6926', practice).
narrative_ontology:cs_interpretation_layer_present('c86bbdfa-5b6a-44d2-9943-fd06847a6926').
narrative_ontology:cs_reading_relation('c86bbdfa-5b6a-44d2-9943-fd06847a6926', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('c86bbdfa-5b6a-44d2-9943-fd06847a6926', exercise_as_competence_maintenance__hybrid_decay_reading, forecloses).
narrative_ontology:cs_axiom('c86bbdfa-5b6a-44d2-9943-fd06847a6926', foundational, real_stakes_exclusive_exercise).
narrative_ontology:cs_axiom_status(real_stakes_exclusive_exercise, holdable).
narrative_ontology:cs_axiom_grounding('c86bbdfa-5b6a-44d2-9943-fd06847a6926', real_stakes_exclusive_exercise, empirically_contingent).
narrative_ontology:cs_axiom('c86bbdfa-5b6a-44d2-9943-fd06847a6926', foundational, covert_inter_activation_decay).
narrative_ontology:cs_axiom_status(covert_inter_activation_decay, holdable).
narrative_ontology:cs_axiom_grounding('c86bbdfa-5b6a-44d2-9943-fd06847a6926', covert_inter_activation_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('c86bbdfa-5b6a-44d2-9943-fd06847a6926', unitary_real_stakes_kernel).
narrative_ontology:cs_drift_state('c86bbdfa-5b6a-44d2-9943-fd06847a6926', post_simulation_maturation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c86bbdfa-5b6a-44d2-9943-fd06847a6926', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, crisis_veteran_operators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, high_event_rate_institutions).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, licensing_and_promotion_authorities).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, publics_served_by_untested_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, junior_practitioners_awaiting_crisis).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_training_departments).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, rare_event_industry_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_training_departments).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_transfer_limits_hypothesis).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, stress_inoculation_superiority_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior practitioners whose authority in incident command, review boards, and training design rests on having operated in actual catastrophes. The profession's standard of proof — that only real-event performance demonstrates readiness — makes their experience the scarce credential everything else is measured against. Their standing would erode if simulated performance became accepted evidence of competence; stepping away from the doctrine would mean surrendering the foundation of their authority, which is bound up with their account of who they are.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, crisis_veteran_operators, beneficiary,
    organized, biographical, identity_locked, national).

% Organizations whose operating tempo supplies frequent real events — busy metropolitan fire services, deployed military units, high-volume trauma centers. Each real event converts into verified competence and recruiting prestige that low-tempo peers cannot match, and they can rotate staff through high-tempo postings to accumulate qualifying experience, an option closed to organizations facing rare events. Their exposure is real — their people are hurt in the very events that credential them — but the institutional yield is durable legitimacy.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, high_event_rate_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Certification boards, promotion panels, and regulators that write minimum real-event experience into licensure and rank. They adjudicate whose experience counts, which makes them the enforcement point of the standard, and the adjudication role itself concentrates professional authority in their hands. Restructuring the standard toward simulated or portfolio-based evidence would expose them to blame if a later catastrophe found a reformed pathway wanting, so the standard tends to hold.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, licensing_and_promotion_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, licensing_and_promotion_authorities, beneficiary).

% Patients, residents, passengers, and communities served by safety-critical operators. Because real catastrophes are rare, most working operators at any moment have never performed under true stakes; the public carries the residual risk of covertly decayed or never-activated competence. They cannot inspect real-event records, cannot choose their emergency responders, and have no seat in the bodies that decide what counts as readiness.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, publics_served_by_untested_operators, payer,
    powerless, biographical, trapped, national).

% Early- and mid-career operators whose advancement runs through experience requirements they cannot schedule. They can drill relentlessly and score well in exercises, yet remain formally unproven until a real event happens near them. Careers stall or turn on the luck of posting; some leave for professions where evidence of competence can be manufactured. Leaving the field means writing off credentialing investment, so most stay and wait.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, junior_practitioners_awaiting_crisis, payer,
    moderate, biographical, constrained, national).

% Internal training units that build and run exercises. The standard concedes their work is necessary — rehearsal, familiarization, procedure — while denying that it maintains the capacity that matters, capping their budget growth and prestige regardless of fidelity gains. They are funded because the standard calls rehearsal necessary and discounted because it calls rehearsal insufficient; the value of their product is set by a body they do not sit on.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_training_departments, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_training_departments, beneficiary).

% Operators of systems whose catastrophic failure modes occur at most once in an institutional lifetime — nuclear plants, offshore platforms, long-haul aviation. Under a standard that accepts only real-event proof, their entire workforce is permanently unproven; no rotation scheme can manufacture qualifying events. They carry the epistemic burden that readiness can never be demonstrated, only asserted, the liability exposure that follows from that, and the recurring post-accident finding that no amount of drilling would have sufficed.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, rare_event_industry_operators, payer,
    institutional, generational, trapped, global).

% Human-factors and transfer-of-training researchers producing the evidence base on how simulated stress, fidelity, and decision-making practice transfer to real performance. Their findings bear directly on what exercises can and cannot build, but they hold no seat on credentialing bodies; their results enter the standard-setting conversation only when veterans or boards find them convenient.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, training_research_community, excluded,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, crisis_veteran_operators).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a profession-wide standard of proof for readiness claims: only performance under actual, irreversible stakes counts as evidence of operational competence. This prevents organizations from certifying readiness on exercises whose outcomes are reversible, scheduled, and known to participants, and keeps institutional memory of real failure modes inside the credentialing loop.
% TRANSFER_FUNCTION: Moves status, command authority, and career advancement toward operators and institutions holding verified real-catastrophe experience; moves the residual risk of unactivated or covertly decayed competence onto the publics those operators serve; moves training budgets toward live exercises and real-event rotations and away from simulation-only progression.
% ABSENT_VOICES: Transfer-of-training researchers hold the evidence base on what simulation can and cannot build but hold no seat on credentialing bodies; the publics exposed to untested operators appear nowhere in standard-setting; rare-event industries live under the standard's verdict without sitting on the boards that administer it. Unanimity behind the doctrine arises partly because these seats were never in the room.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, credentialing ladders would restructure around demonstrated simulation fidelity and structured judgment assessment, simulation budgets would expand and their prestige cap would lift, veteran status premiums would erode toward ordinary seniority, rare-event industries would regain a satisfiable readiness standard, and organizations would begin claiming verifiable preparedness — the profession's answer to 'are we ready?' would change from 'unknowable until real' to 'testable by proxy.'
% FOUNDING_PROBLEM: Mid-twentieth-century safety-critical professions repeatedly discovered that peacetime fluency and scripted drills did not predict emergency performance: crews rated excellent in routine operation and in exercises failed under real stakes, and exercise-passed response plans collapsed in actual events. The doctrine was built to stop organizations from mistaking rehearsed competence for crisis competence, and to keep the memory of that gap inside credentialing.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: accident investigation boards repeatedly document exercise-to-event transfer failures in their findings, and peer-reviewed human-factors research on stress, arousal, and decision-making under irreversible stakes independently documents simulation-reality gaps; neither source shares the veteran community's status interest. The corroboration attests the founding problem's liveness — it does not settle the contested question of whether only real catastrophe can address it, which is precisely the kernel dispute.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Scores describe the arrangement as it operates at interval end (2025), late in a vindication cycle. Extractiveness 0.62: the doctrine transfers real, effectively uninsurable risk onto publics (most operators at any moment are formally unproven), gates careers on unschedulable events, and imposes an unsatisfiable proof standard on rare-event industries, while returning genuine anti-complacency value. Suppression 0.55: enforcement runs through licensure requirements, promotion criteria, and cultural sanction against drill-floor competence; alternative frameworks survive in adjacent disciplines, so suppression is real but not total. Theater 0.40: a growing share of exercise activity is rehearsal performed under the sign of maintenance — the doctrine itself classes it so — while procedure drilling and team formation retain real function, and the doctrine's own credentialing rituals (war-story testimony before boards) carry performative load. Accessibility collapse 0.66: within the doctrine's frame, accepting the exclusivity premise collapses simulation-based confidence as an alternative almost entirely, though sibling readings persist institutionally. Resistance 0.58: sustained pushback from training researchers, simulation vendors, cost-conscious administrators, and rare-event operators, defended against by veterans and high-tempo institutions. CYCLICAL PATTERN: the series oscillates on a vindication cycle — each real catastrophe (1986, 2001, 2020) spikes extractiveness and suppression as the doctrine is re-proven, followed by multi-year decay as simulation capability matures and hybrid proposals circulate. The oscillation is partly the extraction mechanism itself: intermittent, unpredictable vindication makes the doctrine refutation-proof in practice (every catastrophe confirms it; no simulation success can confirm anything within its frame), and each vindication resets scrutiny of the risk quietly transferred onto publics. Base metrics are measured at 2025, late-cycle: post-pandemic vindication fading, hybrid pressure rising. Suppression is authored as a raw structural property; the engine alone scales extractiveness by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different arrangements from identical structure. From the crisis-veteran and licensing seats, the doctrine is the profession's epistemic conscience: the only honest answer to 'are we ready?' is 'we will not know until it is real,' and the hierarchy rewarding those who have been there is the price of not lying to ourselves. From the junior-practitioner seat, the same structure is an unfalsifiable gate — proof that cannot be scheduled, dispensed by elders whose own proof was luck of posting. From the public seat, it is silent risk transfer: no one asked them to underwrite the gap between rehearsal and reality. Rare-event industries compute an impossible standard: a proof requirement no admissible action can ever satisfy. The engine computes per-seat classifications from the structural data; the divergence between the veteran seat's discipline and the payer seats' extraction is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: crisis_veteran_operators (identity_locked exit — their authority is constituted by the doctrine's proof standard), high_event_rate_institutions (arbitrage — they manufacture qualifying exposure through operating tempo), and licensing_and_promotion_authorities (adjudication authority accrues to the standard's administrators). Victim declarations drive high directionality: publics_served_by_untested_operators (trapped — cannot select or inspect their protectors), junior_practitioners_awaiting_crisis (constrained — sunk credentialing investment), rare_event_industry_operators (trapped by event physics — no rotation scheme manufactures a qualifying catastrophe). simulation_training_departments sit near symmetric: funded as necessary, discounted as insufficient; no directionality override is authored because the paired declarations net out near the middle and the structural derivation approximates that. training_research_community holds an excluded seat — outside the enforcement conversation, its evidence enters only when convenient. Suppression is authored unscaled; scope amplification of effective extraction is the engine's arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — routine and drilled performance demonstrably failing to predict real-stakes performance — is live and corroborated from outside the benefiting parties (accident-board findings, human-factors transfer literature), so the arrangement is not a resolved mandate and mandatrophy_resolved is left undeclared. The tangled_rope claim is what prevents both mislabelings: reading the doctrine as pure extraction erases the genuine coordination it performs (professions genuinely need a guard against certifying readiness on reversible-outcome exercises, and accident investigations keep confirming the underlying gap); reading it as pure coordination erases the asymmetric structure (status concentration, career gating, silent risk transfer onto publics, an unsatisfiable standard imposed on rare-event industries). The piton precursor is visible and worth watching: if transfer research matures until high-fidelity simulation demonstrably exercises the kernel, the doctrine's mandate dies while credentialing ladders persist — theater_ratio's slow climb across the series is the leading indicator, and the measurement series exists to catch that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the exercise_as_competence_maintenance kernel; what would adopting a sibling reading change structurally?',
    'Adopting simulation_sufficiency_reading would shrink the victim set to operators trained below fidelity thresholds and lower epsilon; adopting hybrid_decay_reading would split the victim set by competency component, leaving only judgment-under-stakes gaps as covert risk.',
    'Classification, victim enumeration, and effective extraction all shift with the reading chosen; cross-reading comparison is valid only at the kernel level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is the lived_catastrophe_necessity_reading of a three-way kernel contest.').

omega_variable(
    transfer_effectiveness_empirics,
    'What fraction of real-stakes operational competence does high-fidelity simulation actually build and retain?',
    'Longitudinal linkage of simulation dosage and fidelity to blind-rated performance in subsequent real events, across professions.',
    'High transfer collapses this reading''s exclusivity premise, shrinks the victim set, and drops epsilon toward the simulation_sufficiency_reading''s profile; low transfer hardens the doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_effectiveness_empirics, empirical, 'Whether the exclusivity premise survives the transfer-of-training evidence base.').

omega_variable(
    covert_decay_detectability,
    'Is competence decay between real activations genuinely covert, or are there proxy measures sensitive enough to detect it before failure?',
    'Prospective validation of decay-sensitive markers (response latency, error-recovery quality, physiological stress response) against real-event outcomes.',
    'Detectable decay converts the doctrine''s unavoidable-risk claim into a monitoring requirement, cutting the risk transfer onto publics and lowering suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_decay_detectability, empirical, 'Whether the covert-decay assumption is a fact of measurement or a fact of physiology.').

omega_variable(
    vindication_asymmetry_falsifiability,
    'Is the doctrine''s vindication pattern — every real catastrophe confirms it, no simulation success can confirm anything — an epistemically sound asymmetry or an unfalsifiable structure that harvests confirmation?',
    'Require the doctrine''s holders to specify in advance what simulated outcome would count as exercising the kernel; refusal to specify any marks the asymmetry as structural dogma rather than calibrated caution.',
    'If unfalsifiable, the coordination function is cover and the arrangement drifts toward pure extraction; if specifiable, the anti-complacency function is genuine and the tangled characterization stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vindication_asymmetry_falsifiability, conceptual, 'Whether the intermittent-reinforcement vindication cycle is calibrated caution or dogma.').

omega_variable(
    deliberate_exposure_implication,
    'If only real stakes exercise the kernel, does the doctrine commit its holders to manufacturing real-stakes exposure for trainees — and is the refusal to state that implication where much of the arrangement''s cost hides?',
    'Survey credentialing authorities on whether they endorse supervised real-stakes exposure (live-fire, supervised high-acuity care) as a duty implied by their own standard.',
    'Endorsement makes the extraction explicit and probably intolerable; refusal exposes the standard as one its holders themselves decline to satisfy fully, shifting the burden onto publics by default.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_exposure_implication, preference, 'Whether the doctrine''s unstated practical implication is deliberate exposure or accepted public risk-bearing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 1979, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t1979, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 1979, 0.25).
narrative_ontology:measurement_basis(exer_tr_t1979, observed).
narrative_ontology:measurement(exer_tr_t1986, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 1986, 0.28).
narrative_ontology:measurement_basis(exer_tr_t1986, observed).
narrative_ontology:measurement(exer_tr_t1994, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 1994, 0.33).
narrative_ontology:measurement_basis(exer_tr_t1994, observed).
narrative_ontology:measurement(exer_tr_t2001, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement_basis(exer_tr_t2001, observed).
narrative_ontology:measurement(exer_tr_t2010, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement_basis(exer_tr_t2010, observed).
narrative_ontology:measurement(exer_tr_t2018, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 2018, 0.44).
narrative_ontology:measurement_basis(exer_tr_t2018, observed).
narrative_ontology:measurement(exer_tr_t2025, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(exer_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(exer_be_t1979, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 1979, 0.55).
narrative_ontology:measurement_basis(exer_be_t1979, observed).
narrative_ontology:measurement(exer_be_t1986, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 1986, 0.6).
narrative_ontology:measurement_basis(exer_be_t1986, observed).
narrative_ontology:measurement(exer_be_t1994, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 1994, 0.62).
narrative_ontology:measurement_basis(exer_be_t1994, observed).
narrative_ontology:measurement(exer_be_t2001, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 2001, 0.65).
narrative_ontology:measurement_basis(exer_be_t2001, observed).
narrative_ontology:measurement(exer_be_t2010, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement_basis(exer_be_t2010, observed).
narrative_ontology:measurement(exer_be_t2018, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement_basis(exer_be_t2018, observed).
narrative_ontology:measurement(exer_be_t2025, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(exer_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t1979, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 1979, 0.5).
narrative_ontology:measurement_basis(exer_su_t1979, observed).
narrative_ontology:measurement(exer_su_t1986, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 1986, 0.57).
narrative_ontology:measurement_basis(exer_su_t1986, observed).
narrative_ontology:measurement(exer_su_t1994, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 1994, 0.58).
narrative_ontology:measurement_basis(exer_su_t1994, observed).
narrative_ontology:measurement(exer_su_t2001, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 2001, 0.63).
narrative_ontology:measurement_basis(exer_su_t2001, observed).
narrative_ontology:measurement(exer_su_t2010, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement_basis(exer_su_t2010, observed).
narrative_ontology:measurement(exer_su_t2018, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 2018, 0.61).
narrative_ontology:measurement_basis(exer_su_t2018, observed).
narrative_ontology:measurement(exer_su_t2025, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(exer_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, identity_coordination).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'real experience keeps operators sharp; drills are not the real thing' covers three structurally distinct claims with different epsilon, different victim sets, and different failure modes — decomposed per the epsilon-invariance principle into lived_catastrophe_necessity_reading (this file: unitary kernel, real-only exercise, widest victim set), simulation_sufficiency_reading (simulated catastrophe exercises the kernel; smallest victim set, lowest epsilon), and hybrid_decay_reading (two-component kernel; intermediate victim set keyed to judgment-under-stakes gaps). Upstream/downstream: the lived-catastrophe reading is the traditional upstream claim — its vindications (post-catastrophe findings that drills did not transfer) are cited as evidence against simulation sufficiency — while the hybrid reading mediates by granting simulation partial exercise. Each file links the other two through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
