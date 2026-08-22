% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, []).

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
 *   constraint_id: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
 *   human_readable: Scheduled High-Fidelity Drill Regime — Simulation as Proxy Catastrophe
 *   domain: safety engineering / organizational learning / high-reliability systems
 *
 * SUMMARY:
 *   In high-reliability industries — aviation, nuclear power, surgery,
 *   emergency response — the operative claim that high-fidelity simulation
 *   constitutes genuine practice, and that scheduled drills are functionally
 *   equivalent to real catastrophic events for competence maintenance, is the
 *   load-bearing justification for a global certification regime: regulators
 *   mandate recurrent simulator checks as the auditable proof of operational
 *   competence, organizations purchase full-motion simulators and scenario
 *   libraries to satisfy the mandate, and careers advance or stall on
 *   scheduled performance. This story instantiates the reading that endorses
 *   that arrangement — the regime's own self-understanding — and authors it
 *   as the reading sees it: a genuine coordination achievement with real but
 *   mostly-justified costs. The interval spans roughly 1990 to 2026, the era
 *   in which simulator-based recurrent certification became universal and its
 *   enforcement machinery hardened. The claim and the metrics are authored
 *   independently: the claimed type is tangled_rope because the regime
 *   requires active enforcement, concentrates gains in the simulation
 *   industry, and suppresses rival accounts of competence maintenance, while
 *   epsilon is authored at 0.48 from this reading's own lights over the
 *   standing mandated-drill arrangement — the reading concedes vendor
 *   margins, drill burden beyond demonstrated need, and checkride-optimized
 *   scripting, but counts the regimen's price as largely the cost of the
 *   coordination it delivers. The engine computes each seat's type from the
 *   structural data; divergence between the authored claim and any seat's
 *   computed type is the measurement, not an error.
 *
 * KEY AGENTS:
 *   - simulation_infrastructure_vendors: primary beneficiary (organized/mobile) — collects the regime's revenue; exit into adjacent training markets is realistic
 *   - regulatory_certification_bodies: agenda setter (institutional/constrained) — mandates the drill schedule that is its only auditable competence proxy
 *   - organizational_training_departments: dual beneficiary/administrator (moderate/identity_locked) — designs and runs the drills; careers fused with the mission
 *   - frontline_operators: primary target (moderate/constrained) — pays career-hours and bears the drill-to-reality gap; cannot refuse without losing licensure
 *   - catastrophe_exposed_public: diffuse target (powerless/trapped) — bears residual tail risk with no seat in scenario design
 *   - regulated_operating_organizations: dual-positioned payer/beneficiary (powerful/constrained) — buys compliance, gains insurability and a liability story
 *   - safety_science_researchers: excluded critic (moderate/mobile) — documents the transfer gap outside the certification loop
 *   - accident_investigation_boards: analytical observer (institutional/analytical) — the only post-hoc systematic test of the equivalence claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.48).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.55).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "Scheduled High-Fidelity Drill Regime — Simulation as Proxy Catastrophe").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety engineering / organizational learning / high-reliability systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '49060108-bf01-4d21-9de6-84476e0f9a69').
narrative_ontology:cs_kernel_codification('49060108-bf01-4d21-9de6-84476e0f9a69', formalized).
narrative_ontology:cs_authority_grounding('49060108-bf01-4d21-9de6-84476e0f9a69', expertise).
narrative_ontology:cs_interpretation_layer_present('49060108-bf01-4d21-9de6-84476e0f9a69').
narrative_ontology:cs_reading_relation('49060108-bf01-4d21-9de6-84476e0f9a69', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_reading_relation('49060108-bf01-4d21-9de6-84476e0f9a69', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('49060108-bf01-4d21-9de6-84476e0f9a69', foundational, simulation_constitutes_genuine_practice).
narrative_ontology:cs_axiom_status(simulation_constitutes_genuine_practice, holdable).
narrative_ontology:cs_axiom_grounding('49060108-bf01-4d21-9de6-84476e0f9a69', simulation_constitutes_genuine_practice, empirically_contingent).
narrative_ontology:cs_axiom('49060108-bf01-4d21-9de6-84476e0f9a69', secondary, scheduled_drill_certification_sufficiency).
narrative_ontology:cs_axiom_status(scheduled_drill_certification_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('49060108-bf01-4d21-9de6-84476e0f9a69', scheduled_drill_certification_sufficiency, conventional).
narrative_ontology:cs_reference_frame('49060108-bf01-4d21-9de6-84476e0f9a69', scheduled_simulation_sufficiency_regime).
narrative_ontology:cs_drift_state('49060108-bf01-4d21-9de6-84476e0f9a69', post_af447_uprt_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('49060108-bf01-4d21-9de6-84476e0f9a69', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_certification_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizational_training_departments).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_exposed_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulated_operating_organizations).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulated_operating_organizations).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_transfer_hypothesis).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, scheduled_drill_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and sells full-motion simulators, scenario libraries, and instructor-certification programs to airlines, nuclear operators, hospitals, and emergency-response agencies. Revenue scales directly with the number of mandated recurrent checks, and industry associations fund research supporting the adequacy of simulator-based training. Exit is realistic: the same engineering and pedagogy sells into defense, maritime, and medical training markets.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_vendors, beneficiary,
    organized, biographical, mobile, global).

% Writes and enforces the recurrent-training rules that make scheduled simulator checks the auditable proof of operational competence. It has no cheaper observable to certify against, so the drill schedule underwrites every operating certificate it issues. If scheduled performance were found not to predict catastrophe performance, the certification apparatus would lose its measuring stick; the agency cannot exit its own enforcement role.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_certification_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Designs drill schedules, scripts scenarios, and runs the checkrides inside each operating organization. The department's budget, headcount, and professional standing scale with the training mandate, and careers are built on simulator instruction and standards work. Practitioners' professional identity is fused with the mission of keeping the organization ready; questioning the drill regimen reads from inside as questioning their own life's work.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizational_training_departments, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizational_training_departments, agenda_setter).

% Pilots, reactor operators, surgical teams, and incident commanders spend hundreds of career-hours in scheduled drills and pass or fail licensure on simulator performance. They cannot refuse the regimen without losing certification. They also staff the real events, so any gap between drilled and real-event demands lands on them first; their reports that drills feel scripted are routed back through the same training departments that run the drills.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators, payer,
    moderate, biographical, constrained, global).

% Passengers, plant neighbors, and patients bear the consequences whenever drilled competence fails to match a real catastrophe's demands. They hold no seat in scenario design or checkride standards, cannot exit their exposure to transport, energy, and medical systems, and typically learn of any drill-versus-reality gap only through post-accident investigation.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_exposed_public, payer,
    powerless, generational, trapped, global).

% Airlines, nuclear utilities, and hospital systems purchase the simulators and host the drills to keep their operating certificates. They gain a defensible safety record, insurability, and a liability story anchored in regulatory compliance; they pay the capital and labor cost of the regimen. Declining the drills means losing the certificate, so exit from the arrangement is exit from the industry.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulated_operating_organizations, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulated_operating_organizations, payer).

% Study the transfer of simulator-trained skill to real-event performance and publish on fidelity limits, scripted-scenario effects, and skill decay. Their findings circulate in journals and conference panels but do not gate certification: no finding of theirs suspends a drill mandate. Several hold training appointments inside the very organizations whose regimen they study, which blurs the critic role.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_science_researchers, excluded,
    moderate, generational, mobile, global).

% Reconstruct real catastrophes after the fact and compare crew performance against the training record, making them the only systematic post-hoc test of whether scheduled performance predicts catastrophe performance. Their findings arrive only after failure, and their recommendations — additional real-aircraft maneuver requirements, upset-recovery training — are the historical mechanism by which the drill regimen has been corrected.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accident_investigation_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_vendors).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains catastrophe-avoidance competence across industries where real catastrophes are too rare, too costly, and too dangerous to train against directly: scheduled high-fidelity drills give thousands of distributed crews and teams a common practice cadence, common scenarios, and an auditable standard, solving the underinvestment problem that would otherwise leave rare-event preparedness unprovisioned by every individual operator.
% TRANSFER_FUNCTION: Moves training budgets and operator hours from operating organizations and frontline workers to simulator vendors and training/certification bureaucracies, and moves the burden of proving competence from rare real-world outcomes (uncontrollable, catastrophic when wrong) to frequent scheduled performance (controllable, observable, gradeable).
% ABSENT_VOICES: Safety-science researchers on the simulation-to-reality transfer gap sit outside the certification loop; frontline operators' reports of drill scriptedness are filtered through the training departments that run the drills; the exposed public has no seat in scenario design; and the position that only real catastrophes supply the necessary selection pressure is morally unspeakable in policy, so the strongest structural critique of the equivalence claim never enters the room.
% DISAPPEARANCE_RATIONALE: Certification systems would lose their only auditable competence proxy overnight: regulators would face a choice between grounding fleets, idling reactors, and halting elective surgery, or accepting uncertified competence; simulator procurement and instructor-certification industries would collapse; and organizations would scramble toward near-miss reporting, foreign-incident review, and apprenticeship structures to rebuild a competence-maintenance arrangement from scratch.
% FOUNDING_PROBLEM: After mid-century aviation disasters and early nuclear incidents, high-reliability industries faced a competence-maintenance problem with no natural solution: catastrophic events are too rare to train against and too costly to risk, yet skills, coordination, and organizational response decay without practice. The drill regime was built to answer one question: how do you maintain catastrophe-competence without catastrophes?
% FOUNDING_PROBLEM_CORROBORATION: Accident-investigation boards — outside the vendor, regulator, and training-department beneficiary set — corroborate that the founding problem is live: post-accident reports repeatedly document skill and coordination decay that current drill schedules failed to prevent, the high-altitude stall case (AF447) being the canonical example. Academic safety science attests the decay problem independently of the training industry. No corroborating source outside the beneficiary set attests that the problem is solved.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.48 from this reading's own lights, with the standing mandated-drill regime as referent: the reading concedes vendor margins on mandated procurement, drill hours beyond demonstrated competence need, and checkride-optimized scripting as real costs, but counts the regimen's price as largely the cost of the coordination it delivers — hence neither near-zero nor high. Suppression (0.55) is authored as a raw structural property, unscaled by power or scope: license-contingent mandatory checks leave operators no refusal path, and the rival account of competence (that only real catastrophes supply the necessary selection pressure) is politically unspeakable rather than argued against. Theater (0.44) reflects scripted scenarios optimized for checkride outcomes: a large and growing share of drill activity certifies the regimen rather than stretching competence. Accessibility collapse (0.42) is moderate: abandoning drills is unavailable to any certificate holder and near-miss data is increasingly absorbed as drill-scenario input, but near-miss programs and foreign-incident review persist as partial alternatives. Resistance (0.40) runs through researcher critique, operator grievance, and post-accident correction rather than open refusal. The three series share one grid (seven points across 0-36): extractiveness and enforcement rise through the regime's hardening phase, then plateau and ease slightly after the post-accident era in which investigation boards forced real-aircraft upset-recovery and manual-handling requirements into the regimen — a partial concession that shifted some spending from simulation toward genuine practice and disciplined vendor rents at the margin. Receipt: the regime's gains demonstrably accrue to the vendor seat (mandated, capital-intensive procurement), while fixing the arrangement — replacing scheduled-drill certification with a hybrid competence apparatus — is prohibitive for the regulators who could attempt it: decades of rulemaking, international harmonization, sunk simulator fleets, and blame exposure if a catastrophe follows reform.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the regulator's seat the arrangement is coordination it built and administers: scheduled performance is the only observable it can certify against. From the frontline operator's seat the same schedule is mandatory performance with career stakes attached and an untested gap between the drilled event and the real one. From the public's seat it is an invisible allocation of tail risk made without a seat at the table. Same-level divergence: frontline operators and training departments hold the same moderate power atom but different exits — operators are constrained by licensure, trainers are locked by professional fusion with the training mission — which differentiates their directionalities despite equal standing. Inter-institutional divergence: vendors and regulators both gain from the regime, but the vendor's horizon is the sales cycle while the regulator's is generational legitimacy, and only the regulator bears the blame exposure if the equivalence claim fails publicly.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations put vendors, regulators, and training departments near the beneficiary end: vendors with arbitrage-grade exit sit closest to d=0; regulators, constrained to their enforcement role and bearing legitimacy risk, sit slightly higher; identity-locked trainers sit between. Victim declarations put frontline operators (constrained exit) and the exposed public (trapped, no seat) near the target end, the public highest. Operating organizations are dual-positioned — they gain insurability and a liability story while paying the capital and labor cost — and derive near symmetric. Researchers are excluded rather than coordinated: little flows to or through them, but their critique is kept out of the certification loop. No directionality overrides are authored: the beneficiary/victim plus exit derivation captures these relationships, and an override keyed at the moderate power atom would misfire, since three agents sharing that atom hold opposed structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — maintaining catastrophe-competence when catastrophes are too rare and too costly to train against — is live, so the mandate has not outlived its function and no mandatrophy is declared. The tangled_rope classification prevents both mislabelings: a rope-only reading would miss the concentrated vendor gains, the checkride theater, and the suppression of rival competence accounts; a snare-only reading would miss the genuine and irreplaceable coordination function, since the alternative practice — training against real catastrophes — is ethically and practically unavailable. The contested element is the equivalence axiom, not the mandate itself; if the transfer gap proved large, the arrangement would drift toward pure extraction — resources certified as competence without delivering it — which is the trajectory the transfer-gap and self-sealing omegas watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_gap_magnitude,
    'How large is the gap between competence demonstrated in scheduled high-fidelity drills and performance under real catastrophic conditions?',
    'Systematic cross-referencing of drill and checkride records against post-accident performance findings across the accident corpus, plus fidelity-controlled study of real events that happened to match trained scenarios.',
    'A large gap would push the standing regime toward snare-flavored classification (resources certified as competence without delivering it); a small gap would support the near-rope coordination reading this reading endorses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_gap_magnitude, empirical, 'Magnitude of the drill-to-reality transfer gap.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the simulation_as_proxy_catastrophe reading of the catastrophe_avoidance_retention kernel; what would the sibling readings change structurally, and where is the disagreement located?',
    'The disagreement is located in the transfer-sufficiency premise: whether the ingredients real catastrophes supply (mortality salience, genuine chaos, organizational trauma) are functionally substitutable by simulation. The catastrophe_as_necessary_selector sibling denies substitutability outright; the hybrid_near_miss_learning sibling asserts partial substitutability requiring distributed near-miss channels alongside drills.',
    'If the selector reading prevailed, the standing drill regime''s epsilon would rise sharply (resources consumed without the claimed competence maintenance) and regulatory enforcement would be insufficient by definition; if the hybrid reading prevailed, the regime would need restructuring around near-miss channels rather than scheduled drills.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one of three readings of the catastrophe-competence kernel.').

omega_variable(
    falsification_self_sealing,
    'Is the equivalence claim structurally self-sealing — its falsification event (a catastrophe the drills failed to prevent) is the very event the regime exists to prevent — so can the regime in principle verify its own claim?',
    'Coverage-gap audit: systematically compare drill-scenario libraries against the actual failure modes identified in accident investigations, measuring how much of the real failure space scheduled scenarios sample.',
    'A systematic coverage gap would establish that scheduled drills cannot in principle falsify the equivalence claim, raising the structural floor of the theater ratio and making external (accident-board) verification mandatory rather than supplementary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(falsification_self_sealing, empirical, 'Whether the regime can verify its own equivalence claim or is structurally self-sealing.').

omega_variable(
    hybrid_channel_absorption,
    'Does the drill regime''s absorption of near-miss data as scenario-design input preserve or degrade the independent near-miss learning channel?',
    'Compare near-miss reporting rates, report follow-through, and organizational learning outcomes before and after scenario-library integration in comparable organizations.',
    'If absorption degrades the channel, the regime''s epistemic cost is understated by the authored suppression value and the hybrid sibling''s claim gains empirical force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_channel_absorption, empirical, 'Effect of drill-regime absorption on independent near-miss learning.').

omega_variable(
    selector_position_suppression_mechanism,
    'Is the suppression of the catastrophe-as-selector position structural (moral and political unavailability in policy) or internalized (professionals genuinely believe scheduled drills suffice)?',
    'Post-correction trajectory: track whether training organizations that adopt hybrid elements re-expand scripted drill hours (structural suppression intact) or sustain diversified practice (internalization broken).',
    'If internalized, the regime''s suppression persists even after regulatory diversification — the equivalence claim would survive its own partial refutations as professional common sense.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selector_position_suppression_mechanism, conceptual, 'Structural versus internalized suppression of the rival competence account.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t6, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(cata_tr_t6, observed).
narrative_ontology:measurement(cata_tr_t12, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 12, 0.29).
narrative_ontology:measurement_basis(cata_tr_t12, observed).
narrative_ontology:measurement(cata_tr_t18, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 18, 0.34).
narrative_ontology:measurement_basis(cata_tr_t18, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t36, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 36, 0.44).
narrative_ontology:measurement_basis(cata_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t6, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 6, 0.38).
narrative_ontology:measurement_basis(cata_be_t6, observed).
narrative_ontology:measurement(cata_be_t12, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 12, 0.42).
narrative_ontology:measurement_basis(cata_be_t12, observed).
narrative_ontology:measurement(cata_be_t18, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 18, 0.46).
narrative_ontology:measurement_basis(cata_be_t18, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 24, 0.51).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t36, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 36, 0.48).
narrative_ontology:measurement_basis(cata_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t6, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 6, 0.45).
narrative_ontology:measurement_basis(cata_su_t6, observed).
narrative_ontology:measurement(cata_su_t12, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(cata_su_t12, observed).
narrative_ontology:measurement(cata_su_t18, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 18, 0.58).
narrative_ontology:measurement_basis(cata_su_t18, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 24, 0.61).
narrative_ontology:measurement_basis(cata_su_t24, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t36, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 36, 0.55).
narrative_ontology:measurement_basis(cata_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention__catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention__hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% The colloquial question of how high-reliability organizations keep catastrophe-competence decomposes into three structurally distinct constraint stories over one kernel (catastrophe_avoidance_retention), each with its own epsilon over the same referent — the standing mandated-drill regime. This story instantiates simulation_as_proxy_catastrophe, the regime's self-understanding, and authors epsilon 0.48: costs real but mostly justified. The sibling story catastrophe_as_necessary_selector authors the same standing arrangement as consuming the resources catastrophe-selection would consume while failing to supply the selection pressure — substantially higher epsilon. The sibling story hybrid_near_miss_learning authors it as moderately extractive: under-delivering because scheduled drills crowd out distributed near-miss and foreign-incident learning channels. This story links to both siblings: the drill regime structurally influences the hybrid channel (near-miss data increasingly consumed as drill-scenario input), and its equivalence premise forecloses the selector premise within any single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
