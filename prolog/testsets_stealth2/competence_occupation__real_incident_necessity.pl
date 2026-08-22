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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   domain: organizational/safety-science/high-reliability
 *
 * SUMMARY:
 *   Within high-reliability domains (nuclear operations, commercial aviation,
 *   offshore drilling, chemical processing), a durable doctrine holds that
 *   only actual catastrophic incidents supply the authentic conditions — real
 *   stakes, real stress, irreversible consequence — required to occupy the
 *   competence kernel: the core of judgment needed to prevent and manage
 *   disasters. The doctrine functions as an epistemic gate: incident-derived
 *   knowledge outranks rehearsal-derived knowledge in certification, staffing
 *   seniority, insurance narratives, and regulatory deference, while
 *   simulation is persistently discounted as weightless. The arrangement this
 *   story is about — the standing incident-gated legitimacy regime — is the
 *   epsilon referent, assessed by this reading's own lights: even its holders
 *   concede the tuition is paid in catastrophes, which is why epsilon is high
 *   despite the reading endorsing the arrangement. This file instantiates ONE
 *   reading (real_incident_necessity) of the competence_occupation kernel;
 *   the sibling readings (simulation_sufficiency, hybrid_occupation) are
 *   separate constraints in separate files, linked through
 *   network.affects_constraints. The colloquial label 'how HROs maintain
 *   competence' decomposes into these three readings with distinct epsilon
 *   values and distinct victim sets. Claim/metric independence is preserved:
 *   the claimed type is stated from structural analysis, the metrics from
 *   descriptive operation, and neither was tuned toward the other.
 *
 * KEY AGENTS:
 *   - incident_veteran_experts: Agenda-setting collector (powerful/identity_locked) — chairs review boards, certifies competence sign-offs, converts lived incident participation into a permanent authority premium
 *   - accident_investigation_industry: Secondary collector (organized/mobile) — converts each incident into reports, courses, and retained advisory contracts
 *   - hro_executive_leadership: Dual-positioned bearer (powerful/constrained) — carries license and liability exposure while trading on battle-tested expertise as an asset
 *   - junior_operators: Bearer (moderate/constrained) — must accumulate credibility through incidents they cannot schedule or refuse
 *   - future_catastrophe_victims: Primary bearer (powerless/trapped) — workers and neighboring publics who supply the harm that doubles as the system's only fully credited lesson
 *   - simulation_training_advocates: Excluded voice (organized/mobile) — simulator builders and human-factors practitioners out-ranked in certification decisions
 *   - safety_regulators: Observer (institutional/analytical) — alternates between mandating drills in quiet periods and privileging incident findings after disasters
 *   - human_factors_research_community: Observer (organized/analytical) — produces the transfer-of-training evidence both camps cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.72).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.74).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.72).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, tangled_rope).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real-Incident Necessity Doctrine for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "organizational/safety-science/high-reliability").

domain_priors:requires_active_enforcement(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '59b09a21-8e04-4f68-9611-05b6c503099b').
narrative_ontology:cs_kernel_codification('59b09a21-8e04-4f68-9611-05b6c503099b', distributed).
narrative_ontology:cs_authority_grounding('59b09a21-8e04-4f68-9611-05b6c503099b', practice).
narrative_ontology:cs_interpretation_layer_present('59b09a21-8e04-4f68-9611-05b6c503099b').
narrative_ontology:cs_reading_relation('59b09a21-8e04-4f68-9611-05b6c503099b', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('59b09a21-8e04-4f68-9611-05b6c503099b', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('59b09a21-8e04-4f68-9611-05b6c503099b', foundational, kernel_occupation_requires_real_consequence).
narrative_ontology:cs_axiom_status(kernel_occupation_requires_real_consequence, holdable).
narrative_ontology:cs_axiom_grounding('59b09a21-8e04-4f68-9611-05b6c503099b', kernel_occupation_requires_real_consequence, empirically_contingent).
narrative_ontology:cs_axiom('59b09a21-8e04-4f68-9611-05b6c503099b', secondary, simulated_stress_is_weightless_for_kernel_maintenance).
narrative_ontology:cs_axiom_status(simulated_stress_is_weightless_for_kernel_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('59b09a21-8e04-4f68-9611-05b6c503099b', simulated_stress_is_weightless_for_kernel_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('59b09a21-8e04-4f68-9611-05b6c503099b', lived_consequence_standard).
narrative_ontology:cs_drift_state('59b09a21-8e04-4f68-9611-05b6c503099b', contemporary_high_fidelity_simulation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59b09a21-8e04-4f68-9611-05b6c503099b', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, incident_veteran_experts).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, accident_investigation_industry).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, future_catastrophe_victims).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, junior_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, hro_executive_leadership).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, hro_executive_leadership).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, simulation_fidelity_insufficiency).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, experiential_knowledge_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior operators, chief engineers, and veteran accident investigators who personally lived through major incidents. They chair review boards, sign off competency certifications, and set training doctrine. Their authority premium is inseparable from the scarcity of lived incident knowledge; stepping away from the expert role would forfeit the identity and standing built on having been there, so they remain in the gatekeeping role for entire careers.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_veteran_experts, agenda_setter,
    powerful, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, incident_veteran_experts, beneficiary).

% Forensic engineering firms, root-cause consultancies, and incident-derived training companies. Each catastrophe generates reports, courses, and multi-year advisory retainers flowing to them, and their pipeline depends on incidents being treated as the premier knowledge source. When incident work thins, they pivot into adjacent compliance and audit consulting without losing their firms.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, accident_investigation_industry, beneficiary,
    organized, biographical, mobile, global).

% Boards and executives of nuclear, aviation, offshore, and chemical operators. They carry license, liability, and reputational exposure whenever an incident occurs on their watch, yet they also trade on the resulting battle-tested expertise when negotiating with insurers and regulators. Leaving the industry's safety regime is not available to them; they manage within it.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, hro_executive_leadership, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__real_incident_necessity, hro_executive_leadership, beneficiary).

% Control-room trainees, first officers, and shift engineers early in their careers. Drill performance is discounted in the credibility economy they must navigate, so their route to full professional standing runs through incidents they did not choose and cannot schedule. Changing employers does not change the standard, and declining the path ends the career.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, junior_operators, payer,
    moderate, biographical, constrained, regional).

% Workers on the floor and the publics living beside plants, flight paths, and platforms. They bear the harm of the incidents that double as the system's only fully credited lessons, and they bear it generation after generation. They hold no seat in training-doctrine debates and cannot individually opt out of the risks they live within.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, future_catastrophe_victims, payer,
    powerless, generational, trapped, global).

% Simulator manufacturers, human-factors practitioners, and forward-leaning training directors who argue that high-fidelity rehearsal can carry competence maintenance. They are consulted in working groups but consistently out-ranked in certification and budget decisions by incident-derived authority, and their proposals are received as supplements rather than substitutes.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, simulation_training_advocates, excluded,
    organized, biographical, mobile, global).

% Agencies that accredit training programs, mandate drill hours, and run or commission investigations. Their posture oscillates with the incident cycle: pushing simulation requirements during quiet periods, then privileging incident findings and veteran testimony after disasters. They take testimony from every seat while holding no operational risk themselves.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, safety_regulators, observer,
    institutional, generational, analytical, national).

% Academic researchers studying skill decay, stress inoculation, and simulation-to-field transfer. They produce the evidence base that both camps cite — confirming real gaps between rehearsal and lived consequence while refusing to certify that only real events close them — and they hold no stake in certification outcomes.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, human_factors_research_community, observer,
    organized, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, hard-to-fake evidentiary standard for emergency competence: knowledge proven under real consequence outranks knowledge from rehearsal, which lets organizations, insurers, regulators, and crews coordinate trust allocation about who can be relied upon in disaster conditions.
% TRANSFER_FUNCTION: Moves legitimacy, authority, and fee income toward holders of direct incident experience, and moves the cost of competence maintenance onto those harmed by the incidents themselves — casualties, disrupted operations, and destroyed assets finance the curriculum that the rest of the system learns from.
% ABSENT_VOICES: Future victims of not-yet-occurring incidents have no seat anywhere in the arrangement — they are the payers least represented and least able to object in advance. Simulation advocates are heard in consultation but structurally out-ranked at every certification decision point. Junior operators' interest in preparatory rather than experiential competence surfaces only through unions and professional bodies, weakly.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, certification systems, staffing seniority rules, insurance pricing, and training budgets would reorganize around demonstrated simulation and hybrid competence within a planning cycle; the veteran authority premium would deflate as incident exposure lost its scarcity value; investigation demand would shift from epistemic gatekeeping to ordinary causal analysis; and the standing population at risk would face a differently-shaped (not vanished) hazard profile as rehearsal-based preparation expanded.
% FOUNDING_PROBLEM: Mid-twentieth-century disasters repeatedly showed classroom-trained and lightly drilled crews failing in ways no curriculum had anticipated; the professions concluded that only surviving real events teaches what actually matters under catastrophe, and built credentialing, seniority, and review institutions around accumulated incident exposure.
% FOUNDING_PROBLEM_CORROBORATION: The rehearsal-reality gap itself is corroborated from outside the benefiting parties: the human-factors transfer-of-training literature and official accident investigation reports both document failures of purely simulated preparation. However, no neutral body attests the necessity conclusion — the same research community that corroborates the gap explicitly disputes whether it justifies gating competence on real catastrophes, and the investigation industry's corroboration is compromised by its dependence on the incident pipeline. Corroboration for the founding problem: partial and disputed; corroboration for the reading's strong form: none from outside the benefiting set.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.72 at interval end) because the arrangement's curriculum is financed by casualties: each admissible lesson is purchased with harm to identifiable third parties who never enrolled, and the scarcity pricing of incident knowledge concentrates returns on those who happened to be present. Suppression is higher still (0.74) because persistence depends on actively devaluing the alternative — simulator budgets lose to incident-response line items, drill performance is discounted in promotion boards, and the enforcement machinery (review boards, credentialing, after-action hierarchies) matured steadily across the interval. Theater is moderate and rising between crises (0.48 at end): anniversary reviews, lessons-learned libraries, and war-story rituals increasingly substitute for living instructional use of incident material as memories fade. The measurement series run on one shared time grid (all three metrics at all ten points). The series are CYCLICAL, not monotonic: extractiveness spikes after each major disaster (peak harvest of fresh authentic material circa t10, the late-1980s cluster, and t35, the 2010-2011 offshore/nuclear cluster), then decays as the material ages; theater rises during the inter-crisis troughs. Critically, the oscillation is not noise — it is intermittent reinforcement, and partly the extraction mechanism itself: each catastrophe re-legitimizes the doctrine at maximum intensity, then decay sets in until the next event re-proves the premise. Base_properties scalars reflect the t45 endpoint: a post-crisis decay plateau, late-cycle.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting collector seat and the bearer seats should compute divergent types from identical structural data. From inside the veteran-expert seat, the arrangement is a rigorously earned meritocracy they built and administer: drills genuinely feel weightless from a chair occupied by someone who has watched a control room fail, and the discounting of simulation reads as epistemic honesty rather than exclusion. From the junior-operator and future-victim seats, the same structure operates as an unschedulable toll: careers gated on events they must hope never occur, and tuition billed to people who never consented. The executive seat straddles the gap — bearing the liability the doctrine generates while monetizing the credibility it confers. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. incident_veteran_experts are declared beneficiaries with identity_locked exit — their professional selves are constituted by incident participation, so they sit near the full-beneficiary end despite holding agenda-setting power; identity lock amplifies their subsidy rather than their exposure. accident_investigation_industry are beneficiaries with arbitrage-grade mobility, sitting nearest the beneficiary end. hro_executive_leadership are dual-positioned (bearer with a secondary collecting interest), placing them mid-range. junior_operators are bearers with constrained exit — high directionality toward target. future_catastrophe_victims are bearers who are powerless and trapped: they sit nearest the full-target end, and the engine's amplification for trapped targets applies with full force, which is why effective extraction concentrates on a population with no seat in the arrangement's governance. Observers (regulators, researchers) hold analytical seats outside the chi arithmetic. No directionality overrides were needed: the derivation from declared roles plus exit options reproduces these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents symmetric mislabeling. Calling this a rope (pure coordination) would hide the tuition structure: the coordination good — a shared, hard-to-fake evidentiary standard for competence — is real, but its renewal cost is externalized onto catastrophe victims. Calling it a snare would erase the genuine epistemic content the standard coordinates around: the transfer-of-training literature confirms a real rehearsal-reality gap, so the standard is not pure cover. The mandatrophy question is sharpened by the R5 interview: the founding problem (classroom-trained crews failing in ways no curriculum anticipated) is contested rather than dead — the gap exists but its magnitude, and whether it justifies waiting for catastrophes, is exactly what the sibling readings dispute. The founding_problem_status=contested combined with disappearance_verdict=world_rearranges flags the characteristic hybrid condition: the arrangement persists on the strength of a problem it can only solve by the events it exists to prevent. The expected structural delta — no viable beneficiary structure — is honored as an open question (omega beneficiary_structure_self_consumption) rather than asserted: collectors exist and collect, but every renewal event damages their hosts, making the collection plausibly self-consuming across cycles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_gate,
    'Is the rehearsal-to-reality gap a hard epistemic limit that no simulation can close, or a gatekeeping norm whose persistence serves the authority of incident-experienced practitioners?',
    'Adversarial collaboration comparing transfer-of-training outcomes from maximal-fidelity simulation cohorts against incident-exposed cohorts on matched high-stakes decision tasks, with blind scoring.',
    'A closable gap collapses the necessity premise and pushes the arrangement toward pure extraction riding a dying coordination story; a demonstrably unclosable residue legitimizes part of the gate as irreducible coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_gate, empirical, 'Whether the constraint''s core premise is natural limit or constructed gate.').

omega_variable(
    kernel_reading_contestation,
    'This constraint is the real_incident_necessity reading of the competence_occupation kernel; what would the sibling readings change structurally if adopted?',
    'Counterfactual structural analysis: under simulation_sufficiency, the casualty-financed tuition becomes pure waste and the veteran legitimacy economy loses its basis; under hybrid_occupation, the gate dissolves into a weighting dispute among mechanisms.',
    'Adopting simulation_sufficiency trends the arrangement toward a scaffold around validated simulation standards; adopting hybrid_occupation redistributes extraction across mechanisms and lowers the concentration measured here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: sibling readings of the same kernel would instantiate different constraints with different victim sets.').

omega_variable(
    competence_maintenance_deadlock,
    'If the reading is correct, competence decays between incidents and can only be restored by an unacceptable event — do organizations rationally carry background risk as tuition, and at what accepted rate?',
    'Actuarial comparison of observed incident intervals against measured skill-decay curves, combined with organizational decision records on explicit risk acceptance.',
    'A finding of rational, priced tuition stabilizes the hybrid coordination/extraction reading; refusal to price the tuition exposes the arrangement as unmaintainable on its own premises and drives migration toward sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_maintenance_deadlock, conceptual, 'Whether the arrangement''s implied risk-carrying is a coherent policy or an unresolved deadlock.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the discounting of drill-based competence carried by formal machinery (certification criteria, budget lines, board composition) or by internalized practitioner belief that drills do not count?',
    'Post-reform trajectory analysis: in jurisdictions that re-weight certification toward validated simulation, observe whether drill-discounting persists among veteran assessors after the rules change.',
    'If internalized, effective suppression exceeds the structural measure and predicts slow decay of the arrangement even under formal reform; if structural, rule changes should move it quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of the suppression of simulation alternatives.').

omega_variable(
    beneficiary_structure_self_consumption,
    'Can any seat sustainably collect from an arrangement whose renewal events — catastrophes — damage the collectors'' own host organizations, licenses, and client base?',
    'Longitudinal tracking of expert-authority careers and investigation-industry revenues across multiple incident cycles, testing whether collections survive the events that renew them.',
    'Confirmation of self-consuming collection supports the expected structural delta of no viable beneficiary structure and pushes the between-crisis phases toward inertial persistence; identification of a stable collector (for example insurers repricing on incident data) sustains the current reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_self_consumption, empirical, 'Viability of the beneficiary structure given that catastrophes are unacceptable to the collecting parties themselves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__real_incident_necessity, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__real_incident_necessity, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__real_incident_necessity, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__real_incident_necessity, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__real_incident_necessity, theater_ratio, 25, 0.47).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__real_incident_necessity, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t35, competence_occupation__real_incident_necessity, theater_ratio, 35, 0.36).
narrative_ontology:measurement_basis(comp_tr_t35, observed).
narrative_ontology:measurement(comp_tr_t40, competence_occupation__real_incident_necessity, theater_ratio, 40, 0.43).
narrative_ontology:measurement_basis(comp_tr_t40, observed).
narrative_ontology:measurement(comp_tr_t45, competence_occupation__real_incident_necessity, theater_ratio, 45, 0.48).
narrative_ontology:measurement_basis(comp_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_occupation__real_incident_necessity, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_occupation__real_incident_necessity, base_extractiveness, 10, 0.72).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_occupation__real_incident_necessity, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_occupation__real_incident_necessity, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t25, competence_occupation__real_incident_necessity, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t30, competence_occupation__real_incident_necessity, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t35, competence_occupation__real_incident_necessity, base_extractiveness, 35, 0.74).
narrative_ontology:measurement_basis(comp_be_t35, observed).
narrative_ontology:measurement(comp_be_t40, competence_occupation__real_incident_necessity, base_extractiveness, 40, 0.7).
narrative_ontology:measurement_basis(comp_be_t40, observed).
narrative_ontology:measurement(comp_be_t45, competence_occupation__real_incident_necessity, base_extractiveness, 45, 0.72).
narrative_ontology:measurement_basis(comp_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_occupation__real_incident_necessity, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_occupation__real_incident_necessity, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_occupation__real_incident_necessity, suppression_requirement, 15, 0.63).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_occupation__real_incident_necessity, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t25, competence_occupation__real_incident_necessity, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t30, competence_occupation__real_incident_necessity, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t35, competence_occupation__real_incident_necessity, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(comp_su_t35, observed).
narrative_ontology:measurement(comp_su_t40, competence_occupation__real_incident_necessity, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(comp_su_t40, observed).
narrative_ontology:measurement(comp_su_t45, competence_occupation__real_incident_necessity, suppression_requirement, 45, 0.74).
narrative_ontology:measurement_basis(comp_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, information_standard).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'HRO competence maintenance' decomposes per the epsilon-invariance principle into three readings of the competence_occupation kernel, each with its own epsilon, beneficiaries, and victims. simulation_sufficiency is the upstream technological claim (highest empirical confidence in simulation transfer research); real_incident_necessity (this file) treats the rehearsal-reality gap as decisive and cites incident after-action records as its evidence base; hybrid_occupation mediates as the pragmatic umbrella. This story links to both siblings via affects_constraints; contamination propagates across the family because a validated simulation-fidelity breakthrough degrades this constraint's premise directly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
