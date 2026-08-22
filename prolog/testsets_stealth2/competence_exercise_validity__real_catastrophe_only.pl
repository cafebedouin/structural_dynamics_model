% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real-Catastrophe-Only Competence Validation Norm
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In safety-critical professions — firefighting, emergency medicine,
 *   aviation, nuclear operations, military command — a standing norm holds
 *   that competence is genuinely exercised and validated only by real
 *   catastrophe; simulation, however elaborate, is an insufficient
 *   substitute. The norm governs promotion, exercise budgets, and status:
 *   operators with real-event pedigrees outrank simulator-validated peers,
 *   organizations bank legitimacy from incident archives, and long
 *   incident-free stretches function as untested blind spots rather than as
 *   evidence of adequacy. The arrangement solves a real epistemic problem —
 *   untested readiness claims have repeatedly failed under real conditions —
 *   while charging its tuition to those least positioned to refuse: the
 *   publics on whom untested competence is exercised, and the junior
 *   operators whose first real test arrives unscheduled and at full scale.
 *   KEY AGENTS (by structural relationship):
 *   veteran_operators_with_incident_pedigree (primary beneficiary,
 *   organized/identity_locked); incumbent_operating_organizations (agenda
 *   setter and secondary beneficiary, institutional/arbitrage);
 *   junior_frontline_operators (primary target, powerless/constrained);
 *   untested_service_populations (primary target, powerless/trapped);
 *   simulation_training_programs (excluded challenger, moderate/mobile);
 *   regulatory_safety_bodies (secondary enforcer, institutional/constrained);
 *   safety_science_researchers (analytical observer). This file instantiates
 *   one reading of a contested kernel; see kernel_context for the committer
 *   structure.
 *
 * KEY AGENTS:
 *   - veteran_operators_with_incident_pedigree: primary beneficiary (organized/identity_locked) — converts catastrophe presence into authority, pay premiums, and mentoring slots
 *   - incumbent_operating_organizations: agenda setter with secondary beneficiary position (institutional/arbitrage) — sets validation policy, absorbs real-event costs, banks legitimacy from incident archives
 *   - junior_frontline_operators: primary target (powerless/constrained) — advances only through real-event exposure; first genuine test arrives unscheduled
 *   - untested_service_populations: primary target (powerless/trapped) — patients, passengers, residents; supply the difference between a drill and a catastrophe
 *   - simulation_training_programs: excluded challenger (moderate/mobile) — demonstrates measurable transfer but is barred from validation authority
 *   - regulatory_safety_bodies: secondary enforcer (institutional/constrained) — mandates exercise calendars but not the conclusions drawn from them
 *   - safety_science_researchers: analytical observer (analytical/analytical) — maps what drills deliver and where decay hides between real events
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.62).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.58).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real-Catastrophe-Only Competence Validation Norm").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, '384ae4f9-8b04-4bb2-a60f-9bddb7061c78').
narrative_ontology:cs_kernel_codification('384ae4f9-8b04-4bb2-a60f-9bddb7061c78', distributed).
narrative_ontology:cs_authority_grounding('384ae4f9-8b04-4bb2-a60f-9bddb7061c78', practice).
narrative_ontology:cs_interpretation_layer_present('384ae4f9-8b04-4bb2-a60f-9bddb7061c78').
narrative_ontology:cs_reading_relation('384ae4f9-8b04-4bb2-a60f-9bddb7061c78', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('384ae4f9-8b04-4bb2-a60f-9bddb7061c78', competence_exercise_validity__continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_axiom('384ae4f9-8b04-4bb2-a60f-9bddb7061c78', foundational, real_catastrophe_sole_valid_exercise).
narrative_ontology:cs_axiom_status(real_catastrophe_sole_valid_exercise, holdable).
narrative_ontology:cs_axiom_grounding('384ae4f9-8b04-4bb2-a60f-9bddb7061c78', real_catastrophe_sole_valid_exercise, empirically_contingent).
narrative_ontology:cs_axiom('384ae4f9-8b04-4bb2-a60f-9bddb7061c78', secondary, simulation_confidence_hazard).
narrative_ontology:cs_axiom_status(simulation_confidence_hazard, holdable).
narrative_ontology:cs_axiom_grounding('384ae4f9-8b04-4bb2-a60f-9bddb7061c78', simulation_confidence_hazard, empirically_contingent).
narrative_ontology:cs_reference_frame('384ae4f9-8b04-4bb2-a60f-9bddb7061c78', catastrophe_validated_competence).
narrative_ontology:cs_drift_state('384ae4f9-8b04-4bb2-a60f-9bddb7061c78', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('384ae4f9-8b04-4bb2-a60f-9bddb7061c78', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, veteran_operators_with_incident_pedigree).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, incumbent_operating_organizations).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, untested_service_populations).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, junior_frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, simulation_training_programs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior operators whose authority, pay premiums, and mentoring roles rest on having been present at real fires, crashes, meltdowns, or mass-casualty events. Each new real incident adds to their stock of testimonial capital; a colleague validated only in simulators remains, in their eyes, permanently provisional. Leaving the profession forfeits the credential entirely, and their professional self-account is built around the events they survived.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, veteran_operators_with_incident_pedigree, beneficiary,
    organized, biographical, identity_locked, national).

% Hospitals, fire services, airlines, utilities, and military units that set training policy, promotion criteria, and exercise budgets. Their institutional legitimacy leans on crews with real-event pedigrees, and their incident archives function as proof of seriousness. They also absorb the direct costs of every real event — casualties, litigation, equipment loss, regulatory sanction — and can shift jurisdiction or restructure to escape particular mandates.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, incumbent_operating_organizations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, incumbent_operating_organizations, beneficiary).

% New nurses, rookie firefighters, junior officers, and first-year residents who advance only by accumulating real-event exposure. Until a real event finds them, their readiness is treated as unproven regardless of drill hours; their first genuine test frequently arrives unscheduled, at full scale, with lives at stake. Transferring to a quieter unit slows advancement rather than protecting them.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, junior_frontline_operators, payer,
    powerless, biographical, constrained, national).

% Patients, passengers, plant neighbors, and the public at large, on whom untested competence is exercised whenever real conditions arrive before validation does. They cannot opt out of hospitals, power grids, or emergency response, and they hold no seat in the forums where validation standards are set; they supply the difference between a drill and a catastrophe.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, untested_service_populations, payer,
    powerless, immediate, trapped, regional).

% Simulator centers, mannequin labs, virtual-reality training vendors, and the instructors who run them. They can demonstrate measurable skill transfer, yet the prevailing standard classes their output as categorically short of real exercise, capping their budgets and barring them from certification authority. Some relocate to industries further along the simulation curve; most operate where they can.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_training_programs, excluded,
    moderate, biographical, mobile, global).

% Agencies that mandate minimum drill hours, simulator certifications, and exercise frequencies. Their instruments reach the calendar but not the culture: they can require a drill to occur, not what the organization concludes from it. Mandate changes move slowly and attract industry lobbying in every jurisdiction they oversee.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, regulatory_safety_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Investigators of transfer-of-training, high-reliability organizing, and simulation fidelity who publish on what drills do and do not deliver. They see the full structure — who funds exercises, whose experience is counted, where decay hides between real events — and hold no operational authority over validation standards.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, safety_science_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__real_catastrophe_only, veteran_operators_with_incident_pedigree).
narrative_ontology:fixing_cost_class(competence_exercise_validity__real_catastrophe_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates epistemic skepticism about readiness claims across a safety-critical workforce: no member is treated as proven until they have performed under real, unrehearsed, consequential conditions, which protects organizations against mistaking drill-optimized performance for field capability.
% TRANSFER_FUNCTION: Moves the cost of competence validation onto whoever is present when real conditions arrive — patients, passengers, residents, junior crew — and moves authority, pay premiums, and mentoring slots toward those who have survived real events.
% ABSENT_VOICES: Simulation training programs and training scientists hold evidence on what drills deliver but no vote on what counts as validation; past victims of untested competence are absent from the promotion boards and standards committees where the 'real exercise' bar is set; junior operators attend the standard as its subjects, not its authors.
% DISAPPEARANCE_RATIONALE: If the norm vanished overnight, promotion criteria and exercise budgets would re-anchor on demonstrated simulated performance, simulator investment would surge, the scarcity premium on incident-pedigree credentials would collapse, and the long untested intervals between real events would stop functioning as blind spots — the credential economy built around catastrophe presence would dissolve within a promotion cycle or two.
% FOUNDING_PROBLEM: Repeated early-industrial and wartime failures showed that credentialed, peacetime-routine organizations collapsed under real emergency conditions; the norm was built to stop untested readiness from being mistaken for real capability.
% FOUNDING_PROBLEM_CORROBORATION: Accident-investigation reports and transfer-of-training research corroborate the founding problem — crews with strong drill records have failed under real conditions — from outside the beneficiary cohort; the stronger claim that ONLY catastrophe suffices is attested almost exclusively by catastrophe-pedigree holders themselves, whose authority the claim sustains.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 because the norm prices competence validation in real harm: the gap between a validated operator and an untested one is closed only by exposing someone to the real event, and preventive investment in simulation is systematically discounted. Suppression is 0.58 — enforcement is cultural and procedural (promotion gates keyed to incident exposure, budget caps on simulator time, ridicule of 'checklist' operators) rather than statutory, but it materially blocks the alternative. Theater ratio is 0.34: war-story ritual and after-action performance are real and growing, yet the norm still gates concrete decisions, so it is not yet mostly performance. Accessibility collapse is low (0.38) because alternatives visibly persist — aviation's simulator-first regime demonstrates another equilibrium exists — so understanding the norm does not foreclose exiting it. Resistance is 0.55: sustained pushback from training science, simulation vendors, and patient-safety movements. The claim/metric split is deliberate: I claim tangled_rope because the norm holds BOTH a genuine coordination function (epistemic discipline against untested readiness claims) AND asymmetric extraction (catastrophe tuition charged to the trapped, credential rents collected by the pedigreed), maintained by active enforcement. The measurement series run on one shared time grid (all three metrics at every point 0–30, roughly the decades of high-fidelity simulation maturation); all three rise together, tracing the coordination share shrinking as the credential-defense share grows. Suppression_requirement rises because dismissing progressively more capable simulation takes increasing rhetorical and procedural work — the trajectory models enforcement hardening, not decay.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the veteran seat the norm is epistemic honesty: 'we know what works because we have bled for it,' and simulation-validated peers are presumptively unproven. From the untested-population seat the same norm is conscription: they are the instrument that distinguishes drill from reality, without a seat where the bar is set. From the junior-operator seat it is a lottery: advancement depends on a real event finding them before retirement does. From the simulation-program seat it is a closed market: provable transfer, no admission to the credential. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for veteran operators and incumbent organizations; victim declarations drive high directionality for untested service populations (trapped, powerless — nearest the full-target end) and junior frontline operators (constrained — high but not maximal). Two overrides correct derivations that would miss real cost-bearing: veteran operators derive near the pure-beneficiary end from their beneficiary declaration alone, but they physically staff every real catastrophe and bear casualty risk in the very events that mint their credentials — overridden to 0.22. Incumbent organizations derive near-zero as agenda-setting beneficiaries, but they absorb liability, asset loss, and reputational damage from each real event — overridden to 0.35. Simulation training programs are excluded rather than coordinated: their exclusion from validation authority is the enforcement object itself, parallel to a barred competitor. Safety science researchers hold the analytical seat with no transactional directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents two opposite misreadings. Reading it as pure wisdom (rope) would hide the tuition structure — the fact that validation is financed by trapped third parties and that the credential economy pays its defenders. Reading it as pure extraction (snare) would erase the genuine epistemic discipline that has caught real overconfidence and the documented failure of drill-optimized organizations under real conditions. The founding problem — untested readiness failing under real load — is still live in weakened form, so declaring the mandate dead would be premature; but the shared-grid series shows theater and suppression rising together with extractiveness, the signature of a coordination norm converting into credential defense. If the fidelity-threshold omega resolves affirmatively, the residual norm becomes mostly performance and the type should migrate toward piton; the measurements give the drift detector the series it needs to date that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the real_catastrophe_only reading of the competence_exercise_validity kernel; how would classification shift under the sibling readings?',
    'Generate the sibling stories (simulation_as_proxy, continuous_refresh_hybrid) and compare computed types, victim sets, and drift states across the family.',
    'Under simulation_as_proxy the untested-population victim set vanishes and extraction relocates to decay-masked systems; under continuous_refresh_hybrid the single-catastrophe gate dissolves into recurring cycle costs. Cross-reading comparison, not this file alone, locates the kernel''s true extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    simulation_fidelity_threshold,
    'Is the premise that only real catastrophe exercises competence a fixed feature of human skill acquisition, or contingent on current simulation fidelity sitting below some transfer-sufficiency threshold?',
    'Fidelity-scaling studies and transfer-effect meta-analyses tracking whether measured transfer approaches real-event performance as simulator ecological validity increases.',
    'If a sufficiency threshold exists and is reachable, the norm''s epistemic core collapses into credential defense and drift toward pure extraction accelerates; if unreachable, the norm retains a genuine protective floor and part of the measured extraction is priced risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether the reading''s foundational premise is a natural limit or a moving technological frontier.').

omega_variable(
    credential_rent_vs_priced_risk,
    'How much of the norm''s persistence is credential-scarcity protection by catastrophe-pedigree holders versus honest pricing of simulation''s known limits?',
    'Natural experiment from industries that shifted to simulation-primary validation (e.g., the airline line-oriented flight training transition): track veteran authority and compensation premia and subsequent safety outcomes.',
    'If premia fell without safety degradation, the extraction component is rent and the norm classifies nearer pure extraction; if safety degraded, part of the measured extraction is the price of the discipline itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_rent_vs_priced_risk, empirical, 'Separating credential rent from legitimately priced epistemic risk.').

omega_variable(
    discount_internalization_split,
    'Is the discounting of simulated exercise structural (promotion gates, budget rules, certification requirements) or internalized (operators sincerely believing drills do not count)?',
    'Attitude and practice trajectories in workforces that experienced mandated simulation-parity reforms: if discounting persists after the formal gates fall, it is carried internally.',
    'If internalized, removing formal barriers will not restore simulation''s validation standing within a career generation, and effective suppression exceeds what the structural measure shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discount_internalization_split, empirical, 'Structural versus internalized mechanism behind simulation discounting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_validity__real_catastrophe_only, theater_ratio, 5, 0.21).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__real_catastrophe_only, theater_ratio, 10, 0.24).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__real_catastrophe_only, theater_ratio, 15, 0.27).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__real_catastrophe_only, theater_ratio, 20, 0.3).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_validity__real_catastrophe_only, theater_ratio, 25, 0.32).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_validity__real_catastrophe_only, theater_ratio, 30, 0.34).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(comp_be_t5, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 5, 0.49).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(comp_be_t25, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(comp_be_t30, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(comp_su_t5, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(comp_su_t25, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(comp_su_t30, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the competence_exercise_validity kernel: the colloquial label 'what validates operational competence' covers three structurally distinct arrangements with different epsilon values. This file authors the real_catastrophe_only reading (single-catastrophe gate; untested intervals as blind spots; credential economy on incident pedigrees). simulation_as_proxy authors the drill-as-proxy-catastrophe arrangement (recurring simulated exercise occupies competence; victims shift to decay-masked systems). continuous_refresh_hybrid authors the continuous-cycle arrangement (simulation necessary but not sufficient; retention via recurring cycles). The upstream reading (this file) is the established tradition whose skepticism the downstream readings must answer; each family member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__real_catastrophe_only, organized, 0.22).
constraint_indexing:directionality_override(competence_exercise_validity__real_catastrophe_only, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
