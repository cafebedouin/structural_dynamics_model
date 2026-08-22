% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: Simulator-Cycle Proficiency Adequacy Regime (Reading: Simulation as Adequate Exercise)
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   Since the 1980s, full-flight simulators with structured debriefing
 *   displaced live-aircraft maneuver training as the basis of recurrent
 *   proficiency in commercial aviation and adjacent high-reliability domains.
 *   What began as a solution to a genuine problem — rehearsing unrecoverable
 *   scenarios without killing trainees — hardened into a certification regime
 *   in which completed simulator cycles became the accepted, then the
 *   required, then effectively the sole currency of demonstrated competence.
 *   Zero-flight-time type ratings, simulator-based recency, and audit
 *   frameworks made the reading self-executing: carriers schedule the
 *   mandated cycles, regulators audit the records, insurers price off the
 *   certificates. Catastrophe-free decades are cited as validation, although
 *   outcome silence cannot distinguish simulation adequacy from confounding
 *   safety layers or low base rates. This file instantiates ONE reading of
 *   the competence_exercise_requirement kernel —
 *   simulation_as_adequate_exercise — as a clean, epsilon-invariant
 *   constraint; the catastrophe-anchor and hybrid readings are separate
 *   stories linked through network.affects_constraints. Claim and metrics are
 *   authored independently: the constraint is CLAIMED as tangled_rope
 *   (genuine rehearsal coordination fused with asymmetric risk transfer and
 *   crowding-out of alternatives), and the metrics describe its actual
 *   operation without being tuned to that claim.
 *
 * KEY AGENTS:
 *   - - aviation_regulators: Agenda-setter (institutional/constrained) — writes proficiency rules, accepts simulator records as competence evidence, collects administrative economy
 *   - - airline_operators: Primary beneficiary (powerful/arbitrage) — captures training-cost avoidance under the adequacy warrant; forum-shops across jurisdictions
 *   - - simulation_training_industry: Concentrated beneficiary (organized/mobile) — mandated cycles are its captive recurring revenue base
 *   - - aviation_insurers: Secondary beneficiary (powerful/mobile) — prices unobservable competence risk off auditable cycle completion
 *   - - line_crews: Payer with incidental benefit (moderate/constrained) — lost real-aircraft anchoring; carries residual blame when gaps manifest; credentialed entirely through the system
 *   - - flying_public: Primary target (powerless/trapped) — bears residual rare-event risk with no seat in rulemaking
 *   - - accident_investigation_bodies: Analytical observer (institutional/analytical) — sees the structure only through post-event reconstruction
 *   - - passenger_representation_bodies: Excluded voice (powerless/trapped) — objects episodically, resourced too thinly to sit in the working groups
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.65).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.6).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.65).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "Simulator-Cycle Proficiency Adequacy Regime (Reading: Simulation as Adequate Exercise)").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, '710e832b-cca2-4f9e-bcac-403be8a39942').
narrative_ontology:cs_kernel_codification('710e832b-cca2-4f9e-bcac-403be8a39942', formalized).
narrative_ontology:cs_authority_grounding('710e832b-cca2-4f9e-bcac-403be8a39942', extraction).
narrative_ontology:cs_interpretation_layer_present('710e832b-cca2-4f9e-bcac-403be8a39942').
narrative_ontology:cs_reading_relation('710e832b-cca2-4f9e-bcac-403be8a39942', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('710e832b-cca2-4f9e-bcac-403be8a39942', competence_exercise_requirement__hybrid_dependency, forecloses).
narrative_ontology:cs_axiom('710e832b-cca2-4f9e-bcac-403be8a39942', foundational, scheduled_simulation_sufficiency).
narrative_ontology:cs_axiom_status(scheduled_simulation_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('710e832b-cca2-4f9e-bcac-403be8a39942', scheduled_simulation_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('710e832b-cca2-4f9e-bcac-403be8a39942', secondary, audited_cycles_equate_demonstrated_competence).
narrative_ontology:cs_axiom_status(audited_cycles_equate_demonstrated_competence, holdable).
narrative_ontology:cs_axiom_grounding('710e832b-cca2-4f9e-bcac-403be8a39942', audited_cycles_equate_demonstrated_competence, conventional).
narrative_ontology:cs_reference_frame('710e832b-cca2-4f9e-bcac-403be8a39942', validated_simulation_proficiency_standard).
narrative_ontology:cs_drift_state('710e832b-cca2-4f9e-bcac-403be8a39942', post_upset_accident_inquiry_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('710e832b-cca2-4f9e-bcac-403be8a39942', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, aviation_regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_training_industry).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, aviation_insurers).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, flying_public).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, line_crews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, line_crews).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_transfer_validity_hypothesis).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_compliance_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces the proficiency rules that define what counts as exercised competence. Accepts completed simulator cycles with documented debriefs as satisfying recency-of-experience and type-rating requirements, which replaces the far harder task of judging live operational competence with auditing records. Collects administrative economy and political cover from the arrangement; faces industry pressure on training costs and public pressure after visible accidents; cannot relinquish its certifying role without collapsing the regime it administers.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, aviation_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Schedules the minimum mandated simulator cycles and no more. Money that an older regime spent on real-aircraft handling time, non-jeopardy line audits, and live operational anchoring stays in the training budget or returns to operations. Multi-jurisdictional carriers can shift fleets and registries toward the least demanding proficiency rules, so the floor of the regime is set by the most permissive regulator willing to host them. Bears reputational and liability exposure when a rare event finds a skill gap the simulator never trained.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators, beneficiary,
    powerful, biographical, arbitrage, global).

% Manufactures full-flight simulators costing tens of millions of dollars apiece and operates training centers selling recurrent cycle hours. Mandated simulator-based proficiency converts a discretionary purchase into captive recurring demand across an entire industry. Revenue depends on simulator time remaining the certification currency; the industry promotes device-fidelity credits and expanded approved-device lists that enlarge billable device time, and participates in the working groups where proficiency standards are drafted.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_training_industry, beneficiary,
    organized, biographical, mobile, global).

% Prices hull and liability risk using auditable simulator-compliance records, because the underlying hazard — actual crew competence under surprise — is unobservable to them. The reading hands them a quantifiable proxy: completed cycles and current certificates discount premiums, lapsed ones raise them. Underwriting practice thereby reinforces the regime, rewarding carriers for cycle completion regardless of what the cycles produced.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, aviation_insurers, beneficiary,
    powerful, biographical, mobile, global).

% Fly the recurrent simulator profiles and sign the debrief records. They gain safe rehearsal of emergencies that could never be practiced live, and the simulator is where most first learn engine-out procedures and upset recovery. What they lost relative to earlier regimes is real-aircraft handling time and non-jeopardy line exposure — the slow accumulation of manual skill and composure under genuine surprise. When a rare event finds the gap, legal and moral responsibility lands on the crew, not on the standard that defined their preparation. Their type ratings, recency, and professional standing all run through the simulator system, so questioning its adequacy implicates their own credentials.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, line_crews, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, line_crews, beneficiary).

% Boards aircraft on the assumption that crew competence is being actively maintained to some verified standard. Bears whatever residual risk follows from skills that simulation undertrains — startle response, surprise, genuine ambiguity — concentrated in the rare moments that decide outcomes. Has no seat in the working groups where proficiency standards are drafted, no practical way to compare carriers on real competence rather than certificate currency, and no exit from the regime short of not flying.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, flying_public, payer,
    powerless, immediate, trapped, global).

% Reconstructs rare events after the fact and sees what the routine record cannot: whether crews performed outside the trained envelope, how manual handling degraded, how startle shaped the first seconds. Publishes recommendations — repeated calls for restored manual-flying emphasis after high-profile upsets — that press directly on the adequacy premise but bind no one until rulemaking chooses to act. Observes the structure only through its failures.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, accident_investigation_bodies, observer,
    institutional, generational, analytical, continental).

% Consumer and passenger-interest groups occasionally petition rulemaking dockets for stronger evidence that proficiency standards reflect operational reality rather than device time. Rarely resourced to sustain technical participation across multi-year working-group processes dominated by regulators, operators, and the training industry. Their objections tend to surface publicly only after accidents, when the standard under discussion has already been in force for a decade.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, passenger_representation_bodies, excluded,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_training_industry).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a safe, standardized, repeatable environment in which crews rehearse catastrophic and abnormal scenarios that cannot be practiced in live operations without unacceptable risk, and synchronizes crew responses to shared emergency profiles across a fleet and an industry.
% TRANSFER_FUNCTION: Moves training-budget allocation toward simulator cycle hours and away from real-aircraft anchoring programs; moves residual rare-event risk onto crews' professional standing and the flying public's persons; moves certification certainty to regulators, premium-setting proxies to insurers, and guaranteed recurring revenue to the training industry.
% ABSENT_VOICES: Passengers have no seat in proficiency-standard working groups; passenger representation appears in dockets episodically and under-resourced. Front-line instructors who observe manual-skill decay firsthand report through management chains that filter dissent before it reaches standards committees. Accident investigators speak only after events, when the standard's adequacy has already been tested at public expense.
% DISAPPEARANCE_RATIONALE: If the rule that simulator cycles constitute adequate exercise vanished overnight, certification schedules, training budgets, the training industry's revenue base, and insurance pricing would all reorganize within a planning cycle; carriers would need new evidence of proficiency, real-aircraft anchoring programs would reappear as competitive and regulatory demands, and the meaning of a current certificate would change.
% FOUNDING_PROBLEM: Early jet-era training rehearsed emergencies in live aircraft and learned from accidents: crews died acquiring experience, and unrecoverable scenarios could not be practiced at all. The founding problem was how to rehearse the unrehearsable without killing trainees. High-fidelity simulation with structured debriefing answered it decisively.
% FOUNDING_PROBLEM_CORROBORATION: The founding rehearsal problem is corroborated from outside the benefiting parties by the accident-investigation record itself — the historical sequence of training accidents and the continuing occurrence of scenarios no crew had ever practiced. The extension of the founding problem into a sufficiency claim (that simulation exhausts what competence maintenance requires) is attested almost exclusively from within the benefiting set — regulators, operators, the training industry, and insurer underwriting practice — and no independent body outside that set attests the sufficiency extension; stating that plainly is itself the signal.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.65, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.65: substantial but bounded by the enormous genuine coordination value — no live alternative rehearses unrecoverable events, and the regime did eliminate a class of training deaths. The extraction that remains is the transfer of residual risk from standard-setters to the public and the crowding-out of anchoring practices that would cost operators real money. Suppression is 0.60 and is authored as a raw structural property, unscaled — only extractiveness is scaled by directionality and scope in the engine's computation. Suppression here is regulatory lock-in and budget crowding-out rather than coercive force: once cycle completion is the compliance currency, funding real-aircraft anchoring buys no credit and much criticism. Theater ratio is 0.42 and rising: a growing share of cycle content rehearses predictable check profiles taught to the evaluation rather than the unscripted events that decide outcomes. Accessibility collapse is 0.40 — hybrid alternatives survive at some carriers and in some jurisdictions, but budget pressure collapses them wherever the compliance floor binds. Resistance is 0.55: sustained safety-science critique, union manual-handling campaigns after high-profile upsets, and investigator recommendations, which extracted partial concessions (mandatory upset-prevention-and-recovery training) without displacing the adequacy premise. All three temporal series run on one shared grid (t=0,7,15,22,30,38,45) so every metric is authored at every examined point; the suppression_requirement series is authored deliberately because the story tracks an enforcement ratchet — permissive voluntary adoption, codification of simulator-only recency and zero-flight-time ratings, then audit-hardened compliance machinery — not a static enforcement picture.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and operator seats the arrangement computes as a mature, validated, administrable system: records exist, cycles complete, decades pass without catastrophe, and the machine runs. From the crew and public seats the same structure computes as unverifiable residual risk imposed without consent — the precise hazard (performance under genuine surprise) is the one thing the certification currency never measures. The same nominal institution splits internally: training departments are identity-fused with the simulator regime (their authority, headcount, and budget are the regime), while line operations feel the decay the regime cannot see. Investigators occupy a fourth position — they observe only the tail of the distribution, so every datum they hold is a counterexample the routine record lacks. The engine computes these divergent per-seat classifications from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for aviation_regulators, airline_operators, simulation_training_industry, and aviation_insurers — each collects from the arrangement (administrative economy, cost avoidance, captive demand, underwriting proxies) and none bears its residual risk; the operators' cross-jurisdictional arbitrage pushes them toward the beneficiary pole. Victim declarations drive high directionality for flying_public — powerless, trapped, global scope, bearing the exact risk the regime externalizes — and for line_crews, who are dual-positioned (payer with secondary beneficiary standing) and therefore derive mid-to-high directionality rather than a pole. Scope amplifies: the arrangement operates globally while verification of actual competence occurs nowhere, so the engine scales effective extraction upward for the trapped target seats. No directionality overrides are needed; the beneficiary/victim declarations plus exit options already produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rehearse the unrehearsable without killing trainees — remains live, and the arrangement genuinely solves it; the mandate grew beyond it, extending from rehearsal to a sufficiency claim over all of competence maintenance. The tangled_rope classification prevents two opposite mislabels. Reading the arrangement as pure extraction erases the irreplaceable coordination function: no live alternative lets a crew practice a rejected takeoff at V1 or a dual-engine flameout, and abolishing the regime would kill trainees. Reading it as pure coordination erases the asymmetry: gains concentrate (training budgets, device revenue, underwriting proxies, administrative economy) while risks diffuse (public bodies, crew careers), and the alternatives that would test the sufficiency claim are crowded out by the same rules that certify the currency. The mandatrophy watch-point is forward-looking: if a validated hybrid standard ever displaced the sufficiency premise, the simulator-only mandate would atrophy into inertia — theatrical cycles persisting after the function had moved — which is the trajectory the theater_ratio series is positioned to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the competence_exercise_requirement kernel: does the simulation_as_adequate_exercise instantiation, rather than the catastrophe_as_necessary_anchor or hybrid_dependency readings, correctly describe what maintains the competence kernel of high-reliability operations?',
    'Comparative analysis across jurisdictions and carriers operating under different readings: if carriers maintaining periodic real-world anchoring show materially lower rare-event failure and excursion rates than sim-only peers under matched exposure, the sufficiency premise of this reading fails.',
    'If a sibling reading is adopted, the victim set expands (crews and public gain standing to demand anchoring), the training industry''s captive demand contracts, and this arrangement migrates from its current hybrid shape toward a transitional support awaiting replacement or toward inertial persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest over which exercise regime constitutes adequate competence maintenance; disagreement located on the sufficiency predicate over simulation.').

omega_variable(
    residual_risk_attribution,
    'Are catastrophe-free decades evidence of simulation adequacy, or the product of confounding safety layers (automation depth, fleet renewal, weather detection, traffic control) plus base rates so low that decades of outcome silence are statistically uninformative?',
    'Leading-indicator analysis (near-miss rates, unstable-approach excursions, manual-handling deviations) instead of hull-loss counts; matched comparison of carriers differing in anchoring practice.',
    'If attribution fails, the reading''s validation loop is self-sealing and effective extraction rises, since the public bears risk under a false warrant; if attribution holds, part of the measured burden is mispriced and epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_risk_attribution, empirical, 'Whether outcome silence validates the reading or masks confounded attribution.').

omega_variable(
    startle_surprise_gap_materiality,
    'How large is the competence gap between simulator-trained performance and performance under genuine startle and surprise, and is that gap material to rare-event outcomes?',
    'Startle-effect research programs, line-operational flight-training event recordings, observational line audits flown under waiver, and post-incident crew-performance reconstruction.',
    'A material gap directly undermines the sufficiency premise and shifts the operative structure toward the hybrid reading''s shape; a negligible gap supports the reading and lowers suppression, since alternatives would be genuinely unnecessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(startle_surprise_gap_materiality, empirical, 'Materiality of the startle/surprise deficit that simulation cannot reproduce.').

omega_variable(
    functional_vs_performative_cycle_share,
    'What fraction of mandated simulator-cycle content maintains genuine rare-event competence versus rehearses predictable check profiles taught to the evaluation?',
    'Audit of scenario variance across cycles and carriers; correlation of profile-rehearsal share with check-pass rates and with performance during unscheduled operational events.',
    'A high performative share pushes theater_ratio further upward and strengthens the inertial-drift hypothesis; a low share supports the coordination-function reading and dampens extraction estimates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_performative_cycle_share, empirical, 'Split of simulator-cycle activity between functional rehearsal and compliance performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.14).
narrative_ontology:measurement(comp_tr_t7, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 7, 0.17).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 15, 0.21).
narrative_ontology:measurement(comp_tr_t22, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 22, 0.26).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 30, 0.31).
narrative_ontology:measurement(comp_tr_t38, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 38, 0.37).
narrative_ontology:measurement(comp_tr_t45, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 45, 0.42).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(comp_be_t7, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 7, 0.39).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(comp_be_t22, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 22, 0.51).
narrative_ontology:measurement(comp_be_t30, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(comp_be_t38, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 38, 0.61).
narrative_ontology:measurement(comp_be_t45, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 45, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(comp_su_t7, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 7, 0.34).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(comp_su_t22, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 22, 0.48).
narrative_ontology:measurement(comp_su_t30, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(comp_su_t38, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 38, 0.57).
narrative_ontology:measurement(comp_su_t45, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 45, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial question 'what keeps high-reliability crews competent?' covers three structurally distinct claims with different epsilon values, different beneficiary sets, and different failure modes, and is therefore modeled as three stories linked by network edges rather than one story with a measurement parameter. This story (simulation_as_adequate_exercise) authors epsilon for the standing arrangement in which simulator cycles are the certification currency. The catastrophe-anchor sibling authors epsilon for an arrangement in which only real events exercise the kernel; the hybrid sibling authors epsilon for a mixed regime. Historical influence runs from the catastrophe-anchor reading (the accident-driven origin of all modern training doctrine) toward this reading, whose adoption was justified partly by citing the impossibility of the anchor it replaced; the hybrid reading emerged as the synthesis position and draws evidentiary support from this reading's accumulating counterexamples. Each story links to the others; none contains the contest inside its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
