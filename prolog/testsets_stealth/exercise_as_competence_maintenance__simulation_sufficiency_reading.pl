% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__simulation_sufficiency_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Simulation-Sufficiency Reading of Exercise-Based Competence Maintenance
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   Across aviation, medicine, energy, and emergency management, regulators
 *   mandate recurrent simulated-catastrophe exercises, accredit the providers
 *   who run them, and accept simulator performance metrics as verification
 *   that crisis-response competence is maintained. This story instantiates
 *   ONE reading of the contested kernel exercise_as_competence_maintenance:
 *   the simulation_sufficiency_reading, which holds that simulated
 *   catastrophe constitutes genuine exercise of the competence kernel and
 *   that simulation fidelity determines retention effectiveness. Under this
 *   reading the standing arrangement — the mandate-as-sufficient regime — is
 *   the epsilon referent, assessed by this reading's own lights: drills
 *   really do exercise teams, and the harm the arrangement produces is
 *   concentrated where fidelity was inadequate. The sibling readings
 *   (lived_catastrophe_necessity_reading, hybrid_decay_reading) are separate
 *   constraints in separate files with their own epsilon values and victim
 *   sets; they are linked, not averaged, per the epsilon-invariance
 *   principle. Claim and metrics are authored independently: the claimed type
 *   reflects what I believe structurally true of the standing arrangement,
 *   and the metrics reflect what I believe descriptively true of its
 *   operation — where the engine's per-seat computations diverge from the
 *   claim, that divergence is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - - safety_regulators: Agenda setter (institutional/constrained) — writes the mandates, accredits providers, audits against simulator metrics
 *   - - simulation_training_vendors: Primary collector (organized/arbitrage) — converts mandated minimums directly into contracted revenue across industries
 *   - - corporate_risk_and_compliance_offices: Dual-positioned collector/payer (organized/identity_locked) — buys the exercises, files the compliance record, professionally fused to the metric regime
 *   - - frontline_response_teams: Compulsory participant paying time (moderate/constrained) — staffs the drills, acquires rehearsed procedure, cannot decline
 *   - - fidelity_gap_victims: Ultimate cost-bearers (powerless/trapped) — harmed in real events by conditions no exercise reproduced; enter only through investigation and litigation
 *   - - accident_investigators: Analytical observer (institutional/analytical) — reconstructs failures post hoc; the main external check on the fidelity question
 *   - - liability_insurers: Diffuse collector (institutional/arbitrage) — prices premiums on documented drill compliance without operating any exercise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.58).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.55).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Simulation-Sufficiency Reading of Exercise-Based Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, '09f3a8a4-d4bc-40e2-a4d2-eeabfd019255').
narrative_ontology:cs_kernel_codification('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', formalized).
narrative_ontology:cs_authority_grounding('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', expertise).
narrative_ontology:cs_interpretation_layer_present('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255').
narrative_ontology:cs_reading_relation('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', foundational, simulated_stress_genuinely_exercises_kernel).
narrative_ontology:cs_axiom_status(simulated_stress_genuinely_exercises_kernel, holdable).
narrative_ontology:cs_axiom_grounding('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', simulated_stress_genuinely_exercises_kernel, empirically_contingent).
narrative_ontology:cs_axiom('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', foundational, fidelity_determines_retention_effectiveness).
narrative_ontology:cs_axiom_status(fidelity_determines_retention_effectiveness, holdable).
narrative_ontology:cs_axiom_grounding('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', fidelity_determines_retention_effectiveness, empirically_contingent).
narrative_ontology:cs_axiom('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', secondary, mandated_drill_hours_discharge_preparedness_duty).
narrative_ontology:cs_axiom_status(mandated_drill_hours_discharge_preparedness_duty, holdable).
narrative_ontology:cs_axiom_grounding('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', mandated_drill_hours_discharge_preparedness_duty, conventional).
narrative_ontology:cs_reference_frame('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', simulator_metric_readiness_standard).
narrative_ontology:cs_drift_state('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('09f3a8a4-d4bc-40e2-a4d2-eeabfd019255', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_regulators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_training_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, corporate_risk_and_compliance_offices).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, liability_insurers).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, fidelity_gap_victims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_response_teams).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, corporate_risk_and_compliance_offices).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_response_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and revises the exercise mandates: required drill frequencies, simulator-hour minimums, scoring rubrics, and provider accreditation criteria. Publishes guidance interpreting what counts as a valid exercise, accredits the training industry that delivers it, and audits organizations against the metrics. Annual reporting to legislators consists largely of aggregate drill-compliance figures. Redesigning the mandate would mean reopening standards negotiated across industries and conceding that prior compliance did not guarantee readiness.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Builds and operates the simulators, writes the scenario libraries, scores the runs, and certifies the hours. Mandated minimums convert directly into contracted revenue, and each expansion of the mandate into a new sector opens a new market. Moves freely between industries — aviation, medicine, energy, emergency management — selling variants of the same product, and bears none of the compliance obligation itself.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_training_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Schedules the exercises, purchases the simulator contracts, and files the compliance records that demonstrate due diligence to boards, insurers, and courts. Professional standing rests on running a defensible program measured in completed exercises and passing scores; arguing that the metric regime inadequately measures readiness would undercut the very record the office exists to maintain. Training budgets flow out to vendors; liability cover and audit clearance flow back.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, corporate_risk_and_compliance_offices, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, corporate_risk_and_compliance_offices, payer).

% Staffs the drills: nurses, plant operators, flight crews, and incident commanders who leave operational duty for scheduled exercises, perform scripted scenarios under observation, and are scored. Participation is compulsory and recurrent. They acquire rehearsed procedures and team coordination they could not otherwise safely obtain, and they lose the equivalent hours from other work; declining participation means leaving the post, and they shape scenario content only indirectly through the offices that commission it.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_response_teams, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_response_teams, beneficiary).

% People injured or killed in real emergencies where the response failure traces to conditions no exercise reproduced — cascading equipment states, communication breakdown under genuine mortal stress, novel compound scenarios. They enter the arrangement only after harm, through investigation reports and litigation; before the event they had no seat in scenario design, mandate negotiation, or metric selection, and no way to know which scenario types had never been rehearsed.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, fidelity_gap_victims, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, fidelity_gap_victims, excluded).

% Reconstructs failed responses after the fact and publishes findings that repeatedly cite both the protective value of prior drilling and the specific gap between what was rehearsed and what occurred. Their reports are the principal external check on the fidelity question, though recommendations bind no one until separately adopted into the mandate.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, accident_investigators, observer,
    institutional, generational, analytical, national).

% Prices premiums and sets coverage terms keyed to documented exercise compliance. Completed drill records reduce assessed risk and justify rate structures; the insurer collects across entire portfolios without operating any exercise itself, and can reprice or withdraw from a line far faster than any regulated organization can restructure its training program.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, liability_insurers, beneficiary,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_training_vendors).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__simulation_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Lets many teams rehearse rare, dangerous scenarios safely and simultaneously: procedures, equipment handling, role coordination, and time-pressured decision sequences are practiced without waiting for real disasters, and standardized scores let a regulator verify preparedness across thousands of organizations it could never visit individually.
% TRANSFER_FUNCTION: Moves mandated training budgets from regulated organizations to simulator vendors and accredited providers; moves frontline working hours into scheduled exercises; moves compliance records upward to boards, insurers, and courts as liability cover; and leaves the residual risk of unrehearsed scenario types with whoever is present when one occurs.
% ABSENT_VOICES: Fidelity-gap victims and their survivors have no seat — they arrive only afterward, through investigators and plaintiffs. Independent training-transfer researchers sit outside the accreditation loop that funds and publishes most effectiveness evidence. Frontline teams influence scenario design only through the compliance offices whose metrics the scenarios are written to satisfy.
% DISAPPEARANCE_RATIONALE: If the mandate regime vanished overnight, the vendor industry would lose its demand floor, accreditation and audit architectures would void, insurers would have to rebuild risk models not keyed to drill records, and every organization's preparedness program would lose its compliance backbone. Some voluntary training would survive, but the entire verification economy built around simulator metrics would reorganize within a planning cycle.
% FOUNDING_PROBLEM: Real catastrophe was the only teacher: the events that test crisis response are rare, lethal, and expensive, organizations could not rehearse them without causing them, and regulators had no way to verify readiness except waiting for accidents. Simulation promised rehearsal without the catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards corroborate the founding problem from outside the benefiting parties: their reports repeatedly credit prior drilling with saved lives while documenting the specific unrehearsed conditions behind each failure. Peer-reviewed transfer-of-training research and survivor litigation filings attest independently. Notably, the strongest external corroboration cuts both ways — it confirms the founding problem is live and simultaneously confirms that fidelity gaps persist, which is the tension this reading must carry.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.58 because the arrangement moves real resources — mandated training budgets to vendors, compulsory frontline hours into scored exercises, compliance records upward as liability cover — while the residual risk of unrehearsed scenario types lands on parties with no seat. Suppression is 0.55: participation is compelled by regulation rather than chosen, and the mandate crowds out rival preparedness designs by consuming the compliance budget and conferring legitimacy on metric-measured drills, though organizations remain free to exceed mandates. Theater is 0.46 and rising: scenario libraries are increasingly written to be passable, scores reward drill-taking skill alongside competence, and a growing share of exercise activity defends the metric regime rather than building readiness — but the procedural training underneath remains genuinely functional, which is why theater stays below one-half. Accessibility_collapse is 0.50: alternatives (no-notice drills, lived-exercise rotations, red-team programs) are neither forbidden nor erased, but they are displaced by resource competition and lose the audit value the mandate confers. Resistance is 0.42: frontline unions contest drill burden, training researchers contest sufficiency claims, and some operators push back on mandate expansion, without ever assembling into a coalition that changes the mandate design. All three temporal series run on one shared grid (t=0..30, six points) so no metric row borrows another's endpoints; the rising trajectories encode rent-layering onto a real coordination core (extraction), Goodhart drift of proxy goals (theater), and the enforcement ratchet as mandates expanded sector by sector and audit regimes hardened (suppression_requirement). Suppression is authored as a raw structural property — the engine, not this story, scales extractiveness by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural data is what forces the divergence. From the vendor and regulator seats the arrangement is a functioning coordination machine they operate and profit from legitimately: rehearsal without catastrophe, verification at scale. From the compliance-office seat it is defensible due diligence — the record they maintain is their professional standing. From the frontline seat it is compulsory recurrent hours that buy real procedural competence at real opportunity cost. From the fidelity-gap-victim seat it is the place where residual risk was quietly parked until it came due. Same structure, four different computed types; the engine derives this from power, exit, and position, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection points: vendors collect revenue, regulators collect auditability and institutional justification, compliance offices collect liability cover, insurers collect premium-relevant documentation — all derive low directionality (subsidized or near-symmetric). The sole victim declaration, fidelity_gap_victims, maps to the people who bear the arrangement's deferred cost with no prior seat; they derive near-full-target directionality, amplified by their powerlessness and trapped position. One override is declared: moderate-power agents are pinned at d=0.5 because the reading's own victim-set restriction removes frontline_response_teams from the victim derivation, leaving them with no structural signal, yet they plainly bear compulsory time costs against plainly real acquired competence — symmetric is the honest value, and the override documents why the derivation chain cannot see it. Institutional-power agents (regulators, investigators, insurers) need no override: their declared roles and exits already place them correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — real catastrophe was the only teacher, and neither organizations nor regulators could rehearse or verify readiness without waiting for disasters — remains live, and this reading's own terms concede it: fidelity determines retention, so the problem of maintaining competence without catastrophe is exactly what the arrangement exists to solve. Nothing here is mandatrophy-resolved; the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds a coherent, non-zombie profile. The classification discipline matters in both directions: calling this a snare would erase the genuine coordination function (teams drilled in simulation demonstrably outperform untrained teams on procedural components, and the verification economy solves a real regulator-scale problem); calling it a rope would erase the asymmetric extraction (compulsory costs on frontlines, captured budgets, and residual risk dumped on seatless third parties). Tangled rope names both halves honestly, and the temporal series shows which half is growing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the simulation_sufficiency_reading of the exercise_as_competence_maintenance kernel; what would each sibling reading change structurally if adopted instead?',
    'Not resolvable by data alone — the choice of reading is a framing commitment. The corpus tracks all three readings as separate constraints and compares their computed classifications across the shared referent.',
    'Under lived_catastrophe_necessity_reading the victim set expands to everyone whose readiness atrophied under simulation-only regimes and the mandate regime loses its coordination defense outright; under hybrid_decay_reading the arrangement splits into a procedural-training component (largely benign coordination) and a judgment-component deficit (substantially extractive).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the exercise kernel; sibling readings are separate constraints.').

omega_variable(
    fidelity_construct_ambiguity,
    'Which dimension of ''fidelity'' governs retention — physical realism, scenario coverage breadth, stress inoculation, or novelty exposure — and does any achievable simulator reach the threshold the sufficiency claim requires?',
    'Controlled transfer-of-training studies varying one fidelity dimension at a time against blind real-world performance measures, with no-notice exercise programs serving as a mid-fidelity probe.',
    'If the governing dimension is novelty or authentic-stress exposure, achievable simulators fall short of the threshold and this reading collapses toward hybrid_decay; if procedural coverage dominates retention, the reading stands and current mandates are closer to sufficient than critics allow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_construct_ambiguity, empirical, 'The sufficiency claim''s truth hinges on an unresolved multidimensional fidelity construct.').

omega_variable(
    retention_curve_unmeasured,
    'How long does simulator-established competence persist without reactivation, and do mandated re-drill intervals track the actual decay curve?',
    'Longitudinal skills-decay studies with delayed post-tests extending well past certification windows; epidemiological comparison of incident rates against time-since-last-exercise.',
    'If decay outruns the mandated intervals, part of the measured extraction purchases readiness that silently lapses — payment without delivery; if intervals match or exceed the decay curve, the mandate cadence is calibrated and a larger share of extraction is genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retention_curve_unmeasured, empirical, 'Retention kinetics behind the fidelity-determines-retention premise are largely unmeasured beyond certification horizons.').

omega_variable(
    victim_set_boundary_dispute,
    'Does the victim set include responders whose judgment under authentic stakes atrophied through simulation-only careers, or only third parties harmed by unrehearsed conditions?',
    'Comparative study of decision quality in first-real-event responders trained exclusively in simulators versus peers with prior live-event experience.',
    'Including deskilled responders widens the victim set substantially and pushes the computed classification toward pure extraction; restricting victims to third-party fidelity-gap casualties keeps the hybrid coordination/extraction structure visible. This boundary is precisely what the sibling readings contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_dispute, conceptual, 'The victim-set boundary is the live edge of the kernel dispute routed to omega rather than folded into this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(exer_tr_t0, observed).
narrative_ontology:measurement(exer_tr_t6, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(exer_tr_t6, observed).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement_basis(exer_tr_t12, observed).
narrative_ontology:measurement(exer_tr_t18, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement_basis(exer_tr_t18, observed).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement_basis(exer_tr_t24, observed).
narrative_ontology:measurement(exer_tr_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement_basis(exer_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(exer_be_t0, observed).
narrative_ontology:measurement(exer_be_t6, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement_basis(exer_be_t6, observed).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement_basis(exer_be_t12, observed).
narrative_ontology:measurement(exer_be_t18, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement_basis(exer_be_t18, observed).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(exer_be_t24, observed).
narrative_ontology:measurement(exer_be_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(exer_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(exer_su_t0, observed).
narrative_ontology:measurement(exer_su_t6, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(exer_su_t6, observed).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement_basis(exer_su_t12, observed).
narrative_ontology:measurement(exer_su_t18, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 18, 0.49).
narrative_ontology:measurement_basis(exer_su_t18, observed).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(exer_su_t24, observed).
narrative_ontology:measurement(exer_su_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(exer_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'exercise maintains crisis competence' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. This story carries the simulation_sufficiency_reading (epsilon ~0.58: genuine coordination core with layered extraction and a restricted victim set of fidelity-gap casualties). lived_catastrophe_necessity_reading carries the maximal-skepticism variant (epsilon substantially higher: simulation delivers little durable exercise, so the entire mandate apparatus extracts against near-zero delivery, and the victim set expands to all simulation-only-trained responders). hybrid_decay_reading carries the split-kernel variant (two sub-constraints: procedural training, largely benign; judgment maintenance, substantially extractive). This reading is upstream of both siblings in institutional terms — the mandate regime it underwrites is the object the other two readings critique, and its metric infrastructure supplies the data their critiques consume. Each file links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
