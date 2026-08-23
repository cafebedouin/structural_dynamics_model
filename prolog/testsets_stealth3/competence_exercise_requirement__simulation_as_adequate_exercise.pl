% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: Simulation-Based Competence Currency Doctrine (High-Fidelity Simulation as Adequate Kernel Exercise)
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   A high-reliability-industries doctrine holds that recurring cycles of
 *   high-fidelity simulation followed by structured debriefing constitute
 *   adequate exercise of the operational competence kernel: crews who pass
 *   recurrent simulator checks are fit for line operations, and decades
 *   without catastrophic validation events confirm the arrangement. This file
 *   instantiates ONE reading of the contested kernel
 *   competence_exercise_requirement: simulation_as_adequate_exercise. The
 *   epsilon referent is the standing arrangement under contest, namely the
 *   institutionalized rule-set crediting scheduled simulator currency as
 *   sufficient proof of maintained competence. Two sibling stories
 *   instantiate the other readings:
 *   competence_exercise_requirement__catastrophe_as_necessary_anchor (only
 *   real events or near-misses provide irreducible exercise) and
 *   competence_exercise_requirement__hybrid_dependency (simulation necessary
 *   but insufficient without periodic real-world anchoring). All three
 *   readings share the referent and differ in epsilon by construction: from
 *   the catastrophe-anchor seat the standing arrangement collects compliance
 *   while withholding the exercise it claims to supply (high epsilon,
 *   snare-leaning); from the hybrid seat it is a support structure missing
 *   its complement (transitional pressure); from this seat the arrangement
 *   delivers most of its promised function while concentrating cost-avoidance
 *   gains on operators and externalizing verification onto the flying public
 *   (moderate epsilon).
 *
 * KEY AGENTS:
 *   - - aviation_regulators: Agenda setter (institutional/constrained) — certifies simulators, defines currency, audits compliance; bound to the artifact their oversight runs on
 *   - - airline_operators: Primary beneficiary with agenda influence (powerful/arbitrage) — books avoided live-training spend to margin, optimizes within the rule it co-drafted
 *   - - flight_simulation_industry: Secondary beneficiary (organized/mobile) — converts every credited simulator hour into contracted demand
 *   - - airline_passengers: Primary payer (powerless/trapped) — bears unobservable, unpriceable residual validation risk; no exit via carrier switching
 *   - - line_flight_crews: Dual-positioned payer/beneficiary (organized/identity_locked) — gated on checkride outcomes, absorbs envelope divergences firsthand
 *   - - safety_science_community: Excluded critic (moderate/analytical) — publishes the hybrid and catastrophe-anchor case with no rule-setting seat
 *   - - accident_investigators: Analytical observer (institutional/analytical) — generates the retrospective evidence base without adjudicating the premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.52).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.58).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.52).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "Simulation-Based Competence Currency Doctrine (High-Fidelity Simulation as Adequate Kernel Exercise)").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, '287da189-e24c-48b8-ac43-868abb3e33c8').
narrative_ontology:cs_kernel_codification('287da189-e24c-48b8-ac43-868abb3e33c8', formalized).
narrative_ontology:cs_authority_grounding('287da189-e24c-48b8-ac43-868abb3e33c8', expertise).
narrative_ontology:cs_interpretation_layer_present('287da189-e24c-48b8-ac43-868abb3e33c8').
narrative_ontology:cs_reading_relation('287da189-e24c-48b8-ac43-868abb3e33c8', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('287da189-e24c-48b8-ac43-868abb3e33c8', competence_exercise_requirement__hybrid_dependency, forecloses).
narrative_ontology:cs_axiom('287da189-e24c-48b8-ac43-868abb3e33c8', foundational, high_fidelity_transfer_sufficiency).
narrative_ontology:cs_axiom_status(high_fidelity_transfer_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('287da189-e24c-48b8-ac43-868abb3e33c8', high_fidelity_transfer_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('287da189-e24c-48b8-ac43-868abb3e33c8', secondary, debriefed_cycle_completeness).
narrative_ontology:cs_axiom_status(debriefed_cycle_completeness, holdable).
narrative_ontology:cs_axiom_grounding('287da189-e24c-48b8-ac43-868abb3e33c8', debriefed_cycle_completeness, instrumental).
narrative_ontology:cs_reference_frame('287da189-e24c-48b8-ac43-868abb3e33c8', validated_transfer_sufficiency).
narrative_ontology:cs_drift_state('287da189-e24c-48b8-ac43-868abb3e33c8', post_manual_skills_findings, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('287da189-e24c-48b8-ac43-868abb3e33c8', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, flight_simulation_industry).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, airline_passengers).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, line_flight_crews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, line_flight_crews).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, fidelity_transfer_hypothesis).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_compliance_sufficiency).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, scheduled_recurrency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certify simulator fidelity levels, define recurrent currency requirements, and audit operator compliance. Their oversight legitimacy rests on demonstrable, inspectable evidence of crew proficiency, which the scheduled simulator cycle provides in auditable form. Reversing the adequacy doctrine without accumulated anomaly evidence would destabilize the certification apparatus their authority runs on, so amendments arrive as additive modules rather than premise revisions.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, aviation_regulators, agenda_setter,
    institutional, generational, constrained, global).

% Fund and schedule recurrent training sized to regulatory minima; every simulator hour displaces live-aircraft hours whose fuel, airframe wear, and schedule disruption would otherwise hit operating cost, and the avoided spend books straight to margin. They co-draft the standards through industry advisory bodies, then optimize within the letter of what they drafted: minimum compliant hours, familiar scenario sequencing, outsourced training centers.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators, agenda_setter).

% Manufactures and operates full-flight simulators and training curricula under multi-year service contracts. Every regulatory credit granted to simulator time converts directly into contracted demand, and the industry sits on the standards committees that define the device fidelity tiers constituting its own product ladder. Capital is redeployable into healthcare and defense simulation if aviation demand shifts.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, flight_simulation_industry, beneficiary,
    organized, generational, mobile, global).

% Fly under the regime without any means to observe, evaluate, or price the gap between a crew's simulated proficiency and its line readiness. Switching carriers offers no escape because every operator trains under the same doctrine, so the residual risk of divergence is borne diffusely by everyone who boards. They recover a sliver of the training-cost savings as fare competition.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, airline_passengers, payer,
    powerless, immediate, trapped, global).

% Fly the line and periodically prove proficiency in the simulator; employment and progression gate on checkride outcomes. They benefit from risk-free rehearsal of lethal events and paid professional development, but they absorb firsthand whatever divergence exists between rehearsed profiles and actual events, and their professional identity is fused with the proficiency-check culture: a pilot is, operationally, someone who passes the check.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, line_flight_crews, payer,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, line_flight_crews, beneficiary).

% Human-factors and resilience researchers who publish the case that simulation omits irreducible elements of real operations: genuine novelty, resource scarcity under time pressure, interpersonal stress under threat. They hold no vote in rule-setting; their findings reach policy only after filtering through the agencies whose doctrine is under critique, and the operators who fund much applied research.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, safety_science_community, excluded,
    moderate, generational, analytical, global).

% Reconstruct accidents and serious incidents after the fact. Every event where crew response departed from the rehearsed envelope constitutes retrospective evidence for or against transfer sufficiency. Their reports drive rule amendments, but the investigative mandate documents anomalies without adjudicating whether the adequacy premise itself should stand.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, accident_investigators, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, airline_operators).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes what counts as maintained proficiency so that thousands of operators, regulators, insurers, and crews share one auditable currency of competence, and makes it possible to rehearse events that cannot ethically be rehearsed live: engine failures, windshear escapes, rejected takeoffs. Solves the collective-action problem under which individual operators would underinvest in rare-event preparation.
% TRANSFER_FUNCTION: Moves the cost of competence assurance out of operating budgets (live-aircraft hours, fuel, airframe wear, line disruption) and into amortized simulation infrastructure and contracted training purchases, while moving residual validation risk onto the flying public, who cannot observe whether simulated proficiency equals line proficiency.
% ABSENT_VOICES: The safety-science community holding the catastrophe-anchor and hybrid readings is outside the rule-setting table; passengers have no representative seat anywhere in the standards process; crew voices enter only through union training committees whose standing is consultative, not decisive.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight and simulator time stopped counting as currency, operators would need to rebuild proficiency assurance around live-aircraft anchoring and observed line performance: training costs would spike, fleet availability would drop, the simulation industry would contract sharply, and regulators would re-engineer the entire currency standard. The commercial aviation training economy is arranged around this reading and would visibly reorganize without it.
% FOUNDING_PROBLEM: In the early jet era, airlines lost aircraft and crews during live rehearsal itself: proficiency checks and training flights conducted in real aircraft against simulated emergencies were themselves a significant accident category. The founding problem was exercising crews against rare, lethal events without exposing them to those events during the exercise.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: early-jet-era training-accident rates are documented in accident-investigation board records and insurer loss histories, and the continuing liveness of the underlying hazard is attested by the peer-reviewed human-factors literature and by investigator findings on events where crew response exceeded the trained envelope. No corroboration rests solely on operator or simulator-industry testimony.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.52: the arrangement performs its coordination function genuinely, but the same structure concentrates cost-avoidance on operators, converts mandates into vendor demand, and crowds out the comparisons that would price its residual risk. Suppression 0.58 is authored as a raw structural property, deliberately unscaled in my authorship; the engine owns any scaling. Its sources are regulatory mandate plus the absence of any credited alternative anchoring, with a smaller internalized component (crews absorb the checkride culture as the definition of their craft). Theater 0.32: debriefing is real pedagogy and fidelity is real engineering, but scenario catalogs ossify into checkride ritual, so a growing minority of billed hours rehearses the catalog rather than exercises the kernel. Accessibility collapse 0.45: live-aircraft anchoring programs and non-jeopardy line audits exist and are practiced voluntarily, so understanding the constraint does not close the option set, but no regulatory credit attaches and adoption is therefore uneconomical at scale. Resistance 0.5: sustained scholarly critique, union training-committee friction, and post-event patch mandates without frontal rejection of the doctrine. The measurement series run on ONE shared grid (points 0,10,20,30,40,50 approximating 1975 to 2025) with all three tracked metrics authored at every point; trajectories are monotonic rather than cyclic, driven by budget optimization and expanding regulatory credit rather than intermittent reinforcement. The suppression_requirement series tracks real enforcement-capacity history: inspection machinery matured through the mid-period and then plateaued under risk-based oversight. Fixing the arrangement (crediting hybrid anchoring, re-engineering currency standards around line observation) is prohibitive relative to its perceived benefit while the operational record stays clean, which is precisely what keeps the fix unattempted.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute divergent classifications across seats from this structural data. From the operator seat the arrangement is efficient compliance infrastructure it helped draft: coordination-dominant, low experienced extraction. From the passenger seat it is an unpriceable risk transfer enforced by mandate: extraction-dominant with no exit whatsoever. From the crew seat it is double-edged: risk-free paid rehearsal plus a career gate keyed to a proxy measure. From the excluded researcher seat it is a closed evidentiary loop that credits only the artifact its own administrators control. Two same-level institutional actors differentiate cleanly: the operator arbitrages inside the rule while the regulator is bound to defend it, despite equal nominal standing in the standards process.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (airline_operators, flight_simulation_industry) derive directionality near the subsidy end; the operator's arbitrage-grade exit inside the rule reinforces that placement. Declared victims (airline_passengers, line_flight_crews) derive near the target end, with trapped exit placing passengers nearest full-target. Crews are genuinely dual-positioned: their identity_locked exit, arising from fusion between the proficiency check and professional identity, pulls their effective extraction above the midpoint even though they collect real benefits. Regulators are deliberately left undeclared on both sides rather than forced through an override; their mild structural subsidy, oversight legibility purchased by the compliance artifact, is recorded here in prose. On the receipt surface, the constraint's gains demonstrably accrue to airline_operators: avoided live-training spend booked to margin is the capture seat, while the simulator industry receives contracted revenue it competes for, and regulators receive legibility rather than rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, exercising crews against lethal rare events without live rehearsal deaths, was real and remains substantively addressed, so its status is live rather than dead and no zombie configuration arises from the R5 mismatch check (live status paired with a world_rearranges verdict is the consistent cell; the arrangement is load-bearing, not inertial). The doctrine nonetheless outgrew its transitional character: adopted to replace deadly live rehearsal, it hardened into steady-state orthodoxy with no sunset and steadily expanding credit. The classification guards against both mislabels. Pure-snare labeling would erase the irreplaceable coordination function, since certain events can only ever be rehearsed synthetically and no reading disputes that. Pure-rope labeling would erase the asymmetric receipt surface, since cost avoidance is captured by identifiable seats while verification risk is externalized to a trapped public. Keeping both halves visible is exactly the tangled_rope job.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates only the simulation_as_adequate_exercise reading of the competence_exercise_requirement kernel. How would classification shift under the sibling readings, catastrophe_as_necessary_anchor and hybrid_dependency?',
    'Comparative adjudication across the linked sibling stories against shared outcome data: longitudinal skill-decay studies, upset-event records where crew response departed from rehearsed envelopes, and any jurisdiction running credited real-world anchoring pilots.',
    'Under catastrophe_as_necessary_anchor, the standing arrangement reads as suppression of necessary exercise (epsilon rises sharply, snare-leaning); under hybrid_dependency it reads as a scaffold missing its complement (recomposition pressure rather than steady-state tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure routed here per the kernel-reading rules: one kernel, three readings, this file holds the sufficiency reading.').

omega_variable(
    transfer_validity_empirics,
    'Does proficiency exercised and measured in a high-fidelity simulator with structured debriefing actually transfer to line performance across all relevant skill classes (procedural, manual-handling, adaptive, interpersonal)?',
    'Controlled cohort studies comparing simulator-only currency against simulator-plus-live-anchoring cohorts, with decay curves tracked per skill class rather than in aggregate.',
    'If manual and adaptive skills decay despite current simulator status, the foundational axiom loses empirical ground, epsilon rises, and the axiom_overriding drift already recorded in drift_state advances toward severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_validity_empirics, empirical, 'Whether the fidelity-transfer premise holds universally or leaves an irreducible residue.').

omega_variable(
    clean_record_attribution,
    'Do catastrophe-free decades validate THIS reading specifically, or do they validate the broader safety system (automation depth, traffic control, airframe redundancy, dispatch discipline) with the training doctrine as one contributing layer?',
    'Attribution analysis decomposing historical safety gains by causal layer, plus natural experiments where training regimes changed without corresponding changes elsewhere.',
    'If the clean record is mostly attributable to other layers, the reading''s validation logic is riding on others'' success; the persistence story weakens from demonstrated sufficiency to unfalsified assumption, raising effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clean_record_attribution, empirical, 'Whether the structural delta''s validation claim (catastrophe-free decades prove adequacy) is correctly attributed.').

omega_variable(
    checkrite_measurement_drift,
    'Does the compliance artifact (recurrent checkride outcomes) measure exercise of the competence kernel, or familiarity with a finite, teachable scenario catalog?',
    'Randomized unprompted scenario perturbation studies and correlation tracking between checkride scores and subsequent line-event crew performance.',
    'Growing divergence inflates theater_ratio independently of the debriefing function and corrodes the artifact''s evidentiary value, undermining both regulatory oversight and the clean-record validation chain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(checkrite_measurement_drift, empirical, 'Goodhart drift risk inside the proficiency-measurement instrument itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(comp_tr_t40, observed).
narrative_ontology:measurement(comp_tr_t50, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(comp_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 20, 0.43).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t30, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(comp_be_t40, observed).
narrative_ontology:measurement(comp_be_t50, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(comp_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 10, 0.49).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t30, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 40, 0.59).
narrative_ontology:measurement_basis(comp_su_t40, observed).
narrative_ontology:measurement(comp_su_t50, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(comp_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% The colloquial label 'how high-reliability organizations maintain competence' covers three structurally distinct claims about the necessity and sufficiency relation between synthetic and real exercise, decomposed per the epsilon-invariance principle. This file holds the institutionalized modern reading (tangled_rope, moderate epsilon from its own seat). catastrophe_as_necessary_anchor is the upstream historical baseline: before full-fidelity simulators, real events were the only exercise, and that reading authors a high epsilon for the standing arrangement, citing it as compliance collected while necessary exercise is withheld. hybrid_dependency is the downstream synthesis, authors intermediate epsilon, and carries recomposition pressure. Edges run across all family members: the catastrophe-anchor reading supplies the evidentiary cases (classic command-decision collapses, controlled-flight-into-terrain events, upset recoveries outside trained envelopes) that both other readings must answer; this reading controls the compliance infrastructure that both siblings must argue against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
