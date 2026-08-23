% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Simulation-Equivalence Doctrine in Catastrophe Competence Maintenance
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   Across aviation, nuclear operations, and acute medicine, the competence
 *   to handle catastrophes is maintained through a standing arrangement:
 *   regulators codify scheduled high-fidelity simulation as qualifying
 *   practice, organizations run recurring drill cycles, and a training
 *   industry supplies the mandated infrastructure. The arrangement solves a
 *   real problem — rare-event skills decay without rehearsal, and rehearsing
 *   on real events is unacceptable — while simultaneously generating
 *   mandate-backed demand for simulation products, a checkable compliance
 *   artifact that substitutes for unverifiable outcome data, and a
 *   due-diligence record that serves organizations after incidents. The
 *   public bearing residual risk sits outside the arrangement's decision
 *   surfaces. This story authors that arrangement as a single epsilon-stable
 *   constraint: the simulation-equivalence regime, assessed by the reading
 *   that holds simulation to constitute genuine practice. Interval mapping:
 *   t=0 is 1978 (post-Tenerife CRM genesis and early simulator-qualification
 *   expansion), t=48 is 2026.
 *
 * KEY AGENTS:
 *   - - safety_regulators: Agenda-setter (institutional/analytical) — writes the equivalence standard and verifies through the drill record
 *   - - simulation_training_industry: Primary beneficiary (organized/arbitrage) — collects mandate-backed training revenue
 *   - - operating_organizations: Dual-positioned beneficiary-payer (powerful/constrained) — receives competence maintenance and liability cover, pays budgets and compliance labor
 *   - - catastrophe_exposed_public: Primary target (powerless/trapped) — bears residual risk and indirect costs, holds no decision seat
 *   - - line_practitioners: Dual-positioned payer-beneficiary (organized/identity_locked) — pays time and evaluation pressure, gains maintained skill
 *   - - near_miss_learning_advocates: Excluded voice (moderate/constrained) — the portfolio-learning case kept outside the mandate
 *   - - accident_investigators: Analytical observer (institutional/analytical) — documents simulation-to-reality gaps ex post
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.38).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.42).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "Simulation-Equivalence Doctrine in Catastrophe Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '8cc52451-4a8e-450e-85b2-5293a533fefd').
narrative_ontology:cs_kernel_codification('8cc52451-4a8e-450e-85b2-5293a533fefd', formalized).
narrative_ontology:cs_authority_grounding('8cc52451-4a8e-450e-85b2-5293a533fefd', expertise).
narrative_ontology:cs_interpretation_layer_present('8cc52451-4a8e-450e-85b2-5293a533fefd').
narrative_ontology:cs_reading_relation('8cc52451-4a8e-450e-85b2-5293a533fefd', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, influences).
narrative_ontology:cs_reading_relation('8cc52451-4a8e-450e-85b2-5293a533fefd', catastrophe_avoidance_retention__hybrid_near_miss_learning, forecloses).
narrative_ontology:cs_axiom('8cc52451-4a8e-450e-85b2-5293a533fefd', foundational, simulation_transfer_sufficiency).
narrative_ontology:cs_axiom_status(simulation_transfer_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('8cc52451-4a8e-450e-85b2-5293a533fefd', simulation_transfer_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('8cc52451-4a8e-450e-85b2-5293a533fefd', secondary, scheduled_drill_cycles_adequate_cadence).
narrative_ontology:cs_axiom_status(scheduled_drill_cycles_adequate_cadence, holdable).
narrative_ontology:cs_axiom_grounding('8cc52451-4a8e-450e-85b2-5293a533fefd', scheduled_drill_cycles_adequate_cadence, instrumental).
narrative_ontology:cs_reference_frame('8cc52451-4a8e-450e-85b2-5293a533fefd', simulation_equivalence_baseline).
narrative_ontology:cs_drift_state('8cc52451-4a8e-450e-85b2-5293a533fefd', contemporary_post_gap_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8cc52451-4a8e-450e-85b2-5293a533fefd', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_training_industry).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operating_organizations).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_exposed_public).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, line_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, line_practitioners).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operating_organizations).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_equivalence_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, scheduled_competence_maintenance).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_process_verification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces the rules defining current qualification for safety-critical roles: how many simulator hours, which scenarios, what pass standard, and which training devices count. Certifies simulator devices, audits drill completion, and accepts documented drill performance in place of demonstrated performance in real emergencies. The drill record is their verification instrument; they cannot observe competence against catastrophes that never happen, so the equivalence standard is what makes their oversight administratively possible. After accidents they respond by expanding mandated scenario sets.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_regulators, agenda_setter,
    institutional, generational, analytical, global).

% Builds and operates the simulators, courseware, and training centers the mandates require. Demand is set by regulation rather than customer preference: every qualified practitioner must return on a fixed cycle. Revenue scales with mandated hours and device class, and the industry holds seats on the standards committees that determine what counts as a qualifying device and scenario.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_training_industry, beneficiary,
    organized, generational, arbitrage, global).

% Airlines, reactor licensees, and hospital systems deliver the drill cycles to their workforce, pay the training budgets, and hold the resulting documentation. The drill record maintains baseline competence across the roster and doubles as due-diligence evidence in litigation and insurance negotiations after incidents. They cannot decline the mandate and keep their operating certificates; they shape scenario content through industry working groups.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operating_organizations, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operating_organizations, payer).

% Flies, lives downstream of reactors, occupies hospital beds. Bears whatever probability of catastrophe remains after drilled competence is subtracted, especially the portion arising from events unlike anything drilled. Pays for the training system indirectly through fares, tariffs, and premiums. Has no seat in scenario design, no vote on the equivalence standard, and learns of gaps only through accident reports.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_exposed_public, payer,
    powerless, biographical, trapped, global).

% Pilots, licensed reactor operators, and resuscitation teams spend recurring blocks of working life in simulators; certifications, promotions, and continued employment ride on simulated performance. The drill cycle keeps perishable skills alive and supplies a shared script for emergencies. It also fuses professional self-assessment to checkride outcomes — passing the simulator becomes the operative definition of ready — and leaves little scheduled room for learning from near-misses or foreign accidents.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, line_practitioners, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, line_practitioners, beneficiary).

% Safety scientists, internal reporting-program staff, and foreign-incident analysts who argue the strongest lessons come from events that almost happened and from other organizations' catastrophes. Their channels exist but sit outside the mandate: unfunded relative to simulator cycles, supplementary in audits, and absent from the equivalence standard. They would redirect budget and attention toward portfolio learning if admitted to the conversation.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, near_miss_learning_advocates, excluded,
    moderate, generational, constrained, global).

% Reconstruct real accidents after the fact and compare what crews and teams actually faced with what drills had rehearsed. Produce the principal evidence base on where simulation transfers and where it does not. Hold no role in setting the equivalence standard; their findings enter the system as recommendations that typically translate into additional mandated scenarios rather than revision of the equivalence premise itself.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accident_investigators, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_training_industry).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains rare-event response competence across a large, distributed population of practitioners without waiting for or causing real catastrophes: standardizes what current qualification means, makes competence maintenance schedulable, auditable, and comparable across organizations, and provides an ethical rehearsal surface for events that must never be experienced live.
% TRANSFER_FUNCTION: Moves training revenue from operating organizations (and ultimately fare, tariff, and premium payers) to the simulation-training industry; moves practitioner time into scheduled drill cycles; moves residual catastrophe risk onto the public while moving due-diligence documentation to operating organizations; moves regulatory attention from outcome verification to process compliance.
% ABSENT_VOICES: The public bearing residual risk has no seat in scenario design or equivalence adjudication. Near-miss and foreign-incident learning advocates sit outside the mandate — their channels are supplementary and comparatively unfunded. Accident investigators speak only ex post. The consensus that drills suffice arose in rooms where those who would dispute sufficiency were never present.
% DISAPPEARANCE_RATIONALE: If the mandated simulation-equivalence regime vanished overnight, qualification standards would fragment across organizations, the training industry would lose mandate-backed demand, operators would improvise heterogeneous competence maintenance ranging from rigorous to negligent, insurers would reprice unquantifiable readiness, and regulators would lose their primary verification instrument — the entire credentialing infrastructure of safety-critical sectors would reorganize.
% FOUNDING_PROBLEM: Early jet-era aviation, and later nuclear and acute-medicine operation, confronted a brutal asymmetry: the skills needed to survive unprecedented emergencies decay without practice, but practicing on real emergencies kills people. Post-Tenerife investigations showed crews lacking rehearsed responses to crisis-induced loss of situational awareness. High-fidelity simulation offered rehearsal without catastrophe; regulators codified simulator time as qualifying experience to make the practice universal and inspectable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: accident-investigation reports (NTSB, BEA, ICAO annexes) document the competence-decay and rehearsal deficits of the pre-simulation era, and the academic human-factors and high-reliability-organization literature independently attests both the necessity of rehearsal and the contested sufficiency of simulation. Note the corroboration attests the founding problem is live — it does not attest the current regime is sufficient; investigators and researchers are precisely the seats documenting simulation-to-reality gaps.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).
:- end_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently: I claim tangled_rope because I believe the structure genuinely coordinates (ethical rehearsal of unrare-but-unrepeatable events, standardized qualification) while asymmetrically extracting (mandate-sized training purchases, liability-cover value accruing to operators at the public's risk-bearing, regulator verification convenience), under active enforcement. Extractiveness 0.38 reflects that the regime delivers real rehearsal — from this reading's seat the equivalence premise holds within the drilled envelope, capping extraction below extraction-dominant levels — while training volume is sized by rule rather than need. Suppression 0.42 is a raw structural property, unscaled by power or scope: persistence depends on regulatory mandate, device certification, and certificate dependency, and alternatives (portfolio learning) are not banned but starved of mandate, budget, and audit standing. Theater ratio 0.40: high-fidelity device training is functional, but a growing share of activity optimizes the artifact — predictable scenario rotation, instructor leniency cycles, documentation-grade tabletops. Accessibility collapse 0.45: understanding the regime does not collapse alternatives entirely, but the credentialed, audited path is the drill path. Resistance 0.30: drill fatigue, researcher critique, and budget disputes exist, but broad acceptance holds because the founding problem is undeniable and the alternative teacher (real events) is unthinkable. All three temporal series share one grid ({0,8,16,24,32,40,48}); the suppression_requirement series is authored because the story specifically tracks enforcement-capacity change — from informal acceptance of simulator time to codified training rules, device-certification regimes, international harmonization, and post-accident scenario ratchets (each major accident adds mandated scenarios rather than revisiting the equivalence premise). The trajectories are monotonic, not cyclical: this is an enforcement ratchet with Goodhart drift, not an oscillating reinforcement loop.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same structure. From the training industry's position the regime is a market: regulation is demand. From the regulator's position it is governable order: the drill record makes the unverifiable inspectable. From the operator's position it is a compliance asset and an insurance lever. From the practitioner's position it is simultaneously genuine skill maintenance and a career gate that fuses readiness to checkride outcomes. From the public's position it is invisible until an accident reveals the gap between rehearsed and actual. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The catastrophe-exposed public is powerless and trapped: full-target directionality, amplified by scope-scale verification difficulty — they bear the residual risk the equivalence standard externalizes. Line practitioners are payer-primary with real benefits and identity_locked exit: high-but-not-maximal directionality, pushed toward the target end by the lock. The simulation-training industry is a pure beneficiary with arbitrage exit: nearest the beneficiary end, effectively subsidized by the mandate. Safety regulators are beneficiaries of a different kind — they collect verification convenience and institutional justification rather than revenue — placing them low on the directionality scale but not at zero, since enforcement burdens and post-accident reputational exposure flow back to them. Operating organizations are genuinely dual-positioned: the derivation yields a low d from their beneficiary declaration; their payer side (budgets, compliance labor) is real but smaller than what flows to them, so no override is warranted. No directionality_overrides are authored: the combination of role declarations and exit atoms already separates every seat, and the one ambiguous case (operator duality) is carried by secondary_role rather than a numeric patch.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and the arrangement is load-bearing, so this is not a mandatrophy case: status=live crossed with verdict=world_rearranges yields no zombie flag. The danger this story tracks is forward-looking: rising theater_ratio is the early-warning vector for a tangled_rope-to-piton transition, in which scenario design ossifies into ritual while the equivalence premise quietly erodes — administrators could restore fidelity, but the cost of doing so exceeds what any single seat bears, and the drill record continues to serve its liability function regardless of its training function. The measurement series exists to date that transition if it occurs. The classification prevents mislabeling in both directions: reading the regime as pure coordination ignores the mandate-backed rents and the externalized residual risk; reading it as pure extraction erases the genuine rehearsal function that no alternative currently supplies at scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_depth,
    'Does competence built in high-fidelity simulation transfer to performance under genuine catastrophe conditions — chaos, mortality salience, compound unprecedented failures — or only within the drilled envelope?',
    'Comparative analysis of team performance in real unprecedented events against drilled baselines; natural experiments where drill intensity varies across organizations; accident-investigation reconstructions systematically coding faced-versus-rehearsed condition distance.',
    'Shallow transfer collapses the equivalence premise: the regime''s coordination claim shrinks, effective extraction rises sharply, and classification migrates toward extraction-dominant types. Deep transfer confirms the reading and caps extraction near coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_depth, empirical, 'Whether the equivalence premise holds at the envelope boundary, not just inside it.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the catastrophe_avoidance_retention kernel; which reading governs the standing arrangement, and how would the sibling readings restructure it?',
    'Adoption history and regulatory posture: whether oversight bodies fund portfolio learning channels (hybrid reading), preserve deliberate real-event exposure mechanisms (selector reading), or deepen simulation mandates (this reading); standards-committee records and budget allocations reveal which reading is operative.',
    'Under catastrophe_as_necessary_selector this regime''s epsilon rises steeply — the substitution claim fails and the mandate reads as purchased false assurance, with the public''s risk share growing. Under hybrid_near_miss_learning epsilon moderates and the drill mandate becomes one funded channel among several, with near-miss programs gaining standing. Victim sets and enforcement profiles shift accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame ambiguity across the retention kernel''s three readings; this file instantiates only the simulation_as_proxy_catastrophe reading.').

omega_variable(
    liability_cover_share,
    'What fraction of the drill record''s value to operating organizations is competence maintenance versus due-diligence and liability protection?',
    'Litigation and settlement outcomes comparing organizations with complete versus gapped drill records at matched competence; insurer pricing models; discovery records showing how training files are deployed after incidents.',
    'A high liability share raises effective extraction — the same artifact that rehearses crews also shields organizations at the public''s expense — pushing classification toward extraction-dominant types. A low share supports the coordination-first reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_cover_share, empirical, 'Competence function versus liability-cover function of the drill record.').

omega_variable(
    goodhart_scenario_gaming,
    'How much drill activity optimizes pass-rate rather than readiness — scenario leakage, instructor leniency cycles, rehearsed scripts, documentation-grade exercises?',
    'Blind-scenario variation trials; longitudinal pass-rate distributions against scenario novelty; instructor standardization audits; comparison of graded performance with unscheduled performance sampling.',
    'Sets the theater_ratio trajectory and the piton-drift risk; sustained gaming would date a tangled_rope-to-piton transition and indicate the rehearsal function is being replaced by its performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(goodhart_scenario_gaming, empirical, 'Extent of Goodhart drift in scenario design and grading.').

omega_variable(
    internalized_readiness_assurance,
    'Is practitioners'' drill-calibrated confidence — passing the simulator as the operative definition of ready — a structural product of the mandate, or an internalized identity fusion that would persist if the mandate relaxed?',
    'Study cohorts moving into lightly regulated domains (general aviation, contract operations, voluntary training): does sim-anchored confidence persist and shape risk behavior once enforcement is removed?',
    'If internalized, effective suppression exceeds the structural measure — the regime''s grip travels with the practitioner after exit from the mandated environment — raising the payer seat''s computed extraction and sharpening the identity-lock dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_readiness_assurance, empirical, 'Structural versus internalized component of drill-regime suppression on practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 8, 0.17).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 32, 0.31).
narrative_ontology:measurement_basis(cata_tr_t32, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 40, 0.36).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t48, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 48, 0.4).
narrative_ontology:measurement_basis(cata_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 8, 0.24).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 16, 0.28).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 24, 0.31).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 32, 0.34).
narrative_ontology:measurement_basis(cata_be_t32, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 40, 0.37).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t48, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 48, 0.38).
narrative_ontology:measurement_basis(cata_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 8, 0.28).
narrative_ontology:measurement_basis(cata_su_t8, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 16, 0.31).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 24, 0.34).
narrative_ontology:measurement_basis(cata_su_t24, observed).
narrative_ontology:measurement(cata_su_t32, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 32, 0.37).
narrative_ontology:measurement_basis(cata_su_t32, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 40, 0.4).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t48, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 48, 0.42).
narrative_ontology:measurement_basis(cata_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention__catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention__hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how organizations retain catastrophe-avoidance competence' covers three structurally distinct arrangements with different epsilon, beneficiary sets, and enforcement profiles, decomposed per the epsilon-invariance principle. This file is the simulation-substitution regime (mandated drill equivalence). Downstream/upstream edges: this reading's operational success suppresses the supply of real catastrophes, changing the operating environment and evidentiary base of the selector reading; it competes with the hybrid reading for mandate share, budget, and audit standing. Sibling stories carry their own epsilon and stakeholder structures; all three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
