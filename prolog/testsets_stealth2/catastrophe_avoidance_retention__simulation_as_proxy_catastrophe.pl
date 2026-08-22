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
 *   human_readable: Simulation-as-Proxy-Catastrophe Doctrine: Mandated High-Fidelity Drill Regimes for Catastrophe-Avoidance Competence
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   Since roughly 1980 (interval t=0: post-Three-Mile-Island reform and the
 *   maturation of full-mission flight simulation), high-hazard industries —
 *   nuclear power, aviation, petrochemical processing, emergency medicine —
 *   have converged on mandated high-fidelity simulation as the primary
 *   instrument for maintaining catastrophe-avoidance competence. The
 *   constraint examined here is the doctrine embedded in that regime: that a
 *   sufficiently faithful simulation constitutes genuine practice, so that
 *   scheduled drills are functionally equivalent to real catastrophic events
 *   for keeping crews and response organizations competent. The doctrine is
 *   operationalized as regulatory drill-hour minimums, certified simulator
 *   devices, and documented scenario completion, enforced through licensing.
 *   KEY AGENTS (by structural relationship): simulation_vendors
 *   (organized/arbitrage) — primary beneficiary selling the mandated
 *   instruments; safety_regulators (institutional/constrained) —
 *   agenda_setter whose verification capacity depends on the regime;
 *   high_hazard_operators (powerful/constrained) — principal payer with
 *   secondary benefit; frontline_operators (moderate/identity_locked) —
 *   payers whose drill time buys skill and whose professional identity is
 *   fused to drill credentials; general_public (powerless/trapped) —
 *   conditional beneficiaries; accident_investigation_boards
 *   (institutional/analytical) — observers holding the drill-versus-reality
 *   comparison data; near_miss_program_advocates (organized/constrained) —
 *   excluded voices whose learning channels the regime crowds out. This file
 *   is one reading of the catastrophe_avoidance_retention kernel (see
 *   kernel_context); the sibling readings are separate constraint stories
 *   linked in network.affects_constraints, and this file's epsilon is
 *   authored for the standing drill-mandate regime from this reading's own
 *   seat, not averaged across readings.
 *
 * KEY AGENTS:
 *   - simulation_vendors: primary beneficiary (organized/arbitrage) — sells the simulators, scenario libraries, and recurring training the mandates require; portable across industries
 *   - safety_regulators: agenda_setter (institutional/constrained) — mandates drill hours and certifies fidelity; drill records are their primary readiness-evidence instrument
 *   - high_hazard_operators: principal payer, secondary beneficiary (powerful/constrained) — fund simulator fleets and staff drill hours; receive credentialed-ready workforces and license continuity
 *   - frontline_operators: payer, secondary beneficiary (moderate/identity_locked) — spend recurring duty time in drills; careers and professional self-concept are structured around drill certifications
 *   - general_public: beneficiary (powerless/trapped) — receives promised protection whose delivery depends on the contested equivalence premise
 *   - accident_investigation_boards: observer (institutional/analytical) — hold the only systematic drill-score versus real-event performance comparison
 *   - near_miss_program_advocates: excluded (organized/constrained) — promote near-miss and foreign-incident learning; lose budget share whenever drill-hours satisfy compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.34).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.45).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.34).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "Simulation-as-Proxy-Catastrophe Doctrine: Mandated High-Fidelity Drill Regimes for Catastrophe-Avoidance Competence").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f').
narrative_ontology:cs_kernel_codification('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f', formalized).
narrative_ontology:cs_authority_grounding('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f', expertise).
narrative_ontology:cs_interpretation_layer_present('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f').
narrative_ontology:cs_reading_relation('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_reading_relation('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f', catastrophe_avoidance_retention__hybrid_near_miss_learning, forecloses).
narrative_ontology:cs_axiom('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f', foundational, simulation_fidelity_suffices_for_competence).
narrative_ontology:cs_axiom_status(simulation_fidelity_suffices_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f', simulation_fidelity_suffices_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f', secondary, scheduled_drills_arrest_competence_decay).
narrative_ontology:cs_axiom_status(scheduled_drills_arrest_competence_decay, holdable).
narrative_ontology:cs_axiom_grounding('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f', scheduled_drills_arrest_competence_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f', drill_performance_as_readiness_evidence).
narrative_ontology:cs_drift_state('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f', contemporary_post_accident_investigations, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8e732ed4-9a5e-4c48-99b0-98dfc85f6f5f', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, general_public).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, high_hazard_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, high_hazard_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_fidelity_sufficiency).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, scheduled_practice_decay_arrest_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and sell full-mission simulators, scenario libraries, instructor services, and recurring training contracts. Revenue is anchored in regulatory mandates that require certified-device hours, giving them a demand floor insulated from ordinary market competition. Product lines and engineering expertise port across aviation, nuclear, maritime, and healthcare customers, so exit from any single sector's regime is cheap and re-entry elsewhere is routine.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Set minimum drill-hour requirements, certify simulator fidelity, and audit completion records. Drill documentation is their primary instrument for verifying readiness across thousands of licensed operations — observable, schedulable, and auditable in a way that latent competence is not. Abandoning the instrument would leave them without an enforceable readiness standard, so their administration of the regime is self-interested as well as statutory.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Nuclear utilities, airlines, chemical processors, and hospital systems. They finance simulator acquisition, dedicated training centers, backfill staffing for crews in rehearsal, and compliance documentation. In return they receive credentialed-ready workforces, license continuity, and liability defensibility. They cannot exit their hazard class — a utility cannot stop being a nuclear utility — so their engagement takes the form of lobbying over mandate scope, cost recovery, and acceptable evidence, not exit.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, high_hazard_operators, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, high_hazard_operators, beneficiary).

% Flight crews, reactor control-room teams, surgical and emergency-response teams. They spend recurring duty time in drills, gaining genuine procedural fluency, crew-resource-management habits, and equipment familiarity. Their careers, seniority progression, and legal authorization to operate run through type ratings, check rides, and recertification cycles; professional self-concept as a competent operator is constituted by these credentials. Opting out ends the career, not merely the training, and the credential identity travels with them between employers.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators, beneficiary).

% Communities living downwind, downstream, beneath flight paths, and around hazardous facilities. They receive the promised protection of drill-trained crews and response organizations, and they bear the consequences if that protection is hollow. They cannot observe drill quality, verify competence, or relocate away from systemic hazard exposure at individual scale; their protection is entirely mediated by institutions they do not control.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, general_public, beneficiary,
    powerless, generational, trapped, regional).

% Independent investigative bodies that examine real events after they occur. They hold the only systematic dataset comparing drill-documented readiness against realized performance for the same organizations, and their findings periodically surface cases where drill-successful organizations failed real events. They publish recommendations but do not administer the mandate structure their findings interrogate.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accident_investigation_boards, observer,
    institutional, generational, analytical, national).

% Safety scientists, resilience engineers, and practitioners who promote learning from incident reports, foreign-incident analysis, and normal-operation data. Their programs compete for the same training budgets and regulatory attention as mandated drills; whenever documented drill-hours satisfy compliance requirements, their proposals lose the room. They are not seated in the standards committees that define adequate preparation, and they encounter the regime mainly as a budget line that displaces theirs.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, near_miss_program_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_vendors).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the rare-event preparation problem: catastrophic scenarios are too infrequent and too dangerous for on-the-job learning, so scheduled high-fidelity simulation gives crews and response teams repeated exposure to catastrophic-scale decision-making without waiting for real events. It also standardizes readiness evidence so a regulator can verify preparation across many heterogeneous organizations with one auditable instrument.
% TRANSFER_FUNCTION: Moves training budgets and duty time from high-hazard operators and frontline staff to simulation vendors, training academies, and compliance documentation; moves documented readiness evidence upward to regulators; and, if the equivalence premise fails, silently transfers unmitigated catastrophe risk outward to the public.
% ABSENT_VOICES: Near-miss program advocates and safety scientists who hold that drill-hours crowd out richer learning channels are not seated in the standards committees that define adequate preparation. Communities exposed to facility failures enter the conversation only retrospectively, as case material in accident investigation reports. Both would contest the adequacy standard itself; both stand outside the rooms where drill-hour minimums and simulator certification criteria are set.
% DISAPPEARANCE_RATIONALE: If the mandate and its equivalence doctrine vanished overnight, simulator procurement and recurring-training contracts would unwind, regulators would lose their primary readiness-verification instrument and scramble for substitutes, operators would reallocate training budgets toward whatever evidence the successor regime accepts, and a large installed base of training infrastructure and credentialing would depreciate rapidly. The preparedness economy would reorganize around the next accepted evidence standard rather than persist as-is.
% FOUNDING_PROBLEM: After mid-century high-hazard accidents made visible that critical skills decay between rare events and that real catastrophes are unavailable as teachers, regulators needed an instrument to keep catastrophe-avoidance competence continuously exercised and verifiable across thousands of licensed operations.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident investigation boards — outside both the training industry and the regulated operators — attest that competence decay between events is real and that some deliberate maintenance is necessary. The academic high-reliability and resilience-engineering literature corroborates the decay problem while disputing this regime's sufficiency claim. No external party attests the equivalence premise itself: corroboration covers the founding problem, not this reading's proposed solution.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.34, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.34: from this reading's own lights the bulk of drill expenditure is genuine coordination cost (full-mission simulation is expensive but functional), yet a real rent layer sits on top — proprietary simulator lock-in priced above marginal cost, consultant-driven compliance programs, and documentation burdens that reward paper over practice. Suppression is 0.45, authored as a raw structural property and deliberately unscaled: license-contingent mandates leave no opt-out, and compliance satisfaction crowds out alternative learning channels (structural suppression, not internalized). Theater_ratio is 0.32: scripted known-answer scenarios and normalized pass rates coexist with genuinely demanding full-mission exercises. Accessibility_collapse is 0.35 — near-miss programs, foreign-incident study, and veteran apprenticeship persist as alternatives but are crowded out rather than eliminated. Resistance is 0.30: unions negotiate drill-hour loads and operators lobby mandate scope, but almost no seat disputes drilling as such. The three measurement series run on one shared time grid (t = 0, 8, 16, 24, 32, 40, 44, mapping approximately 1980 to 2024) so every metric is authored at every examined point; all trajectories are monotonic — no cyclical dynamics are claimed. Claim and metrics are independent authored facts: tangled_rope is stated from structural analysis (genuine coordination function plus asymmetric extraction plus active enforcement), not tuned to any predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes divergent per-seat classifications from the structural data, and the divergence here is wide. From the regulator seat the regime is a working verification instrument — coordination it can audit, close to a rope. From the vendor seat it is a demand floor with arbitrage-grade portability. From the operator seat it is a mandated cost with uncertain payoff and license leverage held over it. From the frontline seat it is simultaneously skill formation and identity constitution: check-ride culture fuses professional self-concept with drill credentials, an identity_locked exit that persists across employers — the identity-lock mechanism is professional-identity fusion, and if that frame broke (credentials decoupled from self-concept), the frontline seat would compute as merely constrained rather than locked. From the public seat the entire benefit stream is contingent on the equivalence premise, which no seat inside the regime can verify from its own position.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations plus exit options drive the derivation, and no directionality_overrides are authored because the derivation reproduces the intended ordering for every seat. simulation_vendors (beneficiary, arbitrage exit across industries) derive nearest the beneficiary pole. safety_regulators (agenda_setter collecting oversight tractability at low personal cost) sit low-d. general_public derives low d within this reading's lights — subsidized protection — with the conditionality of that benefit carried in the kernel_reading_contestation omega rather than forced into the scalar. high_hazard_operators (payer primary, beneficiary secondary, constrained exit) derive mid-range. frontline_operators (payer primary, identity_locked) derive nearer the target pole; identity lock amplifies effective extraction per the derivation chain. accident_investigation_boards (observer) and near_miss_program_advocates (excluded) sit outside the extraction circuit; per the R3 ruling the excluded seat is commentary-grade only and drives no classification correction.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy resolution is declared: the founding problem — competence decay between rare events — is live and externally corroborated, so the regime is not administering a dead mandate. The temporal series nonetheless traces the piton precursor: theater_ratio climbs monotonically from 0.12 to 0.32 as scripted, known-answer scenarios displace exploratory ones, and extractiveness rises as compliance formalizes. The tangled_rope classification guards against both mislabels: reading the regime as pure rope erases the vendor-rent asymmetry and the crowding-out of alternative learning channels; reading it as snare erases the genuine procedural competence and crew-coordination skill that drills demonstrably build. The category holds both truths at once — real coordination function, real asymmetric extraction, active enforcement holding the joint — and the omega variables mark exactly where the balance could tip.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the simulation_as_proxy_catastrophe reading of the catastrophe_avoidance_retention kernel; how would classification change if the standing drill-mandate regime were evaluated under the sibling readings catastrophe_as_necessary_selector or hybrid_near_miss_learning?',
    'Author and evaluate the sibling stories against the same referent (the standing drill-mandate regime, not each reading''s endorsed alternative); matched comparisons of drill-documented versus event-experienced performance determine which reading the evidence supports.',
    'Under catastrophe_as_necessary_selector the regime''s coordination claim collapses toward cover and epsilon rises sharply (snare-flavored: enforced ritual substituting for the only effective teacher, with the public converted from beneficiary to residual victim). Under hybrid_near_miss_learning epsilon moderates and the regime demotes to one component of a learning portfolio, with suppression rising via crowding-out of the other components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings restructure the beneficiary/victim map over the same referent.').

omega_variable(
    fidelity_transfer_gap,
    'Does high-fidelity drill performance transfer to real catastrophic conditions (degraded information, cascading failure, time compression, mortal stakes), or does the residual fidelity gap leave a systematic competence deficit the regime''s own evidence cannot see?',
    'Matched comparison of drill scores and realized real-event performance for the same organizations and teams across accident investigation archives.',
    'Poor transfer converts a large share of the regime''s measured coordination function into theater: theater_ratio is understated by the scalar, and the constraint drifts toward piton (ritual maintenance of a doctrine) or snare (enforced purchase of false assurance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_transfer_gap, empirical, 'Whether simulated adversity produces the competence real events demand.').

omega_variable(
    mortality_salience_substitutability,
    'Are the psychological and organizational states induced only by real catastrophes — stress inoculation under mortal stakes, trauma-forged vigilance, irreversible-consequence decision discipline — producible in simulation at any achievable fidelity?',
    'Longitudinal cohort studies comparing teams whose formative experience includes real emergencies against simulation-only cohorts under matched subsequent real-event exposure.',
    'If the states are not simulable, the selector reading''s core premise survives inside this regime and the equivalence axiom is empirically overridden regardless of drill volume; if partially simulable (high-arousal protocols, immersive stress induction), the hybrid reading absorbs the difference.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mortality_salience_substitutability, empirical, 'Substitutability of real-catastrophe psychological states by engineered simulation.').

omega_variable(
    crowding_out_alternative_learning,
    'Does satisfying regulators through documented drill hours actively suppress near-miss reporting, foreign-incident study, and normal-operation learning — or do those channels decline for reasons independent of the drill regime?',
    'Training-budget and attention time-series within regulated organizations cross-referenced with compliance-cycle timing; natural experiments in jurisdictions that accept alternative evidence portfolios.',
    'If crowding-out is causal, the regime''s suppression is higher than the scalar encodes, and this reading is destroying the institutional preconditions of the hybrid sibling — raising the cross-reading stakes of the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_alternative_learning, empirical, 'Whether drill compliance mechanically displaces competing learning channels.').

omega_variable(
    professional_identity_fusion_depth,
    'How much of frontline_operators'' identity_locked exit is professional-identity fusion with drill credentials (self-concept constituted by check-ride and recertification culture) versus ordinary employment dependence?',
    'Post-mandate cohort behavior: if operators freed from drill requirements continue seeking simulation voluntarily at comparable rates, fusion is identity-level; if usage collapses, participation was compliance-driven.',
    'Identity-level fusion sustains the practice even under deregulation (persistence without enforcement, rope-like robustness); compliance-driven participation means the mandate is load-bearing and removal collapses the practice outright.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_identity_fusion_depth, empirical, 'Depth of the identity-lock binding frontline operators to the drill regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 8, 0.17).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 32, 0.28).
narrative_ontology:measurement_basis(cata_tr_t32, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t44, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 44, 0.32).
narrative_ontology:measurement_basis(cata_tr_t44, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 8, 0.26).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 16, 0.29).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 24, 0.31).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 32, 0.33).
narrative_ontology:measurement_basis(cata_be_t32, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 40, 0.34).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t44, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 44, 0.34).
narrative_ontology:measurement_basis(cata_be_t44, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t8, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 8, 0.34).
narrative_ontology:measurement_basis(cata_su_t8, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 16, 0.37).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 24, 0.4).
narrative_ontology:measurement_basis(cata_su_t24, observed).
narrative_ontology:measurement(cata_su_t32, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 32, 0.43).
narrative_ontology:measurement_basis(cata_su_t32, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t44, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 44, 0.45).
narrative_ontology:measurement_basis(cata_su_t44, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention__catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention__hybrid_near_miss_learning).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, near_miss_reporting_systems).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how organizations retain catastrophe-avoidance competence' decomposes per the epsilon-invariance principle into three structurally distinct claims with different epsilon values over the same standing regime — this file (simulation suffices), catastrophe_as_necessary_selector (only real catastrophes teach), and hybrid_near_miss_learning (distributed portfolio). Each is a separate story with its own beneficiaries, victims, and classification; they are linked here because the upstream reading's institutionalization (drill-hour compliance) structurally influences the downstream readings' resource environment — documented drill hours crowd out near-miss budgets, which is why near_miss_reporting_systems appears in affects_constraints. No single story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
