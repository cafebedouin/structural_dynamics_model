% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe as Necessary Selector for Competence Retention
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the
 *   'catastrophe_as_necessary_selector' reading of the
 *   'catastrophe_avoidance_retention' kernel. The reading asserts that only
 *   actual catastrophes — with their chaos, mortality salience, and
 *   organizational trauma — generate the selection pressure necessary to
 *   maintain competence in high-reliability systems. Long peacetime periods
 *   inevitably produce competence decay; simulation creates false confidence;
 *   industries without recent catastrophes are vulnerable to black swan
 *   re-emergence. The reading claims Mountain status (natural law), but the
 *   authored metrics describe a constraint with substantial extraction
 *   (peacetime decay, trauma costs, black swan risk), active suppression of
 *   alternatives (simulation, near-miss learning), and rising theater
 *   (performative safety rituals replacing genuine competence). This
 *   claim/metric divergence is deliberate: the engine measures it; do not
 *   reconcile.
 *
 * KEY AGENTS:
 *   - high_reliability_organizations: Primary agenda_setter (institutional/identity_locked) — administers safety systems, bears peacetime decay costs, receives post-catastrophe selection pressure
 *   - safety_regulators: Secondary agenda_setter (institutional/constrained) — enforces reactive regulations, captures post-disaster mandate expansion
 *   - frontline_operators: Primary payer (moderate/identity_locked) — bears trauma of catastrophes, competence decay during peacetime, cannot exit professional identity
 *   - simulation_training_industry: Excluded beneficiary (organized/trapped) — would provide peacetime competence maintenance but structurally excluded by 'simulation creates false confidence' narrative
 *   - disaster_survivors: Payer (powerless/trapped) — bear mortality salience and trauma that the reading treats as 'selection pressure'
 *   - public_at_black_swan_risk: Payer (organized/constrained) — bears systemic risk when peacetime decay meets low-probability high-consequence events
 *   - independent_safety_scientists: Observer (analytical/analytical) — study competence decay curves, simulation fidelity, near-miss learning efficacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.65).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.72).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, mountain).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe as Necessary Selector for Competence Retention").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).
domain_priors:emerges_naturally(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'cdaa269b-1530-4387-a1b2-8485fa6c9375').
narrative_ontology:cs_kernel_codification('cdaa269b-1530-4387-a1b2-8485fa6c9375', distributed).
narrative_ontology:cs_authority_grounding('cdaa269b-1530-4387-a1b2-8485fa6c9375', practice).
narrative_ontology:cs_interpretation_layer_present('cdaa269b-1530-4387-a1b2-8485fa6c9375').
narrative_ontology:cs_reading_relation('cdaa269b-1530-4387-a1b2-8485fa6c9375', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('cdaa269b-1530-4387-a1b2-8485fa6c9375', catastrophe_avoidance_retention__hybrid_near_miss_learning, forecloses).
narrative_ontology:cs_axiom('cdaa269b-1530-4387-a1b2-8485fa6c9375', foundational, catastrophe_unique_selection_pressure).
narrative_ontology:cs_axiom_status(catastrophe_unique_selection_pressure, holdable).
narrative_ontology:cs_axiom_grounding('cdaa269b-1530-4387-a1b2-8485fa6c9375', catastrophe_unique_selection_pressure, empirically_contingent).
narrative_ontology:cs_axiom('cdaa269b-1530-4387-a1b2-8485fa6c9375', secondary, simulation_necessarily_creates_false_confidence).
narrative_ontology:cs_axiom_status(simulation_necessarily_creates_false_confidence, holdable).
narrative_ontology:cs_axiom_grounding('cdaa269b-1530-4387-a1b2-8485fa6c9375', simulation_necessarily_creates_false_confidence, empirically_contingent).
narrative_ontology:cs_axiom('cdaa269b-1530-4387-a1b2-8485fa6c9375', secondary, near_miss_learning_insufficient_for_black_swans).
narrative_ontology:cs_axiom_status(near_miss_learning_insufficient_for_black_swans, holdable).
narrative_ontology:cs_axiom_grounding('cdaa269b-1530-4387-a1b2-8485fa6c9375', near_miss_learning_insufficient_for_black_swans, empirically_contingent).
narrative_ontology:cs_reference_frame('cdaa269b-1530-4387-a1b2-8485fa6c9375', catastrophe_as_sole_teacher).
narrative_ontology:cs_drift_state('cdaa269b-1530-4387-a1b2-8485fa6c9375', contemporary_resilience_engineering_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cdaa269b-1530-4387-a1b2-8485fa6c9375', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, complacent_industry_leadership).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, post_disaster_consultants).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, reactive_regulators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, peacetime_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_survivors).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, public_at_black_swan_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, disaster_survivors).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, selection_pressure_requires_real_stakes).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_inevitably_creates_false_confidence).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, near_miss_learning_insufficient_for_black_swans).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate nuclear plants, airlines, chemical facilities. Administer safety management systems and bear peacetime competence decay costs. After catastrophes, receive 'selection pressure' that forces reform but at cost of lives, liability, and public trust. Professional identity fused with the system — cannot exit without organizational dissolution. Justify underinvestment in simulation/near-miss learning by citing 'only real events teach.'
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, high_reliability_organizations, agenda_setter,
    institutional, generational, identity_locked, global).

% Set and enforce safety regulations. Mandate expands dramatically after catastrophes (new rules, budgets, authority). During peacetime, face political pressure to reduce 'burdensome' regulations. Capture post-disaster reform mandate; reactive posture justified by 'we regulate based on lessons learned.' Structural incentive to wait for catastrophes rather than invest in peacetime prevention.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Control room operators, pilots, maintenance crews. Bear the trauma of catastrophes (mortality salience, survivor guilt, career destruction). During peacetime, experience competence decay as training degrades, procedures become ritualistic, and management defers investment. Professional identity fused with the high-hazard role — exiting means abandoning career, community, and self-concept. No voice in whether 'selection pressure' is worth the cost.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Develop high-fidelity simulators, VR training, synthetic environments. Would provide peacetime competence maintenance at fraction of catastrophe cost. Structurally excluded by the 'simulation creates false confidence' narrative that dominates regulatory and industry discourse. Trapped: cannot access the market unless a catastrophe forces regulatory change, but their exclusion prevents proving their value.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_training_industry, excluded,
    organized, biographical, trapped, global).

% Workers and communities directly affected by catastrophes. Bear the mortality salience and organizational trauma that the reading treats as 'necessary selection pressure.' No choice in being the selection mechanism. Long-term health, psychological, and economic costs externalized by the constraint. Their suffering is the 'teaching moment' for the industry.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, disaster_survivors, payer,
    powerless, immediate, trapped, local).

% Populations downwind of nuclear plants, under flight paths, near chemical corridors. Bear systemic risk when peacetime competence decay meets low-probability high-consequence events. Can organize politically but only after catastrophes (too late). During peacetime, 'no accident' record is cited as proof of safety — the absence of evidence is treated as evidence of absence.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, public_at_black_swan_risk, payer,
    organized, generational, constrained, global).

% Researchers in resilience engineering, human factors, organizational safety. Study competence decay curves, simulation fidelity validation, near-miss learning efficacy, cross-industry incident databases. See the full structural picture: competence CAN be maintained without catastrophes (aviation 1990s-2020s, nuclear navy), but the dominant paradigm blocks adoption. Their evidence contests the reading but lacks institutional power to shift the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, independent_safety_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Post-catastrophe learning coordinates industry-wide safety improvements through shared trauma, regulatory response, and mandatory reform — a genuine coordination function that has historically reduced repeat catastrophes in nuclear, aviation, and chemical sectors.
% TRANSFER_FUNCTION: Moves competence from peacetime organizations (which decay) to post-catastrophe organizations (which receive selection pressure), at cost of lives, trauma, and systemic risk borne by frontline operators, disaster survivors, and the public. The transfer is: peacetime decay + catastrophe trauma → post-catastrophe competence.
% ABSENT_VOICES: Simulation/training industry (excluded by 'false confidence' narrative), near-miss learning advocates (resilience engineering community), foreign incident databases (treated as irrelevant), and would-be safety innovators who cannot get funding without a 'lesson learned' from a real disaster. These voices are structurally excluded because their inclusion would falsify the reading's core axiom.
% DISAPPEARANCE_RATIONALE: If the 'only catastrophes teach' constraint vanished overnight, high-hazard industries would be forced to invest in simulation fidelity validation, near-miss analysis systems, and foreign incident learning as primary competence-maintenance mechanisms. Regulatory frameworks would shift from reactive to proactive. The simulation industry would become a core safety infrastructure rather than excluded alternative. Black swan vulnerability would decrease as peacetime decay is actively managed.
% FOUNDING_PROBLEM: Early high-hazard industries (1940s-1970s nuclear, aviation, chemical) had no systematic learning infrastructure; no simulation fidelity, no near-miss databases, no cross-industry sharing. Catastrophes were the only feedback mechanism that forced systemic reform — the first generation of safety regulations were written in blood.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by early nuclear/aviation pioneers (e.g., Admiral Rickover's nuclear navy culture, early FAA accident investigators). However, independent safety scientists (Hollnagel, Woods, Dekker, Leveson) and post-1990s aviation reformers corroborate that the problem is substantially solved: simulation fidelity now validated, near-miss systems (ASAP, NASA ASRS) operational, foreign incident sharing (IAEA, WANO) institutionalized. The reading's beneficiaries (complacent leadership, reactive regulators) are the only parties claiming the problem remains live.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_avoidance_retention__catastrophe_as_necessary_selector),
    narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint extracts competence from peacetime organizations (decay), trauma from catastrophe survivors, and systemic risk from the public — all while the reading claims this extraction is 'necessary selection pressure.' Suppression (0.65) is substantial because the constraint actively suppresses peacetime alternatives: simulation investment is discouraged by 'false confidence' narrative; near-miss reporting systems are underfunded; foreign incident learning is treated as irrelevant. Theater ratio (0.42) rises over time as industries perform safety rituals (compliance paperwork, tabletop exercises) that substitute for genuine competence maintenance. Accessibility collapse (0.78) is high because once an organization accepts 'only catastrophes teach,' alternatives become cognitively inaccessible — the belief itself blocks the evidence that would falsify it. Resistance (0.55) is moderate: simulation advocates, near-miss proponents, and resilience engineering researchers contest the reading but lack institutional power to shift the dominant paradigm.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (regulators, industry leadership post-catastrophe), the constraint appears as genuine coordination: 'We learned from Chernobyl/Deepwater Horizon/Challenger and got safer.' From the payer seats (peacetime operators, frontline workers, public), the same structure operates as enforced extraction: 'We decay during peacetime, pay in blood when catastrophe hits, and the system only reforms after bodies drop.' The engine computes this divergence from the structural data — the authored claim (Mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations are the structural agenda_setters but also payers — they administer the constraint yet suffer peacetime decay (d ~ 0.55, symmetric). Safety regulators are agenda_setters who benefit from post-disaster mandate expansion (d ~ 0.25, beneficiary-tilted). Frontline operators are identity_locked payers — professional identity fused with the system, cannot exit without career destruction (d ~ 0.85). Simulation industry is excluded but would be beneficiary if admitted (d ~ 0.15 if included). Disaster survivors are trapped payers (d ~ 0.95). Public is constrained payer bearing black swan risk (d ~ 0.7). The reading's claim that 'catastrophe is necessary' serves as ideological cover that dampens resistance from payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'early high-hazard industries had no systematic learning; catastrophes were the only feedback mechanism' — is dead for mature industries (aviation, nuclear) that now have simulation, near-miss databases, and foreign incident learning. Yet the arrangement persists because: (1) peacetime decay creates recurring 'need' for catastrophe-driven reform, (2) post-disaster consultants and reactive regulators capture the reform mandate, (3) the 'simulation creates false confidence' axiom blocks peacetime alternatives. This is mandatrophy: the mandate (learn from disasters) outlived its function (now we can learn without disasters) but persists via extraction from peacetime decay and trauma.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_belief,
    'Is the claim that only actual catastrophes maintain competence a genuine natural law of organizational dynamics, or a constructed belief that benefits identifiable agents who avoid investing in peacetime safety?',
    'Longitudinal study of industries that invested heavily in simulation/near-miss learning without catastrophes (e.g., commercial aviation post-1990s, nuclear navy) versus those that did not; measure competence decay rates and black swan vulnerability.',
    'If natural law, the constraint is Mountain and FSM does not apply; if constructed belief with beneficiaries, FSM triggers reclassification to tangled_rope and the beneficiary structure becomes the explanatory variable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_belief, conceptual, 'Whether the catastrophe-as-selector claim describes physics or power.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (catastrophe_as_necessary_selector) of the contested kernel catastrophe_avoidance_retention. What would the sibling readings (simulation_as_proxy_catastrophe, hybrid_near_miss_learning) change structurally if they were the operative reading?',
    'Map the beneficiary/victim structures, enforcement requirements, and extraction profiles of each sibling reading; identify which structural elements shift and which remain invariant across readings.',
    'If sibling readings produce substantially different ε and stakeholder maps, the kernel is a genuine site of contestation; if they converge, the dispute is semantic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: which structural elements are reading-dependent vs kernel-invariant.').

omega_variable(
    suppression_mechanism_peacetime,
    'Is the suppression of peacetime competence-maintenance alternatives (simulation, near-miss analysis, foreign incident learning) structural (regulatory capture, budget allocation rules) or internalized (industry belief that ''we don''t need it until something happens'')?',
    'Post-catastrophe policy tracing: if suppression mechanisms dissolve after a catastrophe (budgets open, simulation purchased, near-miss systems implemented), the suppression was structural; if beliefs persist despite resource availability, internalized component dominates.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agents even after regulatory reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_peacetime, empirical, 'Structural vs internalized suppression of peacetime learning alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t0, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t10, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t20, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t30, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t40, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t50, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t0, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t10, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t20, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t30, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 40, 0.7).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t40, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t50, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 50, 0.72).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t0, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t10, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t20, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t30, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t30, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t40, observed).
narrative_ontology:measurement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t50, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 50, 0.65).
narrative_ontology:measurement_basis(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, hybrid_near_miss_learning).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, post_disaster_regulatory_capture).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, near_miss_reporting_suppression).

% DUAL FORMULATION NOTE:
% This constraint is one member of the catastrophe_avoidance_retention constraint family. The kernel decomposes into three structurally distinct readings with different ε values: catastrophe_as_necessary_selector (high ε, Mountain claim, FSM candidate), simulation_as_proxy_catastrophe (moderate ε, Rope claim if simulation works, Snare if it doesn't), hybrid_near_miss_learning (low ε, Tangled Rope claim — coordination via distributed learning with extraction from legacy-only organizations). All three are linked via affects_constraints. The ε-invariance principle requires separate stories because measuring 'competence maintenance' via post-disaster improvement rates vs. simulation fidelity metrics vs. near-miss learning curves yields different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, institutional, 0.3).
constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, moderate, 0.85).
constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
