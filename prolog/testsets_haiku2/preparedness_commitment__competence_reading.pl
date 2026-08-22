% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: institutional/governance/commitment_system
 *
 * SUMMARY:
 *   Preparedness as live exercised knowledge models a constraint where
 *   institutional memory is maintained through continuous active practice
 *   rather than passive documentation. In the competence reading, drills and
 *   scenario exercises test real decision-making capacity; generational
 *   turnover is absorbed through the same exercise mechanism that maintains
 *   the constraint. The constraint's persistence depends on stakeholders'
 *   belief that live exercise is the only reliable way to transfer tacit
 *   knowledge of crisis response. Extraction is low (0.28) because the direct
 *   beneficiaries—institutional capacity and practicing operators—are the
 *   same parties paying the cost; theater is moderate (0.22) because some
 *   exercises are ritual reinforcement of commitment rather than novel
 *   capability testing, but the overall function remains genuine. The
 *   measurement series tracks modest increase over time as budget pressure
 *   mounts and some theater creeps in, then stabilizes as the institution
 *   rebalances.
 *
 * KEY AGENTS:
 *   - Institutional stewards: allocates training budget and exercises; bears administrative overhead and opportunity cost
 *   - Practicing operators: inherits competence through drills; executes decisions with rehearsed knowledge; pays cost of perpetual training disruption
 *   - Generational cohorts: incoming personnel acquire competence through mentored participation in live exercises; knowledge transfer tested in drills
 *   - Institutional memory: the collective embodied knowledge; survives only if transfer mechanism is itself live
 *   - Budgeting authority: decides whether to fund the ongoing cycle or redeploy resources; observes whether exercises test real decisions or are theatrical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.28).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.15).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "institutional/governance/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, 'e76eb2dd-cdda-4fd8-8b26-3c4597be36ca').
narrative_ontology:cs_kernel_codification('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca', formalized).
narrative_ontology:cs_authority_grounding('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca', lineage).
narrative_ontology:cs_interpretation_layer_present('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca').
narrative_ontology:cs_reading_relation('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca', foundational, live_exercise_transmits_tacit_knowledge).
narrative_ontology:cs_axiom_status(live_exercise_transmits_tacit_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca', live_exercise_transmits_tacit_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca', foundational, competence_survives_turnover_through_active_practice).
narrative_ontology:cs_axiom_status(competence_survives_turnover_through_active_practice, holdable).
narrative_ontology:cs_axiom_grounding('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca', competence_survives_turnover_through_active_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca', live_competence_as_founding_commitment).
narrative_ontology:cs_drift_state('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca', contemporary_budget_pressure_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e76eb2dd-cdda-4fd8-8b26-3c4597be36ca', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, continuous_institutional_capacity).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, generational_knowledge_transfer).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, crisis_response_readiness).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, practicing_operators).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, generational_cohorts).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, practicing_operators).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, adaptive_organizational_learning).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, memory_as_embodied_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the regimen of live drills, scenario testing, and competence validation. Allocates resources to training cycles, refresher certification, after-action review. Bears the recurring operational cost of maintaining live exercised knowledge through each generational cohort. Decisions are made through deliberative processes that weigh readiness against budget constraints.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, institutional_stewards, agenda_setter,
    institutional, generational, mobile, national).

% Gain from continuous competence maintenance: they execute decisions in live crises with rehearsed knowledge, face lower error rates, have muscle-memory integration of complex procedures. They also bear the cost of perpetual training cycles, regular scenario exercises that disrupt normal operations, and the cognitive load of maintaining readiness in parallel with routine work. The constraint is experienced as the price of genuine preparedness.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, practicing_operators, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, practicing_operators, payer).

% Incoming cohorts inherit operational competence through structured mentorship embedded in the exercise cycle. Knowledge transfer is tested in live drills: gaps surface immediately when younger personnel execute decisions alongside experienced ones. Cohort turnover does not break the constraint because competence transfer happens through the same mechanism that maintains it—active exercise.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, generational_cohorts, beneficiary,
    organized, biographical, constrained, national).

% Allocates resources for the exercise cycle. Faces pressure to reduce training costs or redeploy personnel to non-crisis-related work. In the competence reading, maintains commitment to funding because the alternative—loss of institutional capacity—is worse than the ongoing cost. This seat can see whether training cycles actually test decision-making or are merely theatrical.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, budgeting_authority, observer,
    institutional, biographical, mobile, national).

% Real events that test the constraint's actual function. In this reading, crises are not performative tests but authentic demand signals. If operators fail a real crisis, the constraint has failed—the competence was not live. The constraint's existence is vindicated by success in real crises; the drills are practice for the real thing, not substitutes.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, crisis_events, excluded,
    analytical, immediate, analytical, universal).
narrative_ontology:stakeholder_non_agent(preparedness_commitment__competence_reading, crisis_events).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains institutional capacity to respond to crises by ensuring that knowledge of response procedures remains embodied in active operators through continuous live exercise. Solves the problem of generational knowledge loss: without the constraint, each new cohort must re-derive procedures from documents, losing the experiential learning that prevents critical errors under stress.
% TRANSFER_FUNCTION: Moves institutional resources (personnel time, budget for simulation and scenario development, opportunity cost of disrupted routine operations) into the maintenance of operational competence across generational turnovers. The transfer is not to an identifiable beneficiary but to the sustained capacity of the organization itself.
% ABSENT_VOICES: Personnel who could argue for abandoning the exercise cycle and relying instead on documentation and just-in-time training are structurally excluded: they would not discover the incompleteness of their knowledge until a real crisis revealed it. Their argument (exercises are wasteful overhead) cannot be heard as a voice in the deliberation; it can only be expressed after failure.
% DISAPPEARANCE_RATIONALE: If live exercised knowledge disappeared overnight, institutional capacity for crisis response would degrade with each generational turnover. Documentation and procedure manuals alone cannot transmit the tacit knowledge of decision-making under uncertainty. Within a few years, a crisis would reveal the loss; the institution would face catastrophic failure or be forced to rebuild competence at crisis cost rather than planned cost.
% FOUNDING_PROBLEM: Institutional memory dies with operators if knowledge transfer is passive (written manuals, lectures). Active exercise tests whether knowledge is actually transferred and catches gaps before they cause crisis failure. The founding problem is: how does an organization remember across generations when humans are the storage medium?
% FOUNDING_PROBLEM_CORROBORATION: Post-crisis analyses consistently find that organizations with continuous exercise cycles recover faster and make fewer compounding errors than those without; this is documented in independent after-action reviews across military, emergency management, and public health domains. Personnel who have participated in both live exercise and crisis response attest that the muscle-memory integration from drills is the difference between executing procedures and improvising under stress.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) and stable across the interval because the constraint is a genuine coordination mechanism: the institutional stewards and operators both benefit from the same process that costs them. The slight rise over the interval (0.22 to 0.29) reflects mounting budget pressure and the marginal intrusion of ritual—some exercises become rote rather than probing—but this is suppressed by the continuing belief that live exercise is necessary. Theater ratio (0.22) is moderate because the constraint legitimately involves performative elements (ceremonial reaffirmation of commitment, visible drill cycles) that serve a real function: making the commitment visible and maintaining stakeholder buy-in. The measurements share one time grid; every metric is authored at every point to avoid misalignment-driven type transitions. The interval spans 40 years (generation-scale); the slow accumulation of theater reflects institutional friction (budget cycles become routine, exercises lose novelty) balanced against the persistent validation of live exercise by post-crisis reviews.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional steward seat, the constraint is a necessary investment in sustained capacity—the alternative (documentation-only) is cheaper short-term but catastrophic long-term. From the practicing operator seat, the constraint is the price of genuine preparedness; the drills feel costly and disruptive but deliver the muscle-memory integration that prevents critical errors under stress. The budgeting authority seat sees the constraint as an ongoing unfunded mandate vulnerable to cost-cutting; this seat must continually re-validate the choice to fund exercises rather than deploy personnel elsewhere. The generational cohort seat experiences the constraint as mentorship embedded in practice—knowledge transfer happens naturally through participation in exercises. The engine computes these divergent d values from the structural data: institutional stewards are near the beneficiary end (they gain institutional capacity without collecting extraction); practicing operators are near symmetric (genuine coordination, but the disruption cost is real); budgeting authority is analytical (observes but does not directly collect or pay). The claim (rope) matches the computed type because the constraint solves a genuine coordination problem with minimal coercive overhead—stakeholders participate because they believe live exercise works, not because they are forced to.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (continuous_institutional_capacity, generational_knowledge_transfer, crisis_response_readiness) are not agents; they are the coordination targets that accrue to all stakeholders who maintain the constraint. The practicing operators are the primary net beneficiaries: they survive crises with lower error rates and cognitive confidence. The institutional stewards benefit by sustaining the organization's long-term capacity. The budgeting authority benefits only if commitment persists—if the authority cuts funding and a crisis reveals lost competence, the authority's seat becomes highly vulnerable. Exit options differentiate stakeholder positions: institutional stewards are mobile (they can choose to defund and redeploy, though the consequences are severe); practicing operators are constrained (they cannot exit the organization easily without losing the competence they have built); generational cohorts are constrained (they must participate in the training cycle to acquire competence). The spatial scope is national for all stakeholder seats: preparedness operates within institutional boundaries, and cohorts are drawn from the same labor pool.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not at risk of mandatrophy in the competence reading because the founding problem (knowledge transfer across generations) remains live and is continuously validated by post-crisis observations. The risk of mandatrophy is different in the husk reading, where exercises become purely ceremonial and the founding problem is no longer believed. In the competence reading, the exercise cycle persists because it demonstrably produces better crisis outcomes. The theater ratio reflects institutional inertia (some exercises become routine) but does not yet displace the real function. Mandatrophy would emerge if theater ratio crossed 0.5, indicating that performative maintenance had become the primary function. The measurement trajectory shows theater rising to 0.23 by midpoint then stabilizing, suggesting that the constraint maintains its function-first posture even under budget pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    live_exercise_sufficiency,
    'Is continuous live exercise the only reliable mechanism for maintaining institutional knowledge of crisis response across generational turnovers, or can competence be preserved through documentation, periodic refresher training, and simulation?',
    'Comparative analysis of post-crisis outcomes in institutions with continuous live exercise versus those with documentation-primary models; tracking knowledge loss rates after cohort turnover in both regimes.',
    'If live exercise is necessary, the constraint''s coordination function is genuine and cannot be replaced by cheaper alternatives without degradation. If competence can be maintained through hybrid approaches with lower cost, the constraint may be over-specified and extractive overhead could be reduced without loss of function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(live_exercise_sufficiency, empirical, 'Whether live exercise is the necessary vs. merely sufficient mechanism for competence transfer.').

omega_variable(
    theater_threshold_ambiguity,
    'At what point does the theater ratio (ritual and performative elements of the exercise cycle) become the primary function, displacing the competence-testing function? What observable shifts signal this transition?',
    'Analysis of exercise design and execution: do drills probe novel scenarios and decision constraints, or do they follow rote templates? Do after-action reviews surface real capability gaps, or do they become ceremonial validation of existing procedures? Do operators report that exercises change their decision-making, or that exercises feel like requirements to be completed?',
    'If theater has already become primary, this constraint may be transitioning toward the husk reading. If theater is still subordinate to function, the competence reading remains accurate. The threshold varies by institutional context and stakeholder perception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_threshold_ambiguity, empirical, 'The point at which performative maintenance displaces genuine function.').

omega_variable(
    committer_frame_rehearsing_function,
    'Is this reading a claim about an actual commitment system that preparedness instantiates (a kernel text or doctrine that authorizes the exercise cycle), or is it describing what the exercise cycle functionally does without reference to a legitimizing kernel?',
    'Examination of institutional charters, legislation, and doctrine: do they claim that preparedness persists through live exercise as a foundational principle, or are exercises described as optional tools? Do institutional actors cite a kernel doctrine to justify the exercise cycle, or do they justify exercises by reference to post-crisis outcomes?',
    'If a kernel text legitimizes the competence reading, the constraint operates as a commitment system; if there is no kernel doctrine and the exercise cycle is pragmatically justified by its effectiveness, the constraint is not a commitment system and the cs_structure block may be over-specified. The reading_relations and axioms assume a kernel contest; if no kernel is actually being interpreted by different parties, the committer frame is inaccurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_rehearsing_function, conceptual, 'Whether the competence reading is an authorized interpretation of a kernel commitment or a pragmatic description of functional causation.').

omega_variable(
    generational_cohort_identity_lock,
    'Do practicing operators become identity-locked to the institutional competence cycle such that they cannot exit even if external alternatives became available? Is the constraint extractive at the relational level even if it is coordinative at the institutional level?',
    'Longitudinal analysis of operator career trajectories and mobility: do operators who have undergone extensive training cycles remain trapped in the institutional context, or can they transition to other employment with portable skills? Do operators report subjective identity fusion with the institution?',
    'If identity-locked, the constraint carries suppressed extraction of relational autonomy: operators are coordinated into a single loyalty structure and cannot credibly threaten exit. This would argue for increasing the suppression metric and reconsidering whether the constraint is genuinely symmetric between stewards and operators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_cohort_identity_lock, empirical, 'Whether generational cohorts become relationally identity-locked through competence training.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__competence_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__competence_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__competence_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__competence_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_commitment__competence_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(prep_tr_t30, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__competence_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__competence_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__competence_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__competence_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_commitment__competence_reading, base_extractiveness, 25, 0.29).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement_basis(prep_be_t30, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_commitment__competence_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__competence_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_commitment__competence_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__competence_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(prep_su_t20, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_commitment__competence_reading, suppression_requirement, 25, 0.16).
narrative_ontology:measurement_basis(prep_su_t25, observed).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__competence_reading, suppression_requirement, 30, 0.16).
narrative_ontology:measurement_basis(prep_su_t30, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__competence_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement_basis(prep_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel admits three distinct readings, each instantiating a different constraint with different ε values and structural assumptions. The competence_reading (this file) asserts that live exercised knowledge is the operative mechanism and extraction is low because stakeholders benefit from the same process. The husk_reading would claim exercises have become ceremonial and the constraint is sustained by theater rather than function. The hybrid_reading would assert both functions coexist in a layered system. All three are linked via network.affects_constraints to model the constraint family structure and enable cross-reading contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__competence_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
