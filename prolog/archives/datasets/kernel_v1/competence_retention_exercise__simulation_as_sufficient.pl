% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: Simulation-as-Sufficient Competence Retention in High-Reliability Organizations
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   The simulation-as-sufficient reading constitutes a institutional
 *   commitment that high-fidelity simulation training provides genuine
 *   exercise of catastrophe-avoidance competence and can serve as the primary
 *   mechanism for maintaining organizational competence across generations of
 *   operators who never experience real catastrophes. This reading
 *   instantiates a specific epistemological claim: the cognitive and
 *   procedural demands of simulator scenarios, when designed with sufficient
 *   fidelity, are structurally equivalent to real catastrophe events,
 *   enabling operators to develop and validate the competence needed to
 *   prevent or manage actual crises. The constraint emerges from a structural
 *   tension in high-reliability organizations: catastrophes are
 *   simultaneously the most informative learning events and the most costly
 *   to experience. If simulation can substitute for catastrophe as a
 *   competence-maintenance mechanism, organizations can avoid the human and
 *   financial costs of learning through failure. If simulation cannot achieve
 *   functional equivalence, organizations that rely exclusively on simulators
 *   risk silent competence degradation — operators may hold valid
 *   certifications while lacking genuine catastrophe-avoidance capacity. The
 *   measurement trajectory shows rising extractiveness and theater ratio over
 *   the interval: simulator certification infrastructure has expanded,
 *   operator career pathways have become increasingly simulator-dependent,
 *   and the institutional claim that simulator performance predicts field
 *   competence has become more difficult to validate as field experience
 *   decreases. The suppression requirement has also risen — field mentorship
 *   alternatives, operator self-assessment, and near-miss incident learning
 *   as alternative competence pathways are increasingly displaced by
 *   simulator-standardized metrics.
 *
 * KEY AGENTS:
 *   - Field Operators: Primary victims (powerless/trapped) — career and certification tied to simulator performance; real-world experiential competence actively de-emphasized; no exit pathway
 *   - Training Infrastructure Providers: Primary beneficiaries (institutional/arbitrage) — capture renewable revenue from simulator licensing, curriculum development, and technology upgrades; can arbitrage into adjacent markets
 *   - Organizational Safety Leadership: Mixed role (powerful/mobile) — benefits from standardized safety metrics and liability reduction but bears risk if field competence degrades
 *   - Regulatory Bodies: Institutional actors (institutional/constrained) — enforce simulator requirements; experience mixed coordination (standardized safety) and extraction (suppression of alternative competence validation)
 *   - Safety Engineering Community: Moderate power (moderate/constrained) — benefits from standardized assessment but bears risk of skill atrophy; constrained exit due to regulatory lock-in
 *   - Regulatory Reform Coalition: Organized challengers (organized/constrained) — safety unions, field worker associations, near-miss incident networks building alternative competence validation pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contested institutional arrangement as an inherent feature of human learning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.38).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.52).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "Simulation-as-Sufficient Competence Retention in High-Reliability Organizations").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, 'df4bc734-066f-4b2a-a5b1-1dde0a876d61').
narrative_ontology:cs_kernel_codification('df4bc734-066f-4b2a-a5b1-1dde0a876d61', formalized).
narrative_ontology:cs_authority_grounding('df4bc734-066f-4b2a-a5b1-1dde0a876d61', extraction).
narrative_ontology:cs_interpretation_layer_present('df4bc734-066f-4b2a-a5b1-1dde0a876d61').
narrative_ontology:cs_reading_relation('df4bc734-066f-4b2a-a5b1-1dde0a876d61', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_reading_relation('df4bc734-066f-4b2a-a5b1-1dde0a876d61', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('df4bc734-066f-4b2a-a5b1-1dde0a876d61', foundational, simulator_fidelity_can_achieve_cognitive_equivalence).
narrative_ontology:cs_axiom_status(simulator_fidelity_can_achieve_cognitive_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('df4bc734-066f-4b2a-a5b1-1dde0a876d61', simulator_fidelity_can_achieve_cognitive_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('df4bc734-066f-4b2a-a5b1-1dde0a876d61', foundational, competence_maintenance_without_catastrophe_experience).
narrative_ontology:cs_axiom_status(competence_maintenance_without_catastrophe_experience, holdable).
narrative_ontology:cs_axiom_grounding('df4bc734-066f-4b2a-a5b1-1dde0a876d61', competence_maintenance_without_catastrophe_experience, instrumental).
narrative_ontology:cs_reference_frame('df4bc734-066f-4b2a-a5b1-1dde0a876d61', simulator_fidelity_equivalence_framework).
narrative_ontology:cs_drift_state('df4bc734-066f-4b2a-a5b1-1dde0a876d61', contemporary_incident_driven_learning, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('df4bc734-066f-4b2a-a5b1-1dde0a876d61', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_infrastructure_providers).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, organizational_management).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, risk_mitigation_credibility).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, field_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, experiential_learning_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD OPERATOR (SNARE) — Powerless to exit the simulator-first regime. Career advancement and certification tied to simulator performance metrics. Structural inability to argue that real-world tacit knowledge is valid; real-world experiential competence is actively de-emphasized. No alternative pathway to establish competence. Maximum experienced extraction because the constraint forecloses the operator's own epistemic authority about their own competence.
constraint_indexing:constraint_classification(competence_retention_exercise__simulation_as_sufficient, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SAFETY ENGINEERING COMMUNITY (TANGLED_ROPE) — Moderate power, constrained exit. Benefits from simulator-standardized competence assessment (reduces liability, creates measurable safety metrics) but also bears costs of potential skill atrophy in the field. Exit is costly — abandoning simulator infrastructure requires regulatory approval and institutional coordination. Mixed coordination (standardized safety measurement) and extraction (suppression of alternative competence pathways).
constraint_indexing:constraint_classification(competence_retention_exercise__simulation_as_sufficient, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRAINING INFRASTRUCTURE PROVIDER (ROPE) — Institutional actor with arbitrage options. Primary beneficiary — captures renewable revenue streams from simulator licensing, maintenance, and curriculum updates. Experiences the constraint as coordination: simulator-standardization enables market expansion and regulatory legitimacy. Low experienced extraction because the agent benefits from the constraint's expansion and can arbitrage into adjacent markets (virtual reality, gamification, AI-adaptive training).
constraint_indexing:constraint_classification(competence_retention_exercise__simulation_as_sufficient, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — Organized agents (safety unions, field worker associations, accident investigator networks) see simulator-sufficiency as a temporary institutional frame with a sunset. Field-based feedback loops (near-miss reporting, operator debriefs, root-cause analysis) are building alternative competence validation pathways. Sunset mechanism: as near-miss incident databases mature and predictive analytics improve, the need for high-fidelity simulator training as the sole competence measure diminishes. Exit path is visible but constrained by regulatory lag.
constraint_indexing:constraint_classification(competence_retention_exercise__simulation_as_sufficient, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL CERTIFICATION APPARATUS (PITON) — The formal claim that 'simulator performance equals field competence' is substantially performative. Regulators and organizations know that simulator training produces certification but are uncertain whether it produces genuine catastrophe-avoidance competence at scale. The apparatus persists through institutional inertia and liability protection rather than because it provably works. High theater ratio reflects that certification rituals substitute for systematic field validation. The institution sees its own process as degraded but maintains it because alternatives require renegotiation of liability frameworks.
constraint_indexing:constraint_classification(competence_retention_exercise__simulation_as_sufficient, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZATIONAL SAFETY LEADERSHIP (TANGLED_ROPE) — Powerful institutional actor with mobile exit options. Benefits from simulator-standardization (measurable safety metrics, liability reduction, regulatory compliance) but also depends on genuine field competence to prevent catastrophes. Mobile exit because the organization can invest in alternative competence validation (field mentorship, near-miss incident learning systems, adaptive human performance monitoring) without losing regulatory standing. Mixed experience: coordination function (standardized safety culture) + extraction (suppression of alternative competence measurement methods).
constraint_indexing:constraint_classification(competence_retention_exercise__simulation_as_sufficient, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational frame, some gap between training and real-world competence is inherent to complex skill transfer: no simulator can fully replicate the cognitive load, environmental variability, and consequence permanence of real catastrophe events. This perspective sees simulator-sufficiency as an impossible claim grounded in the nature of human learning. However, the structural data contradicts pure natural law — the constraint has beneficiaries, victims, and active enforcement. The engine will classify this as a false summit, revealing that 'impossible by nature' naturalizes what is actually a contested institutional arrangement.
constraint_indexing:constraint_classification(competence_retention_exercise__simulation_as_sufficient, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_retention_exercise__simulation_as_sufficient, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_retention_exercise__simulation_as_sufficient, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, TR),
    TR >= 0.70.

:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint produces asymmetric benefits: training infrastructure providers capture renewable revenue while field operators face career constraints tied to simulator metrics. However, extractiveness is not severe because the constraint also produces genuine safety coordination benefits (standardized competence measurement, documented liability reduction, reduced catastrophe frequency) that distribute across the system. The training infrastructure market is competitive enough that extraction is partially offset by service differentiation. Suppression (0.52): Moderate-high. Significant barriers to alternative competence pathways: regulatory requirement for simulator certification, organizational investment in simulator infrastructure, career risk of questioning simulator-sufficiency, and suppression of field-based feedback loops as primary competence validation. But suppression is not total — near-miss incident systems, safety unions, and field mentorship programs continue to operate; they are de-emphasized rather than eliminated. Theater ratio (0.58): Moderate-high, rising over the interval. Simulator certification produces measurable, auditable competence claims that satisfy regulatory requirements and liability frameworks. However, the theater has increased as field experience has become rarer — the institutional claim that simulator performance predicts catastrophe-avoidance competence is harder to validate when operators have never faced real crises. Certification rituals substitute for systematic field validation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. The training infrastructure provider (institutional/arbitrage) sees pure coordination and market opportunity (Rope) — simulator standardization enables business expansion and regulatory legitimacy. The field operator (powerless/trapped) sees extraction and foreclosure of alternative competence pathways (Snare) — career advancement requires simulator performance metrics they cannot challenge. The safety engineering community (moderate/constrained) sees mixed coordination and extraction (Tangled Rope) — genuine safety benefits alongside skill suppression. The reform coalition (organized/constrained) sees a temporary regime with a sunset (Scaffold) — near-miss incident learning and field-based validation will eventually provide alternative frameworks. The certification apparatus (institutional/arbitrage) sees its own degradation (Piton) — simulator-sufficiency is maintained through institutional inertia and liability protection rather than proven effectiveness. The organizational leadership (powerful/mobile) sees mixed gains: standardized safety culture alongside potential field competence risks (Tangled Rope). The analytical observer (analytical/analytical) risks seeing an immutable law of human learning (Mountain) but the structural data reveals this as a false summit — simulator-sufficiency is a contested institutional commitment with identifiable beneficiaries and victims, not an inherent feature of knowledge transfer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the competence-retention exercise constraint. Training infrastructure providers benefit from expansion and lock-in (low d, derived from beneficiary status + arbitrage exit options → negative f(d)). Field operators bear costs without agency (high d, derived from victim status + trapped exit → high f(d)). Organizational leadership balances benefits (standardized metrics) and risks (competence atrophy), with mobile exit options enabling some hedging (moderate d, derived from mixed beneficiary-victim status + mobile exit). The safety engineering community experiences both standardized assessment (benefit) and skill suppression (cost), with constrained exit due to regulatory requirements (moderate d). Regulators enforce simulator requirements (partial beneficiary status — reduced liability) but also face risks if field competence degrades (partial victim status), with constrained exit (moderate d). The reform coalition organizes around alternative competence pathways (victim-adjacent), with organized power and constrained exit (moderate-high d). The piton classification emerges from the theater gate rather than from severe experienced extraction — the institutional apparatus maintains simulator-sufficiency through certification ritual rather than functional competence validation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by making explicit that 'simulator performance predicts catastrophe-avoidance competence' is an institutional reading rather than an empirical fact. The mandatrophy arises from the tension between coordination (simulators do standardize safety culture and reduce liability) and extraction (operators are constrained to simulator-only career pathways and real-world experiential competence is suppressed). The constraint is genuinely tangled — it produces real safety benefits while also suppressing alternative competence validation. The resolution is not to declare one side 'correct' but to identify the omega variables that would permit resolution: What fidelity threshold would demonstrate functional equivalence? Do simulator-only operators show measurable competence degradation? Do near-miss incident learning systems provide sufficient feedback? Is simulator-sufficiency a legitimate operational proxy or an institutional fiction? These questions map directly to the reading contest — they are the empirical and conceptual tests that would determine whether this reading forecloses its siblings or coexists with them in organizational practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_equivalence_threshold,
    'What level of simulator fidelity constitutes ''structurally equivalent'' cognitive and procedural demands to real catastrophe events?',
    'Comparative neuroscience: stress-response hormones, decision-tree activation patterns, error-recovery time under time-pressure across simulator vs actual incident scenarios. Longitudinal operator interviews: subjective mapping of simulator training intensity to real-world incident response.',
    'If threshold is achievable: simulation-as-sufficient is coherent. If threshold requires full catastrophe-level stakes (emotional, financial, reputational consequence): simulator can never be sufficient, and this reading forecloses to catastrophe_as_necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_equivalence_threshold, empirical, 'Whether simulator fidelity can achieve equivalence to real catastrophe-level demands').

omega_variable(
    field_competence_atrophy_rate,
    'Do operators trained exclusively on simulators without field experience show measurable competence degradation in rare real-world catastrophe scenarios?',
    'Incident data analysis: performance metrics (response time, decision quality, error rate) for operators with high-simulator-only training vs operators with mixed simulator + field experience, controlled for incident severity and operator tenure. Post-incident investigation reports.',
    'If atrophy is significant: simulator-sufficiency fails empirically, field operators are trapped in a degraded regime. If atrophy is negligible: simulator-sufficiency is validated, and the snare classification may reclassify to rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(field_competence_atrophy_rate, empirical, 'Observable field competence degradation in simulator-only trained operators').

omega_variable(
    near_miss_learning_closure,
    'Do near-miss incident feedback loops (without full catastrophes) provide sufficient signal to update simulator training and validate operator competence?',
    'Systematic review: correlation between near-miss incident patterns and simulator curriculum updates; incident causality analysis isolating whether near-miss learning closed competence gaps before catastrophe. Comparison with catastrophe-driven learning: does near-miss evidence produce equivalent safety improvements per unit cost?',
    'If near-miss learning is sufficient: scaffold perspective confirmed — simulator + near-miss feedback creates sustainable competence maintenance without requiring catastrophes. If near-miss learning is insufficient: this reading coexists with catastrophe_as_necessary rather than foreclosing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_learning_closure, empirical, 'Whether near-miss incident learning provides sufficient simulator validation').

omega_variable(
    institutional_competence_fiction,
    'Is the claim ''simulator performance demonstrates catastrophe-avoidance competence'' a legitimate operational proxy or an institutional fiction that substitutes liability protection for genuine safety validation?',
    'Longitudinal organizational analysis: do organizations systematically hide or downplay real-world incidents that contradict simulator-certified competence? Are competence metrics from simulators used to avoid investing in field mentorship despite incident investigation recommendations? Do near-miss patterns contradict simulator performance profiles?',
    'If legitimate proxy: simulation-as-sufficient is foundational to organizational learning. If institutional fiction: the piton classification is correct — the constraint persists through certification theater rather than functional competence maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_competence_fiction, conceptual, 'Whether simulator competence certification is a functional proxy or institutional liability protection').

omega_variable(
    reading_contest_foreclosure,
    'Does this reading''s core axiom (simulator fidelity can be functionally equivalent to real catastrophe) logically foreclose the catastrophe_as_necessary reading, or can both coexist as institutional positions?',
    'Philosophical and organizational analysis: Can an organization adopt the simulation-as-sufficient framework while maintaining that catastrophes are also necessary? Do the two readings require incompatible commitment structures or incompatible resource allocations?',
    'If forecloses: this reading and catastrophe_as_necessary cannot coexist within a single organizational framework; one must dominate. If coexists: organizations can hold both readings in tension, deploying simulation-as-sufficient for routine competence maintenance while treating catastrophes as inevitable learning events.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether simulation-as-sufficient forecloses catastrophe_as_necessary or permits coexistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_sim_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.42).
narrative_ontology:measurement(comp_sim_tr_t5, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 5, 0.52).
narrative_ontology:measurement(comp_sim_tr_t10, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_sim_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_sim_be_t5, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(comp_sim_be_t10, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comp_sim_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(comp_sim_su_t5, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(comp_sim_su_t10, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% The competence_retention_exercise kernel decomposes into three structurally distinct constraints, each instantiating a different reading of how organizations should maintain catastrophe-avoidance competence. This constraint (simulation_as_sufficient) has ε=0.38 and is tangled_rope. The catastrophe_as_necessary reading would have higher ε and classify as snare (extraction is explicit and undefended). The near_miss_as_bridge reading would have lower ε and classify as scaffold or rope (alternative validation pathway with sunset or pure coordination logic). The three constraints are linked through reading_relations in their respective cs_structure blocks; they are not independent variations on measurement but fundamentally different institutional commitments with different empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__simulation_as_sufficient, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
