% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Competence Occupation—Hybrid Multi-Mechanism Exercise (Simulation + Procedural + Audit)
 *   domain: organizational_safety/training_effectiveness/competence_maintenance
 *
 * SUMMARY:
 *   Competence occupation in high-reliability organizations (nuclear power,
 *   aviation, rail, chemical processing) requires continuous demonstration
 *   that operators retain the knowledge, procedural fluency, and
 *   decision-making capacity to prevent catastrophic failures. The kernel
 *   under contest is: 'What constitutes adequate competence occupation—what
 *   mechanisms and frequency actually maintain competence and prevent
 *   failure?' This reading (hybrid_occupation) answers: a continuous
 *   multi-mechanism occupation combining simulation, refresher training,
 *   procedural reinforcement, and line audits—without consensus on the
 *   optimal configuration. The constraint exhibits dual character: genuine
 *   coordination function (different mechanisms address different failure
 *   modes; some degree of continuous occupation is necessary) AND asymmetric
 *   extraction (operators bear burden of all four mechanisms; training
 *   providers and regulatory authorities benefit from the mandate; no
 *   empirical resolution of mechanism necessity means burden cannot be
 *   optimized downward). The theater_ratio trajectory (0.42 → 0.58 over the
 *   interval) reflects increasing institutional risk-aversion and
 *   liability-protection motivation: post-incident regulatory responses
 *   typically mandate MORE mechanisms rather than empirically optimizing
 *   existing ones. Extractiveness rising (0.32 → 0.48) reflects accumulating
 *   compliance burden without evidence-based reduction. Suppression rising
 *   (0.55 → 0.62) reflects regulatory tightening of enforcement and
 *   license-suspension consequences for non-compliance. This reading coexists
 *   with two sibling readings that would resolve the kernel differently: (a)
 *   simulation_sufficiency argues only simulation-based drills are necessary,
 *   (b) real_incident_necessity argues only actual catastrophic incidents
 *   provide the authentic conditions necessary to occupy competence.
 *
 * KEY AGENTS:
 *   - Line Operators: Primary victims (powerless/trapped) — bear full multi-mechanism burden without exit; career licensing depends on continuous occupation of all mechanisms; trapped by regulatory enforcement
 *   - Training Infrastructure Providers: Primary beneficiaries (institutional/arbitrage) — revenue streams from all four mechanisms; can adjust service offerings as regulatory requirements shift; benefit from under-optimized (multi-mechanism) mandate
 *   - Regulatory Compliance Authorities: Beneficiary/coordinator (institutional/arbitrage) — mandate clarity and authority preserved through multi-mechanism requirement; low personal cost (operators and providers bear burden); can adjust requirements without organizational disruption
 *   - Safety Research Coalition: Organized agents (organized/constrained) — conducting empirical research on mechanism necessity and sufficiency; constrained by dependency on regulatory access and operator data; building case for evidence-based optimization
 *   - Institutional Compliance Apparatus: Maintainer of theater (institutional/arbitrage) — perpetuates current multi-mechanism configuration through institutional inertia and liability-aversion logic; arbitrage position allows easy exit if political pressure changes, but institutional incentive favors status quo
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.48).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.62).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.48).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Competence Occupation—Hybrid Multi-Mechanism Exercise (Simulation + Procedural + Audit)").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "organizational_safety/training_effectiveness/competence_maintenance").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, 'd67ba520-1478-4217-adb0-7ca90e6a5e08').
narrative_ontology:cs_kernel_codification('d67ba520-1478-4217-adb0-7ca90e6a5e08', fixed_text).
narrative_ontology:cs_authority_grounding('d67ba520-1478-4217-adb0-7ca90e6a5e08', extraction).
narrative_ontology:cs_interpretation_layer_present('d67ba520-1478-4217-adb0-7ca90e6a5e08').
narrative_ontology:cs_reading_relation('d67ba520-1478-4217-adb0-7ca90e6a5e08', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('d67ba520-1478-4217-adb0-7ca90e6a5e08', competence_occupation__real_incident_necessity, influences).
narrative_ontology:cs_axiom('d67ba520-1478-4217-adb0-7ca90e6a5e08', foundational, multi_mechanism_necessity_principle).
narrative_ontology:cs_axiom_status(multi_mechanism_necessity_principle, holdable).
narrative_ontology:cs_axiom_grounding('d67ba520-1478-4217-adb0-7ca90e6a5e08', multi_mechanism_necessity_principle, empirically_contingent).
narrative_ontology:cs_axiom('d67ba520-1478-4217-adb0-7ca90e6a5e08', foundational, mechanism_optimization_incompleteness).
narrative_ontology:cs_axiom_status(mechanism_optimization_incompleteness, holdable).
narrative_ontology:cs_axiom_grounding('d67ba520-1478-4217-adb0-7ca90e6a5e08', mechanism_optimization_incompleteness, empirically_contingent).
narrative_ontology:cs_reference_frame('d67ba520-1478-4217-adb0-7ca90e6a5e08', comprehensive_competence_occupation).
narrative_ontology:cs_drift_state('d67ba520-1478-4217-adb0-7ca90e6a5e08', contemporary_post_incident_environment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d67ba520-1478-4217-adb0-7ca90e6a5e08', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_infrastructure_providers).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, regulatory_compliance_authorities).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, line_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, knowledge_diffusion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LINE OPERATOR (SNARE) — Trapped in perpetual multi-mechanism occupation: cannot exit simulation requirements (career-critical competence card), cannot refuse refresher cycles (regulatory mandate), cannot avoid audit scrutiny (licensing enforcement). High suppression (0.62): refusal of any mechanism triggers regulatory action or license suspension. Low escape routes — the operator's career depends on occupying the competence kernel via all four mechanisms simultaneously. No consensus on what actually maintains competence means burden is maximized, not optimized.
constraint_indexing:constraint_classification(competence_occupation__hybrid_occupation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRAINING PROVIDER (TANGLED ROPE) — Both benefits from and extracts via the hybrid occupation mandate. Genuine coordination function: multiple mechanisms DO address different failure modes (simulation catches procedure lapses, refreshers maintain knowledge in long gaps, line audits catch context-specific degradation). But also exhibits asymmetric extraction: providers capture revenue from all four mechanisms; operators bear time/compliance burden; no party can unilaterally reduce mechanism count without losing certification authority. Constrained exit: providers depend on regulatory mandate but could theoretically exit if standards changed. Mixed experience: partial beneficiary, partial target.
constraint_indexing:constraint_classification(competence_occupation__hybrid_occupation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (ROPE) — Pure coordination perspective. The hybrid mandate is solving a genuine collective action problem: individual training providers would under-invest in refresher cycles and audit mechanisms (cheaper to run only simulations); operators would optimize for minimal compliance theater rather than genuine competence. The regulatory standard coordinates all parties toward comprehensive occupation of the competence kernel. Arbitrage exit: authority can adjust requirements anytime and does not experience cost from the mechanisms themselves. Net beneficiary through institutional authority and mandate clarity.
constraint_indexing:constraint_classification(competence_occupation__hybrid_occupation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SAFETY RESEARCH COALITION (SCAFFOLD) — Organized agents (academic research groups, incident investigation bodies, operational learning networks) see the hybrid mechanism as temporary pending resolution of the underlying research question: 'What is the minimal sufficient set of mechanisms to maintain competence and prevent catastrophic failure?' Current multi-mechanism approach is precautionary (assume all four are necessary until proven otherwise). But active research is systematically testing mechanism efficacy: skill decay curves, simulation-to-real-incident transfer, audit sensitivity to near-miss detection. Sunset logic: as empirical evidence accumulates on mechanism sufficiency, configuration can be optimized downward. Constrained exit because researchers depend on current regulatory structure for access to operators and incident data.
constraint_indexing:constraint_classification(competence_occupation__hybrid_occupation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL COMPLIANCE APPARATUS (PITON) — The four-mechanism requirement has become largely performative at the civilizational level. Regulatory bodies maintain the mandate because nobody wants to be the authority that reduced simulation requirements before a major incident occurred, not because evidence demonstrates all four mechanisms are equally critical. Theater ratio (0.58): significant portion of the multi-mechanism occupation is risk-aversion theater and liability protection (if incident occurs, 'we required four mechanisms' is a defense) rather than optimized competence maintenance. The apparatus knows the configuration is over-determined but institutional inertia prevents simplification. Arbitrage exit: regulators could change rules anytime; they persist in current form through institutional self-protection.
constraint_indexing:constraint_classification(competence_occupation__hybrid_occupation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the hybrid occupation constraint exhibits genuine coordination (multiple mechanisms address distinct failure modes) AND genuine asymmetric extraction (operator burden is maximized across all four mechanisms without consensus on necessity). The constraint is neither natural law nor pure rent-seeking: it represents an under-optimized equilibrium. No single mechanism is sufficient in isolation, but the current configuration lacks evidence for the specific combination and intensity. The analytical observer sees a coordination problem (prevent competence decay) that has been solved via a multi-mechanism mandate without empirical resolution of the trade-offs between comprehensiveness, cost, and actual competence maintenance.
constraint_indexing:constraint_classification(competence_occupation__hybrid_occupation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_occupation__hybrid_occupation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_occupation__hybrid_occupation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_occupation__hybrid_occupation, TR),
    TR >= 0.70.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint exhibits genuine coordination function (prevention of catastrophic failure genuinely requires some degree of continuous competence occupation; mechanisms address distinct failure modes—simulation catches procedure errors, refreshers prevent knowledge decay over long operational gaps, procedural reinforcement maintains protocol fluency, audits detect context-specific degradation and decision-making drift). However, the lack of consensus on optimal configuration means operators bear burden across all four mechanisms without evidence that all four are necessary in the current intensity/frequency combination. The extractive component reflects that training providers and regulatory authorities benefit from preserving the multi-mechanism mandate (revenue, regulatory authority) while operators bear the accumulated cost. The rising trajectory (0.32 → 0.48) reflects that post-incident regulatory response has been to add requirements rather than optimize existing ones—each major incident or near-miss leads to a new mechanism or intensified frequency, without sunsetting prior mechanisms. Suppression (0.62): Moderate-high. Operators face significant barriers to exit: license suspension for non-compliance, employment termination for refusal of required training, regulatory background that flags competence-maintenance violations in future hiring. However, suppression is not total (some jurisdictions have regulatory flexibility, some operators can relocate, some alternatives exist for career continuation). Theater ratio (0.58): Moderate-high. Significant portion of the multi-mechanism occupation is institutional risk-aversion and liability protection rather than optimized competence maintenance. Post-incident regulatory responses typically mandate MORE mechanisms rather than empirically testing sufficiency—'we required comprehensive occupation' becomes an institutional defense. The theater has increased over the interval as institutional memory of past failures fades and regulatory caution hardens into habit.
 *
 * PERSPECTIVAL GAP:
 *   Dramatic perspectival divergence emerges from the same structural constraint. The line operator sees a Snare: trapped in perpetual, under-optimized occupation with no exit and maximum burden. The training provider sees Tangled Rope: genuine coordination problem solved, but also revenue stream protected by lack of optimization. The regulatory authority sees Rope: pure coordination, solving the collective action problem of under-investment in competence maintenance. The safety research coalition sees Scaffold: current configuration is temporary precaution pending evidence-based optimization. The institutional apparatus sees Piton: the multi-mechanism mandate is becoming largely performative, maintained through institutional inertia and liability-aversion logic. The analytical observer sees Tangled Rope: a coordination problem (competence maintenance) solved via a mechanism that also extracts from operators without evidence-based justification for the specific configuration. The core perspectival gap: who experiences the constraint as solved (regulatory authority: Rope) vs. who experiences it as a burden (operators: Snare) reveals that 'coordination' is asymmetric. The regulatory authority is coordinating OTHER PARTIES' behavior, not their own.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from each agent's structural position relative to the extraction flow. Line operators are victims (d ≈ 0.95) with no arbitrage: they bear full burden and have trapped exit, producing high f(d) ≈ 1.42 experienced extractiveness. Training providers are beneficiaries (d ≈ 0.10) with arbitrage exit: revenue protected by mandate, low cost to provider, producing low/negative f(d) ≈ -0.01 experienced extractiveness. Regulatory authority is beneficiary (d ≈ 0.05) with arbitrage exit: mandate authority, low cost, producing negative f(d) ≈ -0.12 experienced extractiveness. The analytical observer (d ≈ 0.72) is neither beneficiary nor victim but sees the asymmetry: produces moderate f(d) ≈ 1.15 analytical extractiveness. The perspectival gap is not merely one of opinion but of structural position: the beneficiaries' low d produces Rope classification; the victims' high d produces Snare classification; the moderate/organized agents with constrained exit see Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The constraint is correctly classified as Tangled Rope because it exhibits BOTH genuine coordination (multiple mechanisms address distinct competence failure modes; some continuous occupation is necessary) AND asymmetric extraction (operator burden is maximized across all four mechanisms without consensus on necessity; training providers and regulators benefit from the under-optimized mandate). The constraint is not pure coordination (Rope) because mechanism sufficiency is contested and the mandate extracts from operators without evidence for the specific configuration. The constraint is not pure extraction (Snare) because significant coordination function exists—removing all mechanisms would genuinely increase catastrophic failure risk. The theater_ratio rising (0.42 → 0.58) indicates that institutional inertia and liability-aversion are increasingly motivating the requirement—but the empirical uncertainty about mechanism necessity is the root cause of the extractive component. If evidence resolved the kernel contest, classification would shift: if simulation_sufficiency is validated, constraint becomes Rope (coordination only). If real_incident_necessity is validated, constraint becomes Snare/Piton (institutional theater masking actual mechanism of competence maintenance). If hybrid occupation is validated with optimized subset of mechanisms, constraint becomes Scaffold (temporary precautionary measure with sunset logic toward evidence-based configuration).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_necessity_threshold,
    'Which subset of the four mechanisms (simulation, refresher, procedural reinforcement, line audit) is actually necessary and sufficient to maintain operator competence and prevent catastrophic failure?',
    'Longitudinal empirical analysis: track skill decay curves across operators exposed to different mechanism subsets (where regulatory flexibility permits); correlate mechanism intensity with incident rates and near-miss detection; controlled variation in simulation cadence, refresher frequency, audit intensity. Post-incident analysis of competence state: did operators who failed have gaps in specific mechanisms or comprehensive gaps?',
    'If only 2–3 mechanisms are necessary: configuration can be optimized downward, reducing operator burden (shifts toward Rope/Scaffold). If all four are equally critical: current Tangled Rope classification is correct and justified. If mechanism necessity is context-specific (depends on operator experience, system design, failure mode): constraint must decompose into separate stories for different contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mechanism_necessity_threshold, empirical, 'Empirical necessity and sufficiency of multi-mechanism combination').

omega_variable(
    simulation_transfer_fidelity,
    'Do operators maintain competence patterns observable in simulation environments when placed in actual line conditions? How much transfer does simulation performance predict for real-incident response?',
    'Comparison of operator performance in high-fidelity simulators vs. actual incident response (confidential incident data); measurement of skill components in simulation (procedural fluency, decision speed, error recovery) vs. their expression in real incidents; analysis of operators who performed well in simulation but failed in incident (transfer gap diagnosis).',
    'High transfer fidelity: simulation is a sufficient mechanism; hybrid occupation may be over-determined. Low transfer: simulation alone is insufficient; multi-mechanism approach justified. Differential transfer (some components transfer, others do not): suggests that mechanism subset optimization is necessary but possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_fidelity, empirical, 'Fidelity of simulation transfer to real incident response').

omega_variable(
    audit_sensitivity_to_early_failure,
    'Do line audit mechanisms reliably detect operators approaching catastrophic failure before incidents occur? What is the lag between audit detection and actual failure risk?',
    'Analysis of audit findings (performance issues, protocol deviations, decision errors) and their correlation with subsequent incident-free periods vs. incident occurrence; measurement of detection latency (how many audit cycles before a failing operator is identified); comparison of audit sensitivity across different audit protocols and audit personnel.',
    'High sensitivity: audits are preventing failure escalation; keep all mechanisms. Low sensitivity: audits are detecting failures after they''ve already occurred (too late); audit mechanism may need fundamental redesign or supplementation with earlier warning signals. Latency too long: audit cycle frequency may be inadequate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_sensitivity_to_early_failure, empirical, 'Audit effectiveness at detecting pre-failure competence degradation').

omega_variable(
    reading_contest_mechanism_necessity,
    'This constraint is one reading of a contested kernel: hybrid_occupation (multi-mechanism required), simulation_sufficiency (simulation alone sufficient), and real_incident_necessity (only actual incidents provide authentic competence proof). Which reading''s core premise best explains the empirical evidence of competence maintenance and failure?',
    'Historical analysis of competence-critical incident data: examine operators who failed despite meeting all four hybrid mechanisms (contradicts hybrid reading); examine operators who maintained competence with only simulation training (supports simulation_sufficiency); examine whether incidents serve as the actual forcing function for competence maintenance (supports real_incident_necessity). Decompose by mechanism: which mechanisms appear in common failure chains, which are redundant.',
    'If hybrid is best explanation: constraint stands as Tangled Rope. If simulation_sufficiency is better supported: hybrid reading is an over-cautious false summit (constraint collapses to Rope). If real_incident_necessity is better supported: hybrid occupation is theater masking the truth that only crisis forces competence maintenance (constraint becomes Snare with piton elements). If evidence is mixed by context: constraint must decompose into separate stories for different organizational/technical domains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_mechanism_necessity, empirical, 'Kernel reading empirical validation — which reading best explains competence maintenance phenomena').

omega_variable(
    regulatory_authority_extraction_motive,
    'To what extent does the regulatory mandate for multi-mechanism occupation serve genuine competence maintenance vs. regulatory self-protection (liability hedge, institutional authority preservation)?',
    'Analysis of regulatory decision-making: are mechanism updates driven by competence research evidence or by incident-response institutional pressure? Review of regulatory justifications for specific mechanism requirements: how much is evidence-based vs. precautionary. Comparison of regulatory standards across jurisdictions: do differences track different competence science or different risk-aversion postures? Interview-based analysis of regulatory officer decision logic (restricted access but valuable signal).',
    'If primarily competence-maintenance driven: regulatory authority''s Rope perspective is justified. If significant extraction/self-protection component: extraction component (Snare/Tangled Rope) is larger than base_extractiveness (0.48) suggests; may need upward revision. If largely self-protection: reframe as primarily Piton (institutional theater) with residual Tangled Rope coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_authority_extraction_motive, empirical, 'Regulatory authority''s motivation mix: competence maintenance vs. institutional self-protection').

omega_variable(
    operator_burden_optimality,
    'Is the operator burden from multi-mechanism occupation (time, cognitive load, compliance cost) optimally distributed, or are there high-burden mechanisms that could be replaced or consolidated without competence loss?',
    'Operator workload analysis: time/effort accounting for each mechanism (simulation hours per year, refresher class time, audit preparation, real-world procedural compliance burden). Correlation of burden distribution with actual competence outcomes: which mechanisms correlate strongest with incident prevention? Are high-burden mechanisms high-payoff? Operator preference and feedback analysis (subject to noise and bias, but informative about perceived vs. actual burden).',
    'If burden is well-matched to mechanism payoff: current Tangled Rope classification appropriate. If high-burden mechanisms are lower-payoff: suggests over-optimization toward comprehensiveness rather than efficiency; may indicate extraction component is larger than measured. If burden could be consolidated (e.g., integrated simulation + procedural training): suggests configuration is not yet optimized, supporting Scaffold perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operator_burden_optimality, empirical, 'Operator burden distribution and alignment with competence maintenance payoff').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_hyb_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(comp_hyb_tr_t5, competence_occupation__hybrid_occupation, theater_ratio, 5, 0.5).
narrative_ontology:measurement(comp_hyb_tr_t10, competence_occupation__hybrid_occupation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_hyb_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(comp_hyb_be_t5, competence_occupation__hybrid_occupation, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(comp_hyb_be_t10, competence_occupation__hybrid_occupation, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(comp_hyb_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comp_hyb_su_t5, competence_occupation__hybrid_occupation, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(comp_hyb_su_t10, competence_occupation__hybrid_occupation, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__real_incident_necessity).

% DUAL FORMULATION NOTE:
% The competence_occupation kernel has three structurally distinct readings, each with its own epsilon and classification. The hybrid_occupation reading (this story) proposes that competence is maintained through a multi-mechanism approach. The simulation_sufficiency reading would claim ε ≈ 0.08 (Rope: pure coordination without extraction). The real_incident_necessity reading would claim ε ≈ 0.65 (Snare: incidents are the actual competence-occupation mechanism; everything else is institutional theater). Each reading's epsilon is ε-invariant under its own observables and classification logic. The network links represent kernel interdependence: if simulation_sufficiency is validated, hybrid_occupation loses its coordination justification and becomes pure extraction (reclassifies to Snare). If real_incident_necessity is validated, both hybrid and simulation readings are revealed as theater.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__hybrid_occupation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
