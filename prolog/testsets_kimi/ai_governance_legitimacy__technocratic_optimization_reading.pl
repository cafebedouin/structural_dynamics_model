% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__technocratic_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__technocratic_optimization_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ai_governance_legitimacy__technocratic_optimization_reading
 *   human_readable: Technocratic Optimization Reading of AI Governance Legitimacy
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint story models the technocratic optimization reading of AI
 *   governance legitimacy: the claim that authority in AI governance derives
 *   from maximizing aggregate welfare, efficiency, and innovation through
 *   technical expertise. It is one of four contested readings of the
 *   ai_governance_legitimacy kernel. Under this reading, ethical
 *   constraintsâincluding principles articulated in the encyclicalâare
 *   treated as secondary optimization parameters or aspirational values
 *   rather than as primary legitimating grounds. The constraint coordinates
 *   global AI development around shared efficiency metrics, but
 *   asymmetrically extracts from displaced labor, digitally excluded
 *   communities, and populations subject to opaque algorithmic profiling.
 *   Tech firms, investors, high-skill workers, and early adopters are the net
 *   beneficiaries. The structural data (beneficiaries + victims + active
 *   enforcement through regulatory capture and expert consensus) is authored
 *   independently of the claimed type, which follows this reading's
 *   self-presentation as coordination (rope). The engine will measure whether
 *   the metric profile supports that classification or flags tangled_rope /
 *   snare drift.
 *
 * KEY AGENTS:
 *   - tech_firms: Agenda-setter (institutional/global) â define optimization metrics and deploy systems
 *   - investors: Beneficiary (powerful/global) â capture returns from scaled AI deployment
 *   - displaced_workers: Payer (powerless/local) â bear labor-market churn costs
 *   - digitally_excluded_communities: Excluded (powerless/regional) â lack infrastructure to participate in efficiency gains
 *   - algorithmically_profiled_populations: Payer (powerless/national) â subject to opaque optimization-driven decisions
 *   - ethics_policy_observers: Observer (organized/global) â contest the subordination of dignity to efficiency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__technocratic_optimization_reading, 0.37).
domain_priors:suppression_score(ai_governance_legitimacy__technocratic_optimization_reading, 0.45).
domain_priors:theater_ratio(ai_governance_legitimacy__technocratic_optimization_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, extractiveness, 0.37).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__technocratic_optimization_reading, rope).
narrative_ontology:human_readable(ai_governance_legitimacy__technocratic_optimization_reading, "Technocratic Optimization Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__technocratic_optimization_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__technocratic_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__technocratic_optimization_reading, 'e57193c7-89d3-424a-84f5-68ed35d06791').
narrative_ontology:cs_kernel_codification('e57193c7-89d3-424a-84f5-68ed35d06791', implicit).
narrative_ontology:cs_authority_grounding('e57193c7-89d3-424a-84f5-68ed35d06791', expertise).
narrative_ontology:cs_interpretation_layer_present('e57193c7-89d3-424a-84f5-68ed35d06791').
narrative_ontology:cs_reading_relation('e57193c7-89d3-424a-84f5-68ed35d06791', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e57193c7-89d3-424a-84f5-68ed35d06791', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e57193c7-89d3-424a-84f5-68ed35d06791', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('e57193c7-89d3-424a-84f5-68ed35d06791', foundational, aggregate_welfare_as_legitimacy_source).
narrative_ontology:cs_axiom_status(aggregate_welfare_as_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('e57193c7-89d3-424a-84f5-68ed35d06791', aggregate_welfare_as_legitimacy_source, instrumental).
narrative_ontology:cs_axiom('e57193c7-89d3-424a-84f5-68ed35d06791', foundational, expertise_as_authority_ground).
narrative_ontology:cs_axiom_status(expertise_as_authority_ground, holdable).
narrative_ontology:cs_axiom_grounding('e57193c7-89d3-424a-84f5-68ed35d06791', expertise_as_authority_ground, empirically_contingent).
narrative_ontology:cs_reference_frame('e57193c7-89d3-424a-84f5-68ed35d06791', welfare_maximization_framework).
narrative_ontology:cs_drift_state('e57193c7-89d3-424a-84f5-68ed35d06791', post_encyclical_intervention, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e57193c7-89d3-424a-84f5-68ed35d06791', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, tech_firms).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, digitally_excluded_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the optimization metrics (accuracy, efficiency, scale) that govern AI development; deploy the systems that operationalize the technocratic frame; capture revenue from scaled deployment. They set the agenda through lobbying, standard-setting bodies, and control of computational infrastructure. Exit is arbitrage-grade: they can relocate jurisdictions, restructure, or pivot models.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, tech_firms, agenda_setter,
    institutional, generational, arbitrage, global).

% Provide capital to scaled AI ventures and capture returns from efficiency-maximizing deployment. They do not administer the constraint but benefit from its legitimization of rapid, lightly regulated scaling. Exit is mobile across jurisdictions and sectors.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, investors, beneficiary,
    powerful, generational, arbitrage, global).

% Capture wage premiums and career opportunities in the optimization-driven AI sector. Their human capital is in demand, giving them mobility across firms and regions, though they do not set the governance frame.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers, beneficiary,
    moderate, biographical, mobile, national).

% Receive consumer surplus and productivity gains from early access to optimized AI tools. They benefit from the coordination but do not control it; their exit is mobile (they can adopt or abandon tools).
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters, beneficiary,
    moderate, biographical, mobile, national).

% Bear the costs of labor-market churn, deskilling, and wage suppression as AI systems optimize for efficiency. They are structurally trapped: retraining pathways are underfunded, geographic mobility is limited, and social safety nets are designed around the assumption that growth will trickle down.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers, payer,
    powerless, immediate, trapped, local).

% Lack the broadband, devices, and literacy to participate in the efficiency gains coordinated by the constraint. They are excluded from the optimization calculusâtheir absence is treated as a lagging indicator rather than a governance failure. Exit is blocked by infrastructure poverty.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, digitally_excluded_communities, excluded,
    powerless, generational, trapped, regional).

% Subject to opaque, optimization-driven scoring and sorting systems (credit, policing, benefits) whose criteria are proprietary and whose errors are costly. They pay through constrained life chances. Exit is trapped because the profiling is invisible, non-consensual, and often legally shielded.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_populations, payer,
    powerless, biographical, trapped, national).

% Academic, civil society, and religious ethicists who contest the subordination of dignity to efficiency. They analyze and criticize the constraint but do not set its agenda or collect its benefits. Their exit is analytical: they can withdraw attention or shift frames, but their institutional power to alter the constraint is limited.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, ethics_policy_observers, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global AI research, investment, and deployment by establishing aggregate welfare and efficiency as the shared metric of legitimacy, reducing friction between technical communities, capital markets, and regulatory bodies.
% TRANSFER_FUNCTION: Moves economic surplus, governance authority, and labor-market opportunity from displaced workers, excluded communities, and profiled populations to technology firms, investors, credentialed experts, and early adopters.
% ABSENT_VOICES: Workers in sectors vulnerable to automation, communities without broadband or digital literacy, and traditions that treat human dignity as non-fungible are structurally absent from the optimization forums where metrics are set; their costs are externalized as 'trade-offs' rather than entered as constraints.
% DISAPPEARANCE_RATIONALE: If the technocratic optimization frame vanished, AI governance would revert to contested legitimacy principles (democratic mandate, magisterial authority, or market voluntarism), redirecting investment criteria, regulatory standards, and the distribution of authority between engineers and public institutions.
% FOUNDING_PROBLEM: How to coordinate rapidly scaling artificial intelligence capabilities across global markets and technical communities without being paralyzed by incommensurable ethical disagreements or fragmented, incompatible regulatory regimes.
% FOUNDING_PROBLEM_CORROBORATION: Technology firms and expert institutions attest the coordination problem is live and requires efficiency-based governance. Labor advocates, theological ethicists, and democratic theorists attest that the 'problem' is itself a framing maneuver that preempts substantive moral negotiation and democratic accountability. No uncontested corroboration exists from outside the benefiting parties; the corroboration record is polarized.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__technocratic_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__technocratic_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__technocratic_optimization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__technocratic_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__technocratic_optimization_reading, 0.37, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__technocratic_optimization_reading_tests).
:- end_tests(ai_governance_legitimacy__technocratic_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.37) is moderate: the constraint generates genuine coordination benefits (innovation, infrastructure, consumer surplus) but these are accompanied by diffuse externalities (labor displacement, digital exclusion, profiling harms) that are not priced into the optimization function. Suppression (0.45) reflects the structural weight of regulatory capture and expert consensus in marginalizing alternative legitimacy frames (democratic, magisterial, solidaristic). Theater ratio (0.30) captures the performative ethics industryâethics boards, aspiration statements, and 'responsible AI' frameworksâthat operationalizes dignity as a compliance cost rather than a governance foundation. Accessibility collapse (0.58) is moderate: once the efficiency frame is accepted, non-optimizing alternatives appear naive or infeasible, though they persist in subordinate institutional niches. Resistance (0.48) is moderate and growing, driven by labor advocates, civil society, and theological critics. The measurement series show a gradual intensification of extraction and theater as the optimization regime matures, on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats experience the constraint as genuine coordination: a framework that channels innovation and prevents fragmented, inefficient governance. The payer/excluded seats experience it as extraction dressed in technical neutrality: their costs are treated as externalities, and their exit is blocked by the absence of alternative governance infrastructures. The engine will compute per-seat types accordinglyâlikely rope/beneficiary for the tech seat and tangled_rope/snare for the displaced worker seatâproducing the divergence the corpus is designed to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Tech firms and investors sit at the beneficiary end (low d): the constraint subsidizes their accumulation by externalizing costs and legitimating their authority. High-skill workers and early adopters also benefit (low-to-moderate d), though less directly. Displaced workers, digitally excluded communities, and algorithmically profiled populations sit at the target end (high d): they bear the costs of optimization without sharing in its governance. The ethics_policy_observer seat is analytical (d â 0.5) but structurally excluded from agenda-setting power. No directionality overrides are needed because the structural derivation from beneficiary/victim declarations plus exit options correctly maps the asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâhow to coordinate rapidly scaling AI capabilities without paralysisâis still live, preventing a simple piton verdict. However, the constraint shows early theater drift (theater_ratio rising from 0.12 to 0.30) and extraction accumulation, suggesting that even if the coordination function is genuine, the mechanism is layering rent-seeking and cost-shifting onto the original problem. A mandatrophy flag would fire if founding_problem_status were dead while the constraint persisted; here it is contested, so the constraint sits in the ambiguous zone where coordination and extraction are structurally fused. The rope claim is thus plausible as self-description but requires continuous verification against the victim data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_uncertainty,
    'This constraint is one reading of the ai_governance_legitimacy kernel. Do the sibling readings (magisterial_subsidiarity, democratic_pluralist, market_libertarian) represent genuinely coexisting alternatives, or does the technocratic reading''s capture of regulatory and funding institutions structurally marginalize siblings to ceremonial status?',
    'Comparative institutional analysis of agenda-setting power across the four readings in major AI governance forums (OECD, EU AI Act implementation, IEEE standards, Vatican consultations).',
    'If siblings are structurally marginalized, this reading functions as a de facto snare despite its rope self-presentation; if they remain live alternatives, the rope classification is structurally supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_uncertainty, conceptual, 'Whether sibling readings are genuinely coexistent or ceremonially marginalized').

omega_variable(
    aggregate_welfare_measurability,
    'Can aggregate welfare be measured independently of normative framing such that optimization is empirically tractable, or does every welfare metric embed a contestable conception of the good?',
    'Audit of the implicit value commitments in dominant AI evaluation benchmarks and cost-benefit methodologies.',
    'If welfare is inescapably normative, the technocratic reading''s claim to value-neutral coordination collapses, and extraction is revealed as a contested political choice rather than a technical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_welfare_measurability, empirical, 'Whether aggregate welfare is measurable without normative embedding').

omega_variable(
    expertise_authority_erosion,
    'Does the post-encyclical erosion of technocratic authority constitute a temporary legitimacy crisis or a structural shift toward hybrid governance frames?',
    'Track whether AI governance institutions are incorporating non-expert voices (affected communities, democratic assemblies, theological ethicists) into binding decision-making or merely consultative roles.',
    'If binding, the technocratic reading drifts toward tangled_rope as its monopoly fragments; if merely consultative, the rope framing is preserved through theatrical inclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expertise_authority_erosion, empirical, 'Whether authority erosion is temporary or structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__technocratic_optimization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_gov_tech_opt_tr_t0, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ai_gov_tech_opt_tr_t5, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ai_gov_tech_opt_tr_t10, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(ai_gov_tech_opt_tr_t15, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(ai_gov_tech_opt_tr_t20, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(ai_gov_tech_opt_tr_t25, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(ai_gov_tech_opt_tr_t30, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_gov_tech_opt_be_t0, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ai_gov_tech_opt_be_t5, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 5, 0.26).
narrative_ontology:measurement(ai_gov_tech_opt_be_t10, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 10, 0.29).
narrative_ontology:measurement(ai_gov_tech_opt_be_t15, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(ai_gov_tech_opt_be_t20, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(ai_gov_tech_opt_be_t25, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 25, 0.36).
narrative_ontology:measurement(ai_gov_tech_opt_be_t30, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 30, 0.37).

% Suppression requirement over time
narrative_ontology:measurement(ai_gov_tech_opt_su_t0, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_gov_tech_opt_su_t5, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(ai_gov_tech_opt_su_t10, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(ai_gov_tech_opt_su_t15, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(ai_gov_tech_opt_su_t20, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(ai_gov_tech_opt_su_t25, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 25, 0.43).
narrative_ontology:measurement(ai_gov_tech_opt_su_t30, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__technocratic_optimization_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_governance_legitimacy kernel. It is structurally coupled to its sibling readings through shared contestation over the same governance domain. The epsilon values differ because the legitimating principle changes: this reading optimizes welfare; siblings optimize conformity, consent, or exchange.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
