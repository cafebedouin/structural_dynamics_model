% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__technocratic_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ai_governance_legitimacy__technocratic_optimization_reading
 *   human_readable: AI Governance Legitimacy via Aggregate Welfare/Efficiency Optimization (Technocratic Reading)
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This story instantiates the technocratic optimization reading of the
 *   contested ai_governance_legitimacy kernel: legitimacy is grounded in
 *   demonstrated aggregate welfare, efficiency, and innovation gains, with
 *   ethical principles (including those articulated in Catholic social
 *   teaching encyclicals on technology and human dignity) treated as
 *   feasibility constraints to be balanced against growth rather than binding
 *   floors. This is a genuine coordination mechanism — it lets firms,
 *   investors, and standards bodies converge quickly on deployment decisions
 *   using shared quantifiable criteria — but the coordination rides alongside
 *   asymmetric extraction: benefits concentrate on capital and technical
 *   labor, while adjustment costs land on displaced workers,
 *   infrastructure-poor communities, and profiled individuals who have no
 *   voice in benchmark design. Sibling readings
 *   (magisterial_subsidiarity_reading, democratic_pluralist_reading,
 *   market_libertarian_reading) are distinct constraints with different ε,
 *   different beneficiary/victim structures, and different enforcement
 *   mechanisms; they are not alternate measurements of this constraint.
 *
 * KEY AGENTS:
 *   - large_technology_firms: agenda_setter/beneficiary (institutional/arbitrage) — sets benchmarks, captures rulemaking
 *   - venture_investors: beneficiary (powerful/arbitrage) — funds growth-friendly deployment
 *   - high_skill_technical_workers: beneficiary (moderate/mobile) — commands premium from expertise-as-authority framing
 *   - early_adopter_consumers: beneficiary (moderate/mobile) — accesses gains before regulatory catch-up
 *   - displaced_low_skill_workers: payer (powerless/trapped) — absorbs automation costs uncompensated
 *   - digitally_underserved_communities: payer (powerless/trapped) — counted in aggregate metrics but excluded from benefit
 *   - algorithmically_profiled_populations: payer (powerless/trapped) — bears concentrated harm masked by aggregate accuracy
 *   - technical_standards_bodies: agenda_setter/observer (institutional/constrained) — legitimizes framing via industry-affiliated certification
 *   - magisterial_and_civil_society_critics: excluded (moderate/constrained) — voice without binding authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__technocratic_optimization_reading, 0.35).
domain_priors:suppression_score(ai_governance_legitimacy__technocratic_optimization_reading, 0.42).
domain_priors:theater_ratio(ai_governance_legitimacy__technocratic_optimization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__technocratic_optimization_reading, rope).
narrative_ontology:human_readable(ai_governance_legitimacy__technocratic_optimization_reading, "AI Governance Legitimacy via Aggregate Welfare/Efficiency Optimization (Technocratic Reading)").
narrative_ontology:topic_domain(ai_governance_legitimacy__technocratic_optimization_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__technocratic_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__technocratic_optimization_reading, 'c128e67f-20aa-4b92-b4b5-284ee51f6189').
narrative_ontology:cs_kernel_codification('c128e67f-20aa-4b92-b4b5-284ee51f6189', distributed).
narrative_ontology:cs_authority_grounding('c128e67f-20aa-4b92-b4b5-284ee51f6189', expertise).
narrative_ontology:cs_interpretation_layer_present('c128e67f-20aa-4b92-b4b5-284ee51f6189').
narrative_ontology:cs_reading_relation('c128e67f-20aa-4b92-b4b5-284ee51f6189', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('c128e67f-20aa-4b92-b4b5-284ee51f6189', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c128e67f-20aa-4b92-b4b5-284ee51f6189', ai_governance_legitimacy__market_libertarian_reading, influences).
narrative_ontology:cs_axiom('c128e67f-20aa-4b92-b4b5-284ee51f6189', foundational, efficiency_and_welfare_maximization_as_primary_legitimacy_criterion).
narrative_ontology:cs_axiom_status(efficiency_and_welfare_maximization_as_primary_legitimacy_criterion, holdable).
narrative_ontology:cs_axiom_grounding('c128e67f-20aa-4b92-b4b5-284ee51f6189', efficiency_and_welfare_maximization_as_primary_legitimacy_criterion, instrumental).
narrative_ontology:cs_axiom('c128e67f-20aa-4b92-b4b5-284ee51f6189', foundational, dignity_as_constraint_parameter_not_optimization_target).
narrative_ontology:cs_axiom_status(dignity_as_constraint_parameter_not_optimization_target, holdable).
narrative_ontology:cs_axiom_grounding('c128e67f-20aa-4b92-b4b5-284ee51f6189', dignity_as_constraint_parameter_not_optimization_target, conventional).
narrative_ontology:cs_axiom('c128e67f-20aa-4b92-b4b5-284ee51f6189', secondary, technical_expertise_supersedes_doctrinal_authority_in_governance).
narrative_ontology:cs_axiom_status(technical_expertise_supersedes_doctrinal_authority_in_governance, holdable).
narrative_ontology:cs_axiom_grounding('c128e67f-20aa-4b92-b4b5-284ee51f6189', technical_expertise_supersedes_doctrinal_authority_in_governance, instrumental).
narrative_ontology:cs_reference_frame('c128e67f-20aa-4b92-b4b5-284ee51f6189', encyclical_principles_as_aspirational_feasibility_constraints).
narrative_ontology:cs_drift_state('c128e67f-20aa-4b92-b4b5-284ee51f6189', contemporary_ai_deployment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c128e67f-20aa-4b92-b4b5-284ee51f6189', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, large_technology_firms).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, venture_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_technical_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, early_adopter_consumers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, displaced_low_skill_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, digitally_underserved_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_populations).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, efficiency_maximization_as_legitimacy_criterion).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, technical_expertise_as_governing_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, deploy, and set the performance benchmarks by which AI systems are judged legitimate. Frame ethical review as a compliance cost to be balanced against efficiency and growth metrics they themselves define and measure. Capture regulatory rulemaking through technical advisory roles and revolving-door staffing of oversight bodies.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, large_technology_firms, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__technocratic_optimization_reading, large_technology_firms, beneficiary).

% Fund AI development on the expectation that welfare-maximization framing keeps regulatory friction low and growth trajectories unconstrained. Can reallocate capital across jurisdictions instantly if a jurisdiction imposes dignity-first constraints that compress projected returns.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, venture_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Command premium wages and career mobility precisely because the optimization framing treats their expertise as the legitimate arbiter of what counts as acceptable tradeoffs. Their skills are portable across firms and borders; they bear little of the downside risk they help design.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_technical_workers, beneficiary,
    moderate, biographical, mobile, global).

% Access efficiency gains, personalization, and convenience from rapidly deployed AI systems before slower-moving regulatory or ethical review can intervene. Can switch providers or platforms if a given optimization tradeoff becomes personally costly.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, early_adopter_consumers, beneficiary,
    moderate, immediate, mobile, national).

% Lose employment or bargaining power as automation is justified on aggregate-welfare grounds that do not require compensating identifiable losers. Retraining and safety-net programs are treated as optional feasibility considerations rather than obligations, and are the first items cut when growth targets tighten.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, displaced_low_skill_workers, payer,
    powerless, biographical, trapped, national).

% Lack the infrastructure to participate in or benefit from AI-driven efficiency gains, yet are counted in the aggregate welfare metrics used to legitimate the systems that bypass them. Have no practical exit from jurisdictions or markets where these systems are deployed without local consultation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, digitally_underserved_communities, payer,
    powerless, generational, trapped, regional).

% Are scored, sorted, and denied opportunities by opaque systems whose accuracy at the aggregate level is treated as sufficient justification, regardless of concentrated harm to specific individuals or groups. Have limited recourse because the technical basis for adverse decisions is proprietary or not disclosed.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_populations, payer,
    powerless, immediate, trapped, national).

% Certify AI systems against performance and efficiency benchmarks, lending legitimacy to the technocratic framing. Composed substantially of industry-affiliated experts, which blurs the line between independent oversight and self-regulation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, technical_standards_bodies, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__technocratic_optimization_reading, technical_standards_bodies, observer).

% Argue that treating dignity as a constraint parameter rather than the optimization target inverts the proper ordering of technology to the human person. Are invited to conferences and consultations but have no binding authority over deployment decisions or benchmark design.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, magisterial_and_civil_society_critics, excluded,
    moderate, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__technocratic_optimization_reading, large_technology_firms).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__technocratic_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common metric language (efficiency, aggregate welfare, demonstrated performance) that lets firms, investors, and regulators converge quickly on deployment decisions without protracted value negotiation, enabling rapid iteration and cross-border interoperability of AI systems.
% TRANSFER_FUNCTION: Moves decision-making authority and the benefits of rapid deployment toward those who control technical expertise and capital, while moving adjustment costs (job loss, algorithmic misclassification, infrastructure exclusion) onto populations with no seat in benchmark design or standards bodies.
% ABSENT_VOICES: Displaced workers, underserved communities, and profiled individuals have no structured channel into benchmark-setting; magisterial and civil-society critics are consulted ceremonially but hold no binding authority. Their objection — that dignity is being treated as a tradeoff variable rather than a floor — is heard in commentary, not encoded in the optimization function.
% DISAPPEARANCE_RATIONALE: If this legitimacy framing vanished, technology firms would lose the ready justification for rapid deployment ahead of ethical review, standards bodies would need new non-efficiency-based legitimacy criteria, and displaced or profiled populations would gain grounds to demand compensation or redesign rather than being absorbed into an aggregate welfare calculation. Investment and deployment timelines would likely slow.
% FOUNDING_PROBLEM: Early AI deployment stalled under fragmented, inconsistent ethical review processes; the technocratic framing solved a real coordination problem — how to evaluate and deploy systems quickly across jurisdictions using shared, quantifiable performance criteria instead of contested, slow-moving value negotiations.
% FOUNDING_PROBLEM_CORROBORATION: Technical standards bodies and firms attest the coordination problem remains live and unsolved by any other mechanism. Independent labor economists, displaced-worker advocacy groups, and Catholic Social Doctrine commentators (outside the beneficiary set) attest that the framing has drifted from solving a genuine coordination bottleneck into a legitimating cover for externalizing costs onto powerless populations; no fully independent audit of benchmark design has been published to adjudicate between these attestations.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__technocratic_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__technocratic_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__technocratic_optimization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__technocratic_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__technocratic_optimization_reading, 0.35, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.35 at interval end) sits in the moderate band the manifest anticipates: this is not naked extraction dressed as coordination (a snare would show victims with no genuine coordination benefit) but a real efficiency-coordination function whose benefits and costs are asymmetrically distributed along power lines. Suppression (0.42) reflects that alternatives are not fully foreclosed — democratic and magisterial readings remain articulable and are voiced in public discourse — but regulatory capture and expert-consensus gatekeeping make it costly for excluded parties to convert voice into binding change. Theater ratio rises across the interval (0.20 to 0.40) as ethics review processes proliferate publicly while benchmark design and deployment decisions continue to be made primarily on efficiency criteria — a Goodhart-style substitution where visible ethics infrastructure increasingly substitutes for binding constraint on the underlying optimization target.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology firms and investors sit near the full-beneficiary end: they set the benchmarks, capture the surplus, and hold arbitrage-grade exit across jurisdictions if any single regime imposes dignity-first constraints. High-skill workers and early adopters are moderate beneficiaries with mobile exit. Displaced workers, underserved communities, and profiled populations sit near the full-target end: trapped exit options, no voice in benchmark design, and costs that are aggregated away in the welfare metric used to justify the arrangement. Technical standards bodies are structurally positioned as agenda-setters but are populated substantially by industry-affiliated experts, which is why they carry both agenda_setter and observer roles — their nominal independence is partially compromised.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (fragmented, slow ethical review blocking cross-border AI deployment) was genuinely live at the framework's origin and is corroborated as still-live by firms and standards bodies. But independent labor economists and displaced-worker advocates attest the framework has drifted into a legitimating shield for cost externalization — the disappearance_verdict of world_rearranges combined with a contested founding_problem_status is exactly the mismatch pattern that flags a possible zombie-mandate: the coordination function persists, but its distributive logic increasingly serves as cover for extraction that the original coordination problem does not require.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the technocratic optimization reading a legitimate application of encyclical principles under conditions of feasibility, or a structural inversion of the encyclical''s ordering of technology to the human person?',
    'Compare deployment outcomes and cost distribution under this reading against outcomes under the magisterial_subsidiarity_reading in comparable jurisdictions or sectors; assess whether dignity-as-constraint systematically produces higher uncompensated harm to identifiable victims than dignity-as-target frameworks.',
    'If dignity-as-constraint reliably produces worse outcomes for the powerless without offsetting aggregate gains that reach them, this reading''s coordination claim weakens relative to its extraction, pushing the classification toward tangled_rope. If aggregate gains genuinely diffuse to the affected populations over time, the rope classification is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether dignity-as-optimization-constraint is a defensible feasibility balancing or a foreclosure of the encyclical''s core ordering claim.').

omega_variable(
    aggregate_metric_masking,
    'Do the aggregate welfare and efficiency metrics used to justify deployment decisions adequately capture concentrated harms to displaced workers and profiled populations, or do they structurally mask them?',
    'Disaggregate published welfare/efficiency benchmarks by income decile, region, and algorithmic-decision exposure; compare distributional data against the aggregate figures used in public legitimation.',
    'If disaggregation reveals systematic masking, the extraction component of this reading is understated by the current ε and the constraint moves closer to tangled_rope territory; if disaggregated data tracks the aggregate closely, the rope classification is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_metric_masking, empirical, 'Whether aggregate metrics conceal concentrated distributional harm.').

omega_variable(
    standards_body_capture_degree,
    'To what degree are technical standards bodies independent adjudicators versus industry-captured legitimation vehicles?',
    'Audit standards-body membership composition, funding sources, and revolving-door employment patterns against the firms whose systems they certify.',
    'High capture would support treating the standards bodies'' agenda_setter role as substantially aligned with the beneficiary firms, increasing effective suppression; low capture would support their observer role as genuinely independent oversight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(standards_body_capture_degree, empirical, 'Degree of regulatory capture in AI standards-setting bodies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__technocratic_optimization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 4, 0.26).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 8, 0.29).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 12, 0.31).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 24, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(ai_g_su_t16, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(ai_g_su_t24, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__technocratic_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__technocratic_optimization_reading, 0.15).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This is one of four sibling constraints decomposing the natural-language label 'AI governance legitimacy per the encyclical' into structurally distinct kernel readings, per the ε-invariance principle. Each reading has its own ε, beneficiary/victim structure, and classification: this technocratic reading (rope, ε≈0.35, moderate extraction via efficiency-coordination with asymmetric distribution) forecloses the magisterial_subsidiarity_reading (which grounds legitimacy in dignity-as-target under Magisterial authority — the two premises cannot coexist in one governance framework), coexists_with the democratic_pluralist_reading (both remain live positions among different governing coalitions), and influences the market_libertarian_reading (technocratic efficiency framing creates downstream pressure toward deregulation that the libertarian reading exploits, without the two being identical).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
