% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__technocratic_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: AI Governance Legitimacy — Technocratic Optimization Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint story captures the technocratic optimization reading of
 *   AI governance legitimacy: the claim that legitimacy derives from
 *   maximizing aggregate welfare, efficiency, and innovation, with ethical
 *   principles (including those from the encyclical) treated as aspirational
 *   values to be balanced against feasibility and growth imperatives.
 *   Authority rests with technical expertise and demonstrated performance.
 *   The constraint coordinates AI development around quantifiable
 *   optimization targets, but the coordination function is inseparable from
 *   an extraction pattern that concentrates gains among firms, investors, and
 *   high-skill workers while externalizing transition costs onto displaced
 *   workers, digitally excluded communities, and algorithmically profiled
 *   subjects. The claim/metric gap is deliberate: the reading claims rope
 *   (coordination around efficiency) while the authored metrics reveal
 *   moderate extraction and growing suppression — the engine measures that
 *   divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__technocratic_optimization_reading, 0.35).
domain_priors:suppression_score(ai_governance_legitimacy__technocratic_optimization_reading, 0.45).
domain_priors:theater_ratio(ai_governance_legitimacy__technocratic_optimization_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__technocratic_optimization_reading, rope).
narrative_ontology:human_readable(ai_governance_legitimacy__technocratic_optimization_reading, "AI Governance Legitimacy — Technocratic Optimization Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__technocratic_optimization_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__technocratic_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__technocratic_optimization_reading, 'f1495d94-db22-4349-b9e7-6956353f1c53').
narrative_ontology:cs_kernel_codification('f1495d94-db22-4349-b9e7-6956353f1c53', distributed).
narrative_ontology:cs_authority_grounding('f1495d94-db22-4349-b9e7-6956353f1c53', expertise).
narrative_ontology:cs_interpretation_layer_present('f1495d94-db22-4349-b9e7-6956353f1c53').
narrative_ontology:cs_reading_relation('f1495d94-db22-4349-b9e7-6956353f1c53', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1495d94-db22-4349-b9e7-6956353f1c53', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1495d94-db22-4349-b9e7-6956353f1c53', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('f1495d94-db22-4349-b9e7-6956353f1c53', foundational, aggregate_welfare_maximization_is_legitimacy).
narrative_ontology:cs_axiom_status(aggregate_welfare_maximization_is_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f1495d94-db22-4349-b9e7-6956353f1c53', aggregate_welfare_maximization_is_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('f1495d94-db22-4349-b9e7-6956353f1c53', foundational, dignity_as_optimization_constraint).
narrative_ontology:cs_axiom_status(dignity_as_optimization_constraint, holdable).
narrative_ontology:cs_axiom_grounding('f1495d94-db22-4349-b9e7-6956353f1c53', dignity_as_optimization_constraint, deontological).
narrative_ontology:cs_axiom('f1495d94-db22-4349-b9e7-6956353f1c53', secondary, technical_expertise_as_authority).
narrative_ontology:cs_axiom_status(technical_expertise_as_authority, holdable).
narrative_ontology:cs_axiom_grounding('f1495d94-db22-4349-b9e7-6956353f1c53', technical_expertise_as_authority, conventional).
narrative_ontology:cs_reference_frame('f1495d94-db22-4349-b9e7-6956353f1c53', technocratic_legitimacy_framework).
narrative_ontology:cs_drift_state('f1495d94-db22-4349-b9e7-6956353f1c53', post_ai_regulation_wave, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f1495d94-db22-4349-b9e7-6956353f1c53', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, tech_firms).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, digitally_excluded_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_subjects).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, aggregate_welfare_maximization).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, technical_expertise_as_legitimate_authority).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, innovation_as_primary_good).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the optimization objectives, control the infrastructure, and capture the majority of value created. They fund and direct the research agenda, set platform governance rules, and lobby for regulatory frameworks that treat efficiency as the primary legitimacy criterion.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, tech_firms, agenda_setter,
    institutional, generational, arbitrage, global).

% Allocate capital to firms and projects that maximize return on investment through efficiency gains. Their returns compound when governance frameworks prioritize scale and speed over distributive justice or participatory processes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, investors, beneficiary,
    powerful, biographical, mobile, global).

% Capture wage premiums and career opportunities in AI-adjacent fields. Their skills are complementary to automation, and they participate in the expert consensus that legitimizes technocratic governance. Exit is credible — they can move across firms, sectors, and jurisdictions.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers, beneficiary,
    organized, biographical, mobile, global).

% Gain first-mover advantages in productivity, convenience, and access to new services. They are primarily urban, educated, and digitally fluent. Their adoption patterns reinforce the optimization metrics that firms optimize for.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters, beneficiary,
    moderate, immediate, constrained, global).

% Bear the costs of automation-driven job loss, wage stagnation, and deskilling without commensurate participation in the gains. Retraining programs are underfunded and often misaligned with actual labor market needs. Geographic and occupational mobility is limited by family, housing, and credential barriers.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers, payer,
    powerless, biographical, constrained, regional).

% Lack the infrastructure, literacy, and affordability to participate in the digital economy that the optimization framework assumes as baseline. They experience service degradation (e.g., branch closures, digital-only government services) as the world optimizes around connected users. Exit from exclusion requires collective investment they cannot command.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, digitally_excluded_communities, payer,
    powerless, generational, trapped, regional).

% Are scored, sorted, and gatekept by opaque models trained on historical data that encodes prior inequities. Their life chances — credit, insurance, hiring, policing — are mediated by systems they cannot audit or contest. Identity-locked because the profiling constructs the very categories through which they are known and governed.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_subjects, payer,
    powerless, biographical, identity_locked, global).

% Staff the standards bodies (IEEE, ISO, W3C), advisory councils, and corporate ethics boards that translate 'efficiency' into operational metrics. Their authority derives from credentialed competence and peer recognition. They move fluidly between academia, industry, and policy roles.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, technical_experts, agenda_setter,
    institutional, generational, arbitrage, global).

% Attempt to oversee a domain where technical complexity outpaces legislative capacity. They rely on industry expertise for implementation details, creating a revolving-door dynamic. Some jurisdictions (EU, China) are building independent technical capacity to shift toward agenda_setter.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% Advocate for rights-based, participatory, or redistributive frameworks. They are consulted performatively but rarely hold veto power over optimization parameters. Their funding often comes from the same tech philanthropy they critique.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, civil_society_organizations, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI development and deployment around measurable efficiency, aggregate welfare, and innovation metrics, enabling rapid scaling and resource allocation without value-pluralism bottlenecks or democratic deliberation delays.
% TRANSFER_FUNCTION: Moves decision-making authority and resource allocation to technical experts and firms; distributes gains to early adopters and high-skill workers; externalizes transition costs to displaced workers, digitally excluded communities, and algorithmically profiled subjects.
% ABSENT_VOICES: Displaced workers, digitally excluded communities, algorithmically profiled subjects, and traditions that treat dignity as non-negotiable (e.g., Catholic Social Doctrine, human rights frameworks, Indigenous data sovereignty movements) are structurally excluded from the optimization calculus. They would object to dignity being treated as a constraint parameter rather than the optimization target.
% DISAPPEARANCE_RATIONALE: If the technocratic optimization frame vanished overnight, AI governance would reorganize around contested value pluralism: regulatory capture would face direct democratic challenge, efficiency metrics would be subordinated to rights thresholds, and the expert-authority pipeline would require new legitimacy foundations. The current institutional architecture (standards bodies, corporate ethics boards, sandbox regulators) is built around this frame.
% FOUNDING_PROBLEM: The problem of coordinating AI development at planetary scale without getting bogged down in intractable value conflicts, democratic deliberation delays, and regulatory fragmentation — framed as the need for a single commensurable metric (aggregate welfare/efficiency) to align decentralized innovation.
% FOUNDING_PROBLEM_CORROBORATION: Industry and policy documents from outside the benefiting parties (OECD AI Principles, UN Global Digital Compact, academic literature on AI governance coordination problems) attest to the genuine coordination challenge. Labor advocates (UNI Global Union, AFL-CIO Tech Institute), digital rights groups (EFF, Access Now, Algorithmic Justice League), and theological ethicists (Vatican Dicastery for Promoting Integral Human Development, WCC) dispute that technocratic optimization was ever the only or best solution, documenting how the frame itself produces the exclusion it claims to manage.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__technocratic_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__technocratic_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__technocratic_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__technocratic_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__technocratic_optimization_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.35) is moderate: the constraint does not primarily extract through direct coercion but through structural exclusion — the optimization framework itself defines who counts as a beneficiary and who bears the externalized costs. Suppression (0.45) reflects the soft power of expert consensus, regulatory capture, and market discipline that marginalize alternative governance frames. Theater ratio (0.25) captures the ethics-washing layer: principles committees, AI ethics boards, and 'responsible AI' frameworks that legitimize the optimization frame without constraining its parameters. Accessibility collapse (0.5) indicates that alternative governance imaginaries exist but are structurally disadvantaged — they lack the technical infrastructure, funding pipelines, and institutional uptake pathways. Resistance (0.4) is growing but fragmented across labor, digital rights, and theological ethics communities.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (tech_firms, technical_experts) experience the constraint as genuine coordination: they built the infrastructure, they maintain the standards, and they deliver measurable welfare gains. The payer seats (displaced_workers, digitally_excluded_communities, algorithmically_profiled_subjects) experience the same structure as enforced extraction: the optimization parameters were chosen without their consent, the externalized costs are borne disproportionately, and the 'efficiency' metrics encode values they reject. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Tech firms and technical experts are structural beneficiaries (d near 0.0-0.2): they set the optimization parameters, control the infrastructure, and capture the value. Investors and high-skill workers are beneficiaries with credible exit (d ~0.2-0.3). Early adopters sit near symmetric (d ~0.4-0.5): they gain convenience but also feed the data that improves the optimization. Displaced workers and digitally excluded communities are payers with constrained or trapped exit (d ~0.7-0.9). Algorithmically profiled subjects are identity-locked payers (d ~0.9): the profiling constructs the categories through which they are governed, making exit from the frame nearly impossible. Regulatory bodies are observers with analytical exit (d ~0.5). Civil society organizations are excluded (d ~0.6-0.7).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating planetary-scale AI without value-pluralism paralysis) was live in the early 2000s when the frame consolidated. By the 2020s, the coordination infrastructure exists but the optimization target has drifted: 'aggregate welfare' operationalizes as engagement metrics, ad revenue, and model performance on benchmarks that correlate poorly with human flourishing. The arrangement persists because the beneficiaries control the agenda-setting machinery and the payers lack coalition power across their differentiated situations. This is not a piton (the function has not atrophied — optimization is more active than ever) but a rope with a widening extraction gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the technocratic_optimization_reading a structurally distinct constraint from its sibling readings, or do they share a common coordination function that differs only in parameter weighting?',
    'Compare the beneficiary/victim structures and enforcement mechanisms across readings. If the same agents occupy payer/beneficiary seats across readings with only metric weights shifting, they are parameter variants of one constraint. If the seat structures differ fundamentally (e.g., magisterial reading excludes technical experts from agenda-setter role), they are distinct constraints.',
    'If parameter variants, the kernel should be modeled as one constraint with a reading_axis parameter. If distinct constraints, each reading gets its own story linked via network.affects_constraints. Current authoring treats them as distinct per ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether sibling readings are distinct constraints or parameter variants of one constraint.').

omega_variable(
    dignity_constraint_vs_target,
    'Does treating dignity as an optimization constraint (this reading) versus the optimization target (magisterial reading) produce a measurable difference in extraction patterns, or is it rhetorical framing over the same structural operation?',
    'Trace specific policy outcomes: when a dignity constraint binds (e.g., banning predictive policing in certain jurisdictions), does the technocratic frame treat it as a hard constraint that reduces measured efficiency, or as a soft penalty that gets optimized around? Compare regulatory sandboxes (technocratic) vs. rights-based moratoria (magisterial).',
    'If dignity-as-constraint is routinely optimized around (soft), the extraction pattern matches unconstrained optimization and the reading is a snare with ethics-washing. If dignity-as-constraint creates hard boundaries that reduce measured efficiency, the coordination function is genuine and the reading is a rope with real trade-offs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_constraint_vs_target, empirical, 'Whether dignity-as-constraint creates hard boundaries or soft penalties in practice.').

omega_variable(
    efficiency_coordination_vs_extraction,
    'Is the coordination around efficiency metrics genuinely necessary for AI deployment at scale, or does the efficiency frame itself create the coordination problem it claims to solve?',
    'Counterfactual: in domains where democratic pluralist or magisterial frames govern (e.g., EU AI Act risk tiers, Vatican AI ethics guidelines), does deployment proceed at comparable speed with different distributive outcomes? Or does the technocratic frame uniquely enable certain deployment patterns (surveillance advertising, algorithmic management) that other frames would block?',
    'If the efficiency frame enables deployment patterns that are extraction-disguised-as-coordination, the constraint is tangled_rope or snare. If it enables genuinely beneficial coordination that other frames would also permit (just slower), it is a rope with moderate extraction as the price of speed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_coordination_vs_extraction, empirical, 'Whether the efficiency coordination function is genuine or manufactured.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__technocratic_optimization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(ai_g_tr_t25, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 25, 0.25).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(ai_g_be_t25, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 25, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(ai_g_su_t25, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 25, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__technocratic_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__technocratic_optimization_reading, 0.15).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This reading and its three siblings form the ai_governance_legitimacy constraint family. They share a kernel (the question of what makes AI governance legitimate) but instantiate distinct constraints with different beneficiary/victim structures, different ε values, and different authority groundings. The technocratic reading (this story) has ε≈0.35, beneficiaries={tech_firms, investors, high_skill_workers, early_adopters}, victims={displaced_workers, digitally_excluded_communities, algorithmically_profiled_subjects}, authority_grounding=expertise. The magisterial reading has lower ε, different victims (the vulnerable as defined by CST), authority_grounding=lineage. The democratic reading has different coordination function (deliberation), authority_grounding=practice. The market reading has different extraction pattern (voluntary exchange), authority_grounding=extraction (property rights enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_legitimacy__technocratic_optimization_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_governance_legitimacy__technocratic_optimization_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
