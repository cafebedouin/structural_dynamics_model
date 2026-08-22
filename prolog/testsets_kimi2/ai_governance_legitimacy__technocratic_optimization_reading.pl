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
 *   human_readable: AI Governance Legitimacy â Technocratic Optimization Reading
 *   domain: theological ethics / technology governance / political theology
 *
 * SUMMARY:
 *   The technocratic optimization reading of AI governance legitimacy treats
 *   aggregate welfare, efficiency, and innovation as the primary criteria for
 *   legitimate AI governance. Authority rests with technical experts and
 *   demonstrated performance; ethical constraints, including those
 *   articulated in the encyclical, are treated as secondary optimization
 *   parameters or aspirational values to be balanced against feasibility and
 *   growth imperatives. The constraint coordinates global AI development
 *   around shared metrics but asymmetrically extracts from displaced workers,
 *   digitally excluded communities, and algorithmically profiled persons who
 *   bear the costs of optimization. It requires active enforcement through
 *   regulatory capture, expert consensus gatekeeping, and market
 *   concentration that suppresses alternative governance frames.
 *
 * KEY AGENTS:
 *   - ai_governance_experts (agenda_setter / institutional / arbitrage): Set metrics and certify systems; capture authority through credentialing.
 *   - tech_firms (beneficiary / powerful / arbitrage): Shape standards to minimize compliance and maximize deployment speed.
 *   - investors (beneficiary / powerful / arbitrage): Capture returns from regulatory permissiveness.
 *   - high_skill_workers (beneficiary / moderate / mobile): Command premiums in the innovation-centric labor market.
 *   - early_adopters (beneficiary / moderate / mobile): Gain early access and influence roadmaps.
 *   - displaced_workers (payer / powerless / trapped): Bear automation costs without transitional support.
 *   - digitally_excluded_communities (payer / powerless / trapped): Invisible to aggregate welfare metrics.
 *   - algorithmically_profiled_persons (payer / powerless / constrained): Subject to opaque optimization systems.
 *   - civil_society_advocates (excluded / moderate / constrained): Marginalized by expert credentialing.
 *   - democratic_publics (excluded / organized / constrained): Deliberative input treated as less legitimate than expertise.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__technocratic_optimization_reading, 0.35).
domain_priors:suppression_score(ai_governance_legitimacy__technocratic_optimization_reading, 0.5).
domain_priors:theater_ratio(ai_governance_legitimacy__technocratic_optimization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__technocratic_optimization_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__technocratic_optimization_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__technocratic_optimization_reading, "AI Governance Legitimacy â Technocratic Optimization Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__technocratic_optimization_reading, "theological ethics / technology governance / political theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__technocratic_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__technocratic_optimization_reading, '96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea').
narrative_ontology:cs_kernel_codification('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', fixed_text).
narrative_ontology:cs_authority_grounding('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', expertise).
narrative_ontology:cs_interpretation_layer_present('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea').
narrative_ontology:cs_reading_relation('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', foundational, aggregate_welfare_optimization_legitimacy).
narrative_ontology:cs_axiom_status(aggregate_welfare_optimization_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', aggregate_welfare_optimization_legitimacy, instrumental).
narrative_ontology:cs_axiom('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', foundational, technical_expertise_authority).
narrative_ontology:cs_axiom_status(technical_expertise_authority, holdable).
narrative_ontology:cs_axiom_grounding('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', technical_expertise_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', technocratic_optimization_framework).
narrative_ontology:cs_drift_state('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', contemporary_ai_policy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('96f6f3c5-7ac6-4db0-a7bf-a811d3b3a7ea', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, tech_firms).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, digitally_excluded_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_persons).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, technical_expertise_authority).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, aggregate_welfare_maximization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the evaluative metrics of welfare, efficiency, and innovation that legitimate AI governance. Certify models, audit systems, and advise regulators. Their authority is self-reinforcing through credentialing, citation networks, and regulatory capture.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_experts, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from governance frameworks that prioritize speed-to-market and scale over precautionary ethical review. They fund and populate standard-setting bodies, shaping rules that minimize compliance burdens while maintaining market dominance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, tech_firms, beneficiary,
    powerful, generational, arbitrage, global).

% Allocate capital to AI ventures based on performance metrics endorsed by the technocratic frame. They capture returns from regulatory permissiveness and the externalization of social costs.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Receive wage premiums and mobility privileges in the AI-driven labor market. Their human capital aligns with the governance frame's emphasis on innovation and skills-based sorting.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers, beneficiary,
    moderate, biographical, mobile, national).

% Gain early access to productivity-enhancing tools and influence product roadmaps. They benefit from rapid deployment cycles that ethics-heavy frameworks would slow.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters, beneficiary,
    moderate, immediate, mobile, global).

% Bear job losses and wage suppression from AI automation without transitional support. The governance frame treats their displacement as an acceptable efficiency cost rather than a governance failure.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers, payer,
    powerless, immediate, trapped, local).

% Lack infrastructure and literacy to participate in AI-enabled services. The aggregate welfare metric obscures their exclusion because they contribute little to measured efficiency or innovation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, digitally_excluded_communities, payer,
    powerless, immediate, trapped, regional).

% Subject to opaque scoring and sorting systems justified under performance optimization. They experience bias and surveillance without meaningful consent or recourse.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_persons, payer,
    powerless, immediate, constrained, national).

% Argue for dignity as the primary governance objective. They are marginal to standard-setting bodies that credential only technical expertise and dismiss normative debate as non-expert.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, civil_society_advocates, excluded,
    moderate, generational, constrained, national).

% Comprise the electorate and affected communities whose deliberative input is treated as less legitimate than expert assessment. Regulatory capture and epistemic closure limit their influence.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, democratic_publics, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global AI development around shared metrics of welfare, efficiency, and innovation, enabling cross-border standardization, interoperable systems, and rapid diffusion of technical advances without being bottlenecked by moral disagreement.
% TRANSFER_FUNCTION: Moves governance authority and resources toward technical expert institutions and capital holders, while moving displacement risk, surveillance exposure, and infrastructural neglect toward low-skill workers and marginalized communities.
% ABSENT_VOICES: Displaced workers in the Global South, communities lacking digital infrastructure, and religious ethicists who treat dignity as non-negotiable are structurally excluded from standard-setting bodies that credential only technical expertise.
% DISAPPEARANCE_RATIONALE: If the technocratic legitimacy constraint vanished overnight, AI governance would lose its central coordinating metric. Development would slow as jurisdictions diverged on safety and ethics standards; capital would retreat from high-risk frontiers; expert institutions would lose their privileged advisory role. The global AI political economy would fragment into competing national or regional frameworks.
% FOUNDING_PROBLEM: The rapid pace of AI development outstrips democratic deliberation and regulatory capacity, creating a governance gap that risks uncoordinated, unsafe deployment across jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: Tech firms and expert institutions attest the problem remains live, citing competitive pressure and safety risks. Labor advocates and marginalized communities attest the founding problem has been used to justify bypassing democratic oversight and social protections; independent academic analysis from outside the benefiting parties supports the claim that coordination has shifted toward rent-seeking and regulatory capture.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__technocratic_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__technocratic_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__technocratic_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__technocratic_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__technocratic_optimization_reading, 0.35, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.35) is moderate because the constraint genuinely coordinates standardization and diffusion but decouples benefits from costs, concentrating gains in capital and expertise while externalizing harms. Suppression (0.50) reflects regulatory capture and epistemic closure that constrain democratic and ethical alternatives without overt totalitarianism. Theater ratio (0.28) captures the performative ethics reviews and safety-washing that accompany deployment. Accessibility collapse (0.50) is moderate because alternative governance frames (magisterial, democratic pluralist) are understood but dismissed as infeasible. Resistance (0.35) is moderate and diffuse, emerging from labor and civil society but lacking institutional leverage. The temporal series show gradual intensification as regulatory capture deepened and AI markets consolidated.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (expert institutions) and beneficiary seats (tech firms, investors) experience the constraint as necessary coordination around objective metrics that solve a governance gap. The payer seats (displaced workers, excluded communities, profiled persons) experience it as an unaccountable extraction mechanism that legitimizes their marginalization. The excluded seats (civil society advocates, democratic publics) experience epistemic capture that renders their normative objections structurally illegitimate. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (tech_firms, investors, high_skill_workers, early_adopters) have low directionality because the constraint subsidizes their position through permissive governance, resource concentration, and skills-based sorting. Victims (displaced_workers, digitally_excluded_communities, algorithmically_profiled_persons) have high directionality because the constraint extracts from them via labor displacement, infrastructural neglect, and opaque surveillance. The agenda_setter (ai_governance_experts) sits near the beneficiary end because it captures authority, prestige, and career rents from the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâuncoordinated, unsafe AI deployment outstripping democratic capacityâwas genuine. However, the classification as tangled_rope rather than rope prevents mislabeling the current arrangement as pure coordination. The presence of victims, active enforcement via regulatory capture, and a rising theater ratio indicate that the coordination function has become inseparable from extraction. If the founding problem were solved (safe coordination achieved), the constraint would likely persist due to the authority and rents it generates, suggesting incipient mandatrophy that the metrics and temporal series already capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does this constraint represent a genuine coordination mechanism around efficiency metrics, or a legitimization frame for asymmetric extraction by expert institutions?',
    'Comparative analysis against the democratic pluralist and magisterial readings of the same kernel; evaluating whether the technocratic reading''s welfare claims are vindicated or contested by observed outcomes.',
    'If primarily legitimization, reclassification toward snare or stronger tangled_rope; if genuine coordination with acceptable externalities, rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether the technocratic reading is coordination or legitimization').

omega_variable(
    expertise_authority_empirical_basis,
    'Does technical expertise in AI governance actually produce superior aggregate welfare outcomes compared to deliberative or magisterial alternatives?',
    'Longitudinal outcome studies comparing governance regimes across jurisdictions with different authority structures.',
    'If expertise does not produce superior outcomes, the foundational axiom of the reading is overridden, supporting axiom_overriding drift classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_authority_empirical_basis, empirical, 'Empirical basis for expertise authority claim').

omega_variable(
    cs_framing_underdetermination,
    'Is the commitment system best framed as expert interpretation of a fixed encyclical text, or as an extraction mechanism using the text as legitimization cover?',
    'Examining whether expert institutions engage substantively with the encyclical''s normative claims or selectively instrumentalize its language to justify pre-committed optimization goals.',
    'Alternative framing shifts authority_grounding from expertise to extraction and changes the classification of the interpretation layer from genuine interpretive buffer to theatrical legitimization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative CS framing of expert authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__technocratic_optimization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_g_tr_t6, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(ai_g_tr_t18, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(ai_g_tr_t30, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ai_g_be_t6, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(ai_g_be_t18, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 18, 0.3).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 24, 0.33).
narrative_ontology:measurement(ai_g_be_t30, ai_governance_legitimacy__technocratic_optimization_reading, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_g_su_t6, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(ai_g_su_t18, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 18, 0.44).
narrative_ontology:measurement(ai_g_su_t24, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(ai_g_su_t30, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__technocratic_optimization_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four structurally distinct constraints derived from the ai_governance_legitimacy kernel. Each reading assigns a different authority source and produces a different beneficiary/victim structure. They are linked as a constraint family under the kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
