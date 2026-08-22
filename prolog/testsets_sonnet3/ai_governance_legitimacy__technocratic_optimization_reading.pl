% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__technocratic_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: AI Governance Legitimacy — Technocratic Optimization Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the technocratic-optimization reading of the
 *   contested ai_governance_legitimacy kernel: legitimacy is grounded in
 *   demonstrated technical performance and aggregate welfare gains, with
 *   encyclical-derived ethical principles (dignity, subsidiarity, solidarity)
 *   treated as soft constraints to be traded off against feasibility and
 *   growth. It coordinates real activity — firms, investors, and standards
 *   bodies converge on shared efficiency benchmarks instead of relitigating
 *   ethics per deployment — but the coordination rides on an asymmetric
 *   transfer: those who set and hit the benchmarks capture the gains, while
 *   those whose costs the benchmarks do not price (displacement, exclusion,
 *   profiling harm) bear them without recourse. This is a distinct constraint
 *   from the sibling readings of the same kernel (magisterial_subsidiarity,
 *   democratic_pluralist, market_libertarian) — each has its own ε,
 *   beneficiary/victim structure, and enforcement mechanism, per the
 *   ε-invariance principle; they are not measurement variants of one
 *   constraint.
 *
 * KEY AGENTS:
 *   - tech_firms: primary agenda-setter and beneficiary (institutional/arbitrage) — sets benchmarks and captures gains
 *   - displaced_workers: primary target (powerless/trapped) — bears externalized costs of automation
 *   - digitally_underserved_communities: structural target (powerless/trapped, generational) — invisible to aggregate metrics
 *   - algorithmically_profiled_individuals: primary target (powerless/trapped, immediate) — bears opaque-system harms
 *   - technical_expert_bodies: co-agenda-setter (organized/constrained) — supplies legitimating expertise
 *   - regulatory_agencies: excluded/weak observer (institutional/constrained) — nominal check, structurally captured
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
narrative_ontology:human_readable(ai_governance_legitimacy__technocratic_optimization_reading, "AI Governance Legitimacy — Technocratic Optimization Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__technocratic_optimization_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__technocratic_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__technocratic_optimization_reading, '957a4485-ac38-479f-b44b-76050e078f95').
narrative_ontology:cs_kernel_codification('957a4485-ac38-479f-b44b-76050e078f95', distributed).
narrative_ontology:cs_authority_grounding('957a4485-ac38-479f-b44b-76050e078f95', expertise).
narrative_ontology:cs_interpretation_layer_present('957a4485-ac38-479f-b44b-76050e078f95').
narrative_ontology:cs_reading_relation('957a4485-ac38-479f-b44b-76050e078f95', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('957a4485-ac38-479f-b44b-76050e078f95', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('957a4485-ac38-479f-b44b-76050e078f95', ai_governance_legitimacy__market_libertarian_reading, influences).
narrative_ontology:cs_axiom('957a4485-ac38-479f-b44b-76050e078f95', foundational, dignity_as_optimization_constraint_not_target).
narrative_ontology:cs_axiom_status(dignity_as_optimization_constraint_not_target, holdable).
narrative_ontology:cs_axiom_grounding('957a4485-ac38-479f-b44b-76050e078f95', dignity_as_optimization_constraint_not_target, instrumental).
narrative_ontology:cs_axiom('957a4485-ac38-479f-b44b-76050e078f95', foundational, technical_performance_as_sufficient_legitimacy_ground).
narrative_ontology:cs_axiom_status(technical_performance_as_sufficient_legitimacy_ground, holdable).
narrative_ontology:cs_axiom_grounding('957a4485-ac38-479f-b44b-76050e078f95', technical_performance_as_sufficient_legitimacy_ground, empirically_contingent).
narrative_ontology:cs_reference_frame('957a4485-ac38-479f-b44b-76050e078f95', pre_ai_expert_technocratic_governance_norm).
narrative_ontology:cs_drift_state('957a4485-ac38-479f-b44b-76050e078f95', contemporary_deployment_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('957a4485-ac38-479f-b44b-76050e078f95', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, tech_firms).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, institutional_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, digitally_underserved_communities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_individuals).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, aggregate_welfare_maximization_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__technocratic_optimization_reading, expertise_based_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and deploys the AI systems, sets the technical benchmarks by which 'demonstrated performance' is measured, and lobbies to keep encyclical-derived ethical review as an advisory rather than binding layer. Captures the efficiency gains directly and can relocate operations across jurisdictions to avoid stricter oversight regimes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, tech_firms, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__technocratic_optimization_reading, tech_firms, beneficiary).

% Fund the growth trajectory that the optimization framing exists to protect; returns are directly tied to unimpeded scaling. Can exit any single firm or jurisdiction if ethical constraints tighten, redirecting capital elsewhere with negligible friction.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, institutional_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Hold complementary skills that rise in value as AI systems are deployed under efficiency-first governance. Their labor is reorganized around the technology rather than displaced by it, and they can move between firms or sectors that adopt the optimization framework.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, high_skill_workers, beneficiary,
    moderate, biographical, mobile, national).

% Consumers and businesses positioned to exploit AI-driven efficiency gains first, capturing outsized advantage before diffusion narrows the benefit. Not bound to any single platform and can switch as better-optimized tools emerge.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, early_adopters, beneficiary,
    moderate, biographical, mobile, national).

% Lose employment or bargaining position as tasks are automated under the growth-and-efficiency mandate. Retraining and relocation costs are borne individually; the optimization calculus treats their displacement as an externality netted against aggregate welfare gains, not as a cost requiring redress.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, displaced_workers, payer,
    powerless, biographical, trapped, national).

% Lack the infrastructure to participate in or benefit from the AI systems being optimized around; their absence from the efficiency curve is invisible to aggregate welfare metrics, which average over populations rather than accounting for those left off the grid entirely.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, digitally_underserved_communities, payer,
    powerless, generational, trapped, regional).

% Subject to automated scoring, sorting, or denial decisions whose internal logic is proprietary. Under the optimization framing, disparate impact is treated as a tunable parameter rather than a dignity violation, and appeal mechanisms are calibrated to system throughput, not individual redress.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, algorithmically_profiled_individuals, payer,
    powerless, immediate, trapped, national).

% Standards bodies, benchmark consortia, and credentialed researchers whose technical determinations substitute for democratic or magisterial deliberation as the source of legitimacy. Their authority depends on continued deference from regulators and firms, which gives them influence but also ties their standing to the optimization paradigm's success.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, technical_expert_bodies, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__technocratic_optimization_reading, technical_expert_bodies, observer).

% Nominally empowered to impose binding ethical constraints but structurally out-resourced and out-paced by the firms and expert bodies they oversee; frequently staffed from and returning to the industry they regulate. Their capacity to insist on dignity-first constraints is present in law but weak in practice.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__technocratic_optimization_reading, regulatory_agencies, excluded,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__technocratic_optimization_reading, regulatory_agencies, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__technocratic_optimization_reading, tech_firms).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__technocratic_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, measurable standard (aggregate welfare, efficiency, benchmarked performance) that lets firms, investors, and regulators converge on what counts as 'good' AI deployment without relitigating first principles for every system, enabling rapid coordinated investment and deployment.
% TRANSFER_FUNCTION: Moves decision-making authority over what counts as legitimate deployment away from democratic bodies, affected communities, and doctrinal ethical review, and toward technical performance metrics controlled by the firms and expert bodies best positioned to define and hit those metrics; moves the costs of externalities (displacement, exclusion, profiling harms) onto those without the exit options to avoid them.
% ABSENT_VOICES: Displaced workers, digitally excluded communities, and algorithmically profiled individuals have no seat in setting the efficiency benchmarks that determine their treatment; regulatory agencies formally represent them but are structurally weak. Magisterial and democratic-pluralist voices are present in public discourse but treated as aspirational commentary rather than binding constraints under this reading.
% DISAPPEARANCE_RATIONALE: If the technocratic-optimization legitimacy claim collapsed overnight, firms could no longer justify prioritizing efficiency metrics over dignity-based review; regulatory agencies and magisterial or democratic bodies would have to be treated as binding rather than advisory, materially slowing deployment timelines and redistributing costs currently borne by displaced and profiled populations back toward the firms and investors who currently benefit.
% FOUNDING_PROBLEM: Early AI governance debates faced genuine paralysis: competing ethical, religious, and political frameworks offered no common metric for evaluating systems, while technical capability was advancing faster than any deliberative process could track. A performance-and-efficiency standard offered a tractable, cross-jurisdictional way to compare and approve systems.
% FOUNDING_PROBLEM_CORROBORATION: Technical expert bodies and firms attest the coordination problem remains live — deliberative processes are still slower than deployment cycles. Independent labor economists, digital-divide researchers, and Catholic Social Doctrine scholars (outside the beneficiary set) attest that the tractability problem has been substantially solved by measurement infrastructure that now exists, and that the optimization framing persists mainly because it lets beneficiaries avoid slower distributive review, not because no faster deliberative mechanism is available.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__technocratic_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__technocratic_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__technocratic_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.35 at interval end, within the 0.30-0.40 expected band) because the arrangement genuinely solves a coordination problem (shared benchmarks for cross-jurisdictional deployment) even as it transfers costs asymmetrically. Suppression (0.42) is meaningfully lower than extraction because the mechanism relies more on regulatory capture and expert consensus than on direct coercion — exits exist in principle (litigation, political mobilization, alternative standards bodies) but are slow and resource-intensive, which is why resistance (0.45) is substantial rather than negligible. Theater ratio rises over the interval (0.20 to 0.40) as 'ethical AI' review boards and impact assessments proliferate alongside continued deployment on the original optimization logic — a rising theater signal consistent with proxy compliance layered onto unchanged extraction dynamics. Accessibility collapse (0.50) reflects that alternative governance framings remain visible and articulated (this is a live kernel contest, not a closed question) even though the dominant institutional path has narrowed around the optimization framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Tech firms, investors, high-skill workers, and early adopters are declared beneficiaries with mobile-to-arbitrage exit options, placing them near the beneficiary end of directionality — the constraint subsidizes their position by legitimating deployment on terms they control. Displaced workers, digitally underserved communities, and algorithmically profiled individuals are declared victims with trapped exit options and powerless standing, placing them near the full-target end — they bear costs the optimization metric does not price and cannot exit the arrangement. Technical expert bodies occupy an intermediate position: organized power, constrained exit, functioning as co-agenda-setters whose authority is itself contingent on the paradigm's continued dominance, which ties their interests to defending the reading even though they do not capture its gains directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (no common cross-jurisdictional metric for evaluating fast-moving AI systems) was genuinely live at the reading's origin and is only partially resolved: measurement infrastructure now exists that could support faster deliberative review, but the optimization framing persists because it lets current beneficiaries avoid the distributive scrutiny that would come with treating dignity as binding rather than aspirational. This is not a fully resolved mandatrophy (the coordination function still does real work) nor pure extraction (efficiency benchmarks are genuinely useful) — it sits as a tangled coordination structure whose disproportionate share of extraction is increasingly hard to justify as the underlying tractability problem it was built to solve recedes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimization_metric_completeness,
    'Do the aggregate welfare and efficiency metrics that ground this reading''s legitimacy claim actually capture the costs borne by displaced workers, excluded communities, and profiled individuals, or are these costs structurally invisible to the metric by construction?',
    'Independent distributional audit comparing aggregate welfare gains against disaggregated harm data for the declared victim groups, conducted by parties outside the beneficiary set (e.g. labor economists, digital-inclusion researchers).',
    'If the metric is structurally blind to these costs (not merely imperfectly measuring them), the ''aggregate welfare'' legitimacy claim is doing coordination-cover work for extraction rather than genuinely balancing competing goods, which would push the reading''s effective classification toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimization_metric_completeness, empirical, 'Whether aggregate welfare metrics structurally exclude the costs borne by this reading''s victim groups.').

omega_variable(
    kernel_reading_selection_legitimacy,
    'Is the technocratic_optimization reading''s dominance in actual AI governance practice a product of its genuine persuasive/coordination merit, or of the structural advantage tech firms and expert bodies hold in shaping which reading of the kernel gets institutionalized?',
    'Comparative institutional analysis: track which reading (technocratic, magisterial, democratic, libertarian) actually governs deployment decisions in jurisdictions with different regulatory capture profiles, and whether outcomes track argument quality or resource asymmetry.',
    'If institutionalization tracks resource asymmetry rather than argument quality, the reading''s practical dominance is itself an extraction outcome rather than a coordination outcome — this would not change this story''s own ε (which is authored from this reading''s own lights) but would sharpen the omega on whether the reading SHOULD be dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_legitimacy, conceptual, 'Whether this reading''s real-world dominance reflects coordination merit or resource capture of the kernel-selection process.').

omega_variable(
    expert_authority_capture_risk,
    'Are the technical_expert_bodies genuinely independent arbiters of performance, or are they substantially funded by and revolving-door connected to the tech_firms whose systems they benchmark?',
    'Funding-source and personnel-flow audit of major AI standards bodies and benchmark consortia relative to the firms whose products they evaluate.',
    'High capture would mean the ''authority rests with technical expertise'' claim is a laundering mechanism for firm self-assessment, raising effective suppression and pushing the reading''s computed type toward tangled_rope even under its own stated logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expert_authority_capture_risk, empirical, 'Degree of independence of technical expert bodies from the firms they benchmark.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__technocratic_optimization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__technocratic_optimization_reading, theater_ratio, 20, 0.38).
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
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(ai_g_su_t24, ai_governance_legitimacy__technocratic_optimization_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__technocratic_optimization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__technocratic_optimization_reading, 0.12).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__technocratic_optimization_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposed from the single natural-language label 'AI governance legitimacy' per the ε-invariance principle. Each sibling reading of the ai_governance_legitimacy kernel (technocratic_optimization, magisterial_subsidiarity, democratic_pluralist, market_libertarian) is authored as its own constraint with its own ε, beneficiary/victim structure, and claimed type, because the four readings differ structurally (not merely in evaluative framing) on who holds authority, what counts as a legitimate constraint, and who bears the costs of deployment decisions. This story (technocratic_optimization) authors the moderate-ε, rope-claimed reading in which efficiency benchmarks coordinate deployment at the cost of externalizing harm onto powerless groups. It is linked bidirectionally to its three siblings via affects_constraints; each sibling's own file documents the same relationship from its side.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
