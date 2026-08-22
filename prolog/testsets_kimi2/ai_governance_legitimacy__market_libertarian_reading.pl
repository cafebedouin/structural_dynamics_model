% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: AI Governance Legitimacy â Market Libertarian Reading
 *   domain: theological ethics / technology governance / political theology
 *
 * SUMMARY:
 *   This constraint story instantiates the market_libertarian_reading of the
 *   ai_governance_legitimacy kernel. It claims that AI governance legitimacy
 *   derives from pre-political property rights and voluntary exchange,
 *   framing centralized oversight and solidarity demands as illegitimate
 *   coercion. The kernel is contested: sibling readings include magisterial
 *   subsidiarity, technocratic optimization, and democratic pluralist
 *   framings. This reading presents itself as a Mountain â a natural
 *   background condition of legitimate order â while the authored metrics
 *   and structural data document identifiable beneficiaries and victims,
 *   triggering false-summit evaluation.
 *
 * KEY AGENTS:
 *   - ai_entrepreneurs: Primary beneficiary (powerful/mobile) â control IP and commercial outcomes under property-rights framing.
 *   - venture_investors: Primary beneficiary (powerful/arbitrage) â capital mobility and return protection.
 *   - high_autonomy_individuals: Secondary beneficiary (moderate/mobile) â consumer choice and exit options.
 *   - monopsony_workers: Primary target (powerless/constrained) â bear labor-market extraction from weak collective bargaining.
 *   - communities_facing_coordination_failures: Primary target (powerless/trapped) â absorb unremediated externalities.
 *   - magisterial_interpreters: Excluded voice (institutional/analytical) â claims for common good rendered illegitimate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.25).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.45).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "AI Governance Legitimacy â Market Libertarian Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological ethics / technology governance / political theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__market_libertarian_reading).
domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, 'c81719eb-7d45-4390-b237-ed639c4e5e7e').
narrative_ontology:cs_kernel_codification('c81719eb-7d45-4390-b237-ed639c4e5e7e', implicit).
narrative_ontology:cs_authority_grounding('c81719eb-7d45-4390-b237-ed639c4e5e7e', self_enforcing).
narrative_ontology:cs_reading_relation('c81719eb-7d45-4390-b237-ed639c4e5e7e', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('c81719eb-7d45-4390-b237-ed639c4e5e7e', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('c81719eb-7d45-4390-b237-ed639c4e5e7e', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_axiom('c81719eb-7d45-4390-b237-ed639c4e5e7e', foundational, property_rights_pre_political).
narrative_ontology:cs_axiom_status(property_rights_pre_political, holdable).
narrative_ontology:cs_axiom_grounding('c81719eb-7d45-4390-b237-ed639c4e5e7e', property_rights_pre_political, deontological).
narrative_ontology:cs_axiom('c81719eb-7d45-4390-b237-ed639c4e5e7e', foundational, collective_mandates_illegitimate).
narrative_ontology:cs_axiom_status(collective_mandates_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('c81719eb-7d45-4390-b237-ed639c4e5e7e', collective_mandates_illegitimate, deontological).
narrative_ontology:cs_reference_frame('c81719eb-7d45-4390-b237-ed639c4e5e7e', spontaneous_order_baseline).
narrative_ontology:cs_drift_state('c81719eb-7d45-4390-b237-ed639c4e5e7e', contemporary_ai_policy_contests, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('c81719eb-7d45-4390-b237-ed639c4e5e7e', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, ai_entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, venture_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, monopsony_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, pre_political_property_rights).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, voluntary_exchange_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and deploy AI systems under a governance framework that treats control of intellectual property and commercial outcomes as pre-political rights. Benefit from low regulatory burden and broad contractual freedom to capture returns.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, ai_entrepreneurs, beneficiary,
    powerful, biographical, mobile, global).

% Allocate capital to AI ventures expecting returns secured by enforceable property rights and limited liability. Move capital across jurisdictions to optimize for regimes with strong contractual enforcement and weak collective mandates.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, venture_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Exercise consumer choice among AI services without centralized oversight. Benefit from competitive supply and exit options, though they lack the leverage of capital holders and carry diffuse risks markets fail to price.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    moderate, biographical, mobile, national).

% Labor in AI supply chains or platform economies where few buyers set wages. Bear the costs of weak collective bargaining protections because governance legitimacy is tied to property rights rather than labor power.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, monopsony_workers, payer,
    powerless, immediate, constrained, national).

% Experience environmental, safety, or distributive harms from AI systems that markets fail to correct. Lack capital and collective mechanisms to enforce accountability because the legitimacy framework categorizes their demands as illegitimate coercion.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures, payer,
    powerless, generational, trapped, regional).

% Advance AI governance grounded in Catholic Social Doctrine, common good, subsidiarity, and solidarity. In this reading their claims are pre-categorized as illegitimate coercion; they are structurally excluded from the legitimacy conversation despite their global institutional presence.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, magisterial_interpreters, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable, decentralized framework for AI development by assigning control through property rights and voluntary contract, reducing uncertainty over ownership and transaction authority without centralized political allocation.
% TRANSFER_FUNCTION: Moves governance legitimacy from collective political processes to decentralized market actors; transfers the risks and externalized costs of AI development to workers and communities who lack the market power to price or exit those harms.
% ABSENT_VOICES: Magisterial interpreters and democratic pluralist advocates are structurally excluded; their claims for solidarity and common good are treated as coercive overreach. Workers in monopsony conditions and communities facing coordination failures are present as market participants but their collective grievances are filtered out by the legitimacy framework.
% DISAPPEARANCE_RATIONALE: Libertarian adherents assert that without property-rights-based governance, AI innovation would collapse into political rent-seeking. Critics assert that democratic and solidaristic alternatives would reorganize allocation toward labor and community protection. Whether the world rearranges or improves is the core dispute.
% FOUNDING_PROBLEM: The threat of political capture of AI by centralized states or majoritarian coalitions, and the need for a governance framework that respects individual autonomy and enables entrepreneurial discovery without collective veto.
% FOUNDING_PROBLEM_CORROBORATION: Libertarian theorists and technology entrepreneurs attest the problem is live, citing regulatory overreach and innovation chilling. Democratic pluralists and magisterial authorities attest the problem is a cover story for capital capture; independent political economists are divided, with no extra-beneficiary consensus.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, contested).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.25, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.25 because the framework genuinely coordinates decentralized AI development through property rights, but still extracts from those lacking market power by denying them collective remedial mechanisms. Suppression is 0.45: the constraint does not rely on raw violence but actively suppresses collective-mandate alternatives through delegitimization and contract-law exclusivity. Theater ratio is 0.25, reflecting moderate performative maintenance of the 'natural law' framing as regulatory pressure mounts. Accessibility collapse is high (0.80) because within the reading's framework no legitimate alternative to market governance exists. Resistance is 0.55 because competing governance traditions (democratic, magisterial, technocratic) actively contest the frame. The claim/metric independence is deliberate: the reading claims Mountain while metrics exhibit extraction, suppression, and resistance consistent with a constructed, enforced constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (entrepreneurs, investors, high-autonomy individuals), the constraint appears as a neutral Mountain â the pre-political background of legitimate order. From the payer seats (monopsony workers, communities facing coordination failures), the same structure operates as an enforced exclusion from governance voice, reading as Snare or Tangled Rope depending on whether the coordination benefit is acknowledged. The excluded magisterial seat experiences the constraint as a foreclosure of its entire discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries have low directionality: the constraint subsidizes their autonomy, control, and exit. Victims have high directionality: the constraint extracts by structurally denying them collective bargaining and public-goods remediation, locking costs in their seats. The excluded magisterial interpreters have no directionality toward this constraint because their claims are treated as illegitimate noise rather than costs to be extracted.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling by separating the claim (Mountain) from the metrics. Pure coordination without extraction would produce Îµ near 0.0 and no identifiable victims. The presence of victims at Îµ=0.25, combined with active enforcement and moderate resistance, signals that the coordination is not pure. The Mountain claim functions as a false summit: a constructed constraint presented as natural law, maintained by ideological closure and legal enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_rights,
    'Are property rights and voluntary exchange genuinely pre-political natural law, or are they legal constructs that advantage holders of capital?',
    'Historical and anthropological evidence on the emergence of property regimes; cross-cultural variation in exchange norms and enforcement.',
    'If constructed, the mountain claim collapses into tangled_rope or snare; the false summit is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_rights, empirical, 'Whether property rights are natural or constructed').

omega_variable(
    enforcement_naturality_ambiguity,
    'Does the constraint''s persistence depend on active enforcement (contract law, private arbitration) or would spontaneous order maintain it without institutional backing?',
    'Comparative analysis of AI governance outcomes in jurisdictions with weak formal contract enforcement.',
    'If active enforcement is required, the mountain classification is false and the constraint is at best a tangled rope maintained by state-backed coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_naturality_ambiguity, empirical, 'Whether the constraint requires active enforcement to persist').

omega_variable(
    kernel_reading_hegemony,
    'Is the market libertarian reading achieving institutional hegemony over its sibling readings, or does the kernel remain genuinely contested?',
    'Track jurisdictional adoption of market-based AI governance frameworks versus democratic and magisterial alternatives over the next decade.',
    'If hegemonic, the kernel may be collapsing toward a single reading, dissolving the contest the committer frame presupposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_hegemony, conceptual, 'Whether the kernel remains multi-valent or is collapsing to one reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(ai_g_tr_t25, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 25, 0.23).
narrative_ontology:measurement(ai_g_tr_t30, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 15, 0.2).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(ai_g_be_t25, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 25, 0.24).
narrative_ontology:measurement(ai_g_be_t30, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 30, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_governance_legitimacy__market_libertarian_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, democratic_pluralist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_governance_legitimacy kernel. Each reading instantiates a structurally distinct claim about the source of governance legitimacy. They form a constraint family linked by sibling relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
