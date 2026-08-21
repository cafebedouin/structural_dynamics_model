% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__magisterial_subsidiarity_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__magisterial_subsidiarity_reading
 *   human_readable: AI Governance Legitimacy (Magisterial Subsidiarity Reading)
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint represents a specific reading of AI governance
 *   legitimacy, grounded in Catholic Social Doctrine as interpreted by the
 *   Magisterium. It asserts that AI must be subordinated to principles of
 *   common good, subsidiarity, solidarity, and universal destination of
 *   goods, demanding transparent accountability and participatory governance.
 *   This reading explicitly rejects purely technocratic or market-driven
 *   approaches, positioning itself as a moral counter-narrative. The claimed
 *   type is 'tangled_rope' because it genuinely seeks to coordinate societal
 *   benefit while simultaneously imposing significant costs on powerful
 *   actors whose operations conflict with its principles, requiring active
 *   enforcement through moral suasion and advocacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.65).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy (Magisterial Subsidiarity Reading)").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'dd1ca37a-d46a-4d95-bca8-a954a978549f').
narrative_ontology:cs_kernel_codification('dd1ca37a-d46a-4d95-bca8-a954a978549f', formalized).
narrative_ontology:cs_authority_grounding('dd1ca37a-d46a-4d95-bca8-a954a978549f', lineage).
narrative_ontology:cs_interpretation_layer_present('dd1ca37a-d46a-4d95-bca8-a954a978549f').
narrative_ontology:cs_reading_relation('dd1ca37a-d46a-4d95-bca8-a954a978549f', ai_governance_legitimacy__technocratic_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('dd1ca37a-d46a-4d95-bca8-a954a978549f', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd1ca37a-d46a-4d95-bca8-a954a978549f', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_axiom('dd1ca37a-d46a-4d95-bca8-a954a978549f', foundational, human_dignity_as_ultimate_end).
narrative_ontology:cs_axiom_status(human_dignity_as_ultimate_end, holdable).
narrative_ontology:cs_axiom_grounding('dd1ca37a-d46a-4d95-bca8-a954a978549f', human_dignity_as_ultimate_end, deontological).
narrative_ontology:cs_axiom('dd1ca37a-d46a-4d95-bca8-a954a978549f', foundational, common_good_trumps_individual_profit).
narrative_ontology:cs_axiom_status(common_good_trumps_individual_profit, holdable).
narrative_ontology:cs_axiom_grounding('dd1ca37a-d46a-4d95-bca8-a954a978549f', common_good_trumps_individual_profit, deontological).
narrative_ontology:cs_reference_frame('dd1ca37a-d46a-4d95-bca8-a954a978549f', magisterial_social_doctrine_tradition).
narrative_ontology:cs_drift_state('dd1ca37a-d46a-4d95-bca8-a954a978549f', contemporary_ai_acceleration, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('dd1ca37a-d46a-4d95-bca8-a954a978549f', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_entities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The authoritative interpreter of Catholic Social Doctrine, articulating principles like common good, subsidiarity, and solidarity as the moral foundation for AI governance. Exerts influence through encyclicals, moral suasion, and advocacy within international bodies. Its legitimacy is grounded in theological lineage.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Benefit from principles that prioritize human labor over automation, advocate for fair wages, and protect against job displacement without just transition. Their dignity and participation are central to the common good framework.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers, beneficiary,
    organized, biographical, constrained, global).

% Benefit from principles of universal destination of goods and solidarity, which demand equitable access to technology's benefits and protection from exploitative practices that exacerbate global inequalities. They are often targets of extractive AI models.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Bear the costs of accountability, transparent governance, and potential regulation that would limit their profit motives and market dominance. Their business models often conflict with the common good and universal destination of goods principles.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    powerful, immediate, constrained, global).

% Faces moral scrutiny and potential restrictions on the development and deployment of autonomous weapons systems and surveillance technologies, which conflict with principles of human dignity and peace. Their operations are often opaque and non-participatory.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Leverage Catholic Social Doctrine to advocate for ethical AI, human rights, and social justice. They gain moral authority and a coherent framework for their activism, amplifying their voice in policy debates.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, civil_society_advocates, beneficiary,
    moderate, biographical, mobile, national).

% Are influenced by, but not bound to, the Magisterium's interpretation. They consider these principles alongside other ethical frameworks and national interests when formulating AI policy. Their role is to balance diverse inputs.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_governments, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, human-centered ethical framework for AI development and deployment, guiding diverse actors (governments, corporations, civil society) toward shared goals of human dignity, social justice, and ecological sustainability, preventing purely profit-driven or technocratic outcomes.
% TRANSFER_FUNCTION: Transfers moral authority and legitimacy from purely technical or market-based justifications to a framework grounded in human dignity and social good. It demands a transfer of power and resources from concentrated tech entities to broader society, especially the vulnerable.
% ABSENT_VOICES: Pure market libertarians and radical technocrats are structurally excluded from this framework's core premises, as their foundational axioms (unfettered markets, pure optimization) are explicitly rejected by Catholic Social Doctrine. They would argue for minimal regulation and maximal innovation without moral constraints.
% DISAPPEARANCE_RATIONALE: If this framework vanished, the ethical discourse around AI would lose a significant, globally influential, and historically deep human-centered voice. The vacuum would likely be filled by more technocratic or market-driven narratives, leading to less protection for vulnerable populations and less emphasis on the common good in AI development.
% FOUNDING_PROBLEM: The problem of technology developing without sufficient ethical guidance, leading to dehumanization, exploitation, and exacerbation of social inequalities, particularly in the context of emerging powerful technologies like AI.
% FOUNDING_PROBLEM_CORROBORATION: Numerous international bodies, secular ethicists, and civil society organizations corroborate the ongoing problem of ethically unguided technological development, even if they do not endorse the specific Magisterial framework. Reports from the UN, UNESCO, and various academic institutions attest to the live nature of this problem.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.52) reflects the significant demands placed on powerful tech and financial entities to reorient their operations towards the common good, which entails a 'cost' to their current profit-maximizing models. Suppression (0.65) is high because this framework actively seeks to suppress alternative, less ethically constrained approaches to AI governance through moral authority, advocacy, and the promotion of international ethical guidelines. The theater ratio is low (0.20) as the Magisterium's engagement is primarily functional, aiming for real-world ethical impact rather than mere performance. Accessibility collapse is moderate (0.40) as alternative ethical frameworks exist, but this reading aims to make them less viable by offering a comprehensive, coherent alternative. Resistance (0.55) is substantial from those whose interests are challenged by this framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Magisterium and its beneficiaries (workers, Global South), this framework is a necessary coordination mechanism for human flourishing. From the perspective of the 'victims' (private tech monopolies, military-industrial complex), it is an extractive imposition that limits innovation and economic freedom. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium, workers, and Global South populations are beneficiaries (low d) as the constraint's principles are designed to protect and empower them. Private tech monopolies and the military-industrial complex are targets (high d) as the constraint demands they internalize social costs and subordinate profit to ethical principles. Civil society advocates are beneficiaries as they gain a powerful framework for their work. Secular governments are observers, weighing this framework among others.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is actively engaged in shaping an emerging field (AI governance), so mandatrophy is not currently a primary concern. Its mandate is live and evolving with technological development. The classification as a Tangled Rope acknowledges its dual function of genuine coordination (for the common good) and asymmetric extraction (from those who would prioritize profit over ethics), preventing mislabeling as either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_impact_measurement,
    'To what extent do the Magisterium''s ethical guidelines for AI actually translate into changes in corporate behavior or government policy, beyond rhetorical adoption?',
    'Longitudinal studies tracking the adoption of specific ethical recommendations by tech companies and governments, and their measurable impact on vulnerable populations.',
    'If the impact is minimal, the constraint''s effective extractiveness and suppression might be lower than intended, indicating a more ''piton-like'' or ''theater-heavy'' operation in practice, despite its strong normative claims. If impact is high, the classification as Tangled Rope is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_impact_measurement, empirical, 'Measures the real-world efficacy of moral suasion in AI governance.').

omega_variable(
    subsidiarity_vs_solidarity_tension,
    'How is the inherent tension between subsidiarity (decentralization, local autonomy) and solidarity (global responsibility, centralized coordination for common good) resolved in practical AI governance applications?',
    'Case studies of AI projects where these principles are applied, analyzing the decision-making processes and outcomes regarding local vs. global control and resource allocation.',
    'An overemphasis on subsidiarity could lead to fragmented, uncoordinated ethical efforts, reducing the constraint''s global impact. An overemphasis on solidarity could lead to overly centralized, top-down governance that stifles local innovation and participation, potentially increasing effective extraction from local actors. The balance affects the constraint''s internal coherence and practical implementation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidiarity_vs_solidarity_tension, conceptual, 'Examines the practical reconciliation of two core Catholic Social Doctrine principles in AI governance.').

omega_variable(
    theological_vs_secular_legitimacy,
    'Is the Magisterium''s authority for AI governance recognized as legitimate by secular actors (governments, corporations, non-religious civil society) on its own terms, or only as one voice among many in a pluralistic ethical landscape?',
    'Analysis of policy documents, international declarations, and corporate ethical statements to identify explicit references to or implicit alignment with Catholic Social Doctrine principles, and the stated rationale for such alignment.',
    'If secular actors primarily view it as ''one voice among many,'' the constraint''s effective suppression and extractiveness on non-adherents would be lower, as its moral authority is not universally binding. If its principles are widely adopted and cited as foundational, its influence and effective extraction would be higher, even on those not explicitly religious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_secular_legitimacy, conceptual, 'Assesses the scope of the Magisterium''s moral authority in a pluralistic global context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'AI governance legitimacy' kernel. Other readings include technocratic_optimization_reading, democratic_pluralist_reading, and market_libertarian_reading, each with distinct beneficiaries, victims, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
