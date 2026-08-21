% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__democratic_pluralist_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__democratic_pluralist_reading
 *   human_readable: AI Governance Legitimacy: Democratic Pluralist Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint describes a democratic-pluralist reading of AI governance
 *   legitimacy, where authority derives from inclusive public deliberation
 *   and consent, rather than from a single religious, technocratic, or
 *   market-driven tradition. It posits that principles for AI emerge from
 *   transparent political processes that balance diverse values. The
 *   encyclical is seen as one important voice among many, not a singular
 *   authoritative interpretation. This reading is framed as a 'scaffold'
 *   because it aims to build and support participatory infrastructure for AI
 *   governance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.4).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.3).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "AI Governance Legitimacy: Democratic Pluralist Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, '8912f96d-a79c-4c18-a8f0-221fc57a419f').
narrative_ontology:cs_kernel_codification('8912f96d-a79c-4c18-a8f0-221fc57a419f', formalized).
narrative_ontology:cs_authority_grounding('8912f96d-a79c-4c18-a8f0-221fc57a419f', practice).
narrative_ontology:cs_interpretation_layer_present('8912f96d-a79c-4c18-a8f0-221fc57a419f').
narrative_ontology:cs_reading_relation('8912f96d-a79c-4c18-a8f0-221fc57a419f', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('8912f96d-a79c-4c18-a8f0-221fc57a419f', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('8912f96d-a79c-4c18-a8f0-221fc57a419f', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('8912f96d-a79c-4c18-a8f0-221fc57a419f', foundational, legitimacy_from_consent_of_governed).
narrative_ontology:cs_axiom_status(legitimacy_from_consent_of_governed, holdable).
narrative_ontology:cs_axiom_grounding('8912f96d-a79c-4c18-a8f0-221fc57a419f', legitimacy_from_consent_of_governed, deontological).
narrative_ontology:cs_axiom('8912f96d-a79c-4c18-a8f0-221fc57a419f', foundational, pluralism_of_values_in_public_reason).
narrative_ontology:cs_axiom_status(pluralism_of_values_in_public_reason, holdable).
narrative_ontology:cs_axiom_grounding('8912f96d-a79c-4c18-a8f0-221fc57a419f', pluralism_of_values_in_public_reason, conventional).
narrative_ontology:cs_reference_frame('8912f96d-a79c-4c18-a8f0-221fc57a419f', inclusive_public_reason_framework).
narrative_ontology:cs_drift_state('8912f96d-a79c-4c18-a8f0-221fc57a419f', contemporary_political_polarization, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('8912f96d-a79c-4c18-a8f0-221fc57a419f', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, excluded_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, authoritarian_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_elites).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, religious_authorities).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, deliberative_democracy_theory).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, human_rights_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for establishing and maintaining the legal and procedural frameworks for democratic deliberation on AI, ensuring transparency, and enforcing decisions through legislative and judicial means. They gain legitimacy and stability from this process.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Actively participate in and benefit from inclusive deliberative processes, advocating for diverse public interests and holding institutions accountable. Their influence and legitimacy are enhanced by this framework.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, biographical, mobile, global).

% Their rights and interests are explicitly protected and given voice within the deliberative framework, preventing their marginalization by dominant groups or powerful actors. They benefit from the inclusive nature of the process.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    powerless, biographical, constrained, global).

% Populations who, due to systemic barriers, lack of access, or authoritarian control, are unable to participate meaningfully in democratic deliberation. They bear the cost of decisions made without their input, even if the intent is inclusive.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, excluded_populations, payer,
    powerless, generational, trapped, global).

% Regimes that actively suppress democratic deliberation and consent. This constraint challenges their legitimacy and operational model, imposing a 'cost' in terms of ideological and political pressure, and potential isolation from global governance norms.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, authoritarian_regimes, payer,
    institutional, generational, constrained, global).

% Groups who prefer governance based on technical expertise and efficiency optimization, rather than broad public deliberation. They bear the 'cost' of having their preferred mode of authority subordinated to democratic processes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_elites, payer,
    powerful, biographical, mobile, global).

% Authorities who claim a unique interpretive monopoly on ethical principles for AI governance. They bear the 'cost' of having their authority relativized to 'one voice among many' within a pluralistic public reason framework.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, religious_authorities, payer,
    organized, generational, constrained, global).

% Academics, researchers, and policy analysts who study the efficacy and legitimacy of AI governance models. They provide critical assessment of whether democratic deliberation genuinely achieves its stated goals.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__democratic_pluralist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the diverse values, ethical principles, and societal priorities of a pluralistic global society into legitimate and actionable AI governance frameworks, ensuring broad public acceptance and trust.
% TRANSFER_FUNCTION: Transfers decision-making authority and interpretive power from concentrated, often self-appointed, elites (technocratic, religious, market-driven) to distributed, inclusive, and accountable democratic processes and institutions.
% ABSENT_VOICES: Future generations, non-human entities (e.g., ecosystems affected by AI infrastructure), and populations under authoritarian regimes who are systematically denied participation. They would advocate for long-term sustainability, ecological justice, and universal human rights in AI development.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, AI governance would likely default to technocratic, market-driven, or ideologically narrow models, leading to fragmented, less legitimate, and potentially unjust outcomes. Public trust would erode, and the social contract around AI would destabilize.
% FOUNDING_PROBLEM: The challenge of legitimizing AI governance in a globally pluralistic and democratic context, preventing its capture by narrow interests, and ensuring that AI development aligns with broad societal values rather than specific ideological or technical imperatives.
% FOUNDING_PROBLEM_CORROBORATION: Democratic theorists, human rights advocates, and international civil society organizations consistently highlight the ongoing and urgent need for inclusive, legitimate, and democratically accountable AI governance, independent of specific religious or technocratic claims. This is corroborated by global surveys showing public distrust in AI governance by unelected bodies.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_governance_legitimacy__democratic_pluralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).
:- end_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.40) is moderate, reflecting the inherent costs and friction of democratic processes, which can 'extract' time, compromise, and resources from participants, but without the asymmetric rent-seeking of a Snare. Suppression (0.30) is present as democratic decisions must be enforced, and non-democratic alternatives (e.g., authoritarian control, technocratic unilateralism) are actively resisted or suppressed. Theater ratio (0.15) is low, indicating a genuine commitment to functional deliberation, though initial stages may involve some performative elements. Accessibility collapse (0.40) is moderate, as while democratic processes aim to be inclusive, they still define and limit the 'accessible' pathways for influence. Resistance (0.50) is moderate, as democratic processes inherently involve contestation and opposition from those whose views are not adopted, but this resistance is channeled within the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of democratic institutions and civil society, this constraint is a legitimate and necessary framework for equitable AI governance. From the perspective of technocratic elites or religious authorities, it might be seen as an illegitimate imposition that dilutes expertise or divine guidance. The engine's per-seat classification will reflect these structural differences.
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic institutions, civil society, and minority rights holders are beneficiaries, gaining legitimacy, voice, and protection. Excluded populations and authoritarian regimes are targets, as the constraint actively challenges their lack of participation or their non-democratic mode of governance. Technocratic elites and religious authorities are also targets, as their claims to singular authority are explicitly relativized within this pluralist framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Scaffold, designed to build and support a new mode of governance. Its 'sunset clause' is implicit in its transitional nature: once robust, inclusive, and legitimate AI governance is established, the 'scaffold' of actively building it might recede, replaced by a more stable 'rope' of ongoing democratic practice. The risk of mandatrophy would be if the deliberative processes became performative (rising theater_ratio) while still claiming to be building legitimacy, or if they became captured by specific interests (rising extractiveness), turning the scaffold into a Tangled Rope or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuineness_of_deliberation,
    'Is the democratic deliberation genuinely inclusive and influential, or is it primarily a performative exercise that legitimizes pre-determined outcomes?',
    'Empirical analysis of policy outcomes: do they reflect the deliberative input, especially from marginalized groups, or do they consistently align with powerful interests? Track participation rates, diversity of voices, and policy impact over time.',
    'If performative, the constraint''s effective extractiveness and theater_ratio are higher, and its classification shifts towards a Snare or Piton, as its coordination function is undermined by its extractive reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuineness_of_deliberation, empirical, 'Assessing the authenticity and impact of democratic deliberation in AI governance.').

omega_variable(
    scope_of_consent_of_governed,
    'How is ''consent of the governed'' defined and measured for future generations, non-human entities, or populations under non-democratic regimes affected by AI?',
    'Development of robust intergenerational equity frameworks, advocacy for non-human rights in AI, and international legal mechanisms to address cross-border AI impacts on unrepresented populations. This is a conceptual and preference-driven resolution.',
    'If consent is narrowly defined, the constraint''s effective suppression and extractiveness are higher for unrepresented groups, potentially shifting its classification towards a Tangled Rope or Snare for those populations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_consent_of_governed, conceptual, 'Defining the boundaries and mechanisms of consent in AI governance for diverse stakeholders.').

omega_variable(
    enforcement_of_pluralism,
    'To what extent can a pluralist democratic framework effectively ''suppress'' or resist the re-assertion of technocratic or religious interpretive monopolies in AI governance?',
    'Longitudinal study of policy debates and institutional power dynamics: does the pluralist framework consistently prevent the dominance of single-tradition claims, or do these claims periodically re-assert themselves and capture the deliberative process?',
    'If the framework fails to resist interpretive monopolies, its effective suppression of alternative views is lower, and its claimed type as a Scaffold for pluralism is undermined, potentially revealing a deeper, more extractive constraint operating beneath.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_of_pluralism, empirical, 'The capacity of democratic pluralism to resist capture by single-tradition claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(ai_g_tr_t30, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(ai_g_tr_t40, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(ai_g_tr_t50, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(ai_g_be_t30, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(ai_g_be_t40, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(ai_g_be_t50, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 10, 0.23).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement(ai_g_su_t30, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(ai_g_su_t40, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement(ai_g_su_t50, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_governance_legitimacy' kernel, which decomposes into multiple structurally distinct claims depending on the interpretive framework. This file represents the democratic-pluralist reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
