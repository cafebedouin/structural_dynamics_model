% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Federalist Millet System for Marriage Authority
 *   domain: legal/political/social
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.18).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.15).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Federalist Millet System for Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/political/social").

domain_priors:requires_active_enforcement(marriage_authority__federalist_millet_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '6582646a-5c5c-4bb7-a94e-df6f451ddd5f').
narrative_ontology:cs_kernel_codification('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', formalized).
narrative_ontology:cs_authority_grounding('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', lineage).
narrative_ontology:cs_interpretation_layer_present('6582646a-5c5c-4bb7-a94e-df6f451ddd5f').
narrative_ontology:cs_reading_relation('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', foundational, fragmented_authority_prevents_tyranny).
narrative_ontology:cs_axiom_status(fragmented_authority_prevents_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', fragmented_authority_prevents_tyranny, instrumental).
narrative_ontology:cs_axiom('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', foundational, minority_rights_require_distinct_personal_law).
narrative_ontology:cs_axiom_status(minority_rights_require_distinct_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', minority_rights_require_distinct_personal_law, deontological).
narrative_ontology:cs_reference_frame('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', consociational_pluralism_framework).
narrative_ontology:cs_drift_state('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6582646a-5c5c-4bb7-a94e-df6f451ddd5f', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_religious_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, diverse_cultural_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, majoritarian_political_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities benefit from the system's protection of their distinct personal laws regarding marriage, allowing them to maintain cultural and religious identity without majoritarian imposition. Exit would mean assimilation or loss of legal recognition for their practices.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_religious_communities, beneficiary,
    moderate, generational, identity_locked, national).

% Similar to religious communities, these groups are protected from a uniform civil code, enabling them to practice their unique marriage customs. Their identity is deeply tied to these practices.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, diverse_cultural_groups, beneficiary,
    moderate, generational, identity_locked, national).

% The federal government administers the pluralistic legal framework, ensuring that the boundaries between different personal law systems are respected and that no single system dominates. It acts as an arbiter and enforcer of the consociational arrangement.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, federal_government, agenda_setter,
    institutional, civilizational, constrained, national).

% These parties 'pay' by foregoing the ability to impose a single, uniform family law code across the entire nation. Their legislative reach in this domain is deliberately limited by the fragmented authority, which they may view as an impediment to national unity or efficiency.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, majoritarian_political_parties, payer,
    powerful, biographical, mobile, national).

% Advocates for a Uniform Civil Code, they are structurally excluded from achieving their primary policy goal within this system, which is designed to prevent majoritarian legislative domination in personal law. Their efforts are channeled into advocacy and legal challenges rather than direct legislative action.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, secularist_advocates, excluded,
    organized, biographical, constrained, national).

% These scholars analyze the effectiveness and implications of the federalist millet system as a mechanism for anti-tyranny and consociational governance, often comparing it to other models of legal pluralism.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse religious and cultural communities by allowing them to maintain distinct personal laws under a common federal umbrella, preventing conflict over family law norms and ensuring social stability through power-sharing.
% TRANSFER_FUNCTION: Transfers the power to define marriage norms from a single majoritarian legislature to multiple recognized community authorities, with the federal state acting as an arbiter of boundaries and guarantor of pluralism.
% ABSENT_VOICES: Secularist advocates and proponents of a Uniform Civil Code are structurally excluded from imposing their vision, as the system is designed to prevent majoritarian legislative domination in this sphere. They would argue for a single, state-defined civil marriage code.
% DISAPPEARANCE_RATIONALE: If this fragmented authority vanished overnight, a single, uniform family law code would likely be imposed, leading to significant social unrest, legal challenges, and the erosion of distinct community identities and practices, fundamentally reorganizing the social and legal landscape.
% FOUNDING_PROBLEM: The historical problem of majoritarian religious or cultural groups imposing their family law norms on minorities, leading to social instability, inter-communal conflict, and the tyranny of the majority in matters of personal identity and practice.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and representatives of minority communities corroborate the ongoing relevance of preventing majoritarian overreach in personal law, citing persistent political pressures for uniformity and the continued importance of distinct cultural identities.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.18, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_bargain_vs_genuine_protection,
    'Is the federalist millet system truly preventing tyranny and protecting all within minority communities, or is it primarily an elite bargain that entrenches the power of community leaders and may not serve vulnerable individuals within those communities?',
    'Empirical studies on intra-community power dynamics, access to justice for marginalized members, and the actual lived experiences of individuals under different personal law codes.',
    'If primarily an elite bargain, the effective extraction from vulnerable individuals within communities might be higher than the base extractiveness suggests, potentially reclassifying it as a Tangled Rope or Snare from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_bargain_vs_genuine_protection, conceptual, 'Whether the system''s benefits accrue broadly or are captured by internal elites.').

omega_variable(
    inconsistent_rights_application,
    'Does the fragmentation of marriage authority lead to inconsistent application of fundamental rights, particularly for women or other vulnerable groups, across different personal law codes?',
    'Comparative legal analysis of judicial outcomes in personal law cases across different communities, and human rights reports on gender equality within these systems.',
    'If significant inconsistencies are found that disadvantage vulnerable groups, the system''s suppression metric would be higher for those groups, and its classification might shift towards a Tangled Rope or Snare from their perspective, indicating a failure to uphold a constitutional floor of rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inconsistent_rights_application, empirical, 'Impact of legal pluralism on uniform rights application.').

omega_variable(
    federal_vs_communal_sovereignty,
    'Where does the ultimate sovereignty over marriage law reside in this system: with the federal constitutional framework or with the autonomous religious/cultural communities?',
    'Analysis of landmark judicial decisions regarding the limits of communal autonomy and the scope of federal constitutional review in personal law matters.',
    'If federal sovereignty is consistently asserted to override communal norms, the ''communal_autonomy_reading'' would be foreclosed, and this reading''s ''rope'' classification might be challenged if the federal role becomes more extractive or suppressive of communal distinctiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_vs_communal_sovereignty, conceptual, 'Clarifying the locus of ultimate authority in a pluralistic legal system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__federalist_millet_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__federalist_millet_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__federalist_millet_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__federalist_millet_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__federalist_millet_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(marr_be_t10, marriage_authority__federalist_millet_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(marr_be_t20, marriage_authority__federalist_millet_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(marr_be_t30, marriage_authority__federalist_millet_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(marr_be_t40, marriage_authority__federalist_millet_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(marr_be_t50, marriage_authority__federalist_millet_reading, base_extractiveness, 50, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__federalist_millet_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(marr_su_t10, marriage_authority__federalist_millet_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(marr_su_t20, marriage_authority__federalist_millet_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(marr_su_t30, marriage_authority__federalist_millet_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(marr_su_t40, marriage_authority__federalist_millet_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(marr_su_t50, marriage_authority__federalist_millet_reading, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
