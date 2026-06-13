% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Protections: Expansive Reading of Minority Religious Governance
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint represents the 'expansive reading' of the Treaty of
 *   Lausanne's protections for non-Muslim minorities in Turkey. Under this
 *   reading, the treaty guarantees the functional continuity of pre-1923
 *   religious governance, including institutional self-administration,
 *   property rights, and the ability to form clergy via theological schools.
 *   This interpretation positions the protections as a coordination mechanism
 *   for minority self-governance, with minimal extraction from beneficiaries,
 *   though it requires active enforcement against more restrictive
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.2).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.3).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Protections: Expansive Reading of Minority Religious Governance").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '76624e80-2258-49a5-bcad-e2e3b642d696').
narrative_ontology:cs_kernel_codification('76624e80-2258-49a5-bcad-e2e3b642d696', fixed_text).
narrative_ontology:cs_authority_grounding('76624e80-2258-49a5-bcad-e2e3b642d696', lineage).
narrative_ontology:cs_interpretation_layer_present('76624e80-2258-49a5-bcad-e2e3b642d696').
narrative_ontology:cs_reading_relation('76624e80-2258-49a5-bcad-e2e3b642d696', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('76624e80-2258-49a5-bcad-e2e3b642d696', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('76624e80-2258-49a5-bcad-e2e3b642d696', foundational, institutional_autonomy_is_fundamental).
narrative_ontology:cs_axiom_status(institutional_autonomy_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('76624e80-2258-49a5-bcad-e2e3b642d696', institutional_autonomy_is_fundamental, deontological).
narrative_ontology:cs_axiom('76624e80-2258-49a5-bcad-e2e3b642d696', foundational, pre_1923_status_quo_is_protected).
narrative_ontology:cs_axiom_status(pre_1923_status_quo_is_protected, holdable).
narrative_ontology:cs_axiom_grounding('76624e80-2258-49a5-bcad-e2e3b642d696', pre_1923_status_quo_is_protected, conventional).
narrative_ontology:cs_reference_frame('76624e80-2258-49a5-bcad-e2e3b642d696', full_institutional_autonomy_1923).
narrative_ontology:cs_drift_state('76624e80-2258-49a5-bcad-e2e3b642d696', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('76624e80-2258-49a5-bcad-e2e3b642d696', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, non_muslim_minority_foundations).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_religious_communities).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, minority_rights_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, religious_freedom_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These foundations (e.g., Greek Orthodox, Armenian, Jewish) rely on the expansive reading to maintain their institutional autonomy, property, and educational facilities. Their existence is tied to this interpretation, making exit unthinkable.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, non_muslim_minority_foundations, beneficiary,
    organized, generational, identity_locked, national).

% The members of these communities directly benefit from the continuity of their religious institutions, schools, and clergy. Their cultural and religious identity is deeply intertwined with these structures.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_religious_communities, beneficiary,
    powerless, generational, identity_locked, local).

% As the signatory state, Turkey is bound by the Treaty of Lausanne. Its interpretation and enforcement of the protections directly determine the scope of minority rights. The state's position often shifts between expansive and restrictive readings based on political context.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, generational, constrained, national).

% Signatories to the Treaty of Lausanne (e.g., UK, France, Italy) and other international actors (e.g., EU, Council of Europe) monitor Turkey's compliance. Their diplomatic pressure and legal mechanisms (like the ECHR) can influence the Turkish state's interpretation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_states, observer,
    institutional, generational, analytical, global).

% These bodies (e.g., UN Human Rights Committee) provide legal opinions and recommendations, influencing the international discourse around minority rights and the interpretation of the Lausanne Treaty. They do not directly enforce but shape legitimacy.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, international_human_rights_bodies, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continued existence and self-administration of pre-1923 non-Muslim religious minority institutions within the Turkish state, ensuring their functional continuity despite changes in sovereignty.
% TRANSFER_FUNCTION: Transfers the right to self-administer religious institutions, manage property, and educate clergy to minority communities, rather than these functions being fully absorbed by the state. It also transfers the burden of upholding these rights to the Turkish state.
% ABSENT_VOICES: Ultra-nationalist factions within Turkey would object, arguing that such expansive rights undermine national unity and sovereignty. They are present in domestic political discourse but are structurally excluded from the international legal framework of the treaty itself.
% DISAPPEARANCE_RATIONALE: If these protections vanished, the legal basis for minority religious institutions' autonomy, property, and educational rights would disappear. The Turkish state would likely absorb or nationalize these functions, leading to the rapid dissolution or severe curtailment of minority religious life as it currently exists.
% FOUNDING_PROBLEM: The problem of ensuring the rights and continued existence of non-Muslim minorities in the newly formed Republic of Turkey after the collapse of the Ottoman Empire, preventing forced assimilation or displacement.
% FOUNDING_PROBLEM_CORROBORATION: Minority religious communities and international human rights organizations consistently attest that the problem of protecting minority rights and cultural continuity remains live, citing ongoing challenges to property rights, educational autonomy, and the functioning of religious institutions. The Turkish state's official position also acknowledges the treaty's role in minority protection, though its interpretation varies.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.2) because the primary function is to coordinate the self-governance of minority institutions, not to extract resources from them. Suppression is moderate (0.3) as the Turkish state's interpretation has historically varied, requiring active advocacy and international pressure to uphold the expansive reading. Theater ratio is low (0.1) as the protections, when upheld, genuinely enable the functioning of these institutions. The temporal measurements reflect periods of increased pressure on minority rights (e.g., mid-20th century) and subsequent periods of partial liberalization or international scrutiny.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of minority communities, this constraint is a vital Rope, ensuring their survival. From the perspective of the Turkish state, particularly those advocating a restrictive reading, it can be seen as an external imposition on national sovereignty, requiring 'active enforcement' to limit its scope. The engine's classification will reflect the structural benefits to minorities and the enforcement burden on the state.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious communities and their foundations are the primary beneficiaries (d near 0.0) as the constraint directly enables their institutional existence and autonomy. The Turkish state, while the agenda-setter, is not a beneficiary of this specific expansive reading; rather, it bears the cost of upholding these rights (d near 0.5, or slightly higher when resisting the expansive reading). There are no direct 'victims' of this reading, as it aims to protect, not extract from, the minority groups.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_magnitude,
    'To what extent has the Turkish state''s actual practice and legal interpretation drifted from this expansive reading over time?',
    'Analysis of court rulings, property confiscations, educational policies, and international human rights reports over the past century.',
    'If drift is substantial and unacknowledged, the constraint''s effective extractiveness and suppression for minorities would be higher than measured, potentially reclassifying it as a Tangled Rope or Snare from their perspective, despite the ''claimed_type'' of Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_drift_magnitude, empirical, 'Gap between the expansive reading and state practice.').

omega_variable(
    international_enforcement_efficacy,
    'How effective are guarantor states and international human rights bodies in compelling adherence to this expansive reading?',
    'Case studies of diplomatic interventions, ECHR judgments, and their implementation rates by Turkey.',
    'If enforcement is weak, the constraint''s ''rope'' nature is precarious, making it vulnerable to becoming a Piton (if ignored) or a Snare (if actively undermined without consequence). If strong, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforcement_efficacy, empirical, 'Efficacy of external enforcement of minority protections.').

omega_variable(
    natural_law_vs_treaty_obligation,
    'Is the protection of minority religious governance an inherent natural right, or solely a contingent treaty obligation?',
    'Philosophical and legal analysis of human rights theory vs. positive international law. This is a conceptual distinction.',
    'If viewed as a natural right, the constraint gains a ''mountain-like'' quality in its normative force, making its violation a deeper injustice. If solely a treaty obligation, its persistence is more vulnerable to political will and renegotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_treaty_obligation, conceptual, 'Conceptual grounding of minority rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1923, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__expansive_reading, theater_ratio, 1923, 0.05).
narrative_ontology:measurement(laus_tr_t1950, lausanne_minority_protections__expansive_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(laus_tr_t1980, lausanne_minority_protections__expansive_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(laus_tr_t2000, lausanne_minority_protections__expansive_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(laus_tr_t2023, lausanne_minority_protections__expansive_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 1923, 0.1).
narrative_ontology:measurement(laus_be_t1950, lausanne_minority_protections__expansive_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(laus_be_t1980, lausanne_minority_protections__expansive_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(laus_be_t2000, lausanne_minority_protections__expansive_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(laus_be_t2023, lausanne_minority_protections__expansive_reading, base_extractiveness, 2023, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__expansive_reading, suppression_requirement, 1923, 0.2).
narrative_ontology:measurement(laus_su_t1950, lausanne_minority_protections__expansive_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(laus_su_t1980, lausanne_minority_protections__expansive_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(laus_su_t2000, lausanne_minority_protections__expansive_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(laus_su_t2023, lausanne_minority_protections__expansive_reading, suppression_requirement, 2023, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, identity_coordination).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Lausanne minority protections kernel. This 'expansive reading' focuses on institutional autonomy, property, and education, distinct from a 'restrictive reading' (individual worship only) and a 'guarantor reading' (international enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
