% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Expansive Universalist Reading of Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the 'expansive universalist' reading of
 *   constitutional equality clauses, which posits that equality is a
 *   self-evident, universal truth applicable to all humans, irrespective of
 *   historical exclusions. It views historical discrimination as a failure to
 *   live up to this truth, rather than as binding precedent. This reading
 *   supports a low legitimacy threshold for rights expansion via judicial
 *   interpretation, aiming for a universal beneficiary set. It is one reading
 *   of the 'equality_clause_scope' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.15).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.2).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.15).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Expansive Universalist Reading of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '2541cb07-d8cb-414e-8eb8-1e8534b1a03d').
narrative_ontology:cs_kernel_codification('2541cb07-d8cb-414e-8eb8-1e8534b1a03d', fixed_text).
narrative_ontology:cs_authority_grounding('2541cb07-d8cb-414e-8eb8-1e8534b1a03d', lineage).
narrative_ontology:cs_interpretation_layer_present('2541cb07-d8cb-414e-8eb8-1e8534b1a03d').
narrative_ontology:cs_reading_relation('2541cb07-d8cb-414e-8eb8-1e8534b1a03d', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('2541cb07-d8cb-414e-8eb8-1e8534b1a03d', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('2541cb07-d8cb-414e-8eb8-1e8534b1a03d', foundational, equality_is_universal_and_inherent).
narrative_ontology:cs_axiom_status(equality_is_universal_and_inherent, holdable).
narrative_ontology:cs_axiom_grounding('2541cb07-d8cb-414e-8eb8-1e8534b1a03d', equality_is_universal_and_inherent, deontological).
narrative_ontology:cs_axiom('2541cb07-d8cb-414e-8eb8-1e8534b1a03d', foundational, historical_exclusion_is_hypocrisy_not_precedent).
narrative_ontology:cs_axiom_status(historical_exclusion_is_hypocrisy_not_precedent, holdable).
narrative_ontology:cs_axiom_grounding('2541cb07-d8cb-414e-8eb8-1e8534b1a03d', historical_exclusion_is_hypocrisy_not_precedent, conventional).
narrative_ontology:cs_reference_frame('2541cb07-d8cb-414e-8eb8-1e8534b1a03d', natural_rights_republicanism).
narrative_ontology:cs_drift_state('2541cb07-d8cb-414e-8eb8-1e8534b1a03d', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2541cb07-d8cb-414e-8eb8-1e8534b1a03d', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, all_humans).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, civil_rights_advocates).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, judicial_activists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, historical_exclusionary_institutions).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, natural_rights_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, universal_human_dignity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The theoretical beneficiaries of this reading, as it asserts their inherent and universal right to equality, regardless of any historical or social exclusions. Their benefit is the recognition of their fundamental dignity and rights.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, all_humans, beneficiary,
    powerless, generational, identity_locked, universal).

% Actively champion this reading, using it as a basis for legal challenges and social movements to expand the scope of equality to previously excluded groups. They invest significant resources in litigation and public education to advance this interpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_advocates, agenda_setter,
    organized, generational, constrained, national).

% Judges who interpret constitutional equality clauses broadly, often expanding rights through judicial review rather than waiting for legislative action. This reading provides a strong philosophical grounding for their interpretive approach.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, judicial_activists, beneficiary,
    institutional, biographical, constrained, national).

% Institutions and social structures that historically benefited from or enforced discriminatory practices. This reading challenges their legitimacy and forces them to dismantle exclusionary systems, incurring costs of adaptation and loss of privilege.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historical_exclusionary_institutions, payer,
    institutional, generational, constrained, national).

% Adherents to a competing reading of the equality clause, who believe its scope is fixed by the original intent of the framers, typically limiting it to a narrow set of propertied white males. They view this expansive reading as judicial overreach.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, restrictive_originalists, excluded,
    institutional, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent philosophical and legal framework for understanding and applying the principle of equality across diverse human populations, facilitating social cohesion and justice by asserting universal rights.
% TRANSFER_FUNCTION: Transfers moral and legal standing, and eventually material resources and opportunities, from historically privileged groups and institutions to historically excluded or marginalized groups, by expanding the definition of who is 'equal'.
% ABSENT_VOICES: Those who benefit from existing hierarchies and exclusions, particularly those who adhere to a restrictive originalist interpretation of constitutional texts, are actively marginalized in the discourse of this reading. They would argue against the expansion of rights and for adherence to historical precedent.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal and moral basis for many civil rights advancements would erode. Courts would likely revert to more restrictive interpretations, and social movements for equality would lose a powerful philosophical anchor, leading to a significant rearrangement of legal and social structures.
% FOUNDING_PROBLEM: The historical and ongoing exclusion of various groups (racial minorities, women, LGBTQ+ individuals, etc.) from full participation and equal treatment under the law, despite foundational claims of universal equality.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, international human rights bodies, and academic scholars consistently attest that the problem of exclusion and inequality remains live, requiring ongoing application of this expansive reading. This corroboration comes from outside the direct beneficiaries of judicial activism, reflecting broad societal consensus on the need for continued progress.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).
:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.15) is relatively low because this reading primarily aims to expand benefits and correct historical injustices, rather than to extract rents. Any 'extraction' is from those who previously benefited from exclusion. Suppression (0.2) is also low, as its enforcement primarily involves legal challenges and judicial mandates, not overt coercion against individuals. Theater ratio (0.1) is minimal, as the reading's proponents genuinely seek to realize its stated goals. Accessibility collapse (0.7) is high because, once adopted, this reading significantly narrows the legitimate avenues for maintaining discriminatory practices. Resistance (0.3) is moderate, reflecting ongoing political and legal challenges from opposing interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civil rights advocates, this reading is a pure rope, coordinating society towards a more just future. From the perspective of restrictive originalists, it is a snare, coercively dismantling established social orders and judicial traditions. The engine's classification will reflect the structural reality of expanded benefits for many and costs for those whose privileges are challenged.
 *
 * DIRECTIONALITY LOGIC:
 *   All humans are the ultimate beneficiaries, as this reading seeks to extend equality universally. Civil rights advocates and judicial activists are direct beneficiaries and agenda-setters, as their work is validated and empowered by this interpretation. Historically exclusionary institutions are payers, as they bear the costs of dismantling discriminatory practices. Restrictive originalists are excluded, as their interpretive framework is directly challenged and marginalized by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the ''self-evident universal truth'' of equality a genuine natural law, or a socially constructed and evolving moral consensus?',
    'Philosophical consensus on the grounding of moral claims, or cross-cultural anthropological studies on the universality of specific equality norms.',
    'If a natural law, the constraint''s ''mountain'' aspects are stronger, making its claims less contestable. If a social construct, its persistence depends more on active enforcement and ongoing advocacy, shifting its classification towards a ''rope'' or ''tangled_rope'' for its beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity in the ontological status of equality as a ''self-evident truth''.').

omega_variable(
    judicial_legitimacy_threshold,
    'What is the appropriate legitimacy threshold for judicial expansion of equality rights versus legislative action?',
    'Empirical study of public acceptance of judicially-mandated rights versus legislatively-enacted rights, or constitutional theory debates on judicial review limits.',
    'A higher threshold for judicial action would shift the burden of rights expansion to the legislative process, potentially slowing progress but increasing democratic legitimacy. A lower threshold (as favored by this reading) allows quicker correction of injustices but risks accusations of judicial overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_legitimacy_threshold, preference, 'Debate over the proper institutional mechanism for expanding equality rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1950, equality_clause_scope__expansive_universalist, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(equa_tr_t1970, equality_clause_scope__expansive_universalist, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(equa_tr_t1990, equality_clause_scope__expansive_universalist, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(equa_tr_t2010, equality_clause_scope__expansive_universalist, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(equa_tr_t2024, equality_clause_scope__expansive_universalist, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1950, equality_clause_scope__expansive_universalist, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(equa_be_t1970, equality_clause_scope__expansive_universalist, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(equa_be_t1990, equality_clause_scope__expansive_universalist, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(equa_be_t2010, equality_clause_scope__expansive_universalist, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(equa_be_t2024, equality_clause_scope__expansive_universalist, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1950, equality_clause_scope__expansive_universalist, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(equa_su_t1970, equality_clause_scope__expansive_universalist, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(equa_su_t1990, equality_clause_scope__expansive_universalist, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement(equa_su_t2010, equality_clause_scope__expansive_universalist, suppression_requirement, 2010, 0.19).
narrative_ontology:measurement(equa_su_t2024, equality_clause_scope__expansive_universalist, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'equality_clause_scope' kernel. It focuses on the expansive universalist interpretation, distinct from restrictive originalist and progressive textualist readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
