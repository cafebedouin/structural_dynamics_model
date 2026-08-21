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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Expansive Universalist Reading of Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the 'expansive universalist' reading of
 *   constitutional equality clauses, which interprets equality as a
 *   self-evident, universal truth applicable to all humans, irrespective of
 *   historical exclusions. It views historical discrimination as a failure to
 *   live up to foundational principles, rather than as binding precedent.
 *   This reading supports judicial interpretation to expand rights and
 *   protections, leading to a universal beneficiary set. This story is one
 *   reading of the 'equality_clause_scope' kernel, alongside
 *   'restrictive_originalist' and 'progressive_textualist' readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.15).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.2).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.15).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Expansive Universalist Reading of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '246d326b-8e1e-41a4-82a2-98519108ba61').
narrative_ontology:cs_kernel_codification('246d326b-8e1e-41a4-82a2-98519108ba61', fixed_text).
narrative_ontology:cs_authority_grounding('246d326b-8e1e-41a4-82a2-98519108ba61', lineage).
narrative_ontology:cs_interpretation_layer_present('246d326b-8e1e-41a4-82a2-98519108ba61').
narrative_ontology:cs_reading_relation('246d326b-8e1e-41a4-82a2-98519108ba61', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('246d326b-8e1e-41a4-82a2-98519108ba61', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('246d326b-8e1e-41a4-82a2-98519108ba61', foundational, equality_is_self_evident_universal_truth).
narrative_ontology:cs_axiom_status(equality_is_self_evident_universal_truth, holdable).
narrative_ontology:cs_axiom_grounding('246d326b-8e1e-41a4-82a2-98519108ba61', equality_is_self_evident_universal_truth, deontological).
narrative_ontology:cs_axiom('246d326b-8e1e-41a4-82a2-98519108ba61', foundational, historical_exclusions_are_hypocrisy_not_precedent).
narrative_ontology:cs_axiom_status(historical_exclusions_are_hypocrisy_not_precedent, holdable).
narrative_ontology:cs_axiom_grounding('246d326b-8e1e-41a4-82a2-98519108ba61', historical_exclusions_are_hypocrisy_not_precedent, deontological).
narrative_ontology:cs_reference_frame('246d326b-8e1e-41a4-82a2-98519108ba61', post_enlightenment_universal_rights).
narrative_ontology:cs_drift_state('246d326b-8e1e-41a4-82a2-98519108ba61', contemporary_identity_politics_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('246d326b-8e1e-41a4-82a2-98519108ba61', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_marginalized_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, civil_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, conservative_political_factions).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, universal_human_rights_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, evolving_constitution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are the primary beneficiaries of this reading, as it extends constitutional protections and rights to them, overturning historical exclusions. Their identity is often tied to the struggle for these rights, making exit from the framework unthinkable.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_marginalized_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Actively champion this reading through litigation, public discourse, and legislative lobbying. They benefit from the expansion of rights and the moral authority it grants their cause. Their work is to continuously push the boundaries of 'universal' application.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_advocates, agenda_setter,
    organized, generational, constrained, national).

% Judges and legal scholars who adopt this reading, using judicial review to expand the scope of equality. They are instrumental in its enforcement and evolution, often facing political backlash for their decisions.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, judicial_interpreters, agenda_setter,
    institutional, civilizational, constrained, national).

% Bear the political and social costs of this reading, as it challenges established hierarchies and traditions. They resist its expansion through legislative means, judicial appointments, and public campaigns, viewing it as judicial overreach.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, conservative_political_factions, payer,
    powerful, biographical, constrained, national).

% Scholars who adhere to a restrictive originalist reading, arguing that the equality clause's meaning is fixed to its 18th-century understanding. They are excluded from the interpretive process of this reading, as their foundational premises are rejected.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, restrictive_originalist_scholars, excluded,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared understanding of universal human dignity and rights, providing a moral and legal framework for social cohesion and justice across diverse populations.
% TRANSFER_FUNCTION: Transfers moral and legal authority, as well as material resources (e.g., through anti-discrimination laws), from historically privileged groups and established hierarchies to historically marginalized groups.
% ABSENT_VOICES: The voices of those who would benefit from maintaining historical exclusions are actively suppressed by this reading's moral and legal force. While they are present in political discourse, their foundational arguments for inequality are rendered illegitimate within this framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal and moral basis for many civil rights protections would collapse, leading to a rapid re-entrenchment of historical inequalities and a profound societal reorganization around more restrictive principles.
% FOUNDING_PROBLEM: The historical problem of systemic discrimination and exclusion based on race, gender, religion, and other characteristics, which contradicted the stated ideals of liberty and justice.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, international human rights bodies, and ongoing sociological studies consistently corroborate that systemic discrimination remains a live problem, requiring continuous application and expansion of equality principles. This is attested from outside the immediate beneficiaries by a broad consensus of academic and advocacy groups.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.15) because this reading primarily expands rights and benefits, rather than extracting from its beneficiaries. Suppression (0.2) is also low, as its enforcement is largely through judicial and legislative processes that aim to dismantle existing suppressive structures, though it does suppress arguments for historical exclusion. Theater ratio is minimal (0.05) as the reading is actively applied and genuinely seeks to achieve its stated goals. Accessibility collapse is moderate (0.7) because while it aims for universal application, the practical realization of equality still faces significant barriers. Resistance is low (0.1) from its beneficiaries, but high from those who oppose its expansion (captured in the 'conservative_political_factions' payer seat).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically marginalized groups and civil rights advocates, this reading is a pure Rope, coordinating universal justice. From the perspective of conservative political factions, it is an imposition that extracts traditional privileges and redefines social order, potentially appearing as a Snare or Tangled Rope. The engine will compute these divergent classifications based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically marginalized groups are full beneficiaries (d=0.0) as the constraint expands their rights. Civil rights advocates and judicial interpreters are agenda-setters and beneficiaries (d low) as they actively shape and benefit from this expansion. Conservative political factions are payers (d high) as they bear the costs of challenged hierarchies and legal defeats. Restrictive originalist scholars are excluded (d=1.0) as their interpretive framework is rejected by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_legitimacy_threshold,
    'What is the actual legitimacy threshold for judicial expansion of equality rights without explicit legislative or popular mandate?',
    'Empirical analysis of public and political response to landmark judicial decisions expanding equality, including legislative attempts to overturn or codify.',
    'If the threshold is high, this reading''s reliance on judicial interpretation makes it vulnerable to political backlash and potential erosion; if low, its expansion is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_legitimacy_threshold, empirical, 'The extent to which judicial activism in expanding equality is accepted by the public and other branches of government.').

omega_variable(
    universalism_vs_identity_politics,
    'Does the ''universalist'' framing genuinely apply to all, or does it implicitly privilege certain forms of equality (e.g., formal over substantive) or certain identity groups?',
    'Critical legal studies and intersectional analysis examining the practical outcomes of this reading for diverse marginalized groups, particularly those with intersecting identities.',
    'If it implicitly privileges certain groups or forms of equality, its ''universalist'' claim becomes a form of theater, and its effective extractiveness from other marginalized groups could be higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universalism_vs_identity_politics, conceptual, 'Whether the universalist claim of equality is truly inclusive in practice or masks implicit biases.').

omega_variable(
    historical_exclusion_as_precedent,
    'To what extent do historical exclusions, even if deemed unjust, continue to exert a de facto influence on the application of equality principles, despite this reading''s rejection of them as binding precedent?',
    'Sociological and legal studies tracking the persistence of systemic inequalities in areas where formal equality has been established by this reading.',
    'If historical exclusions continue to exert strong de facto influence, the ''expansive universalist'' reading''s effectiveness is lower, and its suppression of counter-arguments is less complete than it claims, potentially shifting its classification towards a Tangled Rope due to persistent, unacknowledged extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_exclusion_as_precedent, empirical, 'The lingering practical effect of historical exclusions on contemporary equality outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1950, equality_clause_scope__expansive_universalist, theater_ratio, 1950, 0.01).
narrative_ontology:measurement(equa_tr_t1970, equality_clause_scope__expansive_universalist, theater_ratio, 1970, 0.02).
narrative_ontology:measurement(equa_tr_t1990, equality_clause_scope__expansive_universalist, theater_ratio, 1990, 0.03).
narrative_ontology:measurement(equa_tr_t2010, equality_clause_scope__expansive_universalist, theater_ratio, 2010, 0.04).
narrative_ontology:measurement(equa_tr_t2024, equality_clause_scope__expansive_universalist, theater_ratio, 2024, 0.05).

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


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
