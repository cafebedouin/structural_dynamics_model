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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   equality clauses (e.g., in constitutions or human rights declarations).
 *   It asserts that equality is a self-evident, universal truth applicable to
 *   all humans, and that historical exclusions are hypocritical deviations to
 *   be corrected, not binding precedents. This reading drives the continuous
 *   expansion of rights through judicial interpretation and social movements,
 *   often against significant resistance. The claim of 'mountain' reflects
 *   its presentation as a fundamental, immutable truth, but its high
 *   extractiveness and suppression indicate it functions more as an actively
 *   enforced, contested construct.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.8).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.75).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.8).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, mountain).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Expansive Universalist Reading of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).
domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '68622e57-d929-4770-bd1a-405cfabcadba').
narrative_ontology:cs_kernel_codification('68622e57-d929-4770-bd1a-405cfabcadba', fixed_text).
narrative_ontology:cs_authority_grounding('68622e57-d929-4770-bd1a-405cfabcadba', lineage).
narrative_ontology:cs_interpretation_layer_present('68622e57-d929-4770-bd1a-405cfabcadba').
narrative_ontology:cs_reading_relation('68622e57-d929-4770-bd1a-405cfabcadba', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('68622e57-d929-4770-bd1a-405cfabcadba', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('68622e57-d929-4770-bd1a-405cfabcadba', foundational, human_dignity_inherent).
narrative_ontology:cs_axiom_status(human_dignity_inherent, holdable).
narrative_ontology:cs_axiom_grounding('68622e57-d929-4770-bd1a-405cfabcadba', human_dignity_inherent, deontological).
narrative_ontology:cs_axiom('68622e57-d929-4770-bd1a-405cfabcadba', secondary, historical_injustice_requires_remedy).
narrative_ontology:cs_axiom_status(historical_injustice_requires_remedy, holdable).
narrative_ontology:cs_axiom_grounding('68622e57-d929-4770-bd1a-405cfabcadba', historical_injustice_requires_remedy, instrumental).
narrative_ontology:cs_reference_frame('68622e57-d929-4770-bd1a-405cfabcadba', post_enlightenment_universalism).
narrative_ontology:cs_drift_state('68622e57-d929-4770-bd1a-405cfabcadba', contemporary_civil_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('68622e57-d929-4770-bd1a-405cfabcadba', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, civil_rights_advocates).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, beneficiaries_of_historical_exclusions).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, status_quo_defenders).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, universal_human_rights_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, human_dignity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups gain legal recognition, protections, and opportunities as the scope of equality expands. Their identity is often intertwined with the struggle for recognition, making 'exit' from the claim of equality unthinkable, though practical exit from oppressive systems remains constrained.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    moderate, biographical, identity_locked, national).

% Organizations and individuals who actively champion the expansive interpretation of equality, pushing for its application through litigation, legislation, and public discourse. They benefit from the moral authority and social capital derived from this advocacy.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_advocates, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, civil_rights_advocates, beneficiary).

% Groups or individuals who have historically benefited from systems of inequality (e.g., racial, gender, class hierarchies). They bear the costs of expanded equality through loss of unearned privilege, social status, and sometimes material resources. Their 'exit' is often through legal challenge or political resistance.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, beneficiaries_of_historical_exclusions, payer,
    powerful, generational, constrained, national).

% Political and social actors who actively resist the expansion of equality, often by appealing to tradition, original intent, or claims of reverse discrimination. They seek to maintain existing social and legal structures that implicitly or explicitly uphold historical exclusions.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, status_quo_defenders, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, status_quo_defenders, agenda_setter).

% Courts, particularly supreme courts, play a critical role in interpreting and applying the equality principle, often expanding its scope through landmark decisions. Their institutional role constrains their 'exit' from this interpretive function, but they face political and social pressure.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Academics and researchers who analyze the philosophical underpinnings, historical development, and practical implications of equality principles. They provide intellectual frameworks that inform judicial and advocacy efforts, operating from an analytical distance.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal moral and legal baseline for human dignity and rights, enabling collective action towards a more just and inclusive society by providing a shared aspirational goal.
% TRANSFER_FUNCTION: Transfers moral and legal standing, social recognition, and eventually material resources and opportunities from historically privileged groups to historically excluded groups, challenging existing power structures.
% ABSENT_VOICES: Those who would argue for inherent, immutable hierarchies based on race, gender, or other characteristics are increasingly marginalized or excluded from legitimate public and legal discourse, their arguments deemed incompatible with the expansive universalist frame.
% DISAPPEARANCE_RATIONALE: If this expansive universalist reading of equality vanished, the legal and moral framework for civil rights, anti-discrimination laws, and human dignity would collapse. Historical inequalities would likely re-entrench, and society would fundamentally reorganize around more restrictive or hierarchical principles.
% FOUNDING_PROBLEM: The historical reality of systemic discrimination and exclusion, where abstract principles of equality in foundational texts were not applied to all humans, leading to widespread injustice and suffering.
% FOUNDING_PROBLEM_CORROBORATION: International human rights declarations, historical civil rights movements, ongoing social justice advocacy, and academic scholarship in critical race theory and feminist legal theory all corroborate the persistent problem of inequality and the need for an expansive principle. This corroboration comes from diverse sources outside the immediate beneficiaries of the principle's expansion.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, ExtMetricName, E),
    domain_priors:suppression_score(equality_clause_scope__expansive_universalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.8) reflects the significant social, political, and economic costs imposed on those who benefit from historical exclusions as the scope of equality expands. Suppression (0.75) is high because the expansion of equality often requires overcoming entrenched resistance, legal challenges, and active efforts to maintain the status quo. The low theater ratio (0.1) indicates that the struggle for this expansive equality is genuine and not merely performative. Accessibility collapse (0.8) is high for those seeking to maintain discriminatory practices, as the legal and moral ground for such practices erodes. Resistance (0.9) is very high, reflecting the active and often fierce opposition to the dismantling of established hierarchies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically excluded groups, this constraint is a liberating force, a true 'mountain' of justice. From the perspective of those benefiting from historical exclusions, it is a highly extractive 'snare' that dismantles their established order. The engine's reclassification from the claimed 'mountain' to a more extractive type for certain seats will highlight this fundamental perspectival divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups and civil rights advocates are clear beneficiaries, gaining rights and recognition (low directionality). Beneficiaries of historical exclusions and status quo defenders are targets, bearing the costs of change and losing privilege (high directionality). The judiciary acts as an agenda-setter, interpreting and enforcing the expansion, experiencing a more symmetric but still constrained directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to achieve universal equality) is very much 'live' and actively pursued, preventing mandatrophy. The high extractiveness and suppression are not signs of atrophy but of active, ongoing contestation and enforcement against a resistant status quo. The 'mountain' claim, however, is a potential false summit, as its 'naturalness' is actively constructed and enforced, benefiting specific groups while extracting from others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the ''self-evident universal truth'' of equality a genuine natural law (a Mountain) or a powerful social construct actively enforced and expanded (a Snare/Tangled Rope)?',
    'Analysis of the persistence of resistance and the necessity of active enforcement: if the principle requires continuous, coercive application against significant opposition, its ''naturalness'' is questionable.',
    'If primarily a social construct, the classification would shift from Mountain to a more extractive type (e.g., Tangled Rope or Snare) for those resisting its expansion, highlighting the active power dynamics involved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between inherent truth and enforced ideal.').

omega_variable(
    judicial_discovery_vs_creation,
    'To what extent does judicial interpretation ''discover'' pre-existing universal rights versus ''create'' new legal obligations and protections?',
    'Comparative legal analysis across jurisdictions and historical periods, examining the role of judicial activism versus legislative action in expanding equality.',
    'If primarily creation, the ''emerges_naturally'' claim would be weakened, further supporting a reclassification away from Mountain and emphasizing the role of institutional power in shaping the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discovery_vs_creation, empirical, 'Role of judiciary in rights expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1950, equality_clause_scope__expansive_universalist, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(equa_tr_t1965, equality_clause_scope__expansive_universalist, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(equa_tr_t1980, equality_clause_scope__expansive_universalist, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(equa_tr_t1995, equality_clause_scope__expansive_universalist, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(equa_tr_t2010, equality_clause_scope__expansive_universalist, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(equa_tr_t2020, equality_clause_scope__expansive_universalist, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1950, equality_clause_scope__expansive_universalist, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(equa_be_t1965, equality_clause_scope__expansive_universalist, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(equa_be_t1980, equality_clause_scope__expansive_universalist, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement(equa_be_t1995, equality_clause_scope__expansive_universalist, base_extractiveness, 1995, 0.76).
narrative_ontology:measurement(equa_be_t2010, equality_clause_scope__expansive_universalist, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(equa_be_t2020, equality_clause_scope__expansive_universalist, base_extractiveness, 2020, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1950, equality_clause_scope__expansive_universalist, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(equa_su_t1965, equality_clause_scope__expansive_universalist, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(equa_su_t1980, equality_clause_scope__expansive_universalist, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(equa_su_t1995, equality_clause_scope__expansive_universalist, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(equa_su_t2010, equality_clause_scope__expansive_universalist, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(equa_su_t2020, equality_clause_scope__expansive_universalist, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, civil_rights_legislation).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, anti_discrimination_laws).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, affirmative_action_policies).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'equality_clause_scope' kernel. It is linked to 'restrictive_originalist' and 'progressive_textualist' readings, which represent alternative interpretations of the same foundational principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
