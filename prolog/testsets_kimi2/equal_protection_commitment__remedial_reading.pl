% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection Remedial Reading: Dismantling Caste
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint is the remedial reading of the Equal Protection Clause:
 *   the constitutional commitment that the Fourteenth Amendment forbids the
 *   perpetuation of caste-like subordination and affirmatively permits state
 *   actors to use race-conscious measures to dismantle it. It is one reading
 *   of a contested kernel; the colorblind absolutist reading and the
 *   diversity reading are siblings instantiated in separate constraint
 *   stories. In this reading, state remedial actors sit as beneficiaries of
 *   expanded constitutional authority, while historically privileged groups
 *   bear the costs of race-conscious redistribution and appear in the victim
 *   set. The high extractiveness reflects the inversion of beneficiary and
 *   victim depending on the observer's structural position.
 *
 * KEY AGENTS:
 *   - State remedial apparatus (agenda_setter/beneficiary): Courts and agencies that design and enforce race-conscious remedial programs; gain institutional authority.
 *   - Historically subordinated groups (beneficiary): Intended recipients of remedial preferences in education, contracting, and representation.
 *   - Historically privileged groups (payer): Bear costs of redistributive exclusion and heightened scrutiny; mount constitutional challenges.
 *   - Colorblind legal advocates (excluded): Argue all racial classification is unconstitutional; foreclosed from this reading's interpretive framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.55).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.58).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection Remedial Reading: Dismantling Caste").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, '0a043b07-4e4f-4fb5-af57-2500504e5202').
narrative_ontology:cs_kernel_codification('0a043b07-4e4f-4fb5-af57-2500504e5202', fixed_text).
narrative_ontology:cs_authority_grounding('0a043b07-4e4f-4fb5-af57-2500504e5202', lineage).
narrative_ontology:cs_interpretation_layer_present('0a043b07-4e4f-4fb5-af57-2500504e5202').
narrative_ontology:cs_reading_relation('0a043b07-4e4f-4fb5-af57-2500504e5202', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('0a043b07-4e4f-4fb5-af57-2500504e5202', equal_protection_commitment__diversity_reading, influences).
narrative_ontology:cs_axiom('0a043b07-4e4f-4fb5-af57-2500504e5202', foundational, anti_subordination_as_equal_protection_core).
narrative_ontology:cs_axiom_status(anti_subordination_as_equal_protection_core, holdable).
narrative_ontology:cs_axiom_grounding('0a043b07-4e4f-4fb5-af57-2500504e5202', anti_subordination_as_equal_protection_core, deontological).
narrative_ontology:cs_axiom('0a043b07-4e4f-4fb5-af57-2500504e5202', foundational, remedial_race_consciousness_permissible).
narrative_ontology:cs_axiom_status(remedial_race_consciousness_permissible, holdable).
narrative_ontology:cs_axiom_grounding('0a043b07-4e4f-4fb5-af57-2500504e5202', remedial_race_consciousness_permissible, conventional).
narrative_ontology:cs_reference_frame('0a043b07-4e4f-4fb5-af57-2500504e5202', reconstruction_anti_subordination).
narrative_ontology:cs_drift_state('0a043b07-4e4f-4fb5-af57-2500504e5202', contemporary_judicial_retrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0a043b07-4e4f-4fb5-af57-2500504e5202', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_remedial_apparatus).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_groups).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, anti_subordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, remedial_state_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, agencies, and legislatures that design, authorize, and enforce race-conscious remedial programs under the anti-subordination reading of the Equal Protection Clause. They gain institutional authority, policy legitimacy, and an expanded constitutional mandate from this constraint.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_remedial_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, state_remedial_apparatus, beneficiary).

% Groups defined by historical caste subordination who are the intended beneficiaries of race-conscious remedial measures in education, contracting, and political representation. Their structural position is the doctrinal justification for the constraint, and they depend on state remedial action for redress.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_groups, beneficiary,
    organized, generational, constrained, national).

% Groups that historically occupied dominant caste positions and now bear the costs of race-conscious redistribution, including exclusion from preferential programs and heightened scrutiny of their constitutional claims. They mount legal challenges and experience the constraint as extraction of opportunity and status.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_groups, payer,
    powerful, biographical, constrained, national).

% Jurists and advocates who hold that the Equal Protection Clause forbids any state use of racial classification. Their preferred doctrinal framework is structurally foreclosed by the remedial reading, leaving them outside the interpretive community that sets the constraint's parameters despite their continued presence in constitutional discourse.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_legal_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the dismantling of entrenched caste-like subordination through state-administered, race-conscious remedies where formal colorblindness would perpetuate hierarchical outcomes.
% TRANSFER_FUNCTION: Transfers educational, economic, and political opportunities from historically privileged groups to historically subordinated groups via racial preferences; transfers institutional authority and constitutional legitimacy to state actors empowered to design and enforce these remedial programs.
% ABSENT_VOICES: Colorblind absolutists and originalist jurists who view any racial classification as a constitutional violation are structurally excluded from this reading's doctrinal framework; their objections are treated as interpretive errors rather than live alternatives.
% DISAPPEARANCE_RATIONALE: If the remedial reading vanished, race-conscious affirmative action, minority-business preferences, and remedial districting would face fatal constitutional challenge. Historically subordinated groups would lose a primary legal vehicle for redress, while historically privileged groups would regain unrestricted access to contested opportunities. The parties dispute whether this would constitute a catastrophic rearrangement or a restoration of constitutional neutrality.
% FOUNDING_PROBLEM: The post-Civil War persistence of caste-like subordination despite the abolition of slavery and the enactment of formal legal equality, whereby facially neutral laws and social practices continued to reproduce racial hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Critical race scholars and civil rights organizations attest that caste-like subordination persists and justifies ongoing remediation. Empirical social scientists and colorblind legal advocates argue the founding caste system has been substantially dismantled and the arrangement now functions as unjustified group preference; this corroboration comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, contested).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.55 because the doctrine actively redistributes opportunities and legitimacy across racial groups. Suppression is 0.58 because the constraint structurally bars the colorblind alternative and invalidates facially neutral laws that perpetuate hierarchy. Theater ratio is 0.44 because an increasing share of remedial activity has shifted toward performative compliance and symbolic representation rather than deep structural dismantling. Accessibility collapse is 0.62 because once the anti-subordination framework is accepted, colorblind alternatives are legally closed off. Resistance is 0.72 due to sustained, organized constitutional challenges from historically privileged groups and the conservative legal movement.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of historically subordinated groups, the constraint reads as necessary coordination against persistent hierarchy; from the seat of historically privileged groups, it reads as active extraction of opportunity and status. The state remedial apparatus experiences it as legitimate authority expansion. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State actors are near the beneficiary end (low d) because the constraint expands their constitutional authority and policy legitimacy. Historically subordinated groups are also near the beneficiary end (low d) because the constraint subsidizes their access to opportunities. Historically privileged groups are near the target end (high d) because the constraint extracts access and preferential treatment from them. Colorblind advocates are excluded â their preferred framework is structurally barred.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by retaining a genuine coordination function: it solves the collective-action problem of dismantling entrenched caste hierarchy that individual market or social action cannot dislodge. However, the asymmetric extraction is real and necessary to the reading's structure â it is not a bug but the mechanism â which makes it a Tangled Rope rather than a pure Snare or Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    caste_persistence_empirical_status,
    'Is caste-like subordination empirically persistent in contemporary society, or has formal equality eliminated the condition that justifies race-conscious remediation?',
    'Longitudinal analysis of intergenerational wealth gaps, residential segregation indices, and institutional representation rates across subordinated groups.',
    'If subordination is structurally ended, the coordination function is obsolete and the constraint collapses toward pure extraction; if persistent, the tangled rope structure remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_persistence_empirical_status, empirical, 'Empirical persistence of the caste condition the constraint was built to remedy').

omega_variable(
    remedial_colorblind_foreclosure,
    'Does the remedial reading''s core premise logically foreclose the colorblind reading within a single constitutional framework, or do both remain live options for the same jurist?',
    'Analysis of judicial opinions and legal scholarship for internal consistency â whether any jurist simultaneously endorses anti-subordination remediation and absolute colorblindness.',
    'If foreclosure is partial or contextual, the forecloses relation should weaken to influences or coexists_with, altering the constraint family topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_colorblind_foreclosure, conceptual, 'Whether remedial and colorblind readings are mutually exclusive in a single framework').

omega_variable(
    enforcement_doctrinal_or_partisan,
    'Is the active enforcement of this reading driven by doctrinal coherence within the judiciary, or by partisan control of judicial appointments?',
    'Comparative analysis of judicial behavior across appointing regimes and longitudinal tracking of remedial doctrine as court composition changes.',
    'If enforcement is primarily partisan, the constraint''s suppression is external to legal reasoning and its classification tilts toward extraction-over-coordination; if doctrinal, the tangled rope balance holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_doctrinal_or_partisan, conceptual, 'Partisan vs doctrinal basis for enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epr_tr_t0, equal_protection_commitment__remedial_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(epr_tr_t12, equal_protection_commitment__remedial_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(epr_tr_t24, equal_protection_commitment__remedial_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(epr_tr_t36, equal_protection_commitment__remedial_reading, theater_ratio, 36, 0.36).
narrative_ontology:measurement(epr_tr_t48, equal_protection_commitment__remedial_reading, theater_ratio, 48, 0.4).
narrative_ontology:measurement(epr_tr_t60, equal_protection_commitment__remedial_reading, theater_ratio, 60, 0.44).

% Extraction over time
narrative_ontology:measurement(epr_be_t0, equal_protection_commitment__remedial_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(epr_be_t12, equal_protection_commitment__remedial_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(epr_be_t24, equal_protection_commitment__remedial_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(epr_be_t36, equal_protection_commitment__remedial_reading, base_extractiveness, 36, 0.5).
narrative_ontology:measurement(epr_be_t48, equal_protection_commitment__remedial_reading, base_extractiveness, 48, 0.53).
narrative_ontology:measurement(epr_be_t60, equal_protection_commitment__remedial_reading, base_extractiveness, 60, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(epr_su_t0, equal_protection_commitment__remedial_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(epr_su_t12, equal_protection_commitment__remedial_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(epr_su_t24, equal_protection_commitment__remedial_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(epr_su_t36, equal_protection_commitment__remedial_reading, suppression_requirement, 36, 0.53).
narrative_ontology:measurement(epr_su_t48, equal_protection_commitment__remedial_reading, suppression_requirement, 48, 0.56).
narrative_ontology:measurement(epr_su_t60, equal_protection_commitment__remedial_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the equal_protection_commitment kernel. The colorblind_reading and diversity_reading are instantiated as separate constraint stories per the epsilon-invariance principle. The remedial reading is distinguished by its anti-subordination foundation and its treatment of race-conscious state action as constitutionally permissible remedial measure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
