% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__colorblind_reading, []).

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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Clause â Colorblind Reading
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the colorblind reading of the Equal
 *   Protection Clause: the constitutional rule that government may never
 *   classify individuals by race. The reading treats the Clause as a
 *   categorical command of formal neutrality, deriving from a fixed
 *   constitutional text and vindicating individual rights independent of
 *   group membership. It is one of three contested readings of the
 *   equal_protection_clause kernel; the other two (remedial and diversity)
 *   permit or require race-conscious governmental action. The authored
 *   metrics describe a low-extraction coordination mechanismâformal rule
 *   applicationâwhile the structural data acknowledge the contested nature
 *   of the reading's exclusivity claim.
 *
 * KEY AGENTS:
 *   - individual_citizens (moderate/constrained): Universal beneficiaries of the prohibition on governmental racial classification.
 *   - federal_judiciary (institutional/analytical): Agenda-setter that interprets and enforces the colorblind rule through constitutional review.
 *   - public_institutions (institutional/constrained): Payers that forgo race-conscious policies to comply with judicial interpretation.
 *   - race_conscious_policy_advocates (organized/constrained): Excluded voices whose preferred constitutional position is foreclosed by this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.12).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.35).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, rope).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Clause â Colorblind Reading").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, '63123968-d252-452a-a8af-4a598a9faa29').
narrative_ontology:cs_kernel_codification('63123968-d252-452a-a8af-4a598a9faa29', fixed_text).
narrative_ontology:cs_authority_grounding('63123968-d252-452a-a8af-4a598a9faa29', lineage).
narrative_ontology:cs_interpretation_layer_present('63123968-d252-452a-a8af-4a598a9faa29').
narrative_ontology:cs_reading_relation('63123968-d252-452a-a8af-4a598a9faa29', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('63123968-d252-452a-a8af-4a598a9faa29', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('63123968-d252-452a-a8af-4a598a9faa29', foundational, racial_classifications_categorically_forbidden).
narrative_ontology:cs_axiom_status(racial_classifications_categorically_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('63123968-d252-452a-a8af-4a598a9faa29', racial_classifications_categorically_forbidden, conventional).
narrative_ontology:cs_axiom('63123968-d252-452a-a8af-4a598a9faa29', foundational, individual_rights_precede_group_membership).
narrative_ontology:cs_axiom_status(individual_rights_precede_group_membership, holdable).
narrative_ontology:cs_axiom_grounding('63123968-d252-452a-a8af-4a598a9faa29', individual_rights_precede_group_membership, deontological).
narrative_ontology:cs_reference_frame('63123968-d252-452a-a8af-4a598a9faa29', colorblind_constitutional_neutrality).
narrative_ontology:cs_drift_state('63123968-d252-452a-a8af-4a598a9faa29', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('63123968-d252-452a-a8af-4a598a9faa29', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, individual_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, public_institutions).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, constitutional_colorblindness).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, individual_rights_universalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All individuals within the jurisdiction who are protected from being classified or treated differently by government on the basis of race. The reading purports to treat every person as an individual rights-bearer regardless of group membership, conferring a formal equality benefit.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individual_citizens, beneficiary,
    moderate, generational, constrained, national).

% Interprets the Equal Protection Clause and enforces the categorical prohibition against governmental racial classifications through judicial review of statutes, university admissions policies, and other official uses of race. Controls the authoritative meaning of the clause.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Government agencies, public schools, and state universities that must redesign admissions, hiring, contracting, and other policies to exclude race as a decisional criterion. They bear the compliance cost of forgoing race-conscious tools even when pursuing diversity or remediation goals.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, public_institutions, payer,
    institutional, biographical, constrained, national).

% Advocates and organizations promoting affirmative action and race-conscious remediation. Their preferred constitutional positionâthat government may sometimes use race to ameliorate inequality or achieve diversityâis structurally excluded under this reading, though they remain active in political and legal discourse.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, race_conscious_policy_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a polity around the principle that government must treat every person as an individual rights-bearer rather than as a representative of a racial group, preventing the state from using race as a criterion of official decision-making.
% TRANSFER_FUNCTION: Moves policy discretion away from legislative and administrative bodies toward a constitutional floor of individual-rights protection; removes race from the permissible set of governmental classification criteria.
% ABSENT_VOICES: Advocates of race-conscious remediation and educational diversity, who argue that group membership must sometimes be visible to the state in order to correct for historical and ongoing discrimination. They are audible in public discourse but their constitutional argument is excluded by this reading.
% DISAPPEARANCE_RATIONALE: If the colorblind prohibition vanished overnight, public institutions would reintroduce racial classifications in admissions, contracting, and other domains; the legal and educational landscape would reorganize around constitutionally permitted race-consciousness, and the individual-rights floor would drop.
% FOUNDING_PROBLEM: State and federal governments using racial classifications to subordinate, segregate, and exclude individuals from civil and political life, particularly in the Reconstruction and Jim Crow eras.
% FOUNDING_PROBLEM_CORROBORATION: Reconstruction-era legislative history and the 39th Congress debates document the problem of state-enforced racial subordination. Contemporary constitutional historians and civil rights scholars outside the immediate political beneficiary set corroborate the historical record, though they dispute whether the colorblind reading is the appropriate remedy.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).
:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because the constraint operates as a formal prohibition rather than a resource-transfer mechanism; it removes a policy tool rather than extracting wealth or labor. Suppression is moderate (0.35) because judicial review actively invalidates democratically enacted race-conscious policies, structurally suppressing that alternative. Accessibility collapse is high (0.75): once the colorblind reading controls, race-conscious alternatives are constitutionally foreclosed. Resistance is substantial (0.65) because the remedial and diversity readings command significant political, academic, and judicial support. Theater ratio is low (0.10) because enforcement is substantiveâstriking down actual policiesârather than performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (individual citizens) experiences the constraint as protective neutrality. The payer seat (public institutions) experiences it as a restriction on policy autonomy. The excluded seat (race-conscious advocates) experiences it as an illegitimate foreclosure of democratic and constitutional possibilities. The agenda-setter seat (federal judiciary) experiences it as faithful textual interpretation. The engine computes these divergences from the structural data; the authored claim does not resolve them.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual citizens are declared beneficiaries, producing a low directionality toward subsidy and protection. Public institutions are payers in the sense of bearing compliance costs (forgone discretion), but because no resource extraction is declared in base_properties.victims and their institutional power keeps them from being trapped targets, their derived directionality sits nearer symmetric. The federal judiciary, as agenda-setter with analytical exit, derives a neutral/analytical directionality. No directionality overrides are needed because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading prevents mandatrophy mislabeling by clearly identifying a live coordination functionâpreventing state racial discriminationâand keeping extraction very low. If extraction were high, or if the rule persisted despite the disappearance of any realistic risk of governmental racial classification, it would risk piton or snare classification. The authored founding_problem_status is 'live' because governmental racial classification remains a realistic risk, grounding the coordination claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Does the colorblind reading capture the exclusive meaning of the Equal Protection Clause, or do the remedial and diversity readings represent structurally valid alternative instantiations of the same kernel?',
    'Historical-linguistic analysis of the 14th Amendment''s original public meaning; comparative doctrinal analysis of whether the kernel textually permits multiple coherent readings.',
    'If the kernel structurally supports multiple readings, the colorblind reading''s claim to permanence and exclusivity weakens, and its classification may shift from rope to tangled_rope or snare depending on whether the exclusivity claim extracts legitimacy from the kernel for the benefit of specific groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether colorblindness is the sole valid reading of the equal protection kernel').

omega_variable(
    facial_neutrality_asymmetric_impact,
    'Does the formal prohibition of all racial classifications produce substantively equal outcomes across racial groups, or does it lock in pre-existing disparities by foreclosing race-conscious remediation?',
    'Longitudinal comparative studies of jurisdictions with strict colorblind regimes versus those permitting race-conscious remediation, measuring educational, economic, and political participation outcomes by racial group.',
    'If outcomes diverge systematically by race under colorblind regimes, the constraint''s effective extraction is higher than its formal structure suggests, and the rope classification may mask asymmetric extraction (tanglement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(facial_neutrality_asymmetric_impact, empirical, 'Whether formal colorblindness produces equal outcomes or entrenches disparity').

omega_variable(
    judicial_enforcement_as_suppression,
    'Does judicial enforcement of colorblindness against legislative majorities represent necessary constitutional guardianship or structural suppression of democratically preferred race-conscious policies?',
    'Comparative analysis of democratic legitimacy in constitutional regimes with strong judicial review versus those with legislative supremacy over rights interpretation.',
    'If the enforcement mechanism systematically suppresses minority-preferred policies without adequate democratic accountability, the suppression metric may understate the constraint''s coercive force, and the classification may tend toward tangled_rope or scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_enforcement_as_suppression, preference, 'Whether judicial enforcement of colorblindness is coordination or suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_colorblind_tr_t0, equal_protection_clause__colorblind_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ep_colorblind_tr_t30, equal_protection_clause__colorblind_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(ep_colorblind_tr_t60, equal_protection_clause__colorblind_reading, theater_ratio, 60, 0.1).

% Extraction over time
narrative_ontology:measurement(ep_colorblind_be_t0, equal_protection_clause__colorblind_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ep_colorblind_be_t30, equal_protection_clause__colorblind_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(ep_colorblind_be_t60, equal_protection_clause__colorblind_reading, base_extractiveness, 60, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(equal_protection_clause__colorblind_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the equal_protection_clause kernel. The colorblind, remedial, and diversity readings share the same constitutional text but instantiate structurally distinct constraints with different beneficiary/victim structures, extraction profiles, and coordination functions. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
