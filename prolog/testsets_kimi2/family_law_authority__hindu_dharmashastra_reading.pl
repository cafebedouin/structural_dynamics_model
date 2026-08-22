% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__hindu_dharmashastra_reading, []).

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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Hindu Dharmashastra Sacramental Marriage (Pre-1955)
 *   domain: comparative_law/religious_governance/social
 *
 * SUMMARY:
 *   This constraint story captures the Hindu dharmashastra reading of the
 *   family_law_authority kernel, under which marriage is a sacramental
 *   samskara governed by dharmic texts and customary practice. It is one of
 *   five sibling readings of a contested kernel that also includes Muslim
 *   shariat, Christian canonical, Parsi Zoroastrian, and secular contractual
 *   framings. Under this reading, marriage is indissoluble (pre-1955), caste
 *   endogamy is normatively required, property is held jointly by the
 *   patriline, and the wife enters as a ritual participant rather than an
 *   autonomous contracting party. The structural delta from sibling readings
 *   is the sacramental indissolubility and the subordination of individual
 *   autonomy to joint-family and caste continuity.
 *
 * KEY AGENTS:
 *   - dharmic_interpreter (institutional/identity_locked): Administers textual validation and ritual certification, enforcing caste endogamy and sacramental form.
 *   - patrilineal_joint_family (powerful/constrained): Primary beneficiaryâreceives labor, property continuity, and ritual integration of the wife.
 *   - married_women (powerless/trapped): Primary targetâbears the costs of non-autonomy, indissolubility, and joint-family subordination.
 *   - intercaste_couples (powerless/trapped): Payerâbears the costs of exclusion from sacramental legitimacy and social ostracism.
 *   - reform_advocate (organized/analytical): External observerâchallenges the indissolubility and caste norms from secular or reformist positions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.75).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.82).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Dharmashastra Sacramental Marriage (Pre-1955)").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "comparative_law/religious_governance/social").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, 'c93d7c8f-1737-42b1-8f5b-a68fd343a577').
narrative_ontology:cs_kernel_codification('c93d7c8f-1737-42b1-8f5b-a68fd343a577', fixed_text).
narrative_ontology:cs_authority_grounding('c93d7c8f-1737-42b1-8f5b-a68fd343a577', lineage).
narrative_ontology:cs_interpretation_layer_present('c93d7c8f-1737-42b1-8f5b-a68fd343a577').
narrative_ontology:cs_reading_relation('c93d7c8f-1737-42b1-8f5b-a68fd343a577', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('c93d7c8f-1737-42b1-8f5b-a68fd343a577', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('c93d7c8f-1737-42b1-8f5b-a68fd343a577', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('c93d7c8f-1737-42b1-8f5b-a68fd343a577', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('c93d7c8f-1737-42b1-8f5b-a68fd343a577', foundational, samskara_indissolubility).
narrative_ontology:cs_axiom_status(samskara_indissolubility, holdable).
narrative_ontology:cs_axiom_grounding('c93d7c8f-1737-42b1-8f5b-a68fd343a577', samskara_indissolubility, theological).
narrative_ontology:cs_axiom('c93d7c8f-1737-42b1-8f5b-a68fd343a577', foundational, dharmic_endogamy_as_caste_preservation).
narrative_ontology:cs_axiom_status(dharmic_endogamy_as_caste_preservation, holdable).
narrative_ontology:cs_axiom_grounding('c93d7c8f-1737-42b1-8f5b-a68fd343a577', dharmic_endogamy_as_caste_preservation, theological).
narrative_ontology:cs_reference_frame('c93d7c8f-1737-42b1-8f5b-a68fd343a577', classical_dharmashastra_order).
narrative_ontology:cs_drift_state('c93d7c8f-1737-42b1-8f5b-a68fd343a577', late_colonial_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c93d7c8f-1737-42b1-8f5b-a68fd343a577', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, patrilineal_joint_family).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, married_women).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, intercaste_couples).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, sacramental_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, caste_endogamy_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Validates sacramental marriages, interprets smriti texts to enforce caste endogamy, and certifies ritual compliance; their social authority depends on preserving the textual tradition and customary validity of the samskara.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, dharmic_interpreter, agenda_setter,
    institutional, generational, identity_locked, regional).

% Retains ancestral property jointly, receives the wife's labor and reproductive capacity, and secures lineage continuity through a sacramental bond that cannot be dissolved; dissolution would fragment the joint estate.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, patrilineal_joint_family, beneficiary,
    powerful, generational, constrained, local).

% Enters marriage as a sacramental duty, becomes subordinated to the husband's joint family, is excluded from autonomous property rights, and cannot dissolve the bond; her labor and ritual participation flow to the patriline.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, married_women, payer,
    powerless, biographical, trapped, local).

% Excluded from sacramental marriage legitimacy because their union violates caste endogamy norms; their partnerships are rendered socially illegitimate, and any children face ostracism and degraded status.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, intercaste_couples, payer,
    powerless, biographical, trapped, local).

% Advocates for civil marriage, women's property rights, and divorce from outside the dharmic framework; observes the extraction but does not administer the constraint and is structurally excluded from traditional interpretive councils.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, reform_advocate, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, patrilineal_joint_family).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates lineage continuity, joint property holding, and ritual integration of households through a sacramental bond that embeds the couple in caste hierarchy and assigns reproducible roles across generations.
% TRANSFER_FUNCTION: Transfers female labor, reproductive capacity, and ritual participation from the wife's natal family to the husband's joint family; transfers property stewardship to the patriline; transfers social legitimacy only to endogamous unions certified by dharmic authority.
% ABSENT_VOICES: Women seeking autonomous divorce, intercaste couples wanting sacramental legitimacy, and secular reformers are structurally excluded from the dharmic interpretive process; their objections are heard only in colonial courts or reform movements outside the customary framework.
% DISAPPEARANCE_RATIONALE: If the sacramental dharmashastra framework vanished, joint family property structures would lose their ritual legitimation, caste boundaries would require new enforcement mechanisms, and married women would gain autonomous legal personalityâthe social order would reorganize around contractual or alternative sacramental forms.
% FOUNDING_PROBLEM: Regulating sexual reproduction, property transmission, and ritual purity across generations in a caste-stratified society without centralized state family law.
% FOUNDING_PROBLEM_CORROBORATION: Colonial ethnographers and early Hindu reformers attested that the classical order was already partially eroded by the nineteenth century; dharmic authorities assert the problem is eternal. Independent historical sociology from outside the beneficiary seats corroborates the property and lineage function but questions the necessity of sacramental indissolubility.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__hindu_dharmashastra_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__hindu_dharmashastra_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the arrangement transfers labor, reproduction, and property control asymmetrically from women to the patriline while denying exit. Suppression (0.82) is high because persistence depends on active social enforcement: caste councils ostracize transgressors, families coerce compliance, and colonial courts often deferred to dharmic interpreters. Theater_ratio (0.50) reflects that by 1955 an increasing share of activity is performative defense of tradition under reform pressure rather than organic social function. Accessibility_collapse (0.88) is very high because once a woman is married under this framework, alternatives (divorce, autonomous property, intercaste remarriage) are almost entirely closed. Resistance (0.45) is moderate: reform movements and colonial courts produced sustained pressure but did not yet dismantle the constraint. Measurements share one time grid to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The dharmic interpreter seat experiences the constraint as legitimate coordination of sacred duty and social order; the married_women seat experiences the same structure as total subordination of autonomy. The patrilineal_joint_family sees property continuity and ritual purity; the intercaste_couple sees exclusion from legitimacy. The engine computes this divergence from structural data, not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (patrilineal_joint_family) get low d because the constraint subsidizes their property and lineage interests. Payers (married_women, intercaste_couples) get high d because the constraint extracts their autonomy and legitimacy. The dharmic_interpreter is an agenda_setter with identity_locked exit, sitting near the beneficiary end but deriving authority rather than material extraction. No override is needed: the structural derivation captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because it has a genuine coordination function (joint property, lineage continuity, ritual integration) that would make a pure Snare classification inaccurate, while the declared victim set and active enforcement prevent misreading it as a pure Rope. The high theater_ratio and rising extraction over time signal that coordination has atrophied into defensive performance, but the base function is not yet zeroâso Piton is also inaccurate. Tangled Rope is the structurally honest classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the hindu_dharmashastra_reading of kernel family_law_authority; how would the secular_contractual_reading restructure the beneficiary/victim map?',
    'Comparative analysis of the same kernel under the secular_contractual_reading sibling, examining autonomy and property symmetry.',
    'Under the secular reading, married_women would shift from payer to symmetric contractor, collapsing the extraction asymmetry and likely reclassifying the constraint toward rope or mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Sibling reading structural delta for this kernel seat.').

omega_variable(
    cs_framing_alternative,
    'Does the authority of this constraint derive from fixed dharmic texts (lineage) or from living customary practice (practice)?',
    'Examine whether textual deviation in regional custom is treated as violation or legitimate evolution.',
    'If practice-grounded, the constraint is more fluid and effective extraction may be lower; if text-locked, extraction is rigid and the directionality toward women is fixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_alternative, conceptual, 'Alternative CS framing under-determination.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal bar on divorce, caste council ostracism) or internalized (the target believes sacramental duty is cosmically required)?',
    'Post-exit suppression trajectory: if women who leave the framework continue to enforce its norms on themselves or others, the suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure, strengthening the snare-like features of the tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    indissolubility_as_performance,
    'Was sacramental indissolubility a live normative constraint or a performed memory by the late colonial period?',
    'Archival study of desertion, informal dissolution, and colonial court recognition of customary separation.',
    'If largely performed memory, the theater_ratio should be higher than base_extractiveness suggests, pushing the constraint toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indissolubility_as_performance, empirical, 'Empirical status of indissolubility norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hindu_dharmashastra_tr_t0, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hindu_dharmashastra_tr_t10, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(hindu_dharmashastra_tr_t20, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(hindu_dharmashastra_tr_t30, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(hindu_dharmashastra_tr_t40, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(hindu_dharmashastra_tr_t50, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(hindu_dharmashastra_tr_t55, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 55, 0.5).

% Extraction over time
narrative_ontology:measurement(hindu_dharmashastra_be_t0, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(hindu_dharmashastra_be_t10, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 10, 0.69).
narrative_ontology:measurement(hindu_dharmashastra_be_t20, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(hindu_dharmashastra_be_t30, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(hindu_dharmashastra_be_t40, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 40, 0.73).
narrative_ontology:measurement(hindu_dharmashastra_be_t50, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 50, 0.74).
narrative_ontology:measurement(hindu_dharmashastra_be_t55, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 55, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(hindu_dharmashastra_su_t0, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(hindu_dharmashastra_su_t10, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(hindu_dharmashastra_su_t20, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(hindu_dharmashastra_su_t30, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(hindu_dharmashastra_su_t40, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(hindu_dharmashastra_su_t50, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(hindu_dharmashastra_su_t55, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 55, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the family_law_authority kernel. The hindu_dharmashastra_reading shares the kernel with muslim_shariat_reading, christian_canonical_reading, parsi_zoroastrian_reading, and secular_contractual_reading. Each reading has a distinct epsilon, beneficiary/victim structure, and classification. Decomposition follows the epsilon-invariance principle: the label 'family law authority' conflates structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
