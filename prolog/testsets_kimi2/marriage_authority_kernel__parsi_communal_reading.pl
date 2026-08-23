% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__parsi_communal_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Communal Marriage Authority under PMDA 1936
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the parsi_communal_reading of the
 *   marriage_authority_kernel: marriage and family law authority for Parsis
 *   derives from community custom as codified in the Parsi Marriage and
 *   Divorce Act 1936. The kernel is contested across five readings (Hindu
 *   codified, Muslim Shariat, Christian canonical, Parsi communal, secular
 *   civil), each grounding authority in a different source. The Parsi reading
 *   is characterized by community tribunals, statutory endogamy enforcement,
 *   relatively high internal gender equity compared to some other personal
 *   laws, and acute demographic decline that threatens the community's
 *   viability and intensifies the constraint's defensive function.
 *
 * KEY AGENTS:
 *   - Parsi communal tribunals (agenda_setter, institutional): administer the 1936 Act and enforce endogamy.
 *   - Parsi Punchayet (beneficiary, organized): community body benefiting from preserved identity boundaries.
 *   - Parsi individuals (payer, moderate): bear the cost of endogamy restrictions and identity-locked exit.
 *   - Non-Parsi prospective spouses (excluded, powerless): categorically barred from the marriage framework.
 *   - Indian state (observer, institutional): delegates authority but retains constitutional oversight.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.62).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.55).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Communal Marriage Authority under PMDA 1936").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '1de6e93b-5131-421a-bf62-51dea0a59686').
narrative_ontology:cs_kernel_codification('1de6e93b-5131-421a-bf62-51dea0a59686', fixed_text).
narrative_ontology:cs_authority_grounding('1de6e93b-5131-421a-bf62-51dea0a59686', lineage).
narrative_ontology:cs_interpretation_layer_present('1de6e93b-5131-421a-bf62-51dea0a59686').
narrative_ontology:cs_reading_relation('1de6e93b-5131-421a-bf62-51dea0a59686', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('1de6e93b-5131-421a-bf62-51dea0a59686', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('1de6e93b-5131-421a-bf62-51dea0a59686', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('1de6e93b-5131-421a-bf62-51dea0a59686', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('1de6e93b-5131-421a-bf62-51dea0a59686', foundational, endogamy_as_preservation_mandate).
narrative_ontology:cs_axiom_status(endogamy_as_preservation_mandate, holdable).
narrative_ontology:cs_axiom_grounding('1de6e93b-5131-421a-bf62-51dea0a59686', endogamy_as_preservation_mandate, conventional).
narrative_ontology:cs_axiom('1de6e93b-5131-421a-bf62-51dea0a59686', foundational, communal_tribunal_supremacy_in_marriage).
narrative_ontology:cs_axiom_status(communal_tribunal_supremacy_in_marriage, holdable).
narrative_ontology:cs_axiom_grounding('1de6e93b-5131-421a-bf62-51dea0a59686', communal_tribunal_supremacy_in_marriage, conventional).
narrative_ontology:cs_reference_frame('1de6e93b-5131-421a-bf62-51dea0a59686', parsi_customary_endogamy_framework).
narrative_ontology:cs_drift_state('1de6e93b-5131-421a-bf62-51dea0a59686', contemporary_demographic_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1de6e93b-5131-421a-bf62-51dea0a59686', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_punchayet).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates marriage and divorce disputes for Parsis under the Parsi Marriage and Divorce Act 1936, enforcing endogamy requirements and community-specific procedures. Bound by the Act and customary norms but with interpretive discretion over individual cases.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_communal_tribunals, agenda_setter,
    institutional, generational, constrained, national).

% Community body that benefits from preserved Parsi identity and demographic continuity. Promotes endogamy and supports the tribunals, collecting social authority and identity preservation rather than financial rents.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_punchayet, beneficiary,
    organized, generational, identity_locked, national).

% Parsis whose marriage choices are restricted by statutory endogamy requirements. They bear the cost of limited partner choice and potential social ostracism for exogamy, with identity fusion making exit from the communal framework costly.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_individuals, payer,
    moderate, biographical, identity_locked, national).

% Persons outside the Parsi community who are categorically excluded from valid marriage with Parsis under the Act's endogamy provisions. They have no standing in communal tribunals and no pathway into the framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, non_parsi_prospective_spouses, excluded,
    powerless, immediate, trapped, local).

% The secular Indian state that enacted and recognizes the 1936 Act, delegating family law authority to Parsi communal institutions while retaining a distant constitutional supervisory role through general legislation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_state, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a culturally specific marriage and divorce framework for a small religious minority, preserving communal identity through endogamous boundaries and specialized dispute resolution that ordinary civil courts cannot replicate.
% TRANSFER_FUNCTION: Moves authority over Parsi marriage validity and dissolution from secular civil courts to communal tribunals, and transfers the demographic preservation burden onto individual Parsis by restricting their marriage choice to co-religionists.
% ABSENT_VOICES: Young Parsis seeking exogamous marriage and their non-Parsi partners are excluded from tribunal proceedings; secular civil rights advocates and uniform civil code proponents are present in the broader constitutional debate but marginalized within this specific communal framework.
% DISAPPEARANCE_RATIONALE: If the communal authority vanished overnight, Parsi marriages and divorces would fall under the Special Marriage Act or general civil courts, endogamy would lose statutory enforcement, and the community's legally bounded identity would dissolve into the broader Indian family law system.
% FOUNDING_PROBLEM: The need to preserve a small, ethnically distinct religious minority against demographic absorption and to provide marriage adjudication grounded in Parsi customary practice rather than general colonial or civil law.
% FOUNDING_PROBLEM_CORROBORATION: Parsi communal bodies and demographic studies confirm the community's numerical decline, corroborating the preservation problem. However, constitutional law scholars and civil rights advocates outside the beneficiary set contest that statutory endogamy is a legitimate or effective remedy, arguing instead that it violates individual autonomy without securing demographic survival.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) is moderately high because endogamy enforcement restricts a fundamental choice (marriage partner) for communal preservation. Suppression (0.55) reflects both legal enforcement by tribunals and social enforcement through identity lock; alternatives like the Special Marriage Act exist but carry heavy social costs. Theater ratio (0.40) captures the increasingly performative defense of boundaries as demographic decline makes the community's viability uncertain. Accessibility collapse (0.45) is incomplete because legal alternatives exist, but social and identity costs make them practically inaccessible for many. Resistance (0.50) reflects ongoing debate about reform, individual autonomy, and the uniform civil code. The measurement series share one time grid (0-90) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the Punchayet seat, the constraint is essential coordination for a dying community; from the Parsi individual seat, it is a shrinking marriage market enforced by law and custom. The engine computes this divergence from the structural asymmetry in exit options (identity_locked vs. organized) and beneficiary declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The Punchayet and communal tribunals sit near the beneficiary end: they derive authority and identity preservation from the constraint. Parsi individuals are the primary targets (high d) because the constraint directly extracts their marriage autonomy; their identity-locked exit amplifies effective extraction. Non-Parsi prospective spouses are fully excluded (d near 1.0) with no entry point. The Indian state sits near symmetric (d ~0.5) because it both benefits from delegated governance and pays the constitutional cost of managing pluralism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to preserve a vulnerable minority. Demographic data suggest the founding problem (community survival) is acute, but the instrument (endogamy enforcement) may have become more extractive than protective as the marriage pool contracts. The R5 genealogy interview records founding_problem_status as contested because external scholars dispute that the constraint still serves its original preservation function. If the community's decline continues despiteâor because ofâthe constraint, the arrangement risks mandatrophy: persisting as theatrical boundary maintenance after its preservation function has failed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'How would the classification of this constraint change if the marriage authority kernel were read through the secular civil reading instead of the Parsi communal reading?',
    'Comparative analysis with the sibling secular_civil_reading constraint story, which instantiates the same kernel under individual-rights and Special Marriage Act authority.',
    'The secular reading would likely reclassify the type and redistribute beneficiaries and victims, demonstrating that extraction in this kernel is reading-dependent and authority-source-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Structural contingency of this reading on the kernel decomposition').

omega_variable(
    demographic_viability_threshold,
    'Has the Parsi community''s demographic decline crossed the threshold where endogamy enforcement becomes extractive theatre rather than protective coordination?',
    'Longitudinal demographic analysis and ethnographic study of whether younger Parsis experience endogamy as protective or burdensome, relative to community size trends.',
    'If decline is irreversible and endogamy accelerates it by restricting marriage pools, the constraint''s coordination function is hollow and it drifts toward snare or piton; if it preserves a viable core, tangled_rope remains accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_viability_threshold, empirical, 'Whether demographic decline converts preservation into extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of exogamous marriage alternatives structural (legal bars and tribunal enforcement) or internalized (identity fusion making exit unthinkable)?',
    'Post-exit trajectory study of Parsis who marry outside under the Special Marriage Act to measure persistent social ostracism and identity cost after legal penalties are removed.',
    'If suppression is largely internalized, effective extraction exceeds the structural measure and the constraint operates as identity-lock; if purely structural, extraction is bounded by legal enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t18, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement(marr_tr_t36, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 36, 0.3).
narrative_ontology:measurement(marr_tr_t54, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 54, 0.34).
narrative_ontology:measurement(marr_tr_t72, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 72, 0.38).
narrative_ontology:measurement(marr_tr_t90, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 90, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(marr_be_t18, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(marr_be_t36, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 36, 0.46).
narrative_ontology:measurement(marr_be_t54, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 54, 0.53).
narrative_ontology:measurement(marr_be_t72, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 72, 0.59).
narrative_ontology:measurement(marr_be_t90, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 90, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t18, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 18, 0.44).
narrative_ontology:measurement(marr_su_t36, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 36, 0.48).
narrative_ontology:measurement(marr_su_t54, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 54, 0.51).
narrative_ontology:measurement(marr_su_t72, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 72, 0.53).
narrative_ontology:measurement(marr_su_t90, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 90, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__christian_canonical_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority_kernel, which decomposes into five structurally distinct authority claims (Hindu, Muslim, Christian, Parsi, secular). Each reading has a different epsilon, beneficiary structure, and source of legitimacy. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
