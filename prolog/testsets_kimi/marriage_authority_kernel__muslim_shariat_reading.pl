% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__muslim_shariat_reading, []).

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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Muslim Personal Law (Shariat) Marriage Authority
 *   domain: legal/religious/governance
 *
 * SUMMARY:
 *   In Indian legal pluralism, Muslim family law authority derives from
 *   Shariat as interpreted by qazis and Muslim personal law boards, operating
 *   under the Muslim Personal Law (Shariat) Application Act 1937. This
 *   constraint story instantiates the muslim_shariat_reading of the
 *   marriage_authority_kernel. The arrangement coordinates marriage, divorce,
 *   and inheritance governance for the Muslim community through community
 *   tribunals, but embeds structurally asymmetric extraction in the form of
 *   unilateral talaq (now partially judicially restricted), polygamy
 *   permissions, and unequal inheritance shares. State intervention remains
 *   contested, with the Indian state delegating authority while
 *   constitutional equality provisions create persistent friction.
 *
 * KEY AGENTS:
 *   - qazis_and_personal_law_boards: Primary agenda-setter (organized/generational, constrained exit) â interprets Shariat and adjudicates family disputes for the community.
 *   - muslim_male_guardians: Primary beneficiary (moderate/biographical, constrained exit) â holds unilateral talaq, polygamy, and inheritance prerogatives under the interpreted Shariat.
 *   - muslim_women: Primary target/payer (powerless/biographical, identity_locked exit) â bears the asymmetric costs of unilateral divorce, unequal inheritance, and limited tribunal representation.
 *   - indian_state: Institutional observer (institutional/generational, analytical exit) â delegates authority to personal law boards via the Shariat Act 1937; could legislate override but politically abstains.
 *   - womens_rights_advocates: Excluded voice (organized/biographical, mobile exit) â contests gender inequity through public interest litigation and constitutional challenges from outside the tribunal structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.78).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.8).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Muslim Personal Law (Shariat) Marriage Authority").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "legal/religious/governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, 'b6ebe3e3-8ac0-4283-908d-033ac5330189').
narrative_ontology:cs_kernel_codification('b6ebe3e3-8ac0-4283-908d-033ac5330189', fixed_text).
narrative_ontology:cs_authority_grounding('b6ebe3e3-8ac0-4283-908d-033ac5330189', lineage).
narrative_ontology:cs_interpretation_layer_present('b6ebe3e3-8ac0-4283-908d-033ac5330189').
narrative_ontology:cs_reading_relation('b6ebe3e3-8ac0-4283-908d-033ac5330189', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6ebe3e3-8ac0-4283-908d-033ac5330189', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6ebe3e3-8ac0-4283-908d-033ac5330189', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6ebe3e3-8ac0-4283-908d-033ac5330189', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('b6ebe3e3-8ac0-4283-908d-033ac5330189', foundational, shariat_textual_supremacy_family_law).
narrative_ontology:cs_axiom_status(shariat_textual_supremacy_family_law, holdable).
narrative_ontology:cs_axiom_grounding('b6ebe3e3-8ac0-4283-908d-033ac5330189', shariat_textual_supremacy_family_law, theological).
narrative_ontology:cs_axiom('b6ebe3e3-8ac0-4283-908d-033ac5330189', foundational, community_tribunal_jurisdiction_over_muslims).
narrative_ontology:cs_axiom_status(community_tribunal_jurisdiction_over_muslims, holdable).
narrative_ontology:cs_axiom_grounding('b6ebe3e3-8ac0-4283-908d-033ac5330189', community_tribunal_jurisdiction_over_muslims, conventional).
narrative_ontology:cs_reference_frame('b6ebe3e3-8ac0-4283-908d-033ac5330189', classical_shariat_family_order).
narrative_ontology:cs_drift_state('b6ebe3e3-8ac0-4283-908d-033ac5330189', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b6ebe3e3-8ac0-4283-908d-033ac5330189', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazis_and_personal_law_boards).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_male_guardians).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_women).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__muslim_shariat_reading, shariat_supremacy_in_family_matters).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__muslim_shariat_reading, community_self_governance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Shariat texts and adjudicate marriage, divorce, and inheritance disputes for Indian Muslims. Derive authority from community recognition and the Muslim Personal Law (Shariat) Application Act 1937. Their rulings on triple talaq, polygamy, and mehr are enforced through social sanction and state court deference. They cannot easily abandon their interpretive role without losing community standing.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazis_and_personal_law_boards, agenda_setter,
    organized, generational, constrained, national).

% Hold unilateral talaq prerogatives (until judicially restricted), polygamy permissions, and advantageous inheritance shares under the interpreted Shariat. Their family authority is subsidized by the tribunal structure. Exit would require abandoning the legal and social privileges conferred by the community framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_male_guardians, beneficiary,
    moderate, biographical, constrained, national).

% Subject to unilateral divorce, unequal inheritance, and limited representation in tribunal proceedings. Civil remedies exist on paper but are socially and psychologically inaccessible due to community identity fusion and fear of ostracism. Accessing the Special Marriage Act or civil courts is treated as apostasy or family betrayal.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_women, payer,
    powerless, biographical, identity_locked, national).

% Delegates family-law authority to Muslim personal law boards through statutory recognition while retaining constitutional power to legislate a uniform civil code. Politically constrained by minority-protection discourse and vote-bank considerations, so it largely abstains from intervention despite Article 14 and 15 guarantees.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, indian_state, observer,
    institutional, generational, analytical, national).

% Contest gender inequity in personal law through public interest litigation, constitutional challenges, and public campaigns. They are structurally excluded from the qazi and personal law board adjudication process; their arguments reach Muslims only through state courts or media, not through the community tribunal framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, womens_rights_advocates, excluded,
    organized, biographical, mobile, national).

% Advocate for ijtihad and gender-equitable reinterpretation of Shariat from within the Muslim community. They bear social costs of censure and reduced marriageability within the community. Their voices are marginalized by the dominant personal law board consensus and have no institutionalized channel into tribunal rulings.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, dissenting_muslim_progressives, excluded,
    moderate, biographical, constrained, national).

narrative_ontology:fixing_cost_class(marriage_authority_kernel__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, religiously grounded marriage, divorce, and inheritance framework for the Indian Muslim community, adjudicated by community-recognized authorities rather than civil courts, preserving minority religious identity in family law and reducing state judicial burden.
% TRANSFER_FUNCTION: Moves authority over family disputes from the secular civil court system to community tribunals; moves equitable legal protections away from Muslim women to male guardians in matters of unilateral divorce, polygamy, and inheritance shares.
% ABSENT_VOICES: Muslim women seeking full gender parity, secular feminist legal advocates, and progressive theologians arguing for ijtihad are structurally excluded from the tribunal authority structure; their objections are routed through state courts or public campaigns rather than the community framework itself.
% DISAPPEARANCE_RATIONALE: If Shariat-based marriage authority vanished overnight, Muslim family disputes would migrate to civil courts or demand a new statutory framework; male guardians would lose unilateral divorce and polygamy prerogatives; the community's institutional autonomy in family matters would collapse; personal law boards and qazis would lose their adjudicative function.
% FOUNDING_PROBLEM: Colonial and post-colonial governance of religious diversity: how to administer family law for a Muslim minority without imposing a uniform civil code, while preserving community identity and reducing judicial burden on state courts.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars and women's rights organizations outside the beneficiary structure attest that the founding problem of minority protection is partially resolved but has ossified into gender inequality; the state itself has partially contested this through Supreme Court intervention (Shayara Bano v. Union of India) and ongoing political debate, corroborating that the arrangement's persistence is disputed from outside the benefiting parties.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint transfers substantial rights and autonomy from women to male guardians under the guise of religious adjudication. Suppression (0.80) is high because state delegation and community enforcement jointly block access to civil alternatives; the Special Marriage Act exists on paper but is socially inaccessible for most. Theater_ratio (0.35) reflects that while adjudication is functionally real, a significant share of authority maintenance consists of performative assertions of immutable tradition that resist empirical challenge. Accessibility_collapse (0.75) captures the collapse of civil-court alternatives once a Muslim family accepts the tribunal's jurisdiction or is socialized into its necessity. Resistance (0.60) reflects sustained legal challenge (Shayara Bano), feminist mobilization, and progressive theological dissent. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the qazi seat, the constraint is genuine coordination: it preserves community identity, provides accessible dispute resolution, and prevents assimilation into a majoritarian civil code. From the Muslim woman seat, the same structure is active extraction: the 'coordination' is inseparable from the transfer of equitable rights to male authority. The engine computes this divergence from the same structural data â the asymmetry in exit_options (identity_locked vs constrained) and the beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The qazis and personal law boards derive authority (low d, near beneficiary end) from their interpretive monopoly. Muslim male guardians receive structural subsidies in family authority (low d). Muslim women bear the extraction directly â their exit options are identity_locked, pushing d toward the full-target end. The Indian state sits at analytical distance; structurally it is not a target of this constraint, though politically it incurs costs from both intervention and non-intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling in both directions. It is not a pure Snare because the community tribunal function is genuinely valued and used; dissolving it entirely would force a population into a legal framework it does not recognize. It is not a pure Rope because the coordination is inseparable from asymmetric extraction: the same tribunal that resolves disputes also enforces unilateral talaq and unequal inheritance. The Tangled Rope classification captures this hybridity without collapsing it to either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_authority_or_constructed_interpretation,
    'Does this constraint derive its force from an unchangeable divine kernel (Mountain-like), or is it a constructed juridical tradition that extracts asymmetrically to preserve male gatekeeper authority?',
    'Historical analysis of Shariat interpretation variance across time, geography, and school (Hanafi, Shafi''i, etc.); if the rules vary significantly by qazi and board while claiming immutability, the constraint is constructed and the Mountain claim is a false summit.',
    'A true divine kernel would resist reform as apostasy; a constructed interpretation opens space for ijtihad (reinterpretation) and legislative reform without theological contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_authority_or_constructed_interpretation, conceptual, 'Whether Shariat authority is divine natural law or constructed interpretation').

omega_variable(
    suppression_structural_or_internalized,
    'Is the persistence of this constraint due to state legal delegation (structural suppression of civil alternatives) or internalized religious identity that makes Muslim women unwilling to access secular courts?',
    'Comparative study of Muslim women who exit the community framework: if they successfully obtain civil remedies, suppression was primarily structural; if they avoid civil courts even after legal education, internalized identity-lock dominates.',
    'If internalized, effective extraction is higher than structural measures suggest and the constraint operates as a deeper snare; if structural, reform through state legislative override is more viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Structural vs internalized suppression mechanism in personal law adherence').

omega_variable(
    shariat_secular_reading_relationship,
    'Does the Muslim Shariat reading structurally foreclose the secular civil reading for Muslims, or merely coexist with it as an unexercised option?',
    'Empirical measurement of Special Marriage Act usage among Indian Muslims, combined with ethnographic study of community sanctions against couples who opt out of Shariat adjudication.',
    'If foreclosed, the constraint is more extractive because exit to civil law is illusory; if coexistent, the extraction is moderated by a genuine exit option that some Muslims exercise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shariat_secular_reading_relationship, empirical, 'Whether Shariat and secular civil readings are parallel or mutually exclusive for Muslims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 0, 85).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(muslim_shariat_tr_t0, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(muslim_shariat_tr_t17, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 17, 0.18).
narrative_ontology:measurement(muslim_shariat_tr_t34, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 34, 0.22).
narrative_ontology:measurement(muslim_shariat_tr_t51, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 51, 0.27).
narrative_ontology:measurement(muslim_shariat_tr_t68, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 68, 0.31).
narrative_ontology:measurement(muslim_shariat_tr_t85, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 85, 0.35).

% Extraction over time
narrative_ontology:measurement(muslim_shariat_be_t0, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(muslim_shariat_be_t17, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 17, 0.48).
narrative_ontology:measurement(muslim_shariat_be_t34, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 34, 0.56).
narrative_ontology:measurement(muslim_shariat_be_t51, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 51, 0.65).
narrative_ontology:measurement(muslim_shariat_be_t68, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 68, 0.72).
narrative_ontology:measurement(muslim_shariat_be_t85, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 85, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(muslim_shariat_su_t0, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(muslim_shariat_su_t17, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 17, 0.52).
narrative_ontology:measurement(muslim_shariat_su_t34, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 34, 0.6).
narrative_ontology:measurement(muslim_shariat_su_t51, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 51, 0.68).
narrative_ontology:measurement(muslim_shariat_su_t68, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 68, 0.74).
narrative_ontology:measurement(muslim_shariat_su_t85, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 85, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority_kernel, decomposed from the colloquial label 'marriage law authority in India' which conflates multiple structurally distinct commitment systems. Each reading has a distinct kernel codification, beneficiary/victim structure, and epsilon profile. They share a regulatory domain (Indian family law) but instantiate different authority grounds and extraction patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
