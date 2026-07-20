% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Qur'anic Gender Verses â Literal Hierarchical Reading
 *   domain: religious_jurisprudence/gender_studies
 *
 * SUMMARY:
 *   Verses 4:11 (inheritance shares), 2:282 (testimony), and 4:34 (male
 *   guardianship/qawwamun) are read in the literal_hierarchical frame as
 *   direct, timeless legal constraints establishing male authority over women
 *   in family, financial, and legal domains. This reading treats the Arabic
 *   text as unambiguous divine ordinance, channeling authority and resources
 *   to male household heads and religious courts while constraining women's
 *   inheritance, testimony, and legal autonomy. The constraint is one reading
 *   of the quranic_gender_verses kernel; sibling readings
 *   (contextual_egalitarian, progressive_abrogation) dispute the timeless and
 *   hierarchical premises. High exit costs (apostasy, family rupture, social
 *   ostracism) lock women into the arrangement.
 *
 * KEY AGENTS:
 *   - male_household_heads: Primary beneficiary (moderate/mobile) â receive surplus inheritance, household authority, and subordinated labor
 *   - religious_courts: Agenda-setter and institutional beneficiary (institutional/constrained) â enforce the literal reading and derive authority from it
 *   - women: Primary target (powerless/identity_locked) â bear reduced legal autonomy, inheritance discrimination, and testimony discounting
 *   - egalitarian_reformists: Excluded voice (organized/constrained) â advocate alternative readings but are marginalized from interpretive authority
 *   - human_rights_observers: Analytical observer (institutional/analytical) â document discrimination without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.82).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.85).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.82).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, snare).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Qur'anic Gender Verses â Literal Hierarchical Reading").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "religious_jurisprudence/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, '1e17f552-929a-4987-b652-670499ed86cf').
narrative_ontology:cs_kernel_codification('1e17f552-929a-4987-b652-670499ed86cf', fixed_text).
narrative_ontology:cs_authority_grounding('1e17f552-929a-4987-b652-670499ed86cf', lineage).
narrative_ontology:cs_interpretation_layer_present('1e17f552-929a-4987-b652-670499ed86cf').
narrative_ontology:cs_reading_relation('1e17f552-929a-4987-b652-670499ed86cf', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_reading_relation('1e17f552-929a-4987-b652-670499ed86cf', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('1e17f552-929a-4987-b652-670499ed86cf', foundational, verses_are_timeless_divine_ordinance).
narrative_ontology:cs_axiom_status(verses_are_timeless_divine_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('1e17f552-929a-4987-b652-670499ed86cf', verses_are_timeless_divine_ordinance, theological).
narrative_ontology:cs_axiom('1e17f552-929a-4987-b652-670499ed86cf', secondary, male_guardianship_is_legal_default).
narrative_ontology:cs_axiom_status(male_guardianship_is_legal_default, holdable).
narrative_ontology:cs_axiom_grounding('1e17f552-929a-4987-b652-670499ed86cf', male_guardianship_is_legal_default, deontological).
narrative_ontology:cs_reference_frame('1e17f552-929a-4987-b652-670499ed86cf', timeless_revelatory_hierarchy).
narrative_ontology:cs_drift_state('1e17f552-929a-4987-b652-670499ed86cf', contemporary_egalitarian_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1e17f552-929a-4987-b652-670499ed86cf', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_courts).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive larger inheritance shares, control over family economic decisions, and legal authority over household women. The constraint assigns them the status of qawwamun with corresponding entitlements to women's obedience and labor. They can renounce these privileges but rarely do, as the arrangement is socially and religiously valorized.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    moderate, biographical, mobile, global).

% Interpret and enforce the verses as binding family law, deriving institutional authority from being the sole legitimate adjudicators of divine ordinance. They issue rulings on inheritance, marriage, and testimony that track the literal reading, and their legitimacy depends on maintaining that the text is unambiguous.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_courts, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, religious_courts, beneficiary).

% Are subject to reduced inheritance shares, differential testimony weight, and subordinated legal and financial autonomy under the literalist framework. Exiting the constraint requires apostasy, migration, or family rupture, all carrying severe social and often legal penalties.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women, payer,
    powerless, biographical, identity_locked, global).

% Advocate for contextual or abrogation-based readings that would equalize inheritance, testimony, and guardianship. They are systematically excluded from religious court appointments, mainstream fiqh academies, and state fatwa councils, and their readings are denounced as deviation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, egalitarian_reformists, excluded,
    organized, generational, constrained, global).

% Document legal gender discrimination in family codes based on these verses. They publish reports on inheritance inequality and testimony discrimination but lack authority within the religious legal framework to change rulings.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, human_rights_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, divinely authorized framework for family authority, inheritance distribution, and legal testimony, eliminating individual arbitrariness by assigning fixed, hierarchical roles purportedly sanctioned by revelation.
% TRANSFER_FUNCTION: Transfers legal autonomy, economic resources in the form of inheritance shares, and evidentiary weight from women to male household heads and to religious courts that adjudicate according to the literal reading.
% ABSENT_VOICES: Women's legal autonomy advocates, feminist theologians, and secular family-law reformers are structurally excluded from the interpretive authority; their objections are ruled out by the premise that the text is unambiguous divine ordinance and that male scholarly consensus is the gatekeeper.
% DISAPPEARANCE_RATIONALE: If the literal hierarchical reading ceased to be enforceable, family law in applying jurisdictions would shift toward egalitarian or secular codes, inheritance would be redistributed, women would regain full legal capacity in marriage and finance, and the patriarchal family structure maintained by the constraint would unravel.
% FOUNDING_PROBLEM: Establishing social and legal order in the early Muslim community by aligning family structure, inheritance, and commercial testimony with a framework that centralized male authority and provided clear rules for a kinship-based society.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical scholars and human rights organizations outside the beneficiary set attest that the verses addressed specific 7th-century Arabian contexts; traditionalist ulama (beneficiaries) attest the problem is timeless divine order. No neutral corroboration exists for the timeless claim independent of the institutions that benefit from it.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint systematically transfers legal capacity, inheritance, and labor surplus from women to male guardians. Suppression is high (0.85) because persistence depends on criminalizing apostasy, stigmatizing family rupture, and excluding egalitarian readings from religious authority. Theater_ratio (0.45) reflects that a substantial share of enforcement is performative maintenance of patriarchal order dressed in theological language rather than functional dispute resolution. Accessibility_collapse (0.70) is high within the faith framework: once the literal divine-ordinance frame is accepted, egalitarian alternatives collapse for the believer, even though secular alternatives exist externally. Resistance (0.55) is moderate because overt resistance is suppressed within communities, while external human rights pressure is substantial but institutionally excluded.
 *
 * PERSPECTIVAL GAP:
 *   The religious_courts seat computes as a beneficiary/agenda_setter because it administers and gains authority from the constraint; the women seat computes as a target because it bears the extraction. The male_household_heads seat computes as beneficiary. These divergences are structural, not perspectival illusions â the same arrangement channels authority and resources upward in the gender hierarchy while imposing costs downward.
 *
 * DIRECTIONALITY LOGIC:
 *   Male_household_heads and religious_courts are declared beneficiaries; the engine derives low directionality (subsidy) for them. Women are declared victims/payers with identity_locked exit, producing high directionality (target). Egalitarian_reformists are excluded but not victims of direct extraction; their constrained exit produces moderate d. The agenda_setter seat (religious_courts) experiences the constraint as legitimate authority and coordination; the payer seat (women) experiences it as extraction backed by divine threat.
 *
 * MANDATROPHY ANALYSIS:
 *   The literal reading claims timelessness, which if naturalized would read as a Mountain. However, declared beneficiaries (male guardians, religious courts) and victims (women) trigger the False Summit Mountain pathway. The constraint is classified as snare because the coordination story (divine family order) is cover for extraction, and the metrics (high extraction, high suppression, moderate theater) confirm that the primary operation is rent-seeking through patriarchal authority rather than genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_decomposition_delta,
    'This constraint is the literal_hierarchical reading of kernel quranic_gender_verses. Would the contextual_egalitarian reading remove the victim set entirely by treating verses 4:11, 2:282, and 4:34 as historically bounded progressive steps rather than timeless commands?',
    'Historical-critical and linguistic analysis of each verse''s 7th-century Arabian context; comparison with pre-Islamic inheritance and testimony practices.',
    'If the verses are demonstrably historically bounded, the literal_hierarchical reading''s victim set is a construction of anachronistic literalism, and the constraint should be reclassified as tangled_rope or piton rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decomposition_delta, conceptual, 'Whether the kernel''s literal reading creates victims that a sibling reading would dissolve').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state-enforced family law with penalties for apostasy and deviance) or internalized (women and families believe the hierarchy is divinely deserved and resist exit even when legal barriers are absent)?',
    'Post-exit suppression trajectory: if women who leave the faith or jurisdiction still exhibit compliance patterns and identity fusion, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    divine_ordinance_naturalization,
    'Does the timeless divine ordinance framing function as irreducible theological truth or as a naturalization of patriarchal extraction that benefits male household heads and religious courts?',
    'Cross-cultural comparison with non-Abrahamic legal systems exhibiting similar gender hierarchy without divine textual grounding; analysis of beneficiary concentration.',
    'If the framing primarily benefits male authorities while alternative theological framings exist within the same tradition, the constraint operates as a false summit (tangled_rope) rather than a genuine mountain or pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_ordinance_naturalization, conceptual, 'Natural-law versus constructed framing ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qgv_lit_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.3).
narrative_ontology:measurement(qgv_lit_tr_t10, quranic_gender_verses__literal_hierarchical, theater_ratio, 10, 0.33).
narrative_ontology:measurement(qgv_lit_tr_t20, quranic_gender_verses__literal_hierarchical, theater_ratio, 20, 0.36).
narrative_ontology:measurement(qgv_lit_tr_t30, quranic_gender_verses__literal_hierarchical, theater_ratio, 30, 0.39).
narrative_ontology:measurement(qgv_lit_tr_t40, quranic_gender_verses__literal_hierarchical, theater_ratio, 40, 0.42).
narrative_ontology:measurement(qgv_lit_tr_t50, quranic_gender_verses__literal_hierarchical, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(qgv_lit_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(qgv_lit_be_t10, quranic_gender_verses__literal_hierarchical, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(qgv_lit_be_t20, quranic_gender_verses__literal_hierarchical, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(qgv_lit_be_t30, quranic_gender_verses__literal_hierarchical, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(qgv_lit_be_t40, quranic_gender_verses__literal_hierarchical, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(qgv_lit_be_t50, quranic_gender_verses__literal_hierarchical, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(qgv_lit_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(qgv_lit_su_t10, quranic_gender_verses__literal_hierarchical, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(qgv_lit_su_t20, quranic_gender_verses__literal_hierarchical, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(qgv_lit_su_t30, quranic_gender_verses__literal_hierarchical, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(qgv_lit_su_t40, quranic_gender_verses__literal_hierarchical, suppression_requirement, 40, 0.83).
narrative_ontology:measurement(qgv_lit_su_t50, quranic_gender_verses__literal_hierarchical, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, enforcement_mechanism).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the quranic_gender_verses kernel. The literal_hierarchical reading treats verses 4:11, 2:282, and 4:34 as timeless legal constraints with high extraction; the contextual_egalitarian reading treats them as historically situated progressive steps; the progressive_abrogation reading treats them as superseded by later egalitarian principles. The kernel decomposes here because the epsilon values and beneficiary/victim structures differ radically across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
