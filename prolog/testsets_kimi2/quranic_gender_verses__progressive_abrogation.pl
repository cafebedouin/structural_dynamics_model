% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Traditional Gender-Hierarchical Jurisprudence (Progressive Abrogation Reading)
 *   domain: religious/jurisprudential/social
 *
 * SUMMARY:
 *   This constraint story instantiates the progressive abrogation reading of
 *   the quranic_gender_verses kernel. From this reading's perspective, the
 *   gender-differentiated verses (on inheritance, testimony, and
 *   guardianship) constitute an incomplete trajectory that was superseded by
 *   later universalist principles such as Q 49:13 on universal human dignity.
 *   The standing arrangement under contest is the traditional jurisprudential
 *   framework that continues to enforce the earlier rules as timeless divine
 *   ordinance, blocking the application of naskh. The reading sees this
 *   persistence as highly extractive, delegitimizing traditional authority
 *   structures and imposing steep exit costs on scholars who adopt the
 *   abrogation view within traditional institutions.
 *
 * KEY AGENTS:
 *   - Traditional jurists: agenda-setter (institutional/constrained) â administer and enforce the hierarchical interpretation through seminaries, courts, and fatwa councils.
 *   - Women under sharia: primary target (powerless/identity_locked) â bear the differentiated legal status in family law, inheritance, and testimony.
 *   - Male guardians: primary beneficiary (moderate/mobile) â receive legal and economic privileges from the hierarchical framework.
 *   - Progressive jurists: secondary target (moderate/identity_locked) â bear career and social costs for advocating naskh; structurally silenced in traditional institutions.
 *   - Gender studies theorists: analytical observer (analytical/analytical) â document the extraction mechanism from outside the jurisprudential economy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.88).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.82).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Traditional Gender-Hierarchical Jurisprudence (Progressive Abrogation Reading)").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/jurisprudential/social").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, 'cde91327-39f1-42fd-80a5-48974cbc61ac').
narrative_ontology:cs_kernel_codification('cde91327-39f1-42fd-80a5-48974cbc61ac', fixed_text).
narrative_ontology:cs_authority_grounding('cde91327-39f1-42fd-80a5-48974cbc61ac', lineage).
narrative_ontology:cs_interpretation_layer_present('cde91327-39f1-42fd-80a5-48974cbc61ac').
narrative_ontology:cs_reading_relation('cde91327-39f1-42fd-80a5-48974cbc61ac', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('cde91327-39f1-42fd-80a5-48974cbc61ac', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_axiom('cde91327-39f1-42fd-80a5-48974cbc61ac', foundational, naskh_as_universalist_mechanism).
narrative_ontology:cs_axiom_status(naskh_as_universalist_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('cde91327-39f1-42fd-80a5-48974cbc61ac', naskh_as_universalist_mechanism, conventional).
narrative_ontology:cs_axiom('cde91327-39f1-42fd-80a5-48974cbc61ac', foundational, universal_dignity_as_normative_floor).
narrative_ontology:cs_axiom_status(universal_dignity_as_normative_floor, holdable).
narrative_ontology:cs_axiom_grounding('cde91327-39f1-42fd-80a5-48974cbc61ac', universal_dignity_as_normative_floor, deontological).
narrative_ontology:cs_reference_frame('cde91327-39f1-42fd-80a5-48974cbc61ac', universal_human_dignity_principle).
narrative_ontology:cs_drift_state('cde91327-39f1-42fd-80a5-48974cbc61ac', contemporary_jurisprudential_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cde91327-39f1-42fd-80a5-48974cbc61ac', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, traditional_jurists).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, male_guardians).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, women_under_sharia).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, progressive_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy seats in seminaries, courts, and fatwa councils that determine how Qur'anic verses on gender are applied. Their authority derives from chains of transmission and interpretive precedent. They enforce gender-differentiated rules through legal opinions, curricula, and social sanction against dissenting readings. Exit from this role means abandoning institutional authority and community standing.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_jurists, agenda_setter,
    institutional, generational, constrained, national).

% Subject to gender-differentiated rules in marriage, divorce, inheritance, and legal testimony. Their access to independent legal standing is filtered through male guardianship in many jurisdictions. Religious identity and family ties make exit from the framework costly, often requiring social rupture or geographic migration.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, women_under_sharia, payer,
    powerless, biographical, identity_locked, national).

% Receive larger shares of inheritance, hold unilateral divorce initiation in many schools, and function as legal intermediaries for female relatives in certain domains. They are socialized into these privileges as religiously ordained. Renouncing them is structurally possible but carries no incentive and significant family pressure.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, male_guardians, beneficiary,
    moderate, biographical, mobile, national).

% Trained in classical Islamic sciences but argue that later Qur'anic principles abrogate earlier gender-specific rules. They face institutional exclusion from seminary appointments, fatwa councils, and traditional publication channels. Their scholarly identity is fused with the tradition, making exit to secular academia or isolated independent work personally and professionally costly.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_jurists, payer,
    moderate, biographical, identity_locked, national).

% Analyze the jurisprudential framework from outside the interpretive apparatus, documenting the structural effects of gender-differentiated rules and the suppression of abrogation readings. They do not participate in the fatwa economy but provide the analytical frame for understanding the extraction mechanism.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, gender_studies_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes family structure, inheritance transmission, and lineage within a patriarchal framework, providing socially recognized governance of gender relations and religiously legitimated legal predictability.
% TRANSFER_FUNCTION: Transfers legal autonomy, economic share in inheritance, and testimonial weight from women and subordinate parties to male guardians and the institutional jurist class that administers the rules.
% ABSENT_VOICES: Progressive jurists trained in naskh methodology are structurally excluded from traditional seminaries and authoritative councils; women jurisprudents with classical training are denied seating in interpretive bodies that govern their own legal status.
% DISAPPEARANCE_RATIONALE: If the gender-differentiated rules were recognized as abrogated, inheritance distribution, marriage contracts, guardianship authority, and juridical hierarchy would reorganize immediately around universal dignity principles; the current social order is built on these distinctions.
% FOUNDING_PROBLEM: Establishing social order and lineage clarity in a 7th-century Arabian context where pre-existing tribal customs required structured, staged transition.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical Islamic studies scholars and feminist legal theorists outside the benefiting parties attest the hierarchical rules addressed a specific historical context; traditional jurists, who benefit from the current arrangement, dispute this. No institution uncontestedly confirms the founding problem remains live.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.88 because the progressive reading views the persistence of gender-differentiated rules as a comprehensive extraction of legal parity, maintained only by suppressing the later universalist verses. Suppression is 0.82 because the constraint depends on actively excluding progressive jurists from seminaries and councils and on identity-locking women within the framework. Theater ratio at 0.45 reflects increasing performative maintenance of 'unchanging divine order' discourse as historical-critical and feminist challenges mount. Accessibility collapse is high (0.72) because the alternative reading, while intellectually available, is structurally inaccessible to those fused to traditional identity. Resistance is 0.78 due to sustained but suppressed reformist movements.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (traditional jurists) experiences the constraint as legitimate coordination organizing society under divine ordinance; the payer seats (women and progressive jurists) experience the same structure as actively enforced extraction maintained by suppressing the abrogation alternative. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional jurists and male guardians are declared beneficiaries: they collect authority and legal privilege respectively, giving them low directionality toward subsidy. Women under sharia and progressive jurists are declared victims: they bear the legal and professional costs of the arrangement, giving them high directionality toward extraction. The engine will compute high effective extraction for the payer seats and damped or inverted extraction for the beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â establishing social order in a specific 7th-century context â is assessed as dead from this reading's perspective, yet the arrangement persists and extracts. The dead founding problem plus world_rearranges disappearance verdict flags the persistence gap, but the active beneficiary/victim structure and enforcement requirement prevent misclassification as piton: parties clearly benefit and parties clearly bear costs, so inertia alone does not explain persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_gender_applicability,
    'Does the principle of naskh legitimately apply to the gender-differentiated verses, or are these verses categorically exempt from abrogation by later universalist principles?',
    'Jurisprudential archaeology tracing the historical application of naskh to family-law verses versus devotional/ritual verses across classical schools.',
    'If naskh is structurally inapplicable, the progressive reading collapses and the constraint reverts to a literal hierarchical classification; if applicable, the extraction profile rises toward snare as the hierarchical persistence becomes illegitimate by the tradition''s own lights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_gender_applicability, conceptual, 'Whether naskh applies to gender verses').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional gatekeeping, employment sanctions, exclusion from councils) or internalized (community identity fused to literal reading, making epistemic departure feel like apostasy)?',
    'Post-reform suppression trajectory: if progressive readings remain suppressed after institutional barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure; communities carry the suppression with them even if institutional enforcement relaxes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    abrogation_vs_literal_foreclosure,
    'Does the progressive abrogation reading logically foreclose the literal hierarchical reading within a single jurisprudential framework, or can both be held as live hermeneutical options?',
    'Analysis of whether naskh, once invoked on these specific verses, leaves any hermeneutical space for timeless literal application within the same methodological school.',
    'If true foreclosure, the readings are mutually exclusive and the constraint family is committed; if false, they are merely competitive and the engine should treat them as coexisting claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_vs_literal_foreclosure, conceptual, 'Whether progressive abrogation structurally forecloses literal reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qura_tr_t10, quranic_gender_verses__progressive_abrogation, theater_ratio, 10, 0.3).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__progressive_abrogation, theater_ratio, 20, 0.35).
narrative_ontology:measurement(qura_tr_t30, quranic_gender_verses__progressive_abrogation, theater_ratio, 30, 0.38).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.42).
narrative_ontology:measurement(qura_tr_t50, quranic_gender_verses__progressive_abrogation, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(qura_be_t10, quranic_gender_verses__progressive_abrogation, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__progressive_abrogation, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(qura_be_t30, quranic_gender_verses__progressive_abrogation, base_extractiveness, 30, 0.84).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.86).
narrative_ontology:measurement(qura_be_t50, quranic_gender_verses__progressive_abrogation, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(qura_su_t10, quranic_gender_verses__progressive_abrogation, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__progressive_abrogation, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(qura_su_t30, quranic_gender_verses__progressive_abrogation, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(qura_su_t50, quranic_gender_verses__progressive_abrogation, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% The natural-language label 'quranic gender verses' conflates three structurally distinct constraints: a literal hierarchical command system, a contextual egalitarian reinterpretation, and a progressive abrogation reading. Each has a different epsilon, different victim/beneficiary structure, and different epistemic status. They are modeled as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
