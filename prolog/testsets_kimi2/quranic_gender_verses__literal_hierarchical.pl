% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
 *   domain: religious/jurisprudence/gender
 *
 * SUMMARY:
 *   This constraint instantiates the literal_hierarchical reading of the
 *   quranic_gender_verses kernel, treating verses 4:11 (inheritance), 2:282
 *   (testimony), and 4:34 (guardianship) as direct, timeless legal
 *   ordinances. The structural analysis identifies high extraction flowing
 *   from women to male guardians and juristic institutions, mediated through
 *   divine-authority framing that suppresses exit via theological
 *   identity-lock and family rupture costs. The claim/metric independence is
 *   maintained: the reading CLAIMS the coordination function of divinely
 *   ordered society while the metrics describe substantial asymmetric
 *   extraction and active enforcement requirements.
 *
 * KEY AGENTS:
 *   - Traditional jurists (institutional/identity-locked): administer and benefit from the literal interpretive framework.
 *   - Male guardians (powerful/constrained): enforce within households and collect authority and resource advantages.
 *   - Women subject to guardianship (powerless/trapped): bear the costs of constrained inheritance, discounted testimony, and legal subordination.
 *   - Progressive scholars (moderate/constrained): excluded from authoritative interpretation.
 *   - Human rights monitors (organized/analytical): observe and document material effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.82).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.78).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.82).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Qur'anic Gender Verses â Literal Hierarchical Reading").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "religious/jurisprudence/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, '3dff70da-7594-4a00-bd16-d39b4a3015e4').
narrative_ontology:cs_kernel_codification('3dff70da-7594-4a00-bd16-d39b4a3015e4', fixed_text).
narrative_ontology:cs_authority_grounding('3dff70da-7594-4a00-bd16-d39b4a3015e4', lineage).
narrative_ontology:cs_interpretation_layer_present('3dff70da-7594-4a00-bd16-d39b4a3015e4').
narrative_ontology:cs_reading_relation('3dff70da-7594-4a00-bd16-d39b4a3015e4', quranic_gender_verses__contextual_egalitarian, forecloses).
narrative_ontology:cs_reading_relation('3dff70da-7594-4a00-bd16-d39b4a3015e4', quranic_gender_verses__progressive_abrogation, forecloses).
narrative_ontology:cs_axiom('3dff70da-7594-4a00-bd16-d39b4a3015e4', foundational, quranic_gender_hierarchy_timeless_ordinance).
narrative_ontology:cs_axiom_status(quranic_gender_hierarchy_timeless_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('3dff70da-7594-4a00-bd16-d39b4a3015e4', quranic_gender_hierarchy_timeless_ordinance, theological).
narrative_ontology:cs_axiom('3dff70da-7594-4a00-bd16-d39b4a3015e4', foundational, male_guardianship_universal_legal_binding).
narrative_ontology:cs_axiom_status(male_guardianship_universal_legal_binding, holdable).
narrative_ontology:cs_axiom_grounding('3dff70da-7594-4a00-bd16-d39b4a3015e4', male_guardianship_universal_legal_binding, deontological).
narrative_ontology:cs_reference_frame('3dff70da-7594-4a00-bd16-d39b4a3015e4', divine_ordinance_framework).
narrative_ontology:cs_drift_state('3dff70da-7594-4a00-bd16-d39b4a3015e4', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3dff70da-7594-4a00-bd16-d39b4a3015e4', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, traditional_jurists).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_guardians).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_subject_to_guardianship).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, literal_inerrancy_doctrine).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, patriarchal_guardianship_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derive institutional authority from interpreting and enforcing the literal meaning of verses 4:11, 2:282, and 4:34 across the classical schools of law. Issue binding rulings on inheritance shares, testimony admissibility, and marital guardianship. Their scholarly reputation, position, and livelihood depend on maintaining the literal hierarchical tradition.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, traditional_jurists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, traditional_jurists, beneficiary).

% Exercise decision-making authority over female relatives in marriage, travel, and financial matters in jurisdictions applying this reading. Receive larger inheritance shares and their testimony is given greater weight in family and financial disputes. Social standing and legal privileges within the community are tied to this role.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_guardians, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, male_guardians, agenda_setter).

% Require male guardian approval for marriage and other legal acts where this reading is enforced. Receive half the inheritance share of male counterparts in the same relation. Their testimony is discounted or excluded in certain financial proceedings. Leaving the framework risks family rupture, loss of community, and in some jurisdictions legal penalties for apostasy.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_subject_to_guardianship, payer,
    powerless, biographical, trapped, global).

% Produce scholarly arguments for contextual or abrogated readings of the same verses, citing historical circumstance or later egalitarian principles. Are excluded from authoritative interpretation in institutions adhering to the literal hierarchical framework and their work is treated as illegitimate by traditional jurists.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, progressive_scholars, excluded,
    moderate, generational, constrained, global).

% Document differential legal treatment of women under family-law regimes applying these verses. Record material effects on women's legal capacity, economic standing, and freedom of movement without participating in the theological debate.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, human_rights_monitor, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, diffuse).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a divinely ordained hierarchical order for family governance, inheritance distribution, and legal testimony, creating a predictable, asymmetrically gendered framework for property and status transmission within Muslim communities.
% TRANSFER_FUNCTION: Transfers legal autonomy, economic resources via inheritance differentials, and testimonial capacity from women to male household heads and institutionalized juridical authority.
% ABSENT_VOICES: Women contesting the divine authorship of the hierarchy, progressive scholars arguing for contextual or abrogated readings, and secular legal authorities challenging the jurisdiction of religious family law are structurally excluded from authoritative interpretation.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, inheritance shares would equalize, testimony rules would unify, and guardianship structures would collapse in jurisdictions applying this reading; male household heads would lose codified authority and women would gain unilateral legal capacity.
% FOUNDING_PROBLEM: To establish a divinely ordered structure for family governance, inheritance transmission, and legal testimony in the nascent Muslim community, replacing pre-Islamic tribal customs with a theocentric hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and feminist legal scholars outside the beneficiary set attest the problem was specific to 7th-century Arabian social conditions; traditional jurists within the beneficiary set assert it addresses timeless human nature. No corroboration from outside the benefiting parties exists for the timelessness claim â the dispute is the kernel contest itself.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.82) reflects the severe asymmetry in inheritance, testimony, and legal autonomy. Suppression (0.78) is high because exit requires apostasy or family rupture, and state or community enforcement is active. Theater_ratio (0.32) captures the performative maintenance of piety and public gender order that exceeds pure functional necessity. Accessibility_collapse (0.80) is high because the divine-ordinance framing forecloses alternatives for adherents. Resistance (0.45) reflects ongoing but partially suppressed feminist and reformist challenge. Temporal series show gradual intensification of extraction as classical jurisprudence codified the literal reading, with theater rising in the modern period as states deploy the constraint as an identity marker.
 *
 * PERSPECTIVAL GAP:
 *   From the traditional jurist and male guardian seats, the constraint is experienced as legitimate divine order and necessary social coordination; effective extraction is damped or inverted into subsidy for their authority. From the women_subject_to_guardianship seat, the same structure reads as enforced extraction of legal personhood and economic share; effective extraction is amplified by identity-locked exit and large spatial scope. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Male guardians and traditional jurists are declared beneficiaries with constrained or identity-locked exit, placing their directionality near the beneficiary end. Women_subject_to_guardianship are declared victims with trapped exit and universal scope, placing directionality near the full-target end. The resulting effective extraction is asymmetrically concentrated on the powerless victim seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope rather than snare preserves the genuine coordination function the reading claims: the verses do create a predictable, hierarchically ordered system for property and family governance that believers experience as real social structure. However, the asymmetric extraction is structurally inseparable from that coordination: the same guardianship mechanism that coordinates family authority also extracts legal autonomy and economic share. Mandatrophy is not declared resolved because the founding problem is contested and the constraint persists beyond its historically specific origin; the mismatch between contested status and world_rearranges disappearance verdict flags a potential zombie structure, although the literal reading denies this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    timeless_divine_vs_historical_construct,
    'Is the gender hierarchy established by these verses a timeless divine ordinance independent of human construction, or a historically situated legal arrangement whose persistence depends on juristic and state enforcement?',
    'Comparative legal-historical analysis of pre-Islamic Arabian gender norms versus the Qur''anic verses, and examination of enforcement patterns in jurisdictions that have removed or retained the hierarchy.',
    'If historically constructed, the constraint''s natural-law framing is a false summit and effective extraction is higher than the coordination story suggests; if genuinely timeless within the theological frame, the classification shifts toward the coordination end of the tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(timeless_divine_vs_historical_construct, conceptual, 'Whether the hierarchy is divine natural law or constructed legal constraint').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression that maintains this constraint primarily structural (state enforcement, family law courts, economic dependency) or internalized (theological identity fusion, belief in divine ordination, fear of apostasy)?',
    'Post-exit trajectory studies: measuring continued compliance or psychological distress among women who have left jurisdictions or belief structures enforcing the verses.',
    'If internalized suppression dominates, effective extraction exceeds structural measures because the target carries the constraint after formal exit; this would strengthen the snare-like profile relative to the tangled rope framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function of inheritance and family order be separated from the asymmetric extraction of women''s legal autonomy and economic share?',
    'Natural experiments or reform jurisdictions that equalized inheritance and testimony while maintaining a Muslim family-law framework; observing whether family coordination collapses or adapts.',
    'If separable, the asymmetric component is pure extraction layered on coordination, strengthening the snare reading; if inseparable, the extraction is the price of the coordination itself, keeping the tangled rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qgv_lh_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qgv_lh_tr_t280, quranic_gender_verses__literal_hierarchical, theater_ratio, 280, 0.15).
narrative_ontology:measurement(qgv_lh_tr_t560, quranic_gender_verses__literal_hierarchical, theater_ratio, 560, 0.2).
narrative_ontology:measurement(qgv_lh_tr_t840, quranic_gender_verses__literal_hierarchical, theater_ratio, 840, 0.25).
narrative_ontology:measurement(qgv_lh_tr_t1120, quranic_gender_verses__literal_hierarchical, theater_ratio, 1120, 0.3).
narrative_ontology:measurement(qgv_lh_tr_t1400, quranic_gender_verses__literal_hierarchical, theater_ratio, 1400, 0.32).

% Extraction over time
narrative_ontology:measurement(qgv_lh_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(qgv_lh_be_t280, quranic_gender_verses__literal_hierarchical, base_extractiveness, 280, 0.55).
narrative_ontology:measurement(qgv_lh_be_t560, quranic_gender_verses__literal_hierarchical, base_extractiveness, 560, 0.68).
narrative_ontology:measurement(qgv_lh_be_t840, quranic_gender_verses__literal_hierarchical, base_extractiveness, 840, 0.75).
narrative_ontology:measurement(qgv_lh_be_t1120, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1120, 0.8).
narrative_ontology:measurement(qgv_lh_be_t1400, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1400, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(qgv_lh_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qgv_lh_su_t280, quranic_gender_verses__literal_hierarchical, suppression_requirement, 280, 0.45).
narrative_ontology:measurement(qgv_lh_su_t560, quranic_gender_verses__literal_hierarchical, suppression_requirement, 560, 0.6).
narrative_ontology:measurement(qgv_lh_su_t840, quranic_gender_verses__literal_hierarchical, suppression_requirement, 840, 0.7).
narrative_ontology:measurement(qgv_lh_su_t1120, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1120, 0.75).
narrative_ontology:measurement(qgv_lh_su_t1400, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1400, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the quranic_gender_verses kernel. The colloquial label 'Qur'anic gender verses' conflates three structurally distinct readings with different epsilon values, beneficiary sets, and authority groundings. Decomposed per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
