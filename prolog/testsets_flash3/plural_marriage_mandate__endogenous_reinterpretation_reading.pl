% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto: Endogenous Prophetic Reinterpretation
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the 'endogenous reinterpretation'
 *   reading of the 1890 Manifesto, where the Church's suspension of plural
 *   marriage is understood as a legitimate prophetic reinterpretation of
 *   divine will, necessary for the preservation of the Church's salvific
 *   mission. This reading emphasizes the internal theological justification
 *   for the change, framing it as a divinely guided adaptation rather than a
 *   capitulation to external pressure. The constraint coordinates mainstream
 *   members around this new understanding, while extracting from
 *   fundamentalist groups who reject it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.25).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.4).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto: Endogenous Prophetic Reinterpretation").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, '713a2f24-c48f-4173-ab58-3f820d8e48c2').
narrative_ontology:cs_kernel_codification('713a2f24-c48f-4173-ab58-3f820d8e48c2', formalized).
narrative_ontology:cs_authority_grounding('713a2f24-c48f-4173-ab58-3f820d8e48c2', lineage).
narrative_ontology:cs_interpretation_layer_present('713a2f24-c48f-4173-ab58-3f820d8e48c2').
narrative_ontology:cs_reading_relation('713a2f24-c48f-4173-ab58-3f820d8e48c2', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('713a2f24-c48f-4173-ab58-3f820d8e48c2', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('713a2f24-c48f-4173-ab58-3f820d8e48c2', foundational, prophetic_revelation_is_dynamic).
narrative_ontology:cs_axiom_status(prophetic_revelation_is_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('713a2f24-c48f-4173-ab58-3f820d8e48c2', prophetic_revelation_is_dynamic, theological).
narrative_ontology:cs_axiom('713a2f24-c48f-4173-ab58-3f820d8e48c2', foundational, salvific_mission_supersedes_specific_practice).
narrative_ontology:cs_axiom_status(salvific_mission_supersedes_specific_practice, holdable).
narrative_ontology:cs_axiom_grounding('713a2f24-c48f-4173-ab58-3f820d8e48c2', salvific_mission_supersedes_specific_practice, theological).
narrative_ontology:cs_reference_frame('713a2f24-c48f-4173-ab58-3f820d8e48c2', continuous_prophetic_guidance).
narrative_ontology:cs_drift_state('713a2f24-c48f-4173-ab58-3f820d8e48c2', post_1890_manifesto, gap(stable, minor, true)).
narrative_ontology:cs_created_at('713a2f24-c48f-4173-ab58-3f820d8e48c2', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, the_church_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_members).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the central authority, the Church institution issued the Manifesto, interpreting it as a divinely guided reinterpretation to ensure its survival and continued salvific mission. It benefits from legal recognition, continued temple access, and missionary work, which would have been jeopardized by continued plural marriage. It enforces the new interpretation through ecclesiastical courts and excommunication.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, the_church_institution, agenda_setter,
    institutional, generational, constrained, global).

% These members accept the reinterpretation as legitimate prophetic guidance. They benefit from the Church's continued legal status, social acceptance, and access to ordinances like temple marriage, which were threatened by the federal anti-polygamy campaign. Their faith is affirmed by the narrative of divine intervention.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_members, beneficiary,
    organized, biographical, mobile, global).

% These groups reject the 1890 Manifesto as a legitimate reinterpretation, viewing it as a capitulation to secular pressure or a deviation from eternal doctrine. They bear the cost of excommunication, social ostracization, and legal persecution for continuing the practice of plural marriage. Their identity is deeply tied to the original doctrine, making exit from their commitment to it nearly impossible.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_groups, payer,
    powerless, generational, identity_locked, local).

% The federal government exerted coercive pressure through legislation and court cases, leading to the Manifesto. From its perspective, the Manifesto was a necessary step to enforce federal law and uphold societal norms, regardless of the Church's internal theological justification. It observes the Church's compliance.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Church's membership around a unified, legally compliant practice regarding marriage, allowing the institution to continue its salvific mission without federal interference and ensuring access to sacred ordinances.
% TRANSFER_FUNCTION: Transfers the practice of plural marriage from a divinely sanctioned requirement to a temporally suspended doctrine, shifting the burden of compliance from the Church institution to individual members who might otherwise continue the practice, and transferring social and legal legitimacy from the federal government to the Church.
% ABSENT_VOICES: Early adherents who sacrificed for plural marriage and would have viewed its suspension as a betrayal of divine command are absent from the contemporary narrative, their voices largely marginalized or reinterpreted within the dominant discourse.
% DISAPPEARANCE_RATIONALE: If the 1890 Manifesto and its reinterpretation vanished, the Church would face immediate legal challenges, loss of tax-exempt status, and renewed federal persecution. Its global missionary efforts would cease, and temple ordinances would be suspended, fundamentally altering its institutional structure and salvific mission.
% FOUNDING_PROBLEM: The Church faced existential threats from the U.S. federal government due to its practice of plural marriage, including disincorporation, confiscation of property, and imprisonment of leaders, jeopardizing its ability to function as a religious institution.
% FOUNDING_PROBLEM_CORROBORATION: The Church institution attests that the problem of federal persecution is dead due to the Manifesto. Historians and legal scholars outside the benefiting parties corroborate that the immediate threat of federal disincorporation and property confiscation was resolved by the Manifesto, though the underlying tension between religious freedom and secular law remains contested.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is moderate because while fundamentalists face severe costs, the mainstream membership experiences a net benefit from the Church's continued operation. Suppression (0.4) is present through excommunication and social pressure on dissenters, but not extreme, as the primary goal is coordination around a new norm rather than pure extraction. Theater ratio (0.1) is low, as the reinterpretation is genuinely believed by its adherents, and the Church actively works to integrate it into its doctrine and practice. The accessibility collapse (0.6) is moderate; while the official practice of plural marriage ceased, the doctrine was retained, allowing for future reinterpretation or continued belief among some.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Church institution and mainstream members, the Manifesto is a legitimate, divinely inspired act of coordination. From the perspective of fundamentalist groups, it is an illegitimate abandonment of eternal principles, leading to their persecution. The engine's classification will reflect this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The Church institution and mainstream members are beneficiaries (d near 0.0) as they gain legal and social legitimacy, ensuring the continuation of their religious practices. Fundamentalist groups are targets (d near 1.0) as they are excommunicated and ostracized for adhering to the prior interpretation. The federal government is an observer, its coercive pressure having been the catalyst for the reinterpretation, but not directly benefiting from the internal theological framing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_vs_institutional_survival,
    'To what extent was the 1890 Manifesto a genuine prophetic reinterpretation of divine will, versus a strategic institutional adaptation to ensure survival under federal pressure?',
    'Analysis of internal Church records, prophetic statements, and contemporary accounts for evidence of internal theological development preceding external pressure, or explicit acknowledgment of pragmatic motivations.',
    'If primarily pragmatic, the ''endogenous reinterpretation'' reading''s legitimacy is weakened, shifting its classification towards a more extractive or pragmatic type, and strengthening the ''institutional pragmatism'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_will_vs_institutional_survival, conceptual, 'Ambiguity between theological justification and institutional pragmatism.').

omega_variable(
    doctrinal_suspension_vs_abandonment,
    'Was plural marriage truly ''suspended'' with the possibility of future reinstatement, or was the 1890 Manifesto an effective ''abandonment'' of the doctrine?',
    'Future Church policy or explicit doctrinal statements regarding plural marriage. Examination of how the doctrine is taught and understood in contemporary Church discourse.',
    'If effectively abandoned, the ''endogenous reinterpretation'' reading''s claim of doctrinal continuity is weakened, potentially increasing its perceived extractiveness from those who maintained the practice based on its ''eternal'' nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_suspension_vs_abandonment, empirical, 'Ambiguity regarding the permanence of the doctrinal change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.05).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1894, 0.07).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1898, 0.08).
narrative_ontology:measurement(plur_tr_t1901, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1901, 0.09).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.1).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.2).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1894, 0.22).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1898, 0.23).
narrative_ontology:measurement(plur_be_t1901, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1901, 0.24).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.3).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1894, 0.33).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1898, 0.36).
narrative_ontology:measurement(plur_su_t1901, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1901, 0.38).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'plural_marriage_mandate' kernel, focusing on endogenous prophetic reinterpretation. It coexists with 'exogenous_override_reading' and 'institutional_pragmatism_reading', which offer alternative interpretations of the Manifesto's origins and nature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
