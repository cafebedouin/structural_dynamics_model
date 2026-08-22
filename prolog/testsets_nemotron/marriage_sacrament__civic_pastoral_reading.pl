% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Relationship: Compassionate Discernment Reading
 *   domain: religious/legal/social
 *
 * SUMMARY:
 *   This constraint story captures the 'civic pastoral reading' of the
 *   marriage sacrament kernel: indissolubility is upheld as an ideal, but
 *   pastoral discernment in individual cases — especially for divorced and
 *   civilly remarried Catholics — allows for communion access without formal
 *   annulment. The reading gained authoritative expression in Amoris Laetitia
 *   (2016) and subsequent episcopal implementations. It functions as a
 *   tangled rope: it coordinates a genuine pastoral need (accompanying the
 *   wounded) while extracting from those whose identity and institutional
 *   role depend on the normative clarity of indissolubility as constitutive
 *   law. The claimed type is tangled_rope; the metrics reflect moderate
 *   extractiveness rising from 2013-2016 then stabilising, moderate
 *   suppression as enforcement shifts from universal canon to local
 *   discretion, and low theater as the pastoral function is real but
 *   increasingly performs institutional management of dissent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.45).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.35).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Relationship: Compassionate Discernment Reading").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious/legal/social").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d').
narrative_ontology:cs_kernel_codification('16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d', formalized).
narrative_ontology:cs_authority_grounding('16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d', lineage).
narrative_ontology:cs_interpretation_layer_present('16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d').
narrative_ontology:cs_reading_relation('16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d', foundational, indissolubility_as_aspirational_ideal).
narrative_ontology:cs_axiom_status(indissolubility_as_aspirational_ideal, holdable).
narrative_ontology:cs_axiom_grounding('16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d', indissolubility_as_aspirational_ideal, deontological).
narrative_ontology:cs_axiom('16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d', foundational, pastoral_discernment_over_canonical_formalism).
narrative_ontology:cs_axiom_status(pastoral_discernment_over_canonical_formalism, holdable).
narrative_ontology:cs_axiom_grounding('16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d', pastoral_discernment_over_canonical_formalism, instrumental).
narrative_ontology:cs_reference_frame('16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d', tridentine_indissolubility_canon).
narrative_ontology:cs_drift_state('16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d', post_amoris_laetitia, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('16f641d2-9f2f-4fd5-a9e7-bd0e43cf7c1d', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_ministers).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, divorced_catholics_seeking_inclusion).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, local_ordinaries_exercising_discretion).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholics_doctrinal_stability).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, canonical_formalists).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, marriage_tribunal_personnel).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, pastoral_accompaniment_primacy).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, mercy_over_legalism).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, situationist_ethics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Priests, deacons, and lay ministers who administer marriage preparation, accompany couples in irregular situations, and implement diocesan discernment processes. They gain pastoral flexibility and authority to make case-by-case judgments, but remain bound by institutional structures and episcopal oversight. Their exit is constrained by vocation and canonical obedience.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_ministers, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, pastoral_ministers, beneficiary).

% Civilly divorced and remarried Catholics who seek full sacramental participation. They benefit from discernment pathways that may admit them to communion without formal annulment. Their identity is fused with Catholic belonging; exit means leaving the tradition that constitutes their self-understanding, making them identity-locked rather than merely constrained.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, divorced_catholics_seeking_inclusion, beneficiary,
    moderate, biographical, identity_locked, global).

% Laity and clergy for whom indissolubility is a constitutive doctrinal boundary, not an aspirational ideal. They experience doctrinal relativization as extraction: the normative clarity that structured their vocational and marital commitments is destabilized by pastoral exceptions. Their Catholic identity depends on doctrinal stability; exit would rupture their self-understanding and communal belonging, making them identity-locked.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholics_doctrinal_stability, payer,
    organized, generational, identity_locked, global).

% Tribunal judges, canon lawyers, and curial officials whose professional authority rests on consistent application of canonical norms. They bear the cost of institutional incoherence when pastoral discretion overrides formal process. Their exit is constrained by professional specialization and ecclesial office, but they are also excluded from the pastoral discernment conversation that displaces their expertise.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, canonical_formalists, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, canonical_formalists, excluded).

% Bishops and episcopal conferences who authorise and regulate pastoral discernment processes (e.g., the 'internal forum' solution, Amoris Laetitia footnote 352 implementation). They gain governance flexibility and political capital within the Church, with ability to calibrate enforcement to local conditions. Their exit options include moving toward stricter or looser application, giving them arbitrage-grade mobility within the institutional field.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, local_ordinaries_exercising_discretion, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, local_ordinaries_exercising_discretion, beneficiary).

% Judges, advocates, and staff of diocesan and interdiocesan marriage tribunals. Their institutional role and professional identity are built on the formal nullity process; pastoral pathways that bypass tribunals extract from their authority and caseload. Exit is constrained by canonical specialization and ecclesial employment.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, marriage_tribunal_personnel, payer,
    organized, biographical, constrained, global).

% Scholars of canon law, sacramental theology, and church history who analyse the tension between indissolubility as ontological claim and pastoral practice. They neither collect nor pay; they map the structural dynamics.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, theological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a pastoral framework for accompanying Catholics in irregular marital situations toward fuller ecclesial participation, balancing the ideal of indissolubility with the reality of human failure and the call to mercy.
% TRANSFER_FUNCTION: Moves normative authority from universal canonical criteria (formal nullity process) to local pastoral discretion (discernment of conscience, internal forum). Transfers the burden of proof from the petitioner (demonstrating nullity) to the minister (accompanying discernment). Transfers institutional coherence from doctrinal clarity to pastoral flexibility.
% ABSENT_VOICES: The separated-but-faithful spouse in a second union who does not seek communion but bears the social and spiritual cost of the first marriage's dissolution. Also: future generations catechised under a regime where indissolubility is presented as ideal rather than norm. These voices are structurally excluded from the synodal and curial conversations that shape pastoral norms.
% DISAPPEARANCE_RATIONALE: If the pastoral discernment reading vanished overnight, dioceses would revert to the universal nullity process as the sole pathway. Divorced and remarried Catholics would lose access to communion without formal annulment. Local ordinaries would lose discretionary authority. The ecclesial field would rearrange around a single, hierarchically enforced standard.
% FOUNDING_PROBLEM: The gap between the Church's doctrinal teaching on indissolubility and the lived reality of Catholics in failed marriages who seek communion. The rigid application of canonical norms produced pastoral alienation and mass departure from sacramental life.
% FOUNDING_PROBLEM_CORROBORATION: The 2014-2015 Synods on the Family and Amoris Laetitia (2016) document the founding problem from the pastoral side. Critics (e.g., the dubia cardinals, the 2017 Filial Correction, the 2024 Dignitas Infinita commentary on doctrinal stability) attest from outside the benefiting parties that the problem is contested: they argue the pastoral crisis was manufactured by catechetical failure, not by the norm itself, and that the solution relativises the norm.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).
:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the transfer of authority from universal criteria to local discretion: traditional Catholics lose the normative stability that structured their commitments; tribunal personnel lose institutional relevance. Suppression (0.35) is moderate: the constraint does not coerce assent but marginalises dissenting voices through pastoral language that frames opposition as 'rigidity' or 'lack of mercy'. Theater ratio (0.25) captures the gap between the proclaimed ideal (indissolubility) and the operational norm (discernment pathways that function as de facto dissolution). Accessibility collapse (0.4) is partial: alternatives (the nullity process, Eastern Orthodox practice, Protestant communion) exist but are costly or identity-rupturing. Resistance (0.55) is significant: organised traditionalist networks, curial pushback, and contested episcopal implementations demonstrate active contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the pastoral minister's seat, the constraint is a rope: it solves a real coordination problem (how to accompany the divorced and remarried) with minimal coercion. From the traditional Catholic's seat, it is a snare: the coordination story is cover for doctrinal relativisation that extracts their normative security. From the tribunal official's seat, it is a piton: a once-functional structure (the nullity process) degraded by pastoral bypass, maintained theatrically while its authority erodes. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Pastoral ministers and local ordinaries are structural beneficiaries (d ≈ 0.2): they gain discretionary authority and pastoral flexibility. Divorced Catholics seeking inclusion are beneficiaries with identity-locked exit (d ≈ 0.25): they gain communion access but cannot exit the tradition without self-rupture. Traditional Catholics and canonical formalists are payers with identity-locked or constrained exit (d ≈ 0.75-0.8): they bear the cost of doctrinal destabilisation and institutional incoherence. The directionality is asymmetric: the reading's coherence depends on extracting normative stability from one group to confer pastoral flexibility on another.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pastoral alienation of divorced Catholics) is real but contested as to cause and remedy. The arrangement was built to solve a pastoral crisis; the crisis persists, but the solution has created a new extraction structure. Mandatrophy is not resolved: the pastoral function is live, but the extraction from traditional Catholics and canonical formalists is structural, not incidental. The constraint persists because no party has the power to fix it: traditionalists cannot enforce universal norms; pastoralists cannot resolve the doctrinal tension without schism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this constraint represent a genuine pastoral development of doctrine, or a relativisation of the indissolubility norm that constitutes a different kernel reading?',
    'Magisterial clarification on whether Amoris Laetitia chapter 8 footnotes constitute authentic development or rupture; longitudinal study of whether pastoral pathways function as de facto dissolution.',
    'If genuine development, the constraint is a rope/scaffold coordinating mercy within indissolubility. If relativisation, it is a snare/tangled_rope extracting normative stability from traditional Catholics under pastoral cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the civic pastoral reading is continuous with the kernel or constitutes a distinct constraint.').

omega_variable(
    extraction_mechanism_ambiguity,
    'Is the extraction experienced by traditional Catholics structural (institutional incoherence, loss of canonical certainty) or internalised (scrupulosity, perceived betrayal by shepherds)?',
    'Post-implementation survey of traditional Catholic communities measuring doctrinal confidence, trust in hierarchy, and vocational stability over time; comparison with pre-2016 baselines.',
    'If internalised, effective suppression is higher than structural measure suggests — the target carries the extraction as identity wound. If structural, the extraction is bounded by institutional reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_mechanism_ambiguity, empirical, 'Structural vs internalised extraction mechanism for identity-locked traditional Catholics.').

omega_variable(
    discernment_coordination_boundary,
    'Where does compassionate discernment end and de facto marital dissolution begin? Is the pastoral pathway a coordination mechanism for inclusion or an extraction mechanism for institutional relevance?',
    'Comparative analysis of diocesan implementation norms: where discernment processes require firm purpose of amendment vs. where they function as automatic communion access. Track annulment rates vs. discernment admissions over time.',
    'If discernment requires genuine conversion of life, the coordination function is real. If it functions as rubber-stamp, the constraint is a snare using pastoral language to extract institutional coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discernment_coordination_boundary, conceptual, 'Whether the pastoral discernment process coordinates genuine inclusion or performs institutional management.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 2013, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t2013, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2013, 0.1).
narrative_ontology:measurement(marr_tr_t2015, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(marr_tr_t2016, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement(marr_tr_t2018, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2018, 0.24).
narrative_ontology:measurement(marr_tr_t2020, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(marr_tr_t2022, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2022, 0.25).
narrative_ontology:measurement(marr_tr_t2024, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(marr_be_t2013, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2013, 0.25).
narrative_ontology:measurement(marr_be_t2015, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(marr_be_t2016, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2016, 0.42).
narrative_ontology:measurement(marr_be_t2018, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2018, 0.44).
narrative_ontology:measurement(marr_be_t2020, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(marr_be_t2022, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2022, 0.45).
narrative_ontology:measurement(marr_be_t2024, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t2013, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2013, 0.15).
narrative_ontology:measurement(marr_su_t2015, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(marr_su_t2016, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2016, 0.3).
narrative_ontology:measurement(marr_su_t2018, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2018, 0.33).
narrative_ontology:measurement(marr_su_t2020, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2020, 0.35).
narrative_ontology:measurement(marr_su_t2022, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2022, 0.35).
narrative_ontology:measurement(marr_su_t2024, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__civic_pastoral_reading, 0.08).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, canonical_nullity_process).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, eucharistic_coherence_norm).

% DUAL FORMULATION NOTE:
% This constraint and hierarchical_indissolubility_reading form a kernel family decomposing the marriage_sacrament concept. This reading has ε=0.45 (moderate extraction from traditional Catholics via doctrinal relativisation); the sibling reading has ε≈0.1 (low extraction, high suppression of dissent). They are linked by network.affects_constraints because the pastoral reading's discretionary authority structurally undermines the universal norms the hierarchical reading depends on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__civic_pastoral_reading, institutional, 0.15).
constraint_indexing:directionality_override(marriage_sacrament__civic_pastoral_reading, organized, 0.8).
constraint_indexing:directionality_override(marriage_sacrament__civic_pastoral_reading, moderate, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
