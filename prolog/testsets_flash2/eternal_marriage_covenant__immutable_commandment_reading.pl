% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: D&C 132 as Immutable Divine Law (Polygamy Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents the reading of Doctrine and Covenants (D&C)
 *   Section 132 as an immutable, eternal divine law requiring polygamy for
 *   exaltation, particularly during the period of intense federal
 *   anti-polygamy pressure (1852-1890). From this perspective, the
 *   commandment is a 'mountain' of divine will, unchangeable by human decree,
 *   and adherence to it is paramount for spiritual salvation, even if it
 *   means civil disobedience and martyrdom. The federal pressure creates a
 *   'martyrdom constraint' where compliance with divine law means apostasy
 *   from civil law, and vice-versa. There is no legitimate revision path
 *   within this reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.9).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.95).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, mountain).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "D&C 132 as Immutable Divine Law (Polygamy Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).
domain_priors:emerges_naturally(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, 'c8a2d264-d187-4a90-81a6-df8515519b90').
narrative_ontology:cs_kernel_codification('c8a2d264-d187-4a90-81a6-df8515519b90', fixed_text).
narrative_ontology:cs_authority_grounding('c8a2d264-d187-4a90-81a6-df8515519b90', lineage).
narrative_ontology:cs_interpretation_layer_present('c8a2d264-d187-4a90-81a6-df8515519b90').
narrative_ontology:cs_reading_relation('c8a2d264-d187-4a90-81a6-df8515519b90', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('c8a2d264-d187-4a90-81a6-df8515519b90', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('c8a2d264-d187-4a90-81a6-df8515519b90', foundational, divine_commandment_is_immutable).
narrative_ontology:cs_axiom_status(divine_commandment_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('c8a2d264-d187-4a90-81a6-df8515519b90', divine_commandment_is_immutable, theological).
narrative_ontology:cs_axiom('c8a2d264-d187-4a90-81a6-df8515519b90', foundational, plural_marriage_is_essential_for_exaltation).
narrative_ontology:cs_axiom_status(plural_marriage_is_essential_for_exaltation, holdable).
narrative_ontology:cs_axiom_grounding('c8a2d264-d187-4a90-81a6-df8515519b90', plural_marriage_is_essential_for_exaltation, theological).
narrative_ontology:cs_reference_frame('c8a2d264-d187-4a90-81a6-df8515519b90', original_divine_mandate).
narrative_ontology:cs_drift_state('c8a2d264-d187-4a90-81a6-df8515519b90', federal_anti_polygamy_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('c8a2d264-d187-4a90-81a6-df8515519b90', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, faithful_adherents_of_immutable_commandment_reading).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, members_facing_federal_prosecution).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, women_in_polygamous_marriages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Believe D&C 132 is an eternal, unchangeable divine law, essential for exaltation. Their spiritual salvation depends on adherence, even if it means civil disobedience and persecution. They gain eternal blessings and a sense of divine favor through this commitment.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, faithful_adherents_of_immutable_commandment_reading, beneficiary,
    powerless, generational, identity_locked, local).

% Bear the direct legal and social costs of adhering to the immutable commandment, including fines, imprisonment, and social ostracization. Their commitment to the divine law places them in direct conflict with secular law, with severe personal consequences.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, members_facing_federal_prosecution, payer,
    powerless, immediate, trapped, local).

% Are bound by the immutable commandment to participate in polygamous unions, often with limited autonomy or social support outside the community. Their identity and eternal prospects are tied to this practice, making exit extremely difficult and costly.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, women_in_polygamous_marriages, payer,
    powerless, biographical, identity_locked, local).

% Interprets and enforces D&C 132 as an immutable divine law. They guide adherents in maintaining the practice, even in the face of external pressure, seeing themselves as stewards of eternal truth. Their authority is derived from upholding this divine mandate.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, church_leadership_of_immutable_commandment_reading, agenda_setter,
    institutional, generational, constrained, national).

% Enforces anti-polygamy laws, viewing the practice as a violation of civil statutes and human rights. They exert legal and political pressure to suppress the practice, creating a direct conflict with the religious adherents.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_government, agenda_setter,
    institutional, generational, mobile, national).

% Observes the conflict between religious practice and civil law, generally supporting the federal government's stance against polygamy. They view the practice as outdated or harmful, reinforcing external pressure on adherents.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, secular_society, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual and social lives of adherents around a divinely ordained family structure, ensuring adherence to a path believed to lead to eternal exaltation and the continuation of a specific religious lineage.
% TRANSFER_FUNCTION: Transfers spiritual authority and eternal blessings to adherents who comply with the immutable commandment, while transferring legal and social penalties from the federal government to those who practice polygamy.
% ABSENT_VOICES: Former members who left the faith due to the demands of polygamy or the conflict with secular law; they would argue for individual autonomy and the right to choose marital arrangements free from coercive religious or legal pressure.
% DISAPPEARANCE_RATIONALE: If the immutable commandment reading of D&C 132 vanished, the entire social and spiritual structure of the adherent communities would collapse. Marital arrangements would dissolve, eternal salvation narratives would be invalidated, and the community would face an existential crisis, forcing a complete reorganization of their religious and social life.
% FOUNDING_PROBLEM: The problem of ensuring eternal family units and the highest degree of exaltation in the afterlife, which was believed to require plural marriage as a divine ordinance.
% FOUNDING_PROBLEM_CORROBORATION: Adherents of this reading attest that the problem of eternal exaltation and family structure remains live and central to their faith. External observers, including historians and sociologists, corroborate that this was indeed the founding problem, though they may dispute its divine origin or contemporary relevance.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, ExtMetricName, E),
    domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(eternal_marriage_covenant__immutable_commandment_reading),
    narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because adherence demands significant personal sacrifice, legal penalties, and social ostracization. Suppression is extremely high due to the identity-locked nature of the commitment (eternal salvation at stake) and the severe external legal enforcement. The theater ratio is low because the practice is genuinely believed and actively pursued, not merely performed. The claimed type is 'mountain' because, from this reading, the law is divinely ordained and immutable, an irreducible spiritual limit. The increasing extractiveness and suppression over the interval reflect the escalating federal pressure and the hardening of resolve among adherents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adherents to this reading, the constraint is a divine mountain, a path to eternal life. From the federal government's perspective, it is a snare of religious coercion and civil disobedience. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Faithful adherents are beneficiaries in terms of eternal salvation but payers in terms of earthly costs. Church leadership, while setting the agenda, also faces persecution alongside their flock, but their authority is reinforced by upholding the 'immutable' law. The federal government is an external agenda-setter, imposing costs on adherents. Women in polygamous marriages are significant payers, often with limited agency and identity-locked exit options.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_immutability_vs_human_interpretation,
    'Is D&C 132 truly an immutable divine law, or is its interpretation as such a human construction that benefits specific power structures within the religious community?',
    'Theological scholarship examining the historical context and interpretive traditions of D&C 132, alongside sociological analysis of power dynamics within the community. If alternative interpretations with less extractive outcomes are found to be equally textually grounded, the ''immutable'' claim weakens.',
    'If found to be a human construction, the constraint would reclassify from a ''mountain'' to a ''snare'' or ''tangled_rope'', reflecting its constructed and extractive nature. If genuinely immutable, the ''mountain'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_immutability_vs_human_interpretation, conceptual, 'Ambiguity between divine immutability and human interpretive construction.').

omega_variable(
    martyrdom_vs_coercion,
    'To what extent is adherence to polygamy under federal pressure a genuine act of religious martyrdom, versus a coerced practice sustained by identity-lock and lack of viable exit options?',
    'Longitudinal studies of ex-members'' experiences, examining the psychological and social costs of leaving, and the degree to which external support systems facilitate or hinder exit. If exit costs remain high even after external legal pressure subsides, it suggests internalized coercion.',
    'If primarily coercion, the ''suppression'' metric''s effective impact is higher, and the ''resistance'' metric might be re-evaluated as a form of ''trapped'' compliance rather than active defiance. This would strengthen a ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martyrdom_vs_coercion, empirical, 'Distinguishing genuine martyrdom from identity-locked coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 1852, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1852, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1852, 0.05).
narrative_ontology:measurement(eter_tr_t1865, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1865, 0.05).
narrative_ontology:measurement(eter_tr_t1878, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1878, 0.05).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1890, 0.05).

% Extraction over time
narrative_ontology:measurement(eter_be_t1852, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1852, 0.7).
narrative_ontology:measurement(eter_be_t1865, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1865, 0.8).
narrative_ontology:measurement(eter_be_t1878, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1878, 0.85).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1890, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1852, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1852, 0.7).
narrative_ontology:measurement(eter_su_t1865, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1865, 0.8).
narrative_ontology:measurement(eter_su_t1878, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1878, 0.9).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1890, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
