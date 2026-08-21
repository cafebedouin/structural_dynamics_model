% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Discernment in Catholic Practice
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint describes the 'civic pastoral' reading of the Catholic
 *   sacrament of marriage, emphasizing compassionate discernment in
 *   individual cases of marital failure, rather than strict adherence to
 *   indissolubility as an absolute rule. This approach, prominent in recent
 *   pontificates, aims to integrate pastoral care with doctrinal ideals. The
 *   constraint is claimed as a Tangled Rope because it coordinates pastoral
 *   support for many while extracting from traditional Catholics who perceive
 *   a loss of doctrinal clarity and stability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.45).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.35).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Discernment in Catholic Practice").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '8d434b64-ed6e-4acd-9106-3870042c5c68').
narrative_ontology:cs_kernel_codification('8d434b64-ed6e-4acd-9106-3870042c5c68', fixed_text).
narrative_ontology:cs_authority_grounding('8d434b64-ed6e-4acd-9106-3870042c5c68', lineage).
narrative_ontology:cs_interpretation_layer_present('8d434b64-ed6e-4acd-9106-3870042c5c68').
narrative_ontology:cs_reading_relation('8d434b64-ed6e-4acd-9106-3870042c5c68', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('8d434b64-ed6e-4acd-9106-3870042c5c68', foundational, indissolubility_as_ideal_requiring_discernment).
narrative_ontology:cs_axiom_status(indissolubility_as_ideal_requiring_discernment, holdable).
narrative_ontology:cs_axiom_grounding('8d434b64-ed6e-4acd-9106-3870042c5c68', indissolubility_as_ideal_requiring_discernment, deontological).
narrative_ontology:cs_axiom('8d434b64-ed6e-4acd-9106-3870042c5c68', secondary, pastoral_care_for_human_failure_is_paramount).
narrative_ontology:cs_axiom_status(pastoral_care_for_human_failure_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('8d434b64-ed6e-4acd-9106-3870042c5c68', pastoral_care_for_human_failure_is_paramount, deontological).
narrative_ontology:cs_reference_frame('8d434b64-ed6e-4acd-9106-3870042c5c68', pastoral_discernment_framework).
narrative_ontology:cs_drift_state('8d434b64-ed6e-4acd-9106-3870042c5c68', contemporary_church_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8d434b64-ed6e-4acd-9106-3870042c5c68', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, laity_seeking_discernment).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_clergy).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals in complex marital situations (e.g., divorced and remarried) who seek pastoral guidance and a path to fuller participation in the Church, benefiting from a more compassionate and individualized approach.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, laity_seeking_discernment, beneficiary,
    moderate, biographical, constrained, global).

% Priests and bishops tasked with implementing the Church's pastoral approach to marriage, balancing doctrinal ideals with the need for mercy and individual accompaniment. They administer the discernment process.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_clergy, agenda_setter,
    institutional, biographical, constrained, global).

% Members of the laity who adhere strictly to traditional interpretations of marriage indissolubility, experiencing a loss of normative clarity and doctrinal relativization due to the emphasis on discernment.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholics, payer,
    moderate, generational, identity_locked, global).

% Organized groups of theologians, clergy, and laity who actively resist changes to the traditional understanding of marriage, perceiving the discernment approach as undermining core Catholic doctrine and institutional authority.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives, payer,
    organized, generational, identity_locked, global).

% The teaching authority of the Church, including the Pope and bishops, who articulate and guide the pastoral approach, seeking to maintain unity while addressing contemporary challenges. They balance tradition with pastoral needs.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, institutional_magisterium, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% Academics, journalists, and other external analysts who study the Church's evolving stance on marriage from sociological, political, or theological perspectives, without direct participation in its internal structures.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, secular_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide compassionate pastoral care and guidance for individuals in complex marital situations, integrating the ideal of indissolubility with human realities and failures through a process of discernment.
% TRANSFER_FUNCTION: Transfers the burden of strict, universal adherence from individuals to a process of institutional discernment, while transferring a sense of doctrinal instability and erosion of clear norms to traditional adherents.
% ABSENT_VOICES: Those who have left the Church due to perceived rigidity or inconsistency, or those who advocate for a purely secular understanding of marriage, are not directly part of the internal discernment process but are impacted by its outcomes.
% DISAPPEARANCE_RATIONALE: If this pastoral discernment approach vanished overnight, the Church would revert to a more rigid, less compassionate stance on marriage, leading to significant pastoral crises, alienation of many faithful, and a loss of institutional relevance for many seeking spiritual guidance in complex lives.
% FOUNDING_PROBLEM: The perceived disconnect between the Church's absolute doctrine of marriage indissolubility and the lived realities of human failure, divorce, and remarriage among the faithful, leading to pastoral exclusion and suffering.
% FOUNDING_PROBLEM_CORROBORATION: Pastoral experience of clergy, surveys of Catholic faithful, and theological reflections from various schools of thought within the Church, often published in academic and pastoral journals, corroborate the ongoing nature of this problem, indicating a persistent tension between doctrine and lived experience.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.45) because while it offers pastoral benefits, it imposes costs on traditionalists through perceived doctrinal ambiguity. Suppression (0.35) is lower than a strict enforcement regime, reflecting the emphasis on individual discernment, but still present as the institutional process guides and limits individual agency. Theater ratio (0.40) is moderate, as the ideal of indissolubility is maintained rhetorically, but actual practice allows for more flexibility, creating a gap between stated ideal and lived reality. Resistance (0.55) is significant from traditionalist factions. The measurement series reflects a gradual increase in extractiveness and theatricality as this approach has gained prominence over the last decade.
 *
 * PERSPECTIVAL GAP:
 *   The pastoral clergy and laity seeking discernment experience this constraint as a compassionate and necessary evolution of Church practice, a 'Rope' that coordinates care. In contrast, traditional Catholics and doctrinal conservatives experience it as a 'Snare' or 'Tangled Rope' that undermines foundational doctrine and extracts their sense of certainty and tradition. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Laity seeking discernment and pastoral clergy are beneficiaries, as the constraint provides them with a framework for compassionate engagement. Traditional Catholics and doctrinal conservatives are victims, as they experience the 'extraction' of doctrinal stability and clarity. The institutional magisterium acts as an agenda-setter, navigating the tension between these groups. Directionality for beneficiaries is low (subsidized by the constraint), while for victims it is high (targeted by the constraint's effects).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_coherence_ambiguity,
    'Is the pastoral discernment approach truly consistent with traditional Catholic doctrine on marriage indissolubility, or does it implicitly undermine it?',
    'Further theological development and magisterial clarification, or a future pontificate''s re-emphasis on strict interpretation. Empirical observation of long-term effects on Catholic understanding of marriage.',
    'If inconsistent, the constraint''s perceived legitimacy among traditionalists would further erode, increasing resistance and extractiveness from their perspective. If coherent, it would strengthen the ''Rope'' aspect for beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_coherence_ambiguity, conceptual, 'Ambiguity regarding the doctrinal consistency of pastoral discernment.').

omega_variable(
    pastoral_effectiveness_measurement,
    'How can the actual pastoral benefits (e.g., reintegration of the faithful, reduced suffering) of this approach be quantitatively measured against the costs (e.g., doctrinal confusion, alienation of traditionalists)?',
    'Sociological studies of Catholic communities, surveys of clergy and laity, and long-term tracking of sacramental practice and participation rates.',
    'Clear evidence of widespread pastoral benefit would strengthen the coordination narrative. Evidence of significant negative impacts on doctrinal adherence or community cohesion would highlight the extractive aspects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pastoral_effectiveness_measurement, empirical, 'Measuring the net pastoral impact of the discernment approach.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditionalist views structural (e.g., official pronouncements, institutional pressure) or internalized (e.g., self-censorship, fear of marginalization)?',
    'Analysis of internal Church communications, interviews with traditionalist clergy and laity, and observation of public discourse within the Church.',
    'If largely internalized, the effective suppression is higher than structural measures suggest, as traditionalists carry the suppression with them. If structural, it points to direct institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of traditionalist perspectives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 2013, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t2013, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2013, 0.25).
narrative_ontology:measurement(marr_tr_t2015, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(marr_tr_t2017, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2017, 0.33).
narrative_ontology:measurement(marr_tr_t2019, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2019, 0.36).
narrative_ontology:measurement(marr_tr_t2021, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(marr_tr_t2023, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t2013, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2013, 0.35).
narrative_ontology:measurement(marr_be_t2015, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(marr_be_t2017, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2017, 0.4).
narrative_ontology:measurement(marr_be_t2019, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2019, 0.42).
narrative_ontology:measurement(marr_be_t2021, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2021, 0.44).
narrative_ontology:measurement(marr_be_t2023, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2023, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t2013, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2013, 0.3).
narrative_ontology:measurement(marr_su_t2015, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2015, 0.31).
narrative_ontology:measurement(marr_su_t2017, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2017, 0.32).
narrative_ontology:measurement(marr_su_t2019, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2019, 0.33).
narrative_ontology:measurement(marr_su_t2021, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2021, 0.34).
narrative_ontology:measurement(marr_su_t2023, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2023, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, attachment_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, sacramental_theology_interpretation).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, clerical_authority_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_sacrament' kernel, focusing on pastoral discernment. It is linked to the 'hierarchical_indissolubility_reading' which represents a more traditional, strict interpretation of the same kernel. Their differing ε values and stakeholder impacts necessitate separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
