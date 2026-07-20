% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__constitutional_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: Fourth Amendment Warrant Floor for Section 702 U.S. Person Content Queries
 *   domain: legal/constitutional/national_security
 *
 * SUMMARY:
 *   This constraint instantiates the constitutional_floor_reading of the FISA
 *   Section 702 statutory text kernel. It asserts that the Fourth Amendment
 *   requires an individualized probable cause warrant for any government
 *   search of U.S. person communications content, and that querying Section
 *   702-acquired databases for such content constitutes a search triggering
 *   this requirement regardless of whether the initial collection targeted a
 *   foreigner abroad. The reading reframes 702 not as a foreign intelligence
 *   statute but as a criminal procedure question governed by the Warrant
 *   Clause. Sibling readings (incidental_collection_reading,
 *   foreign_target_strict_reading) treat the constitutional and statutory
 *   boundaries differently; this reading treats the foreign/domestic
 *   distinction as irrelevant to the warrant requirement for U.S. person
 *   content queries.
 *
 * KEY AGENTS:
 *   - Executive Branch: Primary payer (institutional/constrained) â bears compliance burden and resists warrant expansion.
 *   - Intelligence Community: Secondary payer (institutional/constrained) â faces operational friction from warrant requirements.
 *   - U.S. Persons: Primary beneficiary (organized/constrained) â receive judicial gatekeeping protection.
 *   - Civil Liberties Bar: Secondary beneficiary (organized/mobile) â advocates for and is vindicated by this reading.
 *   - FISA Court: Agenda-setter (institutional/constrained) â would administer individualized warrant review.
 *   - Foreign Communicants: Excluded (powerless/trapped) â not protected by this reading's warrant floor.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.35).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, mountain).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Warrant Floor for Section 702 U.S. Person Content Queries").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "legal/constitutional/national_security").

domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '488e98c1-3f7f-432f-ac14-830b00abd17b').
narrative_ontology:cs_kernel_codification('488e98c1-3f7f-432f-ac14-830b00abd17b', fixed_text).
narrative_ontology:cs_authority_grounding('488e98c1-3f7f-432f-ac14-830b00abd17b', lineage).
narrative_ontology:cs_interpretation_layer_present('488e98c1-3f7f-432f-ac14-830b00abd17b').
narrative_ontology:cs_reading_relation('488e98c1-3f7f-432f-ac14-830b00abd17b', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('488e98c1-3f7f-432f-ac14-830b00abd17b', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_axiom('488e98c1-3f7f-432f-ac14-830b00abd17b', foundational, content_queries_are_searches).
narrative_ontology:cs_axiom_status(content_queries_are_searches, holdable).
narrative_ontology:cs_axiom_grounding('488e98c1-3f7f-432f-ac14-830b00abd17b', content_queries_are_searches, empirically_contingent).
narrative_ontology:cs_axiom('488e98c1-3f7f-432f-ac14-830b00abd17b', foundational, warrant_requirement_non_negotiable).
narrative_ontology:cs_axiom_status(warrant_requirement_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('488e98c1-3f7f-432f-ac14-830b00abd17b', warrant_requirement_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('488e98c1-3f7f-432f-ac14-830b00abd17b', warrant_preference_constitutional_default).
narrative_ontology:cs_drift_state('488e98c1-3f7f-432f-ac14-830b00abd17b', post_fisa_amendments_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('488e98c1-3f7f-432f-ac14-830b00abd17b', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, u_s_persons).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_bar).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_branch).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, intelligence_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs foreign intelligence surveillance operations and resists expanding individualized warrant requirements to Section 702 database queries, arguing that foreign-targeting frameworks and minimization procedures satisfy constitutional demands. Would bear the operational and legal burden of obtaining warrants for U.S. person content queries if this reading prevails.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_branch, payer,
    institutional, generational, constrained, national).

% Conducts collection and querying under Section 702 authorities. Faces operational slowdown, increased legal documentation, and potential loss of intelligence access if required to obtain individualized probable cause warrants before querying acquired databases for U.S. person communications content.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, intelligence_community, payer,
    institutional, biographical, constrained, national).

% Communications content is subject to Section 702 database queries without individualized judicial approval under current practice. Would receive judicial gatekeeping and probable cause review before government access to their content if this constitutional reading is adopted. Generally lack notice of queries and face standing barriers to challenging surveillance.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, u_s_persons, beneficiary,
    organized, generational, constrained, national).

% Litigates and advocates for warrant requirements in foreign intelligence surveillance. Would be structurally vindicated if courts adopt the reading that the Fourth Amendment independently mandates warrants for U.S. person content queries. Provides legal representation, amicus briefing, and public advocacy in surveillance cases.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_bar, beneficiary,
    organized, generational, mobile, national).

% Currently conducts programmatic review of targeting and minimization procedures for Section 702 collection. Under this reading, would shift to individualized probable cause review for U.S. person content queries, fundamentally changing its role from programmatic overseer to particularized warrant issuer.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, constrained, national).

% Communicate with U.S. persons and are the nominal targets of Section 702 collection. Their privacy interests are not protected by this reading's warrant requirement, which attaches only to queries for U.S. person content. Have no institutional voice in U.S. constitutional interpretation or FISA proceedings.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, foreign_communicants, excluded,
    powerless, immediate, trapped, global).

narrative_ontology:fixing_cost_class(fisa_702_statutory_text__constitutional_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform judicial checkpoint before government access to U.S. person communications content, preventing arbitrary or politically motivated querying of acquired foreign intelligence databases.
% TRANSFER_FUNCTION: Transfers operational speed and administrative autonomy from the intelligence community to judicial oversight and individualized probable cause review; moves legal compliance burden to executive agencies and the FISA Court.
% ABSENT_VOICES: U.S. persons whose communications are queried but who receive no notice and lack standing to challenge; foreign communicants who generate the communications but are excluded from constitutional protections and statutory minimization debates.
% DISAPPEARANCE_RATIONALE: If the warrant requirement for U.S. person queries disappeared, the executive would query Section 702 databases without individualized judicial gatekeeping, the FISA Court's role would contract to programmatic compliance review, and the balance between foreign intelligence speed and domestic privacy would shift sharply toward operational efficiency.
% FOUNDING_PROBLEM: Government surveillance of citizens without judicial oversight, general warrants, and executive discretion to search private papers and communications without particularized suspicion.
% FOUNDING_PROBLEM_CORROBORATION: Historical record of pre-FISA warrantless surveillance abuses documented by the Church Committee corroborates the founding problem. The executive branch attests that minimization procedures and foreign targeting frameworks have solved it; civil liberties organizations and some FISA Court opinions attest that large-scale querying of incidentally collected U.S. person data revives the same abuse pattern.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, ExtMetricName, E),
    domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fisa_702_statutory_text__constitutional_floor_reading),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.25 because the constraint imposes real compliance costs on executive speed and secrecy preferences, though it is not a rent-extraction mechanism. Suppression is moderate (0.35) because the executive resists the reading through classification, statutory argument, and non-compliance postures. Theater ratio is low (0.15) because judicial oversight is largely functional, not performative. Accessibility collapse is high (0.90) because constitutional requirements collapse statutory alternatives that would permit warrantless search. Resistance is substantial (0.55) because the intelligence community and successive administrations have actively opposed expanding warrant requirements to 702 queries.
 *
 * PERSPECTIVAL GAP:
 *   The executive and intelligence community experience this constraint as operational extraction â delay, legal risk, and lost intelligence velocity. U.S. persons and the civil liberties bar experience it as a fixed protective floor. The engine computes this divergence from the structural data: both payer seats are institutional but constrained, while beneficiary seats are organized with limited exit, producing asymmetric directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive_branch and intelligence_community are declared victims because they bear the compliance and operational costs of the warrant requirement; their directionality sits toward the target end. U.S. persons and the civil liberties bar are declared beneficiaries because they receive the protective and vindicatory function of the constraint; their directionality sits toward the beneficiary end. The FISA Court is agenda_setter because it would administer the constraint, with symmetric directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The warrant requirement was built to solve general warrant abuse and unchecked executive surveillance. The executive claims this problem is dead â minimization procedures and foreign targeting are sufficient substitutes. Civil libertarians claim it is live, arguing that large-scale querying of incidentally collected U.S. person data constitutes the functional equivalent of general warrants. The classification as mountain claims the warrant requirement is a fixed constitutional floor; the metrics reveal substantial resistance, suggesting the floor is contested rather than self-enforcing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_floor_or_constructed_interest,
    'Is the Fourth Amendment warrant requirement for 702 queries a genuine fixed constitutional floor, or a constructed legal argument advanced by and benefiting identifiable civil liberties actors?',
    'Supreme Court adoption or rejection of this reading; longitudinal analysis of whether the constraint persists and protects independently of who advocates for it.',
    'If a constructed interest benefiting identifiable parties, the FSM signature reclassifies from mountain to tangled_rope or snare; if a genuine floor, mountain classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_floor_or_constructed_interest, conceptual, 'Natural-law versus constructed ambiguity for constitutional warrant floor').

omega_variable(
    query_as_fourth_amendment_search,
    'Does querying an already-collected database for U.S. person communications content constitute a ''search'' under the Fourth Amendment?',
    'Supreme Court ruling directly on 702 queries or on analogous government database-query scenarios.',
    'If querying is a search, the constraint''s extractiveness from executive operations rises and its protective function for U.S. persons is structurally grounded; if not, the constraint dissolves or reclassifies as inoperative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(query_as_fourth_amendment_search, empirical, 'Whether database queries trigger Fourth Amendment search doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa702_cf_tr_t0, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fisa702_cf_tr_t5, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(fisa702_cf_tr_t10, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(fisa702_cf_tr_t15, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(fisa702_cf_tr_t20, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 20, 0.18).

% Extraction over time
narrative_ontology:measurement(fisa702_cf_be_t0, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(fisa702_cf_be_t5, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(fisa702_cf_be_t10, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(fisa702_cf_be_t15, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 15, 0.23).
narrative_ontology:measurement(fisa702_cf_be_t20, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 20, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fisa_702_statutory_text__constitutional_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fisa_702_statutory_text kernel. The constitutional_floor_reading asserts a Fourth Amendment warrant requirement independent of statutory foreign-target framing; the incidental_collection_reading asserts statutory permission for warrantless query; the foreign_target_strict_reading asserts statutory constraint through foreign-targeting language. They share the same statutory kernel but emit different constraints with different epsilon values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
