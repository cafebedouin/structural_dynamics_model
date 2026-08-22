% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: Fourth Amendment Warrant Requirement for 702 Queries (Constitutional Floor Reading)
 *   domain: constitutional/law_enforcement/surveillance
 *
 * SUMMARY:
 *   This constraint story instantiates the constitutional floor reading of
 *   the FISA Section 702 statutory text. The reading holds that regardless of
 *   how the statute is interpreted — whether collection targets foreigners
 *   abroad, whether incidental collection is minimized — any government query
 *   of a U.S. person's communications content from the 702 database
 *   constitutes a Fourth Amendment search requiring an individualized
 *   probable cause warrant. This reframes 702 from a foreign intelligence
 *   authorization statute into a criminal procedure question: the
 *   government's retention and querying of U.S. person communications without
 *   a warrant violates the constitutional floor. The executive branch's
 *   preference for speed and secrecy in intelligence operations faces a
 *   compliance cost (ε≈0.25) measured not in operational efficiency but in
 *   constitutional process. The FISA Court must transform from a programmatic
 *   authorization body into an individualized warrant-review tribunal.
 *
 * KEY AGENTS:
 *   - us_persons: Primary beneficiary (moderate/constrained) — constitutional protection against warrantless search of communications
 *   - executive_branch_intelligence_agencies: Primary payer (institutional/mobile) — bears compliance costs, delay, institutional redesign
 *   - fisa_court: Secondary payer (institutional/constrained) — absorbs individualized review burden, structural transformation
 *   - prosecutors_using_702_evidence: Payer (organized/constrained) — loses parallel construction pathway, evidence exclusion risk
 *   - privacy_advocates: Beneficiary (organized/analytical) — doctrinal vindication, litigation leverage
 *   - defense_bar: Beneficiary (moderate/analytical) — suppression motions, Fourth Amendment litigation tool
 *   - foreign_intelligence_targets: Excluded (powerless/trapped) — not U.S. persons, outside Fourth Amendment protection but structurally affected by collection limits
 *   - congress: Observer (institutional/analytical) — statutory reauthorization authority, oversight role
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
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Warrant Requirement for 702 Queries (Constitutional Floor Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional/law_enforcement/surveillance").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '96ca28b1-f589-4224-bc4f-9da2432de27c').
narrative_ontology:cs_kernel_codification('96ca28b1-f589-4224-bc4f-9da2432de27c', formalized).
narrative_ontology:cs_authority_grounding('96ca28b1-f589-4224-bc4f-9da2432de27c', lineage).
narrative_ontology:cs_interpretation_layer_present('96ca28b1-f589-4224-bc4f-9da2432de27c').
narrative_ontology:cs_reading_relation('96ca28b1-f589-4224-bc4f-9da2432de27c', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_reading_relation('96ca28b1-f589-4224-bc4f-9da2432de27c', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_axiom('96ca28b1-f589-4224-bc4f-9da2432de27c', foundational, fourth_amendment_applies_to_database_queries).
narrative_ontology:cs_axiom_status(fourth_amendment_applies_to_database_queries, holdable).
narrative_ontology:cs_axiom_grounding('96ca28b1-f589-4224-bc4f-9da2432de27c', fourth_amendment_applies_to_database_queries, deontological).
narrative_ontology:cs_axiom('96ca28b1-f589-4224-bc4f-9da2432de27c', foundational, us_person_status_triggers_warrant_requirement).
narrative_ontology:cs_axiom_status(us_person_status_triggers_warrant_requirement, holdable).
narrative_ontology:cs_axiom_grounding('96ca28b1-f589-4224-bc4f-9da2432de27c', us_person_status_triggers_warrant_requirement, deontological).
narrative_ontology:cs_axiom('96ca28b1-f589-4224-bc4f-9da2432de27c', secondary, collection_legitimacy_does_not_cure_query_search).
narrative_ontology:cs_axiom_status(collection_legitimacy_does_not_cure_query_search, holdable).
narrative_ontology:cs_axiom_grounding('96ca28b1-f589-4224-bc4f-9da2432de27c', collection_legitimacy_does_not_cure_query_search, deontological).
narrative_ontology:cs_reference_frame('96ca28b1-f589-4224-bc4f-9da2432de27c', fisa_702_enactment_2008).
narrative_ontology:cs_drift_state('96ca28b1-f589-4224-bc4f-9da2432de27c', post_carpenter_2018, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('96ca28b1-f589-4224-bc4f-9da2432de27c', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, privacy_advocates).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, defense_bar).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_intelligence_agencies).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, fisa_court).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, prosecutors_using_702_evidence).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_search_doctrine).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, probable_cause_warrant_requirement).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, constitutional_supremacy_over_statute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% U.S. persons whose communications are incidentally collected under 702. They gain Fourth Amendment protection against warrantless query of their communications content. Their exit is constrained: they cannot avoid incidental collection when communicating with foreign targets, but the warrant requirement gives them a legal remedy if queried.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons, beneficiary,
    moderate, biographical, constrained, national).

% NSA, CIA, FBI components that operate 702 collection and querying. They bear the compliance costs: individualized warrant applications for each U.S. person query, delay in time-sensitive investigations, risk of evidence exclusion, and potential need to redesign query architecture. They have mobile exit at the institutional level (can shift methods, seek statutory amendment) but are the primary target of the constraint's extractiveness.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_intelligence_agencies, payer,
    institutional, generational, mobile, global).

% The Foreign Intelligence Surveillance Court must transform from programmatic authorization (approving annual certifications) to individualized probable cause review for each U.S. person query. This imposes massive docket burden, requires new procedures and staffing, and fundamentally changes the court's institutional role. It sets the agenda for how warrants are reviewed but bears the operational cost.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter).

% Federal prosecutors who use 702-derived evidence in criminal cases. They lose the ability to query the 702 database without a warrant for U.S. person communications, and face suppression motions when evidence was obtained without individualized probable cause. Their exit is constrained: they can use traditional Title I FISA warrants or criminal wiretaps, but those have higher thresholds.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, prosecutors_using_702_evidence, payer,
    organized, biographical, constrained, national).

% Civil liberties organizations (ACLU, EFF, etc.) that litigate 702 constitutionality. They gain doctrinal vindication and litigation leverage if the constitutional floor reading prevails. Their role is analytical — they observe and challenge but do not directly bear costs or collect gains from the constraint's operation.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, privacy_advocates, beneficiary,
    organized, generational, analytical, national).

% Criminal defense attorneys representing defendants charged with 702-derived evidence. They gain Fourth Amendment suppression motions as a litigation tool. Their role is analytical — they use the constraint but do not administer or fund it.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, defense_bar, beneficiary,
    moderate, biographical, analytical, national).

% Non-U.S. persons abroad who are the legitimate targets of 702 collection. They are excluded from Fourth Amendment protection but structurally affected by any collection limits the constitutional floor reading implies (e.g., if warrant requirement reduces overall collection capacity). They have no voice in U.S. constitutional adjudication.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, foreign_intelligence_targets, excluded,
    powerless, biographical, trapped, global).

% Congress holds reauthorization authority over Section 702 and conducts oversight. It observes the constitutional debate, receives executive branch and FISC reports, and can amend the statute. Its role is analytical — it sets the statutory framework but does not directly operate the constraint.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Fourth Amendment protection for U.S. persons against suspicionless government search of their communications content, regardless of how the communications were collected. Solves the problem of executive branch using foreign intelligence authority as an end-run around criminal procedure protections.
% TRANSFER_FUNCTION: Moves compliance costs (warrant preparation, judicial review time, evidence exclusion risk, architectural redesign) from U.S. persons (who would bear the privacy harm) to the executive branch intelligence agencies and FISA Court. The transfer is constitutional process substituted for executive convenience.
% ABSENT_VOICES: Foreign intelligence targets (non-U.S. persons abroad) are structurally excluded from Fourth Amendment protection and have no voice in U.S. courts. They would object to any reduction in collection effectiveness that the warrant requirement might cause, but they are outside the constitutional framework. Also absent: future U.S. persons whose communications will be incidentally collected — they cannot yet object but the constraint protects them prospectively.
% DISAPPEARANCE_RATIONALE: If the warrant requirement for 702 queries disappeared overnight, the executive branch would resume warrantless querying of U.S. person communications in the 702 database. The FISA Court would revert to programmatic authorization. Prosecutors would regain parallel construction pathways. U.S. persons would lose the constitutional floor protection. The intelligence-surveillance architecture would reorganize around unrestricted query access.
% FOUNDING_PROBLEM: The post-9/11 foreign intelligence surveillance framework (FISA Amendments Act 2008, Section 702) authorized collection targeting non-U.S. persons abroad but created a database accessible for warrantless query of U.S. person communications. The founding problem was foreign intelligence collection against foreign targets; the constitutional problem (warrantless search of U.S. persons) emerged as a structural byproduct.
% FOUNDING_PROBLEM_CORROBORATION: The foreign intelligence collection problem remains live (corroborated by ongoing executive branch threat assessments, congressional reauthorization debates, and independent national security analyses). The constitutional problem is contested: the executive branch and FISA Court (beneficiaries of the current arrangement) argue minimization procedures solve it; privacy advocates, defense bar, and multiple district court judges (outside the benefiting parties) argue the warrant requirement remains unsatisfied.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).
:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.25) reflects the constitutional compliance cost imposed on executive intelligence operations — delay, individualized review, exclusion risk — not rent extraction. The constraint coordinates Fourth Amendment protection for U.S. persons against suspicionless surveillance (genuine coordination function) while extracting compliance costs from the intelligence apparatus (asymmetric extraction = tangled_rope). Suppression (0.35) is moderate: the constraint suppresses warrantless query practices but does not suppress the intelligence mission itself; alternatives (warrant-based queries, traditional FISA Title I) remain available. Theater ratio (0.15) is low: FISC proceedings are genuine (though secret), not performative. Accessibility collapse (0.45) and resistance (0.55) reflect that the constitutional claim is contested — the executive branch resists, courts have not fully adopted this reading, alternatives (statutory minimization, foreign-target framing) persist.
 *
 * PERSPECTIVAL GAP:
 *   From the executive branch seat, the constraint appears as operational degradation — a coordination function (national security) degraded by constitutional process. From the U.S. person seat, it appears as rights vindication — a coordination function (constitutional protection) enforced against state power. The engine computes this divergence from the declared beneficiary/victim structure and exit options. The constitutional floor reading's claim (tangled_rope) acknowledges both the coordination and extraction; the sibling readings would claim rope (foreign_target_strict) or snare (incidental_collection) depending on their structural framing.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons are structural beneficiaries (d near 0.0): the constraint subsidizes their Fourth Amendment rights. Executive intelligence agencies are structural targets (d near 1.0): they bear the compliance costs, institutional friction, and operational delay. The FISA Court sits at moderate-high d: it absorbs the review burden but gains institutional legitimacy. Prosecutors using 702 evidence are targets (d ~0.7): they lose the parallel-construction pathway. Privacy advocates and defense bar are beneficiaries (d ~0.1): they gain doctrinal tools. Foreign intelligence targets are excluded from the constraint's protection (not U.S. persons) but structurally affected by any collection limits the constraint implies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (foreign intelligence collection against non-U.S. persons abroad) remains live. The constitutional floor reading does not declare the problem dead — it declares the statutory solution constitutionally insufficient for U.S. person communications. Mandatrophy is not resolved; the constraint persists because the constitutional problem (warrantless search of U.S. persons) persists, not because the foreign intelligence problem is gone. The arrangement's persistence is driven by ongoing constitutional violation, not institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the constitutional floor reading instantiate a distinct constraint from its sibling readings of the FISA 702 statutory text, or is it merely an interpretive variant?',
    'Structural comparison of beneficiary/victim sets, extractiveness referents, and coordination functions across the three readings. If ε values differ by >0.15 or structural relationships invert, they are distinct constraints.',
    'If distinct, each reading gets its own constraint story with ε-invariance; if variants, they collapse to one story with parameterized interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether constitutional_floor_reading, foreign_target_strict_reading, and incidental_collection_reading are separate constraints or one constraint with interpretive parameters.').

omega_variable(
    search_classification_boundary,
    'Does a database query of already-collected communications constitute a new Fourth Amendment search, or is the search completed at collection?',
    'Supreme Court precedent on digital search (Carpenter, Riley) applied to 702 architecture; lower court rulings on query-as-search.',
    'If query = search, warrant requirement applies at query time (this reading''s structure). If collection = search, query is post-hoc access and constitutional floor reading overstates constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(search_classification_boundary, conceptual, 'Whether 702 queries are searches triggering warrant requirements independent of collection legitimacy.').

omega_variable(
    foreign_domestic_distinction_collapse,
    'Does the foreign/domestic distinction survive as a structural limit on executive surveillance power under a constitutional floor reading?',
    'Track whether courts apply the distinction to limit warrant requirements, or treat U.S. person status as the sole Fourth Amendment trigger regardless of collection context.',
    'If distinction collapses, constitutional floor reading''s extractiveness profile shifts toward higher compliance costs for executive; if distinction holds, foreign_target_strict_reading captures the residual coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreign_domestic_distinction_collapse, empirical, 'Whether the foreign/domestic targeting distinction retains operational force under constitutional floor framing.').

omega_variable(
    fisa_court_capacity_constraint,
    'Can the FISA Court conduct individualized probable cause review for 702 query volumes without structural transformation?',
    'Compare historical FISC opinion rates, staffing, and statutory deadlines against projected query volumes under warrant requirement.',
    'If capacity is inadequate, the constraint forces either dramatic query reduction (high extractiveness on intelligence function) or institutional redesign (new coordination costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fisa_court_capacity_constraint, empirical, 'Whether FISA Court institutional capacity can absorb the warrant review load this reading imposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 2008, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2008, 0.08).
narrative_ontology:measurement(fisa_tr_t2013, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2013, 0.12).
narrative_ontology:measurement(fisa_tr_t2018, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(fisa_tr_t2023, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2008, 0.15).
narrative_ontology:measurement(fisa_be_t2013, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2013, 0.22).
narrative_ontology:measurement(fisa_be_t2018, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2018, 0.25).
narrative_ontology:measurement(fisa_be_t2023, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2023, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2008, 0.25).
narrative_ontology:measurement(fisa_su_t2013, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2013, 0.32).
narrative_ontology:measurement(fisa_su_t2018, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement(fisa_su_t2023, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2023, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__constitutional_floor_reading, 0.12).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_title_i_warrant_process).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, carpenter_cell_site_location_doctrine).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, riley_cell_phone_search_doctrine).

% DUAL FORMULATION NOTE:
% The fisa_702_statutory_text kernel decomposes into three constraint stories: constitutional_floor_reading (this story, ε≈0.25, tangled_rope), foreign_target_strict_reading (ε≈0.08, rope — statutory minimization as coordination), and incidental_collection_reading (ε≈0.65, snare — warrantless query as extraction). The constitutional floor reading influences both siblings by raising the constitutional floor they must clear; the foreign target reading coexists as a statutory coordination layer; the incidental collection reading is pressured toward foreclosure if the constitutional floor reading prevails.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__constitutional_floor_reading, institutional, 0.15).
constraint_indexing:directionality_override(fisa_702_statutory_text__constitutional_floor_reading, moderate, 0.1).
constraint_indexing:directionality_override(fisa_702_statutory_text__constitutional_floor_reading, organized, 0.1).
constraint_indexing:directionality_override(fisa_702_statutory_text__constitutional_floor_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
