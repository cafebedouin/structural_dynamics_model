% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_incidental_collection_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: FISA § 702 Incidental Collection Reading: Foreign Intelligence Justification for Warrantless U.S. Person Query
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   The Foreign Intelligence Surveillance Amendments Act of 2008 (FISA § 702)
 *   authorizes the government to target foreign persons reasonably believed
 *   to be located abroad for communications surveillance without a warrant.
 *   The statute's incidental-collection reading holds that when the
 *   government lawfully collects communications targeting a foreign person,
 *   any incidentally captured communications from or about U.S. persons may
 *   be retained and subsequently queried without a warrant, provided the
 *   query is justified by 'foreign intelligence purpose' and subject to
 *   internal minimization procedures. This reading treats the statutory
 *   foreign-targeting authorization as extending to incidental U.S. person
 *   data and treats administrative minimization as constitutionally adequate
 *   without warrant-before-search. The constraint is structurally a tangled
 *   rope: it coordinates legitimate foreign intelligence collection against
 *   genuinely foreign-targeted persons, while simultaneously extracting
 *   warrantless surveillance authority over U.S. persons who have no
 *   mechanism to opt out or to challenge the collection. The founding problem
 *   — post-9/11 operational friction from incidental data in foreign-person
 *   targeting — remains contested as to whether the reading adequately solves
 *   it or merely relabels a constitutional problem as an administrative one.
 *   This is one reading of the contested FISA § 702 kernel; sibling readings
 *   are the foreign-target-strict reading (incidental data must be genuinely
 *   minimized and inaccessible for domestic purposes) and the
 *   constitutional-floor reading (Fourth Amendment requires warrant for any
 *   government search of U.S. person communications, regardless of statutory
 *   authorization).
 *
 * KEY AGENTS:
 *   - Intelligence agencies (NSA, FBI, CIA): set the targeting and minimization procedures, operate the § 702 collection infrastructure, benefit directly from low-friction incidental-data query authority, operationally defend the incidental-collection reading as necessary for foreign intelligence effectiveness.
 *   - U.S. persons subject to incidental collection: bear the cost of warrantless surveillance, cannot exit (participation in international communications is unavoidable for many legitimate purposes), have no notice and no statutory remedy, structurally trapped by the powerless exit and the foreign-person-targeting justification that does not explicitly protect them.
 *   - Domestic investigative targets accessed via § 702: use FBI investigative leads derived from incidental queries without warrant, have constrained exit (Fourth Amendment protections constrained by the incidental-collection reading), moderate power but lack effective remedy because the initial collection is read as non-investigative.
 *   - Congress and FISA court: agenda-setters and beneficiaries by statutory delegation; Congress periodically reauthorizes the statute; FISA court reviews targeting procedures but not individual queries or incidental-retention scope.
 *   - Civil liberties advocates and defense bar: excluded by design from the foreign intelligence justification framework; no statutory standing to challenge targeting or minimization in FISA court.
 *   - Federal courts (criminal litigation): observers constrained by the statutory text and the foreign-intelligence-justification framework; rarely encounter § 702 challenges and have been reluctant to impose warrant requirements on foreign-intelligence-purpose queries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.72).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA § 702 Incidental Collection Reading: Foreign Intelligence Justification for Warrantless U.S. Person Query").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, 'ffb5d3a6-9807-4f08-96e7-05d1c2672431').
narrative_ontology:cs_kernel_codification('ffb5d3a6-9807-4f08-96e7-05d1c2672431', fixed_text).
narrative_ontology:cs_authority_grounding('ffb5d3a6-9807-4f08-96e7-05d1c2672431', extraction).
narrative_ontology:cs_interpretation_layer_present('ffb5d3a6-9807-4f08-96e7-05d1c2672431').
narrative_ontology:cs_reading_relation('ffb5d3a6-9807-4f08-96e7-05d1c2672431', fisa_702_statutory_text__foreign_target_strict_reading, forecloses).
narrative_ontology:cs_reading_relation('ffb5d3a6-9807-4f08-96e7-05d1c2672431', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('ffb5d3a6-9807-4f08-96e7-05d1c2672431', foundational, incidental_data_constitutionally_accessible_via_foreign_intelligence_justification).
narrative_ontology:cs_axiom_status(incidental_data_constitutionally_accessible_via_foreign_intelligence_justification, holdable).
narrative_ontology:cs_axiom_grounding('ffb5d3a6-9807-4f08-96e7-05d1c2672431', incidental_data_constitutionally_accessible_via_foreign_intelligence_justification, empirically_contingent).
narrative_ontology:cs_axiom('ffb5d3a6-9807-4f08-96e7-05d1c2672431', foundational, administrative_minimization_procedures_satisfy_fourth_amendment_safeguards).
narrative_ontology:cs_axiom_status(administrative_minimization_procedures_satisfy_fourth_amendment_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('ffb5d3a6-9807-4f08-96e7-05d1c2672431', administrative_minimization_procedures_satisfy_fourth_amendment_safeguards, deontological).
narrative_ontology:cs_reference_frame('ffb5d3a6-9807-4f08-96e7-05d1c2672431', statutory_foreign_targeting_authority_with_incidental_retention).
narrative_ontology:cs_drift_state('ffb5d3a6-9807-4f08-96e7-05d1c2672431', contemporary_fbi_incidental_query_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ffb5d3a6-9807-4f08-96e7-05d1c2672431', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_mission).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, u_s_persons_subject_to_incidental_collection).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, domestic_investigative_targets_accessed_via_702).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__incidental_collection_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__incidental_collection_reading_tests).
:- end_tests(fisa_702_statutory_text__incidental_collection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.45) because the constraint permits warrantless retention and query of U.S. person data, displacing the warrant requirement with a statutory foreign-intelligence-justification standard. This is extraction from rights-holders (U.S. persons) who cannot exit and have no meaningful remedial access. Suppression is substantial (0.72): the constraint's persistence depends on suppressing awareness that incidental data queried against domestic investigative targets functions as warrantless domestic surveillance, and on suppressing the Fourth Amendment remedy by treating administrative minimization as constitutionally adequate. Theater is moderate (0.41): the foreign-intelligence-purpose framing is partially sincere (foreign intelligence is genuinely pursued) but an increasing share of operational activity is the defense of the incidental-query authority against constitutional challenge, not the prosecution of foreign intelligence per se. The measurement trajectory shows modest growth in extractiveness and theater over the 2008-2026 interval, reflecting the accumulation of disclosed incidental-collection scale (NSA datasets grew to billions of records), the FBI's increasing use of § 702 queries for domestic investigations, and the rising gap between the statutory foreign-targeting justification and the operational deployment as a de facto domestic surveillance authority. The theater-ratio growth reflects the increasing role of public-facing minimization-procedure testimony and constitutional-compliance arguments relative to the actual incidental-query authorization growth.
 *
 * PERSPECTIVAL GAP:
 *   The intelligence-agency seat experiences the constraint as workable coordination: foreign targeting is a legitimate need, incidental data is an unavoidable byproduct, minimization procedures provide adequate constitutional safeguards, query authorization for foreign intelligence purpose is a reasonable administrative boundary. The constrained-U.S.-person seat experiences the constraint as warrantless surveillance: they did not choose to enter the constraint, cannot exit, have no notice, no access to remedy, and bear the Fourth Amendment cost. The domestic-investigative-target seat experiences the constraint as an end-run around the warrant requirement: their U.S. person status should trigger warrant protection, but the incidental-collection reading permits investigators to initiate and conduct investigations via § 702 database queries without warrant. The federal-court seat experiences the constraint as structurally deferential to foreign intelligence: when § 702 queries are disclosed in criminal cases, courts have been reluctant to impose warrant requirements on foreign-intelligence-purpose authority, treating statutory authorization as superseding Fourth Amendment doctrine in the foreign intelligence context. The engine computes these divergences from the structural data — the beneficiary and victim declarations, the power and exit options of each seat — without requiring the commentary to reconcile them. The divergence itself is what the framework measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Intelligence agencies are structural beneficiaries (d near 0.0): they set the rules, define the foreign intelligence justification, collect and retain the data, and profit from the low-friction query authority. They have substantial power and analytical exit options. U.S. persons subject to incidental collection are full targets (d near 1.0): they are powerless, identity-locked (cannot exit participation in international communications without fundamental life changes — family abroad, international business, professional networks), and bear the cost of warrantless surveillance. Domestic investigative targets accessed via § 702 sit between full-target (d ≈ 0.75): they are moderate power (can hire counsel, contest investigations in court) but face the functional extraction of warrant protection displaced by the foreign-intelligence justification. Congress and FISA court are complex: they are officially agenda-setters but structurally benefit from the constraint's operation (Congress gains surveillance authority without explicit appropriation of domestic surveillance power; FISA court gains relevance and deference by participating in foreign intelligence authorization). Their directionality is toward beneficiary (d ≈ 0.2-0.3) because they control the constraint and benefit from its operation, despite their nominal role as overseers. Civil liberties advocates are excluded (not in directionality set): they would have high d (near 1.0) as targets if included, but are structurally precluded from the foreign intelligence justification framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was operationally real: post-9/11 foreign-intelligence collection infrastructure generates incidental U.S. person communications unavoidably. The incidental-collection reading addresses this by permitting retention and query without warrant. However, the measured theater-ratio growth (0.25 → 0.41) and the rise in disclosed incidental-collection scale suggest the mandate has drifted: the constraint functions increasingly as a domestic surveillance authority accessed via the foreign intelligence justification, rather than as a coordinated solution to the operational problem of incidental data. Domestically targeted investigations are now initiated via § 702 queries, suggesting the constraint's primary function has shifted from managing foreign intelligence's incidental U.S. person problem to providing a warrant-less authority for FBI domestic investigations that happen to touch the foreign intelligence database. The constraint avoids pure-piton classification (theater_ratio < 0.5, still below the piton threshold of ~0.55-0.60) but sits in the amber zone: the coordination function has substantially atrophied, and the warrant-displacement function has become primary. A strong case can be made that this reading meets the mandatrophy criteria — the founding problem has been operationally solved (incidental data management is routine) but the constraint persists and grows because it serves a different function (domestic surveillance authority) that was not explicitly authorized. The contested founding-problem status reflects exactly this dispute: the intelligence community says the foreign-intelligence-targeting problem is live and § 702 solves it; critics say the founding problem is solved but the constraint now functions as extraction and should be sunset or reformulated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    administrative_minimization_as_fourth_amendment_substitute,
    'Does administrative minimization of incidental U.S. person data (internal agency procedures, not warrant-based) constitute a constitutionally adequate substitute for the warrant requirement, or does the Fourth Amendment mandate warrant-before-search regardless of administrative safeguards?',
    'Supreme Court direct ruling on § 702 constitutionality, or congressional amendment clarifying that minimization procedures satisfy or do not satisfy Fourth Amendment requirements. Empirical measures of minimization efficacy (audit findings, internal compliance rates, documented breaches) would inform but not resolve the constitutional question.',
    'If administrative minimization is constitutionally adequate, this reading persists as valid statutory interpretation. If the Court holds warrant requirement is nondelegable, the incidental-collection reading forecloses in favor of the constitutional-floor reading and § 702 queries of U.S. person data become unconstitutional absent warrant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(administrative_minimization_as_fourth_amendment_substitute, conceptual, 'Whether Fourth Amendment protections can be satisfied by agency policy rather than judicial warrant process.').

omega_variable(
    scope_of_foreign_intelligence_justification,
    'How broad is ''foreign intelligence purpose'' as deployed in § 702 query authorization? Can queries seeking information on U.S. persons engaged in domestic political activity qualify, if the predicate is that the target communicated with a foreign person?',
    'FISA court interpretive guidance on query approval standards, or declassified review of query authorization patterns showing scope of ''foreign intelligence purpose'' as applied. Congressional or judicial clarification of the statutory limits.',
    'A narrow reading confines incidental queries to genuine foreign intelligence; a broad reading permits the constraint to function as an de facto domestic surveillance authority. The measured extractiveness assumes broad deployment; narrowing the justification would reduce ε toward 0.25-0.30.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_foreign_intelligence_justification, empirical, 'Whether ''foreign intelligence purpose'' is genuinely limited or functions as a residual catch-all permitting domestic surveillance.').

omega_variable(
    incidental_vs_primary_investigative_intent_ambiguity,
    'When the government initiates an investigation of a U.S. person by querying the § 702 database (where the initial investigative interest IS the U.S. person), is the U.S. person''s data truly ''incidental'' in any meaningful sense, or has the constraint transformed incidental data into a primary investigative tool that should trigger warrant requirements by definition?',
    'Appellate ruling establishing that primary investigative intent requiring warrant regardless of whether data was initially collected incidentally, or statutory amendment limiting downstream uses of incidental data to cases where the U.S. person was not the primary investigative target.',
    'Acceptance of this ambiguity permits the incidental-collection reading to function; resolution in favor of warrant-requirement would move the constraint toward the constitutional-floor reading and substantially reduce measured extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_vs_primary_investigative_intent_ambiguity, conceptual, 'Whether incidental data queried as the primary target of investigation retains ''incidental'' status or requires warrant by virtue of investigative intent.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the incidental-collection reading''s core premise (foreign targeting justifies warrantless retention and query of incidentally collected U.S. person data) logically foreclose the constitutional-floor reading (Fourth Amendment requires warrant for any government search of U.S. person communications), or are both readings live positions that coexist in the broader statutory/constitutional interpretation space?',
    'Supreme Court ruling on whether § 702 queries are ''searches'' under the Fourth Amendment independent of statutory classification. The constitutional floor reading holds that they are; the incidental-collection reading holds that statutory foreign targeting authority permits them notwithstanding Fourth Amendment doctrine.',
    'If the readings foreclose (one reading''s core premise logically excludes the other), this is a genuine kernel contest with a binary terminal condition. If they coexist (both readings are internally coherent frameworks that different parties adopt), the constraint classification and the sibling constraints'' classifications remain independent. The structural architecture of this story assumes coexistence but documents the foreclosure possibility as an omega.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether readings of the § 702 kernel foreclose or coexist in the statutory/constitutional interpretation space.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 2008, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(fisa_tr_t2011, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2011, 0.28).
narrative_ontology:measurement(fisa_tr_t2014, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2014, 0.32).
narrative_ontology:measurement(fisa_tr_t2017, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2017, 0.36).
narrative_ontology:measurement(fisa_tr_t2020, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(fisa_tr_t2023, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2023, 0.41).
narrative_ontology:measurement(fisa_tr_t2026, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2008, 0.32).
narrative_ontology:measurement(fisa_be_t2011, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2011, 0.36).
narrative_ontology:measurement(fisa_be_t2014, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2014, 0.39).
narrative_ontology:measurement(fisa_be_t2017, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement(fisa_be_t2023, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2023, 0.45).
narrative_ontology:measurement(fisa_be_t2026, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2026, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement(fisa_su_t2011, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2011, 0.6).
narrative_ontology:measurement(fisa_su_t2014, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2014, 0.64).
narrative_ontology:measurement(fisa_su_t2017, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement(fisa_su_t2020, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(fisa_su_t2023, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2023, 0.72).
narrative_ontology:measurement(fisa_su_t2026, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__incidental_collection_reading, 0.18).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (incidental-collection-permitting interpretation) of the contested FISA § 702 kernel. The sibling constraints are alternative readings of the same statutory text and constitutional question: foreign-target-strict-reading interprets § 702 to require genuine minimization of incidental U.S. person data with no domestic investigative access; constitutional-floor-reading holds that Fourth Amendment warrants are nondelegable to statutory foreign intelligence authority. These are not measuring the same constraint from different angles; they are structurally distinct readings with different ε values, beneficiary/victim sets, and classifications. The ε-invariance principle mandates separate constraint stories for each reading because the core structural claim (what § 702 permits) differs across readings. The three stories are linked by network.affects_constraints to show the kernel family relationship. The readings do not coexist incoherently — each is internally stable as a statutory/constitutional interpretation — but they are mutually exclusive in the sense that one reading's core premise about what § 702 permits contradicts another's. This is a coexists_with relationship in the commitment system (both readings are live positions in the policy/constitutional space), not a forecloses relationship (neither reading logically eliminates the others in the same framework, because Congress could choose any reading and courts could validate it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
