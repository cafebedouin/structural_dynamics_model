% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__incidental_collection_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: FISA Section 702 Incidental Collection Reading
 *   domain: constitutional/national_security/surveillance
 *
 * SUMMARY:
 *   This constraint story models the 'incidental collection reading' of FISA
 *   Section 702 statutory text — one of three contested readings of the same
 *   kernel (fisa_702_statutory_text). The reading holds that the statute's
 *   foreign targeting language permits retention and warrantless query of
 *   U.S. person communications incidentally collected during lawful foreign
 *   intelligence targeting. This reading underwrites the 'backdoor search'
 *   practice whereby FBI queries the 702 database for domestic investigations
 *   without probable cause warrants. The constraint is classified as
 *   tangled_rope: it performs a genuine coordination function (foreign
 *   intelligence collection against non-U.S. persons abroad) while extracting
 *   from U.S. persons whose communications are swept in and then used for
 *   domestic purposes without Fourth Amendment process. The structural delta
 *   from sibling readings: constitutional_floor_reading rejects the statutory
 *   authorization entirely on Fourth Amendment grounds;
 *   foreign_target_strict_reading reads the statute narrowly to exclude
 *   retention and domestic use of incidental collection. This reading
 *   occupies the middle ground that has prevailed in practice since 2008.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.78).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA Section 702 Incidental Collection Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional/national_security/surveillance").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, 'd7942acd-ec31-4da4-8efc-1f166f980031').
narrative_ontology:cs_kernel_codification('d7942acd-ec31-4da4-8efc-1f166f980031', formalized).
narrative_ontology:cs_authority_grounding('d7942acd-ec31-4da4-8efc-1f166f980031', extraction).
narrative_ontology:cs_interpretation_layer_present('d7942acd-ec31-4da4-8efc-1f166f980031').
narrative_ontology:cs_reading_relation('d7942acd-ec31-4da4-8efc-1f166f980031', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7942acd-ec31-4da4-8efc-1f166f980031', fisa_702_statutory_text__constitutional_floor_reading, influences).
narrative_ontology:cs_axiom('d7942acd-ec31-4da4-8efc-1f166f980031', foundational, foreign_intelligence_purpose_justifies_incidental_collection_and_query).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_justifies_incidental_collection_and_query, holdable).
narrative_ontology:cs_axiom_grounding('d7942acd-ec31-4da4-8efc-1f166f980031', foreign_intelligence_purpose_justifies_incidental_collection_and_query, conventional).
narrative_ontology:cs_axiom('d7942acd-ec31-4da4-8efc-1f166f980031', foundational, administrative_minimization_satisfies_fourth_amendment_for_incidental_collection).
narrative_ontology:cs_axiom_status(administrative_minimization_satisfies_fourth_amendment_for_incidental_collection, holdable).
narrative_ontology:cs_axiom_grounding('d7942acd-ec31-4da4-8efc-1f166f980031', administrative_minimization_satisfies_fourth_amendment_for_incidental_collection, conventional).
narrative_ontology:cs_reference_frame('d7942acd-ec31-4da4-8efc-1f166f980031', post_911_foreign_intelligence_gap).
narrative_ontology:cs_drift_state('d7942acd-ec31-4da4-8efc-1f166f980031', post_2023_reauthorization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7942acd-ec31-4da4-8efc-1f166f980031', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_community).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, executive_branch_authorities).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_subject_to_backdoor_searches).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, constitutional_floor_rights_holders).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_exception_to_warrant_requirement).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, statutory_authorization_displaces_fourth_amendment_for_incidental_collection).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, administrative_minimization_suffices_for_constitutional_compliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, operates, and defends the 702 collection program. Claims foreign intelligence necessity justifies incidental U.S. person collection and warrantless querying. Controls classification of program details and minimization procedures. Benefits from expanded investigative aperture without warrant burden.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, intelligence_community, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, intelligence_community, beneficiary).

% Accesses 702 database for domestic criminal investigations via 'backdoor searches' — querying U.S. person identifiers without probable cause warrants. Gains investigative leads unavailable through traditional warrant channels. Institutionalized through FBI minimization procedures approved by FISC. Cannot exit the arrangement without losing a major investigative tool.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations, beneficiary,
    institutional, biographical, constrained, national).

% Asserts Article II authority and statutory authorization permit the program. Benefits from foreign intelligence collection that incidentally sweeps in U.S. person communications, then uses those communications for domestic purposes without judicial warrants. Can shift between statutory and constitutional justifications as political conditions change.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, executive_branch_authorities, beneficiary,
    institutional, biographical, arbitrage, global).

% U.S. citizens and lawful permanent residents whose communications are collected incidentally while targeting foreigners abroad. Their emails, chats, calls retained in intelligence databases for years. No notice, no individualized suspicion, no ability to challenge or exit the collection. Subject to warrantless queries by FBI and other agencies.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected, payer,
    powerless, biographical, trapped, global).

% Subset of incidentally collected U.S. persons whose identifiers are affirmatively queried by FBI for domestic investigations. Communications content reviewed by domestic law enforcement without probable cause, without judicial oversight, often without any connection to foreign intelligence. No statutory right to suppress evidence derived from such searches in criminal proceedings.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_subject_to_backdoor_searches, payer,
    powerless, biographical, trapped, national).

% All U.S. persons whose Fourth Amendment protections are structurally displaced by the program's architecture. The constraint operates as a standing exception to warrant requirements for digital communications. Rights-holders cannot opt out of the legal regime; constitutional challenge paths are blocked by standing doctrines and state secrets privileges. Their exclusion from the authorization conversation is structural — the program was designed without their participation and resists their intervention.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, constitutional_floor_rights_holders, excluded,
    moderate, generational, identity_locked, universal).

% Foreign Intelligence Surveillance Court reviews annual certifications and minimization procedures ex parte, without adversarial testing. Issues opinions interpreting statutory authority but lacks enforcement capacity against executive branch non-compliance. Sees classified record but cannot publish reasoning. Institutional position creates pressure toward approval; dissent is rare and non-binding.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fisc_court, observer,
    institutional, generational, analytical, national).

% Statutorily designated oversight bodies (HPSCI, SSCI, House/Senate Judiciary). Receive classified briefings but face structural information asymmetry — executive branch controls what is disclosed. Can legislate reforms but face political pressure to renew authorities. Periodic reauthorization cycles (2008, 2012, 2018, 2023) function as moments of contested legitimacy rather than genuine consent.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congress_oversight_committees, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, congress_oversight_committees, agenda_setter).

% NGOs, academics, journalists challenging the program's constitutionality. Bring test cases (Clapper v. Amnesty, ACLU v. NSA) but face standing barriers — plaintiffs cannot prove their communications were collected. Advocate for warrant requirements, adversarial FISC process, data minimization. Their exclusion from classified proceedings limits effective intervention.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables foreign intelligence collection against non-U.S. persons abroad without individualized warrants for each target, solving the coordination problem of timely signals intelligence in a global communications environment where targets use U.S. infrastructure.
% TRANSFER_FUNCTION: Moves U.S. person communications content and metadata from the private sphere into government databases without consent or warrants; moves investigative access from judicial warrant processes to administrative querying by domestic law enforcement; moves constitutional protection from individualized probable cause to programmatic minimization procedures.
% ABSENT_VOICES: The U.S. persons whose communications are incidentally collected and subsequently queried — they are never notified, never represented in FISC proceedings, and structurally excluded from the authorization process. Their representatives in Congress face classification barriers. The constitutional_floor_rights_holders stakeholder captures this structural absence.
% DISAPPEARANCE_RATIONALE: If the incidental collection reading vanished overnight, the intelligence community would need individualized warrants or new statutory authority to access U.S. person communications content. FBI domestic investigations would lose warrantless access to the 702 database. The legal architecture enabling 'backdoor searches' would collapse, requiring either a return to traditional Fourth Amendment processes or new legislation. The foreign intelligence collection mission would continue but with narrowed aperture for U.S. person data.
% FOUNDING_PROBLEM: Post-9/11 intelligence gaps: the Foreign Intelligence Surveillance Act (1978) required individualized warrants for surveillance of U.S. persons, creating delays and blind spots when foreign targets communicated through U.S. infrastructure. The Protect America Act (2007) and FISA Amendments Act Section 702 (2008) created a programmatic authority targeting non-U.S. persons abroad, accepting incidental U.S. person collection as unavoidable.
% FOUNDING_PROBLEM_CORROBORATION: The intelligence community and executive branch attest the founding problem remains live — foreign targets still use U.S. platforms, encryption still frustrates targeted collection. Civil liberties organizations, multiple FISC opinions (including 2011 and 2018 compliance incidents), PCLOB reports (2014, 2023), and bipartisan congressional critics (Wyden, Paul, Amash) attest the program has drifted from its founding justification into a domestic investigative tool — the 'backdoor search' problem documented by ODNI transparency reports showing FBI queries far exceeding NSA/CIA queries. The 2023 reauthorization debate centered on this drift.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fisa_702_statutory_text__incidental_collection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__incidental_collection_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.45) reflects substantial but not total extraction: U.S. persons lose warrant protection for incidentally collected communications, but the program's primary target remains foreign. The extraction is concentrated on a subset (those queried via backdoor searches) while the broader set bears retention risk. Suppression (0.78) is high because the constraint's persistence depends on actively blocking alternatives: standing doctrines prevent judicial review, state secrets privilege blocks discovery, classification prevents public debate, and minimization procedures substitute for adversarial process. Theater ratio (0.32) reflects that minimization procedures and FISC review perform genuine oversight functions but increasingly serve to legitimate the extraction rather than constrain it — the 2011 and 2018 FISC opinions documenting systemic compliance failures, and the PCLOB's 2023 finding that FBI's querying practices 'raise significant constitutional concerns,' indicate the oversight layer is partially performative. Accessibility collapse (0.68) is substantial: U.S. persons cannot avoid collection by changing behavior (foreign targets choose the platforms), cannot opt out of the legal regime, and face near-insurmountable barriers to judicial challenge. Resistance (0.42) is moderate: congressional reauthorization debates, litigation, and public pressure exist but have not altered the core architecture.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (intelligence community, FBI, executive) experience the constraint as coordination with manageable incidental costs — the program enables their mission. The payer seats (incidentally collected U.S. persons, backdoor search subjects) experience it as extraction without consent or remedy. The excluded seats (constitutional floor rights-holders, civil liberties advocates) experience it as a structural rights displacement that the system refuses to acknowledge. The engine computes per-seat classifications from these structural positions; the divergence between seats IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The intelligence community, FBI, and executive branch are structural beneficiaries (d near 0.0-0.2): they gain investigative aperture, avoid warrant burdens, and control the program's operation and classification. U.S. persons incidentally collected and subject to backdoor searches are structural targets (d near 0.9-1.0): they bear the privacy costs, cannot exit, and have no effective recourse. Constitutional floor rights-holders are identity-locked targets (d ~0.85): their rights are displaced by the program's architecture itself; exit would require constitutional amendment or regime change. FISC and congressional oversight are analytical/constrained observers (d ~0.5): they see the structure but lack effective leverage. Civil liberties advocates are mobile excluded parties (d ~0.3): they can advocate but cannot access the classified record or compel process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (foreign intelligence gaps against targets using U.S. infrastructure) remains live per intelligence community attestation. However, the program has drifted substantially: FBI domestic queries now vastly exceed foreign intelligence queries (ODNI transparency reports: ~200k-3M FBI queries/year vs. ~10k-100k NSA/CIA queries). The minimization procedures, originally justified as protecting U.S. person privacy, have become the mechanism enabling domestic access. This is mandatrophy — the coordination function (foreign intelligence) persists but the extraction function (domestic investigative access to U.S. person data without warrants) has become a primary operational output. The 2023 reauthorization's failure to impose warrant requirements for backdoor searches, despite bipartisan support, indicates the extraction function now has its own institutional constituency (FBI, DOJ) distinct from the founding coordination purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incidental_vs_intentional_collection_boundary,
    'Where is the structural line between ''incidental'' collection (unavoidable byproduct of foreign targeting) and ''intentional'' collection (targeting foreigners to reach U.S. persons)?',
    'Technical analysis of collection architecture: selector targeting logic, tasking instructions, and whether U.S. person identifiers are used as affirmative search terms at collection time (not just query time). Congressional investigation with access to classified tasking records.',
    'If intentional collection of U.S. persons via foreign targets is structurally embedded, the constraint is snare not tangled_rope — the coordination cover is pretext. If truly incidental, the coordination function is genuine and extraction is a byproduct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incidental_vs_intentional_collection_boundary, empirical, 'Whether the incidental collection framing describes operational reality or masks intentional U.S. person targeting').

omega_variable(
    backdoor_search_constitutional_status,
    'Does the Fourth Amendment permit warrantless FBI queries of a database lawfully collected under foreign intelligence authority, when the queries target U.S. persons for domestic investigations?',
    'Supreme Court review of a test case with established standing (requires either government disclosure of querying or a plaintiff who can prove their communications were queried). Alternatively, congressional statute imposing warrant requirement, creating a statutory floor the Court could uphold.',
    'If warrant required, the constraint''s extraction component is unconstitutional — the tangled_rope''s extraction limb is legally void, leaving only the coordination limb (potentially reclassifying as rope or scaffold). If no warrant required, the constitutional_floor_reading is foreclosed as a structural matter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backdoor_search_constitutional_status, conceptual, 'Whether the backdoor search practice survives Fourth Amendment scrutiny').

omega_variable(
    minimization_procedures_effectiveness,
    'Do FISC-approved minimization procedures functionally protect U.S. person privacy, or do they primarily legitimate the retention and querying of U.S. person data?',
    'Independent audit of minimization compliance incidents (2011, 2018 FISC opinions document systemic overcollection and improper querying). Comparative analysis: U.S. person data retention periods, dissemination rules, and query standards vs. traditional FISA Title I warrants.',
    'If minimization is effective, the constraint''s extraction is lower and theater ratio lower — genuine coordination with real safeguards. If minimization is largely performative, extraction and theater are higher — the safeguards are the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_procedures_effectiveness, empirical, 'Whether administrative minimization meaningfully constrains extraction or enables it').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the incidental_collection_reading logically foreclose the constitutional_floor_reading within a single legal framework, or do they coexist as competing interpretations?',
    'Analyze whether a court adopting the incidental_collection_reading as binding precedent (e.g., a Supreme Court holding that 702 queries are not Fourth Amendment searches) would make the constitutional_floor_reading legally impossible to maintain within that jurisdiction''s framework — or whether the constitutional_floor_reading remains a live dissenting position that could be adopted by a future Court.',
    'If forecloses: the readings are mutually exclusive in any single framework — the kernel has a winner-take-all structure. If coexists_with: the kernel sustains permanent interpretive contestation. This determines whether cs_structure.reading_relations records forecloses or coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between this reading and the constitutional_floor_reading sibling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 2008, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(fisa_tr_t2011, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2011, 0.18).
narrative_ontology:measurement(fisa_tr_t2013, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2013, 0.22).
narrative_ontology:measurement(fisa_tr_t2015, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(fisa_tr_t2018, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(fisa_tr_t2021, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(fisa_tr_t2023, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2023, 0.31).
narrative_ontology:measurement(fisa_tr_t2025, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2008, 0.25).
narrative_ontology:measurement(fisa_be_t2011, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2011, 0.32).
narrative_ontology:measurement(fisa_be_t2013, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2013, 0.38).
narrative_ontology:measurement(fisa_be_t2015, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(fisa_be_t2018, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2018, 0.43).
narrative_ontology:measurement(fisa_be_t2021, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2021, 0.44).
narrative_ontology:measurement(fisa_be_t2023, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2023, 0.45).
narrative_ontology:measurement(fisa_be_t2025, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2008, 0.65).
narrative_ontology:measurement(fisa_su_t2011, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2011, 0.68).
narrative_ontology:measurement(fisa_su_t2013, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2013, 0.72).
narrative_ontology:measurement(fisa_su_t2015, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(fisa_su_t2018, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2018, 0.76).
narrative_ontology:measurement(fisa_su_t2021, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2021, 0.77).
narrative_ontology:measurement(fisa_su_t2023, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2023, 0.78).
narrative_ontology:measurement(fisa_su_t2025, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__incidental_collection_reading, 0.12).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fourth_amendment_digital_communications).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fbi_backdoor_search_practice).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisc_oversight_structure).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, state_secrets_privilege_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of kernel fisa_702_statutory_text. The incidental_collection_reading (this story) and foreign_target_strict_reading share the same statutory text but differ on whether incidental U.S. person data may be retained and queried. The constitutional_floor_reading rejects the statutory authorization entirely on Fourth Amendment grounds. All three are linked via affects_constraints. The foreign_target_strict_reading influences this reading by providing the narrow construction that this reading must distinguish itself from; the constitutional_floor_reading is influenced by this reading's operational prevalence (the constitutional challenge must overcome the entrenched regime).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__incidental_collection_reading, institutional, 0.15).
constraint_indexing:directionality_override(fisa_702_statutory_text__incidental_collection_reading, powerless, 0.95).
constraint_indexing:directionality_override(fisa_702_statutory_text__incidental_collection_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
