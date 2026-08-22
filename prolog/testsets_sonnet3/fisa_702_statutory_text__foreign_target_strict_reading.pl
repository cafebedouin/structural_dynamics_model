% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__foreign_target_strict_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA 702 Foreign-Target Strict Reading: Statutory Minimization as Deletion
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This story instantiates the foreign_target_strict_reading of the FISA
 *   Section 702 statutory kernel: the statute's 'foreign target' language is
 *   read as a genuine, categorical limitation — collection must be directed
 *   at non-U.S. persons reasonably believed to be abroad, and any
 *   incidentally collected U.S. person communications must be minimized in a
 *   sense this reading takes to mean deletion or inaccessibility, not merely
 *   restricted access subject to later query. Under this reading, U.S.
 *   persons are not treated as a victim class at all (they retain full Fourth
 *   Amendment protection and the statute is read as honoring it), and FBI
 *   queries of the 702 database for domestic criminal purposes are
 *   categorically prohibited rather than merely disfavored. This is a
 *   distinct constraint from the incidental_collection_reading (which treats
 *   warrantless domestic query of incidentally collected data as permissible
 *   when justified by foreign intelligence purpose) and from the
 *   constitutional_floor_reading (which holds any 702 query of U.S. person
 *   content is itself a Fourth Amendment search requiring a warrant
 *   regardless of statutory text). The three readings share a kernel — the
 *   same statutory text and program — but diverge sharply on who counts as a
 *   rights-holder, what minimization means operationally, and whether the
 *   statute's own terms or the Constitution independently constrain the FBI's
 *   query practice.
 *
 * KEY AGENTS:
 *   - nsa_collection_operators: institutional agenda-setter administering targeting/minimization procedures
 *   - us_persons_incidentally_collected: powerless beneficiary class under this reading's protective interpretation
 *   - fbi_domestic_investigators: institutional excluded party seeking query access this reading forecloses
 *   - foreign_intelligence_targets: powerless, trapped payer class bearing the actual collection
 *   - fisc_and_oversight_courts: institutional observer verifying compliance
 *   - congress: institutional agenda-setter controlling statutory text at reauthorization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.2).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA 702 Foreign-Target Strict Reading: Statutory Minimization as Deletion").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, 'db5290fc-8fcb-4375-82c1-71c9c3e58909').
narrative_ontology:cs_kernel_codification('db5290fc-8fcb-4375-82c1-71c9c3e58909', fixed_text).
narrative_ontology:cs_authority_grounding('db5290fc-8fcb-4375-82c1-71c9c3e58909', lineage).
narrative_ontology:cs_interpretation_layer_present('db5290fc-8fcb-4375-82c1-71c9c3e58909').
narrative_ontology:cs_reading_relation('db5290fc-8fcb-4375-82c1-71c9c3e58909', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('db5290fc-8fcb-4375-82c1-71c9c3e58909', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('db5290fc-8fcb-4375-82c1-71c9c3e58909', foundational, minimization_requires_deletion_or_inaccessibility).
narrative_ontology:cs_axiom_status(minimization_requires_deletion_or_inaccessibility, holdable).
narrative_ontology:cs_axiom_grounding('db5290fc-8fcb-4375-82c1-71c9c3e58909', minimization_requires_deletion_or_inaccessibility, conventional).
narrative_ontology:cs_axiom('db5290fc-8fcb-4375-82c1-71c9c3e58909', secondary, statutory_text_sufficient_absent_independent_constitutional_claim).
narrative_ontology:cs_axiom_status(statutory_text_sufficient_absent_independent_constitutional_claim, holdable).
narrative_ontology:cs_axiom_grounding('db5290fc-8fcb-4375-82c1-71c9c3e58909', statutory_text_sufficient_absent_independent_constitutional_claim, conventional).
narrative_ontology:cs_reference_frame('db5290fc-8fcb-4375-82c1-71c9c3e58909', foreign_intelligence_surveillance_act_1978_warrant_baseline).
narrative_ontology:cs_drift_state('db5290fc-8fcb-4375-82c1-71c9c3e58909', post_snowden_reauthorization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('db5290fc-8fcb-4375-82c1-71c9c3e58909', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, civil_liberties_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_targets).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, fourth_amendment_domestic_protection_doctrine).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, statutory_foreign_target_limitation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate the targeting and minimization procedures required to certify that collection is directed at non-U.S. persons abroad. Under this reading, they must build technical and procedural walls preventing incidentally collected U.S. person content from being queried for domestic purposes, and must purge or wall off such data rather than retain it for future access.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, nsa_collection_operators, agenda_setter,
    institutional, generational, analytical, global).

% Communicate with or are mentioned by foreign targets and have their communications incidentally swept into 702 collection. Under this reading their data is not treated as fair game: it must be minimized (this reading holds minimization means deletion or inaccessibility, not merely restricted access) and cannot be queried by domestic law enforcement without an individualized warrant. They benefit from a categorical rule rather than case-by-case discretion.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_incidentally_collected, beneficiary,
    powerless, biographical, trapped, national).

% Would like to query the 702 database using U.S. person identifiers to support domestic criminal investigations. Under this reading such queries are categorically prohibited regardless of justification, foreclosing an investigative shortcut they would otherwise use; they are structurally excluded from access to this data pool for domestic purposes.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic_investigators, excluded,
    institutional, immediate, constrained, national).

% Non-U.S. persons abroad who are the actual object of collection under the statute's foreign-target language. They bear the collection itself; this reading does not treat them as rights-holders whose interests limit the program, since the statute's protections are read as running only to U.S. persons.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_targets, payer,
    powerless, immediate, trapped, global).

% Review certifications and minimization procedures for compliance with the statute as read under this reading — verifying that targeting is genuinely foreign-directed and that minimization functions as deletion/inaccessibility rather than a paper restriction. Their approval legitimizes the program's operation under this reading's terms.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fisc_and_oversight_courts, observer,
    institutional, generational, analytical, national).

% Enacted and periodically reauthorizes the statutory 'foreign target' language, setting the textual boundary this reading enforces. Holds the power to tighten or loosen the minimization and query-prohibition rules at each reauthorization cycle.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congress, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__foreign_target_strict_reading, congress, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__foreign_target_strict_reading, diffuse).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__foreign_target_strict_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits the government to collect foreign intelligence from non-U.S. persons abroad without individualized warrants — solving the genuine problem that traditional warrant procedures cannot practically apply to foreign communications outside constitutional protection — while categorically walling off incidentally captured U.S. person data from domestic use.
% TRANSFER_FUNCTION: Moves foreign intelligence value from foreign targets to the government's national security apparatus; moves nothing extractive from U.S. persons under this reading because their incidentally collected data is treated as inaccessible rather than as a resource for domestic investigators to draw on.
% ABSENT_VOICES: Foreign targets themselves have no seat in this dispute at all under this reading — the whole point of the 'foreign target' language is that their communications fall outside the protections the debate is about. FBI investigators who want query access are present but structurally overruled by the categorical prohibition this reading imposes.
% DISAPPEARANCE_RATIONALE: If the foreign-target statutory language and its accompanying minimization-as-deletion rule vanished, the categorical bar on FBI domestic queries would fall away, and incidentally collected U.S. person communications would become queryable for domestic law enforcement purposes absent a warrant — collapsing this reading into something structurally identical to the incidental_collection_reading.
% FOUNDING_PROBLEM: Foreign intelligence collection needed a legal framework that did not require individualized warrants for surveillance of non-U.S. persons abroad, while simultaneously constraining the government from using that authority as backdoor access to U.S. persons' communications for ordinary domestic law enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations (Brennan Center, EFF) and several FISC opinions documenting compliance violations attest that the minimization-as-deletion function is only partially realized in practice — query volumes against U.S. person identifiers have been substantial in various reporting periods, suggesting the categorical bar this reading asserts is honored unevenly rather than structurally guaranteed. The government's own compliance reports (declassified) are the primary source and are not fully independent of the benefiting institution, so this corroboration is weaker than ideal — that qualification is itself part of the record.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).
:- end_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.15) because under this reading's own lights, U.S. persons are not extracted from at all — the statute's text is read as a genuine barrier, and where minimization functions as deletion, there is no ongoing capture of their data for domestic use. Suppression is low-moderate (0.2) reflecting the real but modest coercive machinery needed to enforce query-prohibition rules against an institution (FBI) that would prefer broader access. Theater ratio is authored at a modest 0.25, rising slightly over the interval, reflecting documented compliance gaps between the formal minimization requirement and actual practice (FISC compliance opinions have periodically found query violations) — the reading's own account of itself is largely functional, not performative, but is not immaculate. Accessibility collapse is moderate (0.35): once someone examines the statute under this reading, the foreign-target boundary is fairly clear, though contested interpretive space remains at the margins (what counts as 'targeting,' what minimization procedures suffice). Resistance is moderate-high (0.55) because this reading is actively contested by institutions (FBI, elements of the intelligence community) that prefer the incidental_collection_reading's broader access, and by civil libertarians who argue the constitutional_floor_reading is the only reading that actually protects rights regardless of statutory text.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons incidentally collected are coded as beneficiaries under this reading specifically because the reading treats the statutory text as doing real protective work — their data is walled off, not merely subject to weaker safeguards. Foreign intelligence targets are the payer class: they are the actual object of the surveillance the coordination function authorizes, and this reading does not extend Fourth Amendment-style protection to them. FBI investigators are excluded rather than victimized — they lose an investigative shortcut they would otherwise have, but this is a foreclosure of opportunity, not extraction from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling foreign intelligence collection without warrant procedures ill-suited to foreign targets, while walling off U.S. persons from backdoor domestic use) remains partially live: foreign intelligence collection needs persist, but whether the wall function still operates as originally designed is exactly what separates this reading from incidental_collection_reading. This story's founding_problem_status is authored as contested rather than dead or live, because the corroboration record (FISC compliance findings, advocacy group reporting) shows the protective function operating imperfectly rather than having fully atrophied or being fully intact — precisely the kind of live dispute the classification system is built to surface rather than resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimization_as_deletion_vs_access_restriction,
    'Does ''minimization'' under 702 statutory and FISC-approved procedures actually function as deletion/inaccessibility of incidentally collected U.S. person data, or does it function as a weaker access-restriction regime that still permits later query under certain justifications?',
    'Declassified FISC opinions, NSA/FBI minimization procedure texts, and compliance audit reports documenting actual query practice against the U.S. person identifier pool over the reauthorization cycles.',
    'If minimization functions as genuine deletion/inaccessibility, this reading''s low ε (0.15) is well-supported. If it functions as access restriction routinely bypassed by justified query, the structural facts collapse toward the incidental_collection_reading and this reading''s beneficiary declaration for U.S. persons becomes empirically false.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_as_deletion_vs_access_restriction, empirical, 'Whether minimization is deletion or merely restricted access — the operative distinction between this reading and its incidental_collection sibling.').

omega_variable(
    statutory_text_vs_constitutional_floor_sufficiency,
    'Is the statutory ''foreign target'' and minimization language, properly enforced, sufficient on its own to satisfy Fourth Amendment concerns, or does the Constitution impose an independent warrant requirement for 702 queries regardless of how faithfully the statute is followed?',
    'Appellate and Supreme Court resolution of pending challenges to 702 query practices; scholarly and judicial treatment of whether a ''search'' occurs at collection, at query, or at use.',
    'If the statutory floor is constitutionally sufficient, this reading is the legally controlling one and the constitutional_floor_reading is aspirational advocacy rather than binding law. If the Constitution independently requires a warrant, this reading''s protective claim for U.S. persons is legally incomplete even where faithfully executed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(statutory_text_vs_constitutional_floor_sufficiency, conceptual, 'Whether statutory compliance under this reading is constitutionally sufficient or merely a policy floor beneath a higher constitutional requirement.').

omega_variable(
    foreign_target_rights_exclusion,
    'Is it defensible that this reading excludes foreign intelligence targets entirely from the beneficiary/victim analysis, treating their surveillance as categorically outside the rights framework the constraint is built to police?',
    'International human rights law scholarship and foreign government diplomatic protests (e.g., post-Snowden EU/German reactions) addressing whether non-U.S. persons abroad have any cognizable interest the U.S. legal system should weigh.',
    'If foreign targets are recognized as rights-bearing in some framework, this reading''s ε of 0.15 dramatically understates the constraint''s actual extraction, since it is authored solely from the U.S. domestic rights framework''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_target_rights_exclusion, preference, 'Whether excluding foreign targets from the victim analysis is a defensible feature of this reading or an artifact of its domestic-rights framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(fisa_tr_t2011, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2011, 0.18).
narrative_ontology:measurement(fisa_tr_t2014, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(fisa_tr_t2017, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2017, 0.22).
narrative_ontology:measurement(fisa_tr_t2021, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2021, 0.24).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2008, 0.1).
narrative_ontology:measurement(fisa_be_t2011, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2011, 0.11).
narrative_ontology:measurement(fisa_be_t2014, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2014, 0.13).
narrative_ontology:measurement(fisa_be_t2017, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2017, 0.14).
narrative_ontology:measurement(fisa_be_t2021, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2021, 0.15).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2008, 0.15).
narrative_ontology:measurement(fisa_su_t2011, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2011, 0.16).
narrative_ontology:measurement(fisa_su_t2014, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2014, 0.17).
narrative_ontology:measurement(fisa_su_t2017, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2017, 0.18).
narrative_ontology:measurement(fisa_su_t2021, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2021, 0.19).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept of 'the FISA 702 foreign intelligence surveillance framework.' Each sibling reads the same statutory kernel differently and computes a different ε: this reading authors ε≈0.15 (U.S. persons protected, minimization as deletion); incidental_collection_reading would author a substantially higher ε (U.S. persons as a queryable, exploitable data pool); constitutional_floor_reading treats the query itself as the constitutional event regardless of statutory framing, which produces yet a different victim/beneficiary structure centered on the warrant requirement rather than the foreign/domestic targeting line. All three share the same underlying program and text; per the ε-invariance principle they are authored as three separate constraint stories rather than one story with an interpretive parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
