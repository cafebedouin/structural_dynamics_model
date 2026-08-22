% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: FISA 702 Incidental Collection and Warrantless Query Authority
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint is the incidental collection reading of the FISA 702
 *   statute — one interpretation of Section 702 of the Foreign Intelligence
 *   Surveillance Act. Under this reading, the statute permits the NSA to
 *   collect communications of non-U.S. persons targeted abroad and, when
 *   those communications incidentally include U.S. person data, to retain and
 *   query that data without warrants whenever a foreign intelligence
 *   justification can be asserted. The reading displaces Fourth Amendment
 *   warrant requirements for U.S. persons whose communications are captured
 *   as metadata or direct communication with foreign targets. This is a
 *   contested kernel: the constitutional floor reading asserts the Fourth
 *   Amendment mandates warrants regardless of incidental status; the foreign
 *   target strict reading asserts the statute limits retention of U.S. person
 *   data and requires minimization; this reading asserts the statute permits
 *   broad retention and querying under foreign intelligence justification.
 *   The claim/metric gap is intentional: the constraint is CLAIMED as
 *   tangled_rope (coordination function + asymmetric extraction), and the
 *   metrics describe moderate-to-high extractiveness, rising theater ratio
 *   (suggesting performative minimization procedures replace functional
 *   oversight), and high suppression (U.S. persons are structurally excluded
 *   from knowing about or contesting queries).
 *
 * KEY AGENTS:
 *   - Intelligence Community: Institutional beneficiary; retains foreign collection infrastructure and gains warrantless access to U.S. person communications via foreign intelligence justification
 *   - FBI Domestic Operations: Institutional beneficiary-payer; accesses 702 database for domestic investigations under foreign intelligence cover; faces internal compliance only, not warrant review
 *   - U.S. Persons Subject to Surveillance: Powerless payer; incidentally collected, retained indefinitely, queryable without notice or consent; trapped exit (cannot opt out of communications with foreign targets or domestic collection)
 *   - Congress Authorizing Committee: Agenda setter; enacted Section 702 and retains reauthorization authority; receives classified briefings but limited independent oversight capacity
 *   - Courts: Excluded; Fourth Amendment warrant requirement displaced for 702 queries; no judicial real-time review of domestic investigative queries
 *   - Privacy Advocates & Civil Liberties Organizations: Excluded; challenge the reading but lack institutional standing to block queries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.72).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA 702 Incidental Collection and Warrantless Query Authority").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, 'd179bc70-61c1-432b-a1d1-8694fa9eb4f8').
narrative_ontology:cs_kernel_codification('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', fixed_text).
narrative_ontology:cs_authority_grounding('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', extraction).
narrative_ontology:cs_interpretation_layer_present('d179bc70-61c1-432b-a1d1-8694fa9eb4f8').
narrative_ontology:cs_reading_relation('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_reading_relation('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', foundational, foreign_intelligence_purpose_displaces_warrant).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_displaces_warrant, holdable).
narrative_ontology:cs_axiom_grounding('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', foreign_intelligence_purpose_displaces_warrant, conventional).
narrative_ontology:cs_axiom('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', foundational, incidental_us_person_data_retainable_and_queryable).
narrative_ontology:cs_axiom_status(incidental_us_person_data_retainable_and_queryable, holdable).
narrative_ontology:cs_axiom_grounding('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', incidental_us_person_data_retainable_and_queryable, empirically_contingent).
narrative_ontology:cs_axiom('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', secondary, administrative_minimization_replaces_judicial_warrant).
narrative_ontology:cs_axiom_status(administrative_minimization_replaces_judicial_warrant, holdable).
narrative_ontology:cs_axiom_grounding('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', administrative_minimization_replaces_judicial_warrant, conventional).
narrative_ontology:cs_reference_frame('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', foreign_targeting_regime_without_us_person_warrant_protection).
narrative_ontology:cs_drift_state('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', contemporary_expanded_fbi_query_use, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d179bc70-61c1-432b-a1d1-8694fa9eb4f8', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_community).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_operations).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_subject_to_surveillance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_targets).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_operations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains foreign intelligence collection infrastructure and gains warrantless access to incidentally collected U.S. person communications when justified by foreign intelligence purposes. Operates under statutory authority (Section 702) interpreted to permit querying the database for foreign intelligence without warrant or probable cause, using administrative minimization procedures instead of judicial oversight. The constraint enables cost-free expansion of surveillance reach from foreign targets to U.S. persons.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, intelligence_community, beneficiary,
    institutional, generational, arbitrage, global).

% Accesses 702 database for domestic investigations (counterintelligence, criminal cases) when agents assert foreign intelligence justification, bypassing warrant and probable cause requirements. Faces internal compliance review only, not judicial warrant process. Obtains investigative leads without Fourth Amendment burden; constraint enables domestic investigative reach but tethers it to foreign intelligence framing.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_operations, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_operations, payer).

% Incidentally collected in communications with foreign targets or by proximity to foreign intelligence collection programs. Their communications are retained indefinitely and queryable without their knowledge, consent, or judicial review. They have no statutory notice, no remedy at the time of query, no practical exit from the domestic surveillance system. They are subject to warrantless surveillance justified solely by administrative assertion of foreign intelligence purpose.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_subject_to_surveillance, payer,
    powerless, biographical, trapped, national).

% Primary collection targets abroad whose communications drive the justification for retaining incidentally collected U.S. person data. The constraint permits the intelligence community to retain U.S. person communications incidentally collected in the process of targeting them. They are structurally unable to exit the surveillance targeting regime.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_targets, beneficiary,
    powerless, biographical, trapped, global).

% Enacted Section 702 statute and retains reauthorization power every several years. Delegates implementation to the executive branch (NSA, FBI, Attorney General) with statutory limits and procedural minimization standards, but does not require warrants for U.S. person queries. Receives classified briefings on program scope but has limited independent oversight capacity.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congress_authorizing_committee, agenda_setter,
    institutional, generational, analytical, national).

% Excluded from real-time warrant review of 702 queries. No judicial process exists for domestic investigative use of incidentally collected U.S. person data when justified by foreign intelligence purpose. Fourth Amendment warrant requirement is displaced for 702-sourced information, placing courts outside the operational oversight structure.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, courts, excluded,
    institutional, generational, analytical, national).

% Challenge the reading in public discourse, litigation, and legislative advocacy but lack institutional standing to block 702 queries. They argue for Fourth Amendment warrant requirement and transparent accounting of U.S. person surveillance; their exclusion from implementation is structural.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, privacy_advocates, excluded,
    moderate, generational, constrained, national).

% Seek remedies in federal courts but face standing and justiciability barriers: surveillance is often not disclosed, and injury is difficult to prove without access to classified information. They test the constitutional boundaries of the reading through litigation.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_litigants, observer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, intelligence_community).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes foreign intelligence collection infrastructure and permits efficient queries of retained communications data without duplicative warrant processes for each domestic investigative lead. Solves the coordination problem of how to access incidentally collected U.S. person communications during foreign intelligence operations.
% TRANSFER_FUNCTION: Moves Fourth Amendment protection and judicial oversight from U.S. persons whose communications are incidentally collected to the intelligence and law enforcement apparatus, which gains warrantless access justified by administrative assertion of foreign intelligence purpose. The constraint extracts privacy and due-process rights from powerless U.S. persons and transfers them to institutional beneficiaries.
% ABSENT_VOICES: U.S. persons subject to surveillance are structurally excluded — they do not know they are surveilled, receive no notice of queries, and cannot participate in decisions about their data. Courts are excluded from real-time warrant review. Privacy advocacy organizations and civil liberties groups would object that the reading displaces Fourth Amendment protection, but they are kept outside operational implementation. Foreign targets and their communicants (who might object to being collected under foreign intelligence authorities) are not consulted.
% DISAPPEARANCE_RATIONALE: If this reading and its statutory authority vanished, the FBI and intelligence community would lose warrantless access to incidentally collected U.S. person communications; queries would require probable cause warrants issued by courts. The institutional capacity to conduct large-scale surveillance of U.S. persons through the backdoor of foreign intelligence operations would collapse. Domestic investigations would revert to traditional warrant-based surveillance or accept the absence of leads from 702 data.
% FOUNDING_PROBLEM: Post-9/11 foreign intelligence collection under FISA required rapid access to communications to identify terrorist threats and foreign intelligence plots. Traditional warrant processes were too slow for real-time collection targeting non-U.S. persons abroad. Incidental collection of U.S. person data was a technical byproduct of foreign targeting and retaining that data enabled follow-on analysis without starting collection afresh.
% FOUNDING_PROBLEM_CORROBORATION: Intelligence officials attest the founding problem remains live: foreign threats move quickly and warrant delays cost intelligence advantage. Civil liberties organizations and courts attest the founding problem has been substantially solved by modern technical filtering and that the statute is now primarily used to conduct domestic surveillance of U.S. persons under foreign intelligence cover. Legislative critics attest the founding problem has shifted: the real problem now is executive branch expansion of 'foreign intelligence purpose' to include domestic political surveillance and law enforcement unrelated to foreign threats. No corroboration exists from outside the intelligence community that the current scale of U.S. person retention and querying remains justified by the original founding problem.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__incidental_collection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__incidental_collection_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.45 at 2024): the constraint displaces Fourth Amendment rights for an indeterminate number of U.S. persons, but the scale of extraction depends on how broadly intelligence officials interpret 'foreign intelligence purpose' to justify domestic investigative access. Early in the interval (2008), the reading operated closer to its stated bounds (narrower foreign intelligence justification); by 2024, it had drifted toward domestic investigative use. Suppression is high (0.72): U.S. persons are structurally excluded from knowing about queries; there is no ex ante warrant process or ex post notification; the constraint depends on keeping U.S. persons unaware of surveillance and unable to contest it. Theater is rising (0.32→0.58): internal minimization procedures, compliance reviews, and annual certifications to Congress create the appearance of oversight while actual operational decisions remain within the executive branch. The measurement series spans 16 years to capture the drift from narrow foreign intelligence focus (post-9/11) toward broader domestic surveillance justification (post-2016 evidence of FBI queries for political investigations). The rising extractiveness and theater ratio together suggest the constraint has evolved from a narrow coordination solution (coordinate foreign collection with retention efficiency) toward a broader extraction mechanism (coordinate access to U.S. person data under foreign intelligence cover for domestic purposes).
 *
 * PERSPECTIVAL GAP:
 *   From the intelligence community and FBI seats, the constraint is genuine coordination: it solves the problem of accessing incidentally collected data efficiently without restarting collection for each domestic lead. From the U.S. person seats, it is pure extraction: warrantless surveillance authorized by administrative assertion without judicial review. From the court seats, it is unconstitutional displacement of Fourth Amendment rights. From the congressional oversight seat, it is a compromise authorizing foreign collection while imposing administrative minimization safeguards; but oversight capacity is limited by classification and executive branch control. The engine computes each seat's directionality from power, exit, and beneficiary/victim declarations: intelligence and FBI get low d (beneficiaries with high power and arbitrage/constrained exit); U.S. persons get high d (victims with powerless status and trapped exit); Congress gets moderate d (agenda setter but constrained by classification and executive implementation discretion). The perspectival gap is structural, not observational — different seats hold genuinely opposed interests in the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons are the structural targets (d→1.0): powerless, trapped exit, no notice or remedy, benefits are negative (surveillance costs without consent). Intelligence community is the structural beneficiary (d→0.0): institutional power, arbitrage exit (can operate under other statutes if this one changes), gains operational capacity and warrantless access. FBI domestic operations sit between (d≈0.4): institutional power, but constrained by having to frame queries as foreign intelligence; benefits from warrantless access but carries reputational and compliance costs. Congress is the agenda setter (d≈0.3): institutional power, analytical exit (can reauthorize or amend), but trapped by classified briefings and executive branch expertise asymmetry. Courts are excluded (no directionality on this constraint; they would have high d if admitted, but the reading explicitly removes them from the query process). Directionality overrides: none needed — the structural data produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rapid foreign intelligence access in real-time to identify terrorist threats) is contested as to current status: intelligence attests it remains live; critics attest it is substantially solved by modern filtering and is now cover for domestic surveillance. The constraint's persistence despite the contested founding problem indicates: either the coordination function remains genuine (intelligence really does need incidental U.S. person retention for foreign targeting efficiency) or the founding problem has been displaced by extraction goals (using foreign intelligence authority as a backdoor for domestic surveillance). The theater ratio rising from 0.32 to 0.58 suggests the latter — if the constraint were purely coordination, procedural efficiency would improve over time (theater ratio would fall as minimization became routine); instead, rising theater suggests the *procedures* are becoming increasingly performative (more compliance ritual, less actual exclusion of domestic investigative access). The tangled rope classification holds: the constraint does coordinate foreign collection infrastructure (genuine coordination function), but it does so asymmetrically — using administrative authority to retain U.S. person data and grant warrantless access to the FBI, which is the extraction component. Active enforcement is real: the statute requires reauthorization every 5-6 years, internal compliance reviews, and FISA court approval of targeting procedures (theater); but no constraint on queries of retained U.S. person data for domestic purposes (the extraction mechanism). The constraint avoids becoming a pure snare because the coordination function is genuine; it avoids being a pure rope because the asymmetric extraction (warrantless U.S. person access) requires active defense (procedural theater, restricted disclosure, classified briefings).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foreign_intelligence_purpose_scope,
    'What constitutes a valid ''foreign intelligence purpose'' sufficient to justify FBI query of incidentally collected U.S. person communications? Does it include domestic counterintelligence, criminal investigation with foreign nexus, or only direct foreign intelligence threats?',
    'Detailed disclosure of query decision criteria and aggregate statistics on query purposes (declassified or disclosed in FISA court opinions); litigation testing whether particular query justifications meet statutory bounds.',
    'Narrow scope (direct foreign intelligence only) would reduce extractiveness to ≈0.25 and shift classification toward rope; broad scope (any investigation with foreign element) supports current ≈0.45 extraction and tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_intelligence_purpose_scope, empirical, 'Whether ''foreign intelligence purpose'' is a genuine limitation on query scope or a malleable cover for broad domestic surveillance authority.').

omega_variable(
    incidental_vs_targeted_distinction,
    'How many U.S. persons are subject to warrantless 702 query as incidentally collected data, versus how many are targeted indirectly (U.S. persons in contact with foreign targets where the targeting decision prioritizes U.S. person collection)? Is the distinction meaningful or rhetorical?',
    'Declassification or FISA court disclosure of aggregate statistics on query targets'' nationality, investigative purpose, and relationship to primary foreign targets; audit of query justifications for evidence of deliberate targeting of U.S. persons under foreign intelligence cover.',
    'If incidental collection is genuinely byproduct (small percentage), this reading is more defensible as coordination; if U.S. person querying is primary purpose, the constraint is closer to pure snare (warrantless surveillance of U.S. persons under foreign intelligence framing).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incidental_vs_targeted_distinction, empirical, 'Whether incidental U.S. person surveillance is a side effect or the main operational target of the constraint.').

omega_variable(
    fourth_amendment_applicability,
    'Does the Fourth Amendment warrant requirement apply to queries of incidentally collected U.S. person communications, or does Section 702 statutory authorization validly displace warrant requirements for this category of data?',
    'Supreme Court ruling on Fourth Amendment scope when applied to government queries of retained communications; Congress enacts explicit Fourth Amendment carve-out or rejects it through legislative action.',
    'If Fourth Amendment applies, queries require warrants and extractiveness drops to ≈0.05 (becomes pure rope); if statute validly displaces warrant requirement, current ≈0.45 extraction holds or rises as scope expands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fourth_amendment_applicability, conceptual, 'Whether the constitutional floor reading or incidental collection reading is constitutionally sustainable.').

omega_variable(
    suppression_structural_vs_internalized,
    'The high suppression (0.72) reflects U.S. persons'' structural exclusion from knowledge and remedy processes. Is this exclusion primarily structural (they physically cannot access information about their surveillance) or internalized (they could theoretically know but don''t because notice is administratively denied)?',
    'If notification requirements were enacted and enforced, would U.S. persons develop knowledge, seek remedies, and mount resistance? If so, suppression is structural; if they remain compliant despite notice, it is internalized.',
    'Structural suppression would be difficult to remediate without legislative action mandating notice; internalized suppression might ease if transparency increased, suggesting lower effective suppression than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether U.S. person suppression is structural (built into query processes) or internalized (cognitive acceptance of surveillance).').

omega_variable(
    kernel_contest_foreclosure,
    'Can a single legal framework simultaneously hold that the statute permits warrantless retention/query of incidentally collected U.S. person communications (this reading) AND that the Fourth Amendment mandates warrants for the same searches (constitutional floor reading)? Or does one reading''s core premise logically foreclose the other?',
    'Supreme Court or Congress adjudication of whether Section 702 authorization is constitutionally valid or whether the Fourth Amendment foreclosed the statutory authority from inception.',
    'If one reading forecloses the other, the kernel is contested between incompatible frameworks; if both can coexist in different jurisdictions or through compromise, they coexist without foreclosure. Current law reflects partial foreclosure: courts have upheld 702 queries as consistent with Fourth Amendment (implicitly endorsing this reading), but Fourth Amendment scholars contest that conclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Whether the incidental collection and constitutional floor readings are logically foreclosing or merely contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2008, 0.32).
narrative_ontology:measurement_basis(fisa_tr_t2008, observed).
narrative_ontology:measurement(fisa_tr_t2012, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2012, 0.42).
narrative_ontology:measurement_basis(fisa_tr_t2012, observed).
narrative_ontology:measurement(fisa_tr_t2016, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2016, 0.51).
narrative_ontology:measurement_basis(fisa_tr_t2016, observed).
narrative_ontology:measurement(fisa_tr_t2020, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2020, 0.56).
narrative_ontology:measurement_basis(fisa_tr_t2020, observed).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2024, 0.58).
narrative_ontology:measurement_basis(fisa_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2008, 0.28).
narrative_ontology:measurement_basis(fisa_be_t2008, observed).
narrative_ontology:measurement(fisa_be_t2012, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2012, 0.35).
narrative_ontology:measurement_basis(fisa_be_t2012, observed).
narrative_ontology:measurement(fisa_be_t2016, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2016, 0.41).
narrative_ontology:measurement_basis(fisa_be_t2016, observed).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement_basis(fisa_be_t2020, observed).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2024, 0.45).
narrative_ontology:measurement_basis(fisa_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement_basis(fisa_su_t2008, observed).
narrative_ontology:measurement(fisa_su_t2012, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2012, 0.64).
narrative_ontology:measurement_basis(fisa_su_t2012, observed).
narrative_ontology:measurement(fisa_su_t2016, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement_basis(fisa_su_t2016, observed).
narrative_ontology:measurement(fisa_su_t2020, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(fisa_su_t2020, observed).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(fisa_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__incidental_collection_reading, 0.12).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% The fisa_702_statutory_text kernel (stabilized statute permitting foreign targeting) decomposes into three structurally distinct constraint stories representing different readings of the same statutory language. (1) incidental_collection_reading (this story): permits broad retention/query of U.S. person communications under foreign intelligence justification — high extraction, institutional beneficiaries, powerless U.S. person victims. ε≈0.45. (2) foreign_target_strict_reading: constrains collection to communications where both endpoints are non-U.S. persons abroad, with U.S. person data minimized and inaccessible for domestic purposes — lower extraction, rope classification, structural dispute over query scope. (3) constitutional_floor_reading: asserts Fourth Amendment mandates warrants regardless of foreign intelligence purpose or incidental status — would eliminate extraction if adopted, mountain-like constraint (warrant requirement is structural/unchangeable). Each reading has different ε (0.45 vs. ≈0.22 vs. ≈0.02), different victim sets (U.S. persons universally swept vs. narrowly targeted), and different classifications. They are linked by network: incidental reading influences strict reading (makes broad querying structurally possible, creating pressure on minimization procedures) and is foreclosed by constitutional floor reading (if Fourth Amendment applies, the incidental reading's statutory authority is void). No single framework holds all three simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__incidental_collection_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
