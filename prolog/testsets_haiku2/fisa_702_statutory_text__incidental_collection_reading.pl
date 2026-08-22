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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: FISA Section 702 Incidental Collection and Warrantless Query of U.S. Person Communications
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint instantiates the incidental-collection reading of FISA
 *   Section 702: the statutory text, as interpreted by the executive and the
 *   surveillance apparatus, permits the retention of communications from U.S.
 *   persons incidentally captured during foreign intelligence collection, and
 *   permits law enforcement to query that retained data for domestic criminal
 *   investigations without obtaining warrants — provided the query is
 *   certified as serving a 'foreign intelligence purpose.' This reading
 *   displaces the Fourth Amendment warrant requirement from U.S. persons
 *   whose only connection to surveillance is indirect (communicating with
 *   foreign targets). The foundational claim is that the foreign targeting
 *   statute's authority extends to incidental U.S. person data because Fourth
 *   Amendment protection is subordinated to the foreign intelligence
 *   exception. Sibling readings dispute this: the constitutional-floor
 *   reading asserts the Fourth Amendment applies regardless; the
 *   foreign-target-strict reading asserts the statute constrains collection
 *   to exclude U.S. persons from retained, queryable databases. This story
 *   authors ONLY the incidental-collection reading's constraint structure;
 *   sibling readings are separate stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - intelligence_community: institutional agenda-setter, defines and implements collection policy, minimization procedures, retention authority
 *   - law_enforcement_agencies: institutional beneficiary, accesses 702 database for domestic investigations without warrants
 *   - u_s_persons_subject_to_surveillance: powerless victims, incidentally collected, warrantless query targets, no exit
 *   - congress: institutional agenda-setter, enacted and reauthorizes 702 but operates under constraints (classification, intelligence lobbying, Cold War deference norms)
 *   - judicial_oversight: institutional observer, FISA Court reviews foreign targeting only; district courts constrained by standing and secrecy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.72).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA Section 702 Incidental Collection and Warrantless Query of U.S. Person Communications").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, '133296f9-09ff-41dc-af67-396cddea844e').
narrative_ontology:cs_kernel_codification('133296f9-09ff-41dc-af67-396cddea844e', fixed_text).
narrative_ontology:cs_authority_grounding('133296f9-09ff-41dc-af67-396cddea844e', extraction).
narrative_ontology:cs_interpretation_layer_present('133296f9-09ff-41dc-af67-396cddea844e').
narrative_ontology:cs_reading_relation('133296f9-09ff-41dc-af67-396cddea844e', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('133296f9-09ff-41dc-af67-396cddea844e', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('133296f9-09ff-41dc-af67-396cddea844e', foundational, foreign_intelligence_exception_to_warrant_requirement).
narrative_ontology:cs_axiom_status(foreign_intelligence_exception_to_warrant_requirement, holdable).
narrative_ontology:cs_axiom_grounding('133296f9-09ff-41dc-af67-396cddea844e', foreign_intelligence_exception_to_warrant_requirement, deontological).
narrative_ontology:cs_axiom('133296f9-09ff-41dc-af67-396cddea844e', foundational, incidental_u_s_person_data_within_statutory_scope).
narrative_ontology:cs_axiom_status(incidental_u_s_person_data_within_statutory_scope, holdable).
narrative_ontology:cs_axiom_grounding('133296f9-09ff-41dc-af67-396cddea844e', incidental_u_s_person_data_within_statutory_scope, instrumental).
narrative_ontology:cs_reference_frame('133296f9-09ff-41dc-af67-396cddea844e', statutory_foreign_targeting_authority_with_incidental_collection_retention).
narrative_ontology:cs_drift_state('133296f9-09ff-41dc-af67-396cddea844e', contemporary_post_snowden_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('133296f9-09ff-41dc-af67-396cddea844e', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_community).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, u_s_persons_subject_to_surveillance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the 702 collection apparatus targeting foreign persons abroad. Collects incidentally any U.S. person communications passing through or mentioning the foreign target. Sets the policy for what constitutes 'foreign intelligence purpose' justifying retention and query of incidental U.S. data. Argues the arrangement is essential for national security and that minimization procedures (internal administrative review before access) provide sufficient Fourth Amendment protection.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, intelligence_community, agenda_setter,
    institutional, generational, analytical, national).

% Access 702 database to query incidentally collected U.S. person communications for domestic criminal investigations without seeking warrants. Query authority is gated by 'foreign intelligence purpose' certification, but the certification burden is low and internal. Gain investigative leads, evidence, and intelligence from warrantless access to communications that would otherwise require probable-cause warrants in domestic prosecutions.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, law_enforcement_agencies, beneficiary,
    institutional, generational, constrained, national).

% Have their communications incidentally collected, retained, and queried by law enforcement without warrant, without notice, and without opportunity to challenge the queries. Their communications are captured because they communicate with foreign targets or appear in foreign intelligence reports, placing them in the surveillance stream entirely outside their control or knowledge. They bear the cost of government access to their private communications and cannot exit (they cannot choose not to communicate across borders or with persons abroad).
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, u_s_persons_subject_to_surveillance, payer,
    powerless, biographical, trapped, national).

% Are the nominally intended surveillance subjects; collection is justified as targeting them. Yet the constraint's operative extraction applies to U.S. persons incidentally swept into the collection, not to the foreign targets themselves (who remain outside U.S. legal protection regardless). The foreign targets have no voice in U.S. constitutional disputes but are the justificatory hook for the entire apparatus.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, foreign_targets_abroad, excluded,
    moderate, biographical, constrained, global).

% Enacted FISA Section 702 and retains statutory amendment power. Receives classified briefings on implementation. Conducts oversight hearings. The reading instantiated here construes the statute to permit warrantless retention and query; Congress has the authority to rewrite it but faces structural obstacles (intelligence agency lobbying, secrecy constraints on public debate, Cold War-era deference norms to executive on foreign intelligence).
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% The FISA Court nominally reviews collection operations under 702 but reviews only the foreign targeting component; incidental collection and domestic law enforcement queries are outside its scope. District courts occasionally review Fourth Amendment challenges to evidence derived from 702 queries but are bound by the statutory authorization and the executive's national security assertions. Judicial review is constrained by standing doctrine (many U.S. persons never learn they were surveilled) and Espionage Act secrecy.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, judicial_oversight_bodies, observer,
    institutional, generational, analytical, national).

% Object to warrantless surveillance of U.S. persons and argue the arrangement violates Fourth Amendment rights. Are excluded from the statutory structure that implements 702 (they have no seat at briefings, minimization procedure design, or query authorization). Occasionally mount legal challenges but face high procedural barriers (standing, classification, equitable discretion). Their objections are external pressure, not integrated feedback into the constraint's operation.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, privacy_advocates, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, intelligence_community).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables real-time collection and retention of foreign intelligence by targeting communications of foreign persons abroad; incidental U.S. person data retention allows intelligence analysts to preserve context and avoid re-collection delays.
% TRANSFER_FUNCTION: Moves Fourth Amendment protection from U.S. persons whose communications are collected incidentally: the statutory reading substitutes administrative minimization procedures (internal agency review) for judicial warrants, transferring control of access from courts to executive intelligence officials and law enforcement.
% ABSENT_VOICES: U.S. persons subject to warrantless surveillance cannot participate in or even learn of their inclusion; foreign targets have no U.S. constitutional standing; Congress debates in classification constraints; federal judges are excluded from reviewing the incidental-collection and domestic-law-enforcement access layers. The statutory reading concentrates decision-making authority in the executive agencies it authorizes.
% DISAPPEARANCE_RATIONALE: If this constraint vanished (702 sunset, or Court ruled queries unconstitutional), law enforcement would lose access to warrantless surveillance data for domestic investigations. Intelligence agencies would lose incidental-collection retention authority. Thousands of ongoing investigations and surveillance programs would face probable-cause warrant requirements or termination. The domestic and foreign intelligence apparatus would reorganize around warrant-based collection or higher evidentiary thresholds.
% FOUNDING_PROBLEM: Traditional warrant-based surveillance model is slow for foreign intelligence: overseas targets move rapidly, communications routing changes moment-to-moment, courts are not equipped to issue emergency warrants at intelligence speed. Incidental U.S. person data was understood as inevitable collateral in high-volume overseas collection.
% FOUNDING_PROBLEM_CORROBORATION: The intelligence community attests the problem is live and urgent. Privacy advocates and federal judges (in dissenting and concurring opinions, e.g., In re Telephonic Records Request) attest the problem is either overstated or that the warranted solution is faster warrant procedures, not warrant displacement. Congress has extended 702 repeatedly despite stated concerns; academic analysis from outside the security establishment suggests the speed argument conflates technical delay with constitutional deficiency.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) reflects that the constraint transfers Fourth Amendment protections from U.S. persons to the executive agencies—a substantial extraction, but not total (some minimization procedures exist, not all U.S. persons are equally surveilled). Suppression (0.72) is high because the constraint's persistence depends on keeping surveillance classified, preventing standing for legal challenges, and maintaining political consensus that foreign intelligence justifies warrant displacement; the classification itself is a suppression mechanism. Theater (0.48) reflects that half the visible activity is genuine foreign targeting (the stated justification), half is incidental-U.S.-person retention and law enforcement query (the extractive mechanism riding it). Accessibility collapse (0.68) is substantial: once the 702 apparatus is built and the foreign intelligence exception is accepted, alternatives (warrant requirements, real-time foreign review, data deletion after foreign target ceases) collapse for U.S. persons who cannot opt out of communications. Resistance (0.58) is moderate: significant public and academic criticism, some judicial skepticism, but executive and legislative support have sustained the constraint through multiple reauthorizations. The measurement series shows extractiveness increasing initially (t=0 to t=15: 0.38→0.45) as surveillance scope expands and law enforcement query authority hardens in practice, then stabilizing (t=15 to t=25) as political equilibrium holds.
 *
 * PERSPECTIVAL GAP:
 *   From the intelligence and law enforcement seat, the constraint enables necessary coordination and speed—foreign targets move fast, incidental U.S. person data provides context, minimization procedures provide safeguards. From the U.S. person seat, the constraint is pure extraction—they are unknowingly surveilled, searched without warrant, and denied judicial remedy because they cannot prove they were surveilled (classification prevents them from knowing). The gap is structural, not reconcilable by tweaking procedures. The engine should compute dramatically different types from the two seats: from the beneficiary seat, a coordination-first reading; from the target seat, an extraction-dominant reading. This constraint's claimed type (tangled_rope) reflects that both readings coexist in the statute—genuine foreign targeting coordination (the foreign target seat) and extraction from U.S. persons (the victim seat). The classification question is whether the extraction component overwhelms coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Intelligence agencies and law enforcement are structural beneficiaries (d near 0.0-0.2): they collect, retain, query, and access incidental data at will, with internal certification sufficing (no court warrant needed). U.S. persons subject to surveillance are structural targets (d near 0.8-1.0): they are the subject of warrantless queries, have no knowledge of or control over retention, and cannot exit (they cannot choose not to communicate across borders or mention names that trigger foreign intelligence collection). The directionality for U.S. persons is not derived from power alone—a powerless agent could still be mobile if exit options existed—but from the combination of powerlessness AND trapped exit (incidental collection is involuntary; one cannot opt out by not being related to foreign activity).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophic: the founding problem (speed and necessity of foreign intelligence collection) remains live, and the arrangement (foreign targeting + incidental collection) actively solves it. Law enforcement query without warrants is the extraction layer, not evidence of mandate death. If the mandate were dead (foreign intelligence collection were no longer necessary), the constraint would persist as pure extraction and would be a piton; that is not the case here. The constraint lives because both layers (coordination and extraction) persist. Mandatrophy would be a scenario where the foreign targeting was no longer necessary but the incidental-U.S.-person query machinery persisted out of institutional inertia—that is a sibling scenario worth tracking, but not the structure instantiated by this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foreign_intelligence_purpose_definition,
    'What constitutes ''foreign intelligence purpose'' sufficient to justify law enforcement query of incidental U.S. person data? Is the definition constraining or capstone-like?',
    'Declassified internal guidelines, congressional inquiry into minimization procedure standards, FISA Court analysis of query justifications, or legislative amendment setting explicit thresholds.',
    'If ''foreign intelligence purpose'' is narrowly constraining (e.g., only active counterintelligence investigations), extractiveness remains moderate. If capstone-like (any national security rationale), extractiveness approaches 0.6+. The statutory reading''s stability depends on how tight the definition is in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_intelligence_purpose_definition, empirical, 'Definitional constraint or flexibility of ''foreign intelligence purpose'' gate').

omega_variable(
    fourth_amendment_applicability_to_incidental_data,
    'Does the Fourth Amendment apply to law enforcement searches of incidentally collected U.S. person communications? Or does the foreign intelligence exception displace warrant requirements for data collected (even incidentally) under foreign targeting authority?',
    'Supreme Court ruling on Fourth Amendment scope in 702 queries, or amendment to FISA clarifying constitutional baseline (either restoring warrant requirement or codifying exception).',
    'If Fourth Amendment applies: extractiveness drops to ~0.15 (constitutional protection restores warrant requirement). If foreign intelligence exception displaces it: extractiveness remains ~0.45 (extraction persists as statutorily authorized). This omega dissolves if a sibling reading prevails (constitutional_floor_reading), creating a hard foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fourth_amendment_applicability_to_incidental_data, conceptual, 'Constitutional status of incidental-collection query authority').

omega_variable(
    suppression_mechanism_classification,
    'Is the high suppression score (0.72) driven by classification barriers (structural: legal obstacles to learning surveillance occurred), internalized acceptance by targets (cognitive: belief in necessity), or both?',
    'Prospective study of U.S. persons'' response to actual surveillance notice; analysis of legal challenge rates if classification barriers were lifted; survey of privacy attitudes among exposed populations.',
    'If structural suppression dominates: removing classification lifts the suppression (constraint would face legal challenges). If internalized: suppression persists even after disclosure (targets accept the tradeoff). Mixed: suppression is high but partially removable via transparency. This feeds the theater-ratio interpretation: high theater could indicate routine performance of justification precisely because suppression is partly internalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_classification, empirical, 'Whether suppression is structural (classification), internalized (acceptance), or both').

omega_variable(
    statute_versus_constitutional_interpretation,
    'Does this reading''s scope derive from the statutory text itself, or from executive interpretation of the statute? Could Congress have drafted differently and constrained the reading?',
    'Legislative history analysis, testimony from 702 drafters, comparison to statutory language in other jurisdictions, or amendment clarifying intent.',
    'If the reading is faithful to statutory language: changing it requires legislative amendment (high fixing cost). If the reading is executive overreach: judicial correction or administrative reinterpretation could change it without statute amendment. This affects fixing_cost classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statute_versus_constitutional_interpretation, conceptual, 'Locus of the incidental-collection authority: statutory or interpretive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fisa_tr_t5, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement(fisa_tr_t15, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(fisa_tr_t20, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(fisa_tr_t25, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fisa_be_t5, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(fisa_be_t15, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(fisa_be_t25, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 25, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(fisa_su_t5, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 5, 0.69).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(fisa_su_t15, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(fisa_su_t25, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__incidental_collection_reading, 0.18).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of FISA Section 702. The three readings instantiate structurally distinct claims about the statute's scope and Fourth Amendment applicability: (1) incidental_collection_reading (this story): statutory permission for retention and warrantless query when justified by foreign intelligence purpose; (2) foreign_target_strict_reading: statutory constraint to exclude U.S. persons from retained queryable data; (3) constitutional_floor_reading: Fourth Amendment warrant requirement applies regardless of statutory language. Each reading has distinct ε, beneficiary/victim sets, and classification. The sibling readings are separate constraint stories; they are NOT perspectives on this one. Network links enable the decomposition to be tracked as a kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__incidental_collection_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
