% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA 702 Foreign Target Strict Reading: Statutory Minimization Constraint
 *   domain: constitutional/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint instantiates the foreign_target_strict_reading of the
 *   contested FISA Section 702 statutory kernel. The strict reading
 *   interprets the statute's 'foreign target' language as creating a genuine
 *   coordination mechanism that protects Fourth Amendment rights-holders:
 *   foreign intelligence collection is authorized for communications where
 *   both the sender and the primary investigative interest are non-U.S.
 *   persons abroad; incidentally collected U.S. person communications must be
 *   minimized (deleted rather than merely restricted in access) and are
 *   inaccessible for FBI domestic criminal investigations without
 *   individualized warrant. Under this reading, U.S. persons retain Fourth
 *   Amendment protections and are not victims of the constraint—they are
 *   beneficiaries of the minimization obligation. The constraint's
 *   extractiveness is low because the coordination function (enabling foreign
 *   intelligence) is genuine and the protection mechanism is real, though
 *   contested. The measurement series shows slow creep in extractiveness and
 *   suppression over the interval, reflecting the documented gap between
 *   statutory minimum procedures and actual implementation practice
 *   (increased queries, broader interpretation, delayed minimization).
 *
 * KEY AGENTS:
 *   - Fourth Amendment rights-holders (U.S. persons): benefit from minimization-as-deletion obligation and exclusion from FBI domestic query use; trapped and powerless relative to government authority
 *   - NSA foreign intelligence operations: agenda-setter; administers collection and retention policies, certifies foreign-targeting filters and minimization timelines
 *   - FBI domestic investigative offices: payer; prohibited from querying 702 database for domestic purposes; must build cases through conventional warrant-based means or Title III authority
 *   - Foreign Intelligence Surveillance Court: observer; reviews and certifies targeting and minimization procedures; can reject certifications and order remedies
 *   - Civil liberties advocates: observer; monitor implementation and litigate Fourth Amendment questions; provide external corroboration for rights-centered reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.25).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA 702 Foreign Target Strict Reading: Statutory Minimization Constraint").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, '3f3557bf-b6fa-4321-9c6b-051ff8536562').
narrative_ontology:cs_kernel_codification('3f3557bf-b6fa-4321-9c6b-051ff8536562', fixed_text).
narrative_ontology:cs_authority_grounding('3f3557bf-b6fa-4321-9c6b-051ff8536562', lineage).
narrative_ontology:cs_interpretation_layer_present('3f3557bf-b6fa-4321-9c6b-051ff8536562').
narrative_ontology:cs_reading_relation('3f3557bf-b6fa-4321-9c6b-051ff8536562', fisa_702_statutory_text__incidental_collection_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f3557bf-b6fa-4321-9c6b-051ff8536562', fisa_702_statutory_text__constitutional_floor_reading, influences).
narrative_ontology:cs_axiom('3f3557bf-b6fa-4321-9c6b-051ff8536562', foundational, foreign_targeting_statutory_necessity).
narrative_ontology:cs_axiom_status(foreign_targeting_statutory_necessity, holdable).
narrative_ontology:cs_axiom_grounding('3f3557bf-b6fa-4321-9c6b-051ff8536562', foreign_targeting_statutory_necessity, deontological).
narrative_ontology:cs_axiom('3f3557bf-b6fa-4321-9c6b-051ff8536562', foundational, incidental_us_person_deletion_requirement).
narrative_ontology:cs_axiom_status(incidental_us_person_deletion_requirement, holdable).
narrative_ontology:cs_axiom_grounding('3f3557bf-b6fa-4321-9c6b-051ff8536562', incidental_us_person_deletion_requirement, conventional).
narrative_ontology:cs_reference_frame('3f3557bf-b6fa-4321-9c6b-051ff8536562', statutory_foreign_targeting_with_deletion_minimization).
narrative_ontology:cs_drift_state('3f3557bf-b6fa-4321-9c6b-051ff8536562', contemporary_implementation_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f3557bf-b6fa-4321-9c6b-051ff8536562', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, fourth_amendment_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic_investigative_offices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% U.S. persons whose communications are incidentally collected during foreign intelligence surveillance. Under this reading, they retain Fourth Amendment protections; their communications are minimized (deleted rather than merely restricted), and the government cannot query the 702 database for domestic law enforcement purposes without individualized warrant. Their benefit is protection from warrantless domestic surveillance use of incidentally collected data.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fourth_amendment_rights_holders, beneficiary,
    powerless, generational, trapped, national).

% Structurally prohibited from querying the 702 collection database for domestic criminal investigations under this reading. They bear the cost of building separate investigative cases through conventional warrants or other Title III means rather than accessing incidentally collected communications. The constraint cuts off a shortcut to case development.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic_investigative_offices, payer,
    institutional, biographical, constrained, national).

% Administers the collection and retention of foreign target communications under Section 702 authority. Under this reading, operates under a strict interpretation of foreign-target-only collection: they must design collection filters and retention policies to exclude U.S. persons as primary targets, minimize incidental U.S. person data, and enforce deletion timelines. They maintain the technical infrastructure and compliance procedures.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, nsa_foreign_intelligence_operations, agenda_setter,
    institutional, generational, mobile, global).

% The Foreign Intelligence Surveillance Court reviews and certifies the government's targeting and minimization procedures. Under this reading, they evaluate whether the procedures genuinely enforce foreign-targeting-only collection and verify that U.S. person data minimization is deletion-based rather than access-restricted. They can reject certifications and order remedies.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, surveillance_oversight_courts, observer,
    institutional, generational, analytical, national).

% Non-U.S. person entities abroad whose communications are the intended collection target. They are excluded from the domestic rights framework; this reading does not constrain their collection. However, foreign intelligence partners and adversaries would object to the interpretation that narrowly reads foreign targeting as excluding incidental U.S. person queries, because it limits the intelligence value of the collection.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_targets, excluded,
    powerful, generational, trapped, global).

% Monitor and litigate statutory interpretation and Fourth Amendment implications. Under this reading, they argue that the statutory minimization requirement and the foreign-target-only restriction are minimal protections, not maximum ones—that Fourth Amendment warrant requirements should supersede the statutory floor. They provide external corroboration for the rights-centered interpretation.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables foreign intelligence collection (a genuine coordination problem in national security: multiple agencies need centralized access to communications signals) while creating a constraint mechanism to protect U.S. persons from warrantless domestic surveillance. The constraint solves the problem of separating foreign intelligence collection (where warrant requirements are relaxed) from domestic law enforcement (where Fourth Amendment protections apply).
% TRANSFER_FUNCTION: Moves investigative opportunity away from the FBI's domestic offices (they cannot query the 702 database for domestic purposes without individualized warrant) and toward the NSA's foreign intelligence operations (they control the collection, retention, and access policies under court-certified procedures). The constraint transfers the power to determine what U.S. person communications are retained and accessible away from domestic law enforcement to foreign intelligence officials subject to Foreign Intelligence Surveillance Court oversight.
% ABSENT_VOICES: Foreign governments and foreign intelligence operations that would benefit from less-restrictive incidental collection and downstream querying; foreign intelligence targets whose communications are the intended collection target (they do not participate in U.S. domestic framework). Their position is structurally excluded by design—the reading enacts their exclusion.
% DISAPPEARANCE_RATIONALE: If the foreign-target-strict-reading constraint disappeared and the statutory text were replaced with incidental-collection-reading (permitting FBI queries for domestic purposes), the landscape of U.S. person surveillance would shift: incidentally collected communications would become a warrantless investigative resource for domestic crimes, the incentive structure for targeting foreign communications would expand to include secondary benefits for domestic law enforcement, and the Fourth Amendment's traditional requirement of individualized warrant for U.S. person surveillance would be substantially eroded. Civil liberties litigation and international relations would reorganize around the new baseline of warrantless domestic access.
% FOUNDING_PROBLEM: Post-9/11 foreign intelligence agencies required rapid access to communications of non-U.S. persons abroad for counterterrorism and counterproliferation; simultaneously, Fourth Amendment protections required warrant-based access to U.S. person communications even for foreign intelligence purposes. The statutory foreign-targeting requirement solved the coordination problem by authorizing a separate collection authority for foreign-target communications, with minimization procedures to protect incidentally collected U.S. persons.
% FOUNDING_PROBLEM_CORROBORATION: The NSA and executive branch cite ongoing foreign intelligence threats and the necessity of rapid access to foreign communications. The Foreign Intelligence Surveillance Court certifies the foreign-targeting requirement's operational necessity in published opinions. Civil liberties advocates and academic commentators contest that the foreign-targeting statutory floor is sufficient: they argue the Fourth Amendment independent of the statute requires warrant-based access to any U.S. person communications, even incidentally collected ones. Congressional testimony from both positions (Intelligence Committee vs. Judiciary Committee records) attests the disagreement. No single party's corroboration of the founding problem dominates.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.15–0.25) because the reading posits that the statutory minimization requirement and foreign-targeting constraint serve a real coordination purpose (separating foreign intelligence collection from domestic surveillance) while genuinely protecting Fourth Amendment rights. Suppression is moderate-low (0.25–0.33) because the constraint operates through statutory text and court certification, not raw coercion—U.S. persons cannot violate the minimization obligation (they are not the regulated parties; the government is), but NSA and FBI operate under procedural compliance burdens. Theater ratio is very low (0.10–0.18) because the constraint's coordination function is substantive: foreign intelligence collection genuinely requires different authority than domestic law enforcement. The slow upward drift in all three metrics reflects the documented implementation gap—actual practice diverges from the statutory minimum through: (1) broader interpretation of 'foreign intelligence purpose' to include broader domestic implications, (2) delayed deletion of incidental U.S. person data, (3) increased FBI queries (though formally prohibited for domestic crimes, increasing for intelligence purposes). The gap is structural drift, not theater—the metrics capture the constraint weakening under operational pressure.
 *
 * PERSPECTIVAL GAP:
 *   The NSA and the FBI experience this constraint very differently. From NSA's seat (foreign intelligence focused, global scope), the constraint is enabling—it provides clear statutory authority for collection that would otherwise require individual warrants for each target. From FBI's seat (domestic law enforcement centered), the constraint is restrictive—it categorically prohibits access to a database that would otherwise be a warrantless investigative shortcut. A U.S. person incidentally collected experiences it as protective (their communications are deleted, not retained for domestic use), but that protection is contingent on NSA compliance, which the metrics show is degrading. The engine computes these divergences from the power/exit/scope data: NSA (institutional power, mobile exit, global scope, agenda-setter role) sits at low d; FBI (institutional power, constrained exit due to statutory prohibition, national scope, payer role) sits at higher d; U.S. persons (powerless, trapped, national scope, beneficiary role) sit at low d, protected by design—their directionality is derived from their Fourth Amendment-holder status and the statutory minimization obligation.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons retain Fourth Amendment protections under this reading; they are not extracted from—they are the beneficiaries of the minimization obligation. Their directionality is low (near-beneficiary end) because the constraint protects them from warrantless domestic surveillance use. NSA has moderate directionality (beneficiary-side): it gains foreign intelligence collection authority and control of the minimization procedures, though it bears compliance costs. FBI has higher directionality (payer-side): the statutory foreign-targeting restriction and the categorical prohibition on domestic-crime queries constrain its investigative options. The FBI's exit options are 'constrained' rather than 'trapped'—Congress could repeal the 702 statute or amend it, and courts could interpret it differently, but absent statutory change, the domestic-crime prohibition is binding. The measurement series shows FBI's effective extraction rising (extractiveness creeping up) as the implementation gap widens—actual practice permits more FBI access than the strict reading's statutory text authorizes.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits the structural signature of potential mandatrophy. The founding problem (post-9/11 foreign intelligence urgency) was live at inception (2007); by the measurement interval's end (circa 2025), the terrorist threat landscape has evolved, and the necessity of the 702 foreign-targeting constraint is contested. Civil liberties advocates argue the problem is 'dead'—that terrorism is no longer an existential threat justifying warrantless surveillance—while intelligence agencies argue it remains 'live.' The statutory language treats the problem as live (no sunset clause), but the constraint's theater ratio creeping upward suggests the coordination function is becoming decoupled from the extraction mechanism: the procedure of obtaining a Foreign Intelligence Surveillance Court certification for foreign-targeting is increasingly performative, while the actual use of the database for both foreign intelligence and (illegally but increasingly) domestic purposes continues. This is the mandatrophy pattern: a coordination mechanism (foreign/domestic separation) whose primary function has atrophied (the boundary is eroding in practice) but which persists due to institutional inertia and the benefits to NSA/FBI of maintaining the authority. The corrective would be either genuine judicial enforcement of the strict reading (deletion, not access restriction; no domestic queries) or statutory amendment. Neither is occurring; the constraint is maintained theatrically while the boundary erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incidental_collection_boundary_definition,
    'What scope of query justification falls within ''foreign intelligence purpose'' versus domestic law enforcement, and where is the boundary enforceable?',
    'Judicial review of specific query cases; Foreign Intelligence Surveillance Court published opinions clarifying the limits of ''foreign intelligence purpose'' and whether queries serving dual purposes (foreign intelligence + domestic benefit) cross the boundary; congressional amendment specifying permissible query categories.',
    'If the boundary is enforced narrowly (only queries with primary foreign intelligence nexus permitted), the constraint maintains low extractiveness; if ''foreign intelligence purpose'' is interpreted broadly to include any connection to foreign intelligence, extractiveness rises as FBI access expands under the foreign-intelligence framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_collection_boundary_definition, conceptual, 'The enforceability and scope of the foreign-intelligence-purpose boundary.').

omega_variable(
    minimization_deletion_vs_access_restriction,
    'Does the statutory minimization requirement mandate deletion of incidental U.S. person data, or does it permit retention with access restrictions?',
    'Statutory text interpretation by courts; Foreign Intelligence Surveillance Court review of NSA minimization procedures; congressional amendment clarifying minimization as deletion vs. access control; compliance audits measuring actual deletion timelines.',
    'If minimization means deletion, the constraint achieves genuine Fourth Amendment protection (no retained data for domestic use). If minimization means access restriction, retained data remains available for derivative use or reinterpretation, and extractiveness rises as the protection weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimization_deletion_vs_access_restriction, empirical, 'Whether minimization is operationalized as deletion or access control.').

omega_variable(
    fourth_amendment_warrant_requirement_override,
    'Does the statutory Section 702 authority override the Fourth Amendment''s warrant requirement for U.S. person communications, or does the Fourth Amendment apply independent of the statute?',
    'Supreme Court decision on 702 constitutionality; Foreign Intelligence Surveillance Court ruling on the constitutional floor; congressional amendment explicitly addressing Fourth Amendment compliance.',
    'If the Fourth Amendment applies independently, the strict reading''s low extractiveness is correct—U.S. persons retain warrant-based protections. If Section 702 authority overrides warrant requirements by legislative design, extractiveness rises and U.S. persons become victims rather than beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fourth_amendment_warrant_requirement_override, conceptual, 'Whether statutory foreign-targeting authority can override Fourth Amendment protections or merely complements them.').

omega_variable(
    foreign_target_definition_creep,
    'How broadly is ''foreign target'' defined by NSA certification, and does the certification process permit expansion without statutory amendment?',
    'Foreign Intelligence Surveillance Court review of targeting certifications over time; Freedom of Information Act disclosure of certification language and internal guidance; congressional oversight of definition breadth.',
    'If ''foreign target'' definition remains narrow (non-U.S. person with clear foreign intelligence nexus), the constraint preserves the foreign/domestic boundary. If the definition expands to include anyone with any connection to a foreign entity, the boundary erodes and extractiveness rises as incidental collection of U.S. persons increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_target_definition_creep, empirical, 'Whether the foreign-target definition remains stable or expands through administrative interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(fisa_tr_t0, observed).
narrative_ontology:measurement(fisa_tr_t5, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(fisa_tr_t5, observed).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(fisa_tr_t10, observed).
narrative_ontology:measurement(fisa_tr_t15, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement_basis(fisa_tr_t15, observed).
narrative_ontology:measurement(fisa_tr_t20, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(fisa_tr_t20, observed).
narrative_ontology:measurement(fisa_tr_t25, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(fisa_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(fisa_be_t0, observed).
narrative_ontology:measurement(fisa_be_t5, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement_basis(fisa_be_t5, observed).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement_basis(fisa_be_t10, observed).
narrative_ontology:measurement(fisa_be_t15, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 15, 0.2).
narrative_ontology:measurement_basis(fisa_be_t15, observed).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(fisa_be_t20, observed).
narrative_ontology:measurement(fisa_be_t25, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 25, 0.25).
narrative_ontology:measurement_basis(fisa_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(fisa_su_t0, observed).
narrative_ontology:measurement(fisa_su_t5, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement_basis(fisa_su_t5, observed).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement_basis(fisa_su_t10, observed).
narrative_ontology:measurement(fisa_su_t15, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 15, 0.29).
narrative_ontology:measurement_basis(fisa_su_t15, observed).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement_basis(fisa_su_t20, observed).
narrative_ontology:measurement(fisa_su_t25, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 25, 0.33).
narrative_ontology:measurement_basis(fisa_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__foreign_target_strict_reading, 0.1).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% The FISA Section 702 statutory kernel decomposes into three structurally distinct constraint stories: (1) foreign_target_strict_reading (this file)—interprets the statute as creating genuine foreign/domestic separation and Fourth Amendment protection, with ε≈0.15; (2) incidental_collection_reading—interprets the statute as permitting retention and warrantless query of incidentally collected U.S. person communications, with ε≈0.60; (3) constitutional_floor_reading—Fourth Amendment warrant requirement applies independent of statute, with ε≈0.05 for U.S. persons (full protection) or ε≈0.70 for foreign intelligence operations (severe constraint). The three readings share a single kernel (the statutory text) but instantiate different ε values, beneficiary/victim structures, and claim types because they embody different legal and normative frameworks for interpreting the statute and its constitutional implications. The contest is not empirical (what does the statute say) but interpretive (what does it mean and what constraints does it create). The network edges record the conceptual influences: the strict reading influences both siblings by establishing a baseline; the constitutional floor reading forecloses the incidental reading if courts adopt the Fourth Amendment priority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
