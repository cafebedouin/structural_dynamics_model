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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA 702 Foreign Target Statutory Constraint (Strict Reading)
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This constraint is the strict textual reading of the FISA Section 702
 *   'foreign target' predicate: collection must be directed at non-U.S.
 *   persons abroad with a foreign intelligence purpose; U.S. person
 *   communications captured incidentally must be minimized (deleted, not
 *   merely restricted) and cannot be queried for domestic criminal
 *   investigations without separate warrants. This reading prioritizes Fourth
 *   Amendment protection for U.S. persons and treats the foreign-target
 *   requirement as a binding substantive limit on collection scope, not
 *   merely a procedural targeting criterion. It is one of three structurally
 *   distinct readings of the same statutory text (the kernel is the text
 *   itself; the readings are competing interpretations of what that text
 *   permits and requires). This constraint describes what the law SAYS when
 *   read to maximize U.S. person protections; it does not describe current
 *   operational practice or what courts have held the statute permits.
 *
 * KEY AGENTS:
 *   - incidentally_collected_us_persons: Fourth Amendment rights-holders; the beneficiary class under this reading because the constraint's force is their protection
 *   - foreign_target_persons_abroad: powerless, targeted; bear the cost of collection but have no legal recourse
 *   - fbi_intelligence_operations: agenda-setter; must operationally enforce the foreign-target predicate and minimize U.S. person data; cannot use 702 for domestic crimes
 *   - fisa_court: observer/enforcer; verifies targeting justifications and minimization compliance
 *   - congress_oversight_committees: observer/potential override authority; can demand visibility into compliance and change the constraint via legislation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.12).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA 702 Foreign Target Statutory Constraint (Strict Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, 'c8da4ffe-eec7-4337-9800-0a500d43c67a').
narrative_ontology:cs_kernel_codification('c8da4ffe-eec7-4337-9800-0a500d43c67a', fixed_text).
narrative_ontology:cs_authority_grounding('c8da4ffe-eec7-4337-9800-0a500d43c67a', lineage).
narrative_ontology:cs_interpretation_layer_present('c8da4ffe-eec7-4337-9800-0a500d43c67a').
narrative_ontology:cs_reading_relation('c8da4ffe-eec7-4337-9800-0a500d43c67a', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('c8da4ffe-eec7-4337-9800-0a500d43c67a', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('c8da4ffe-eec7-4337-9800-0a500d43c67a', foundational, minimization_requires_deletion).
narrative_ontology:cs_axiom_status(minimization_requires_deletion, holdable).
narrative_ontology:cs_axiom_grounding('c8da4ffe-eec7-4337-9800-0a500d43c67a', minimization_requires_deletion, deontological).
narrative_ontology:cs_axiom('c8da4ffe-eec7-4337-9800-0a500d43c67a', foundational, fourth_amendment_threshold_governs_scope).
narrative_ontology:cs_axiom_status(fourth_amendment_threshold_governs_scope, holdable).
narrative_ontology:cs_axiom_grounding('c8da4ffe-eec7-4337-9800-0a500d43c67a', fourth_amendment_threshold_governs_scope, deontological).
narrative_ontology:cs_reference_frame('c8da4ffe-eec7-4337-9800-0a500d43c67a', statutory_foreign_target_predicate).
narrative_ontology:cs_drift_state('c8da4ffe-eec7-4337-9800-0a500d43c67a', contemporary_post_snowden_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8da4ffe-eec7-4337-9800-0a500d43c67a', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, domestic_privacy_protected_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, incidentally_collected_us_persons).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, foreign_target_persons_abroad).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-U.S. persons abroad whose communications are the lawful collection focus under the foreign-target predicate. Under this reading, they have no Fourth Amendment protection and bear the cost of surveillance; the constraint is meant to ensure they are the ONLY targets, with incidental U.S. person exposure minimized. They cannot opt out, challenge collection retrospectively through U.S. courts, or verify minimization practices.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_target_persons_abroad, payer,
    powerless, immediate, trapped, global).

% U.S. persons whose communications may be incidentally captured when communicating with foreign targets. Under this strict reading, they retain Fourth Amendment protections: their data must be minimized (treated as deletion rather than mere access restriction), must not be queried for domestic investigative purposes, and can only be retained when necessary to understand the foreign intelligence context. They can challenge warrantless query as unconstitutional; their situation improves under this reading relative to alternatives.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, incidentally_collected_us_persons, beneficiary,
    moderate, biographical, constrained, national).

% Administers and enforces the collection targeting rules and minimization procedures. Must justify collection against the foreign-target predicate, must obtain separate warrants for queries against U.S. person data for criminal investigations, must operationally separate counterintelligence from domestic crime operations. The reading constrains their flexibility: they cannot use 702 queries to investigate domestic crimes without individualized warrants, and they cannot retain U.S. person data as a searchable reservoir for future domestic needs.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fbi_intelligence_operations, agenda_setter,
    institutional, generational, constrained, national).

% Interprets and enforces statutory minimization requirements on certifications submitted by intelligence agencies. Under this reading, the court's role is to verify that collection targeting complies with the foreign-target predicate and that minimization procedures actually delete (not merely restrict access to) incidentally collected U.S. person communications. The court can reject certifications or impose more restrictive minimization protocols.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fisa_court, observer,
    institutional, generational, analytical, national).

% Receives classified briefings on 702 collection scope, targeting criteria, and minimization compliance. Under this reading, they can demand that the FBI demonstrate that queries of the 702 database are limited to authorized foreign intelligence purposes and can investigate or legislate if domestic crime queries are discovered.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congress_oversight_committees, observer,
    powerful, generational, analytical, national).

% Non-U.S. governments whose nationals may be foreign targets or incidentally present in communications with U.S. persons. They are excluded from the U.S. legal framework and from challenging collection; they would argue for reciprocal restrictions or data protection agreements but have no formal voice in defining the constraint.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_governments, excluded,
    powerful, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates lawful foreign intelligence collection (targeting non-U.S. persons abroad with a foreign intelligence purpose) from domestic surveillance of U.S. citizens (which requires individualized warrants); prevents the foreign targeting authority from becoming a back door to domestic crime investigation without judicial oversight.
% TRANSFER_FUNCTION: Transfers investigative authority from foreign-target collection (where no warrant is required for foreign targets) to the foreign intelligence agencies that operate globally; restricts domestic law enforcement (FBI criminal divisions, local police) from querying collected data for domestic crimes without obtaining separate warrants; transfers the cost of demonstrating foreign-target status to the collecting agency.
% ABSENT_VOICES: Foreign nationals abroad and foreign governments have no seat in U.S. law and cannot formally object; domestic civil liberties organizations and privacy advocates would argue that minimization is inadequate even under this strict reading and that incidental collection of any U.S. person communications should trigger warrant requirements before collection, not just before query; they are present in litigation and legislation but not in the intelligence community's internal certifications.
% DISAPPEARANCE_RATIONALE: If this constraint vanished and the foreign-target predicate were erased, the FBI and intelligence agencies could collect and query any communications mentioning a foreign person with no targeting predicate and no minimization requirement; domestic law enforcement could access the database freely for any criminal investigation; the risk surface for U.S. persons would expand dramatically because the incidental-collection constraint is the only gate preventing 702 from becoming an unrestricted domestic surveillance tool.
% FOUNDING_PROBLEM: Foreign intelligence collection requires rapid, voluminous surveillance of non-U.S. persons abroad; obtaining individualized warrants for each target would be operationally infeasible at scale and would reveal U.S. intelligence priorities to the targets themselves. Statutory targeting language and minimization requirements were designed to permit efficient foreign intelligence gathering while preserving Fourth Amendment protections for U.S. persons incidentally present in collected communications.
% FOUNDING_PROBLEM_CORROBORATION: The intelligence community attests the operational need for rapid foreign targeting remains live and that warrant requirements for each foreign target would cripple foreign intelligence collection. Courts, in Clapper v. Amnesty International (2013) and subsequent FISA opinions, have acknowledged the operational necessity of foreign intelligence authority without individualized warrants for foreign targets. Civil liberties organizations and the Privacy and Civil Liberties Oversight Board dispute whether minimization is ADEQUATE but do not dispute that foreign targeting serves a legitimate foreign intelligence function — the dispute is over the constraint's stringency and enforcement, not the founding problem itself.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).

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
 *   Extractiveness is low (0.15) because the constraint, when strictly enforced, preserves Fourth Amendment protections for U.S. persons and categorically forbids using 702 data for domestic crimes without warrants — that is a genuine protection, not a veneer. Suppression is also low (0.12) because the constraint's force comes from legal doctrine (Fourth Amendment, statutory text), not from coercion machinery or suppression of exits — U.S. persons can challenge warrantless queries in court and can invoke privacy expectations. Theater is minimal (0.08) because the minimization requirement (deletion, not access restriction) and the domestic-crime-query prohibition are operational rules, not performative theater. Accessibility collapse is high (0.92) because once the foreign-target targeting framework is understood, U.S. persons cannot easily exit or challenge their incidental exposure — they cannot know whether they are incidentally collected, cannot demand deletion before it happens, and cannot sue for the collection itself (only for queries or use that violate the constraint). The measurements show stability because this is the statutory baseline: under this reading, the constraint's force is structural (Fourth Amendment doctrine, statutory text), not dependent on enforcement intensity that varies over time. Enforcement machinery is stable because the FISA Court reviews certifications annually under a fixed statutory standard.
 *
 * PERSPECTIVAL GAP:
 *   The FBI's perspective: this reading is operationally restrictive — they cannot use 702 queries for domestic crimes, cannot retain U.S. person data as an investigative reservoir, and must build separate cases against U.S. persons using traditional warrants. Their directionality is upward (d≈0.65): they bear costs (operational constraints, need for separate warrants) and collect a benefit (foreign intelligence authority without individual warrants for foreign targets), but the constraint pushes them toward beneficiary-side behavior (restraint). The incidentally-collected U.S. person perspective: this reading is protective — it guarantees deletion of their data (not mere access restriction) and forbids domestic repurposing without warrants. Their directionality is downward (d≈0.25): they bear no cost (the constraint protects them) and collect a benefit (legal protection). The FISA Court's perspective: they must verify compliance and can reject certifications, putting them in an oversight role — d≈0.5, symmetric. Foreign targets have no legal standing — their directionality is undefined in U.S. constitutional law.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are incidentally_collected_us_persons because the constraint's entire force is their protection. They are domestic, moderate power (organized civil liberties groups and litigation networks, though individual U.S. persons are powerless), and the constraint guarantees them a benefit (minimization, warrant requirement for queries) with no corresponding cost. The FBI is the constrained institutional actor (d≈0.65 toward target, high institutional power, exit_options=constrained because they cannot abandon foreign intelligence collection but must accept the Fourth Amendment ceiling). Foreign target persons abroad are payers in the sense that they are the surveillance targets, but they have no legal standing in U.S. courts and no participation in the constraint's definition, so their role is analytical rather than volitional.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading dissolves the mandatrophy risk because the constraint's founding problem is still live: foreign intelligence collection requires rapid, voluminous surveillance of non-U.S. persons abroad and is operationally necessary. The constraint persists because it serves a real coordination function — it permits foreign targeting while protecting U.S. persons. However, a deeper mandatrophy question lurks: if operational practice diverges from this strict reading (if FBI actually queries 702 data for domestic crimes, or if minimization is access restriction rather than deletion), the constraint persists as law but atrophies as practice — that is the kernel contest. This story instantiates only the strict reading, which is Rope because the coordination function is real. If the engine finds that practice contradicts this reading substantially, that finding should trigger investigation of the rival readings (incidental_collection_reading, constitutional_floor_reading), not reclassification of this story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimization_definition_ambiguity,
    'Does ''minimization'' require deletion of incidentally collected U.S. person communications, or merely restricted access and prohibition on use for specific purposes?',
    'FISA Court opinions, FBI operating guidelines, and statutory text (50 U.S.C. § 1801(h)). The statute does not define the term; the FISA Court''s minimization procedures are classified.',
    'If minimization is deletion, the constraint protects U.S. persons substantially (extractiveness remains ~0.15). If minimization is access restriction only, U.S. person data persists in the database as a searchable reservoir and extractiveness jumps to ~0.55 (the incidental_collection_reading). This is the primary structural ambiguity between the strict reading and its rival.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimization_definition_ambiguity, empirical, 'Whether minimization procedures delete or restrict access to incidentally collected U.S. person data').

omega_variable(
    domestic_crime_query_prohibition_scope,
    'Does the statutory foreign-target predicate prohibit FBI queries of the 702 database for all domestic crimes, or only for crimes unrelated to foreign intelligence?',
    'FISA Court decisions, Congressional testimony, operational guidelines, litigation discovery in cases where 702 data was used.',
    'If prohibited entirely for domestic crimes, the constraint prevents 702 from becoming a backdoor surveillance system for domestic law enforcement (extractiveness remains low). If permitted when ''incidental'' or ''related'' to foreign intelligence, the database becomes usable for domestic investigations with minimal judicial oversight (extractiveness climbs to ~0.50). This distinction separates the strict reading from the incidental_collection_reading operationally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_crime_query_prohibition_scope, empirical, 'Whether the foreign-target predicate categorically forecloses domestic crime queries or permits incidental use').

omega_variable(
    kernel_reading_contestation,
    'Is this strict reading the authoritative interpretation of the FISA 702 statute, or one reading in a live triadic contest (strict vs. incidental vs. constitutional floor)?',
    'Federal court decisions on 702, Supreme Court review (if it occurs), Congressional action, operational disclosure in litigation or oversight.',
    'If courts or Congress affirm this reading, it becomes the binding law and extractiveness remains ~0.15. If courts adopt the incidental_collection_reading operationally, this constraint persists as statutory text but atrophies as practice — a piton candidate (theater_ratio rises to ~0.60). If courts adopt the constitutional_floor_reading, it supersedes this constraint as a higher ceiling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether this reading is the authoritative interpretation of the statutory text or one contending reading in a live kernel contest').

omega_variable(
    fourth_amendment_incidental_collection,
    'Does the Fourth Amendment protect incidentally collected U.S. person communications from warrantless query even when the initial foreign targeting was lawful?',
    'Supreme Court decision on whether 702 queries constitute new searches triggering warrant requirements (Carpenter v. United States doctrine applied to electronic communications).',
    'If yes (the constitutional_floor_reading), the Fourth Amendment itself requires warrants for any 702 query, making the statutory predicate a minimum baseline that the Constitution exceeds. If no (the current operational status), this statute is the controlling ceiling. This is the doctrinal boundary between this reading and the constitutional_floor_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fourth_amendment_incidental_collection, empirical, 'Whether the Fourth Amendment imposes a warrant requirement for 702 queries independent of statutory predicate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(fisa_tr_t0, observed).
narrative_ontology:measurement(fisa_tr_t4, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement_basis(fisa_tr_t4, observed).
narrative_ontology:measurement(fisa_tr_t8, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 8, 0.08).
narrative_ontology:measurement_basis(fisa_tr_t8, observed).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 12, 0.08).
narrative_ontology:measurement_basis(fisa_tr_t12, observed).
narrative_ontology:measurement(fisa_tr_t16, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 16, 0.08).
narrative_ontology:measurement_basis(fisa_tr_t16, observed).
narrative_ontology:measurement(fisa_tr_t20, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(fisa_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(fisa_be_t0, observed).
narrative_ontology:measurement(fisa_be_t4, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 4, 0.16).
narrative_ontology:measurement_basis(fisa_be_t4, observed).
narrative_ontology:measurement(fisa_be_t8, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement_basis(fisa_be_t8, observed).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 12, 0.15).
narrative_ontology:measurement_basis(fisa_be_t12, observed).
narrative_ontology:measurement(fisa_be_t16, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 16, 0.15).
narrative_ontology:measurement_basis(fisa_be_t16, observed).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(fisa_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.14).
narrative_ontology:measurement_basis(fisa_su_t0, observed).
narrative_ontology:measurement(fisa_su_t4, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 4, 0.13).
narrative_ontology:measurement_basis(fisa_su_t4, observed).
narrative_ontology:measurement(fisa_su_t8, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 8, 0.12).
narrative_ontology:measurement_basis(fisa_su_t8, observed).
narrative_ontology:measurement(fisa_su_t12, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 12, 0.12).
narrative_ontology:measurement_basis(fisa_su_t12, observed).
narrative_ontology:measurement(fisa_su_t16, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 16, 0.12).
narrative_ontology:measurement_basis(fisa_su_t16, observed).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(fisa_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__foreign_target_strict_reading, 0.1).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structural readings of the FISA 702 statutory text. All three share the same kernel (the statute, 50 U.S.C. § 1881a), but instantiate different ε values and beneficiary/victim structures based on how that text is interpreted. The strict_reading treats the foreign-target predicate as binding and minimization as deletion, resulting in low extractiveness (~0.15) and protection for U.S. persons. The incidental_collection_reading treats 'minimization' as access restriction, allowing warrantless queries of U.S. person data for foreign intelligence purposes, resulting in higher extractiveness (~0.55). The constitutional_floor_reading treats the Fourth Amendment as controlling regardless of the statute, requiring warrants for any U.S. person query, resulting in the most protective regime (~0.05 extractiveness but with a constraint on the targeting authority itself). These are not competing measurements of the same constraint; they are competing legal interpretations of a contested statutory text, each with its own structural logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__foreign_target_strict_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
