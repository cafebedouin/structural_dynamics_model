% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: Fourth Amendment Constitutional Floor for 702 Queries
 *   domain: constitutional/legal/surveillance
 *
 * SUMMARY:
 *   This constraint instantiates a constitutional-minimalist reading of the
 *   Fourth Amendment applied to Section 702 database queries. The reading
 *   holds that regardless of the Foreign Intelligence Surveillance Act's
 *   statutory language, the Constitution independently requires probable
 *   cause warrants for any government search of U.S. person communications
 *   content, and that 702 queries constitute searches triggering the warrant
 *   requirement regardless of whether the collection itself was
 *   foreign-targeted or incidental. This reframes 702 from a foreign
 *   intelligence statute into a constitutional criminal procedure question.
 *   The constraint is NOT claimed as a snare or tangled_rope despite low
 *   extractiveness: the low extractiveness reflects constitutional minimalism
 *   (the constraint extracts little from anyone because it is enforcing a
 *   protection, not enabling one). The beneficiary is the U.S. person whose
 *   communications are protected; the payer is the executive agency whose
 *   operational speed and secrecy preferences are constrained.
 *
 * KEY AGENTS:
 *   - u_s_persons_subject_to_query — beneficiary (protected by warrant requirement) / trapped exit (cannot opt out of government databases)
 *   - fisa_court — agenda_setter (approves each query under warrant standard) / institutional power
 *   - executive_intelligence_agencies — payer (bear procedural delays and transparency costs) / institutional power / constrained exit (cannot ignore court orders)
 *   - congress — observer (can amend statute but absent from case-by-case enforcement) / institutional power
 *   - privacy_advocacy_organizations — beneficiary + observer (vindicate the legal theory; no administrative role)
 *   - foreign_targets_abroad — excluded (not protected; affected indirectly by query delays)
 *   - incidental_u_s_person_data_subjects — beneficiary (protected even when collected incidentally) / trapped exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.15).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Constitutional Floor for 702 Queries").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional/legal/surveillance").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '35187711-2ee8-4be8-8f65-d2ac43de8ab9').
narrative_ontology:cs_kernel_codification('35187711-2ee8-4be8-8f65-d2ac43de8ab9', fixed_text).
narrative_ontology:cs_authority_grounding('35187711-2ee8-4be8-8f65-d2ac43de8ab9', lineage).
narrative_ontology:cs_interpretation_layer_present('35187711-2ee8-4be8-8f65-d2ac43de8ab9').
narrative_ontology:cs_reading_relation('35187711-2ee8-4be8-8f65-d2ac43de8ab9', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('35187711-2ee8-4be8-8f65-d2ac43de8ab9', fisa_702_statutory_text__foreign_target_strict_reading, forecloses).
narrative_ontology:cs_axiom('35187711-2ee8-4be8-8f65-d2ac43de8ab9', foundational, fourth_amendment_warrant_requirement_unexcepted).
narrative_ontology:cs_axiom_status(fourth_amendment_warrant_requirement_unexcepted, holdable).
narrative_ontology:cs_axiom_grounding('35187711-2ee8-4be8-8f65-d2ac43de8ab9', fourth_amendment_warrant_requirement_unexcepted, deontological).
narrative_ontology:cs_axiom('35187711-2ee8-4be8-8f65-d2ac43de8ab9', foundational, warrant_requirement_independent_of_collection_legitimacy).
narrative_ontology:cs_axiom_status(warrant_requirement_independent_of_collection_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('35187711-2ee8-4be8-8f65-d2ac43de8ab9', warrant_requirement_independent_of_collection_legitimacy, deontological).
narrative_ontology:cs_reference_frame('35187711-2ee8-4be8-8f65-d2ac43de8ab9', fourth_amendment_as_constitutional_floor).
narrative_ontology:cs_drift_state('35187711-2ee8-4be8-8f65-d2ac43de8ab9', contemporary_foreign_intelligence_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('35187711-2ee8-4be8-8f65-d2ac43de8ab9', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, u_s_persons_subject_to_query).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, privacy_advocacy_organizations).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, incidental_u_s_person_data_subjects).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_intelligence_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% U.S. citizens and persons whose communications are stored in the NSA's Section 702 databases. Under this reading, they benefit from a warrant requirement that limits access to their communications to individualized searches with probable cause findings. Their communications may be in the database as incidental collection, and this reading protects against warrantless rummaging through those communications.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, u_s_persons_subject_to_query, beneficiary,
    powerless, biographical, trapped, national).

% A specialized court authorized to approve foreign intelligence surveillance. Under this reading, the FISA Court must conduct Fourth Amendment probable cause review for every database query seeking to access U.S. person communications, even when the query targets non-U.S. persons. The Court's role shifts from certifying the lawfulness of the original collection to adjudicating each access decision as a potential search.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, analytical, national).

% NSA, FBI, CIA, and other agencies executing foreign intelligence surveillance under Section 702. This reading imposes a warrant requirement for database queries, adding procedural delays and transparency to access decisions that under the current statutory reading can be made without individualized judicial approval. They bear the compliance and reporting costs, and lose speed/operational flexibility.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_intelligence_agencies, payer,
    institutional, generational, constrained, global).

% The legislative body that enacted Section 702 and retains authority to amend it. Under this reading, Congress must decide whether to accept that its statute requires warrant pre-authorization for 702 queries, or rewrite the statute explicitly to carve out a warrant exception for foreign intelligence collection. The reading forces an active legislative choice rather than executive-dominated interpretation.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress, observer,
    institutional, generational, analytical, national).

% Organizations litigating for Fourth Amendment protections and transparency. This reading vindicates their legal theory and creates a framework for ongoing court review of query practices. They do not administrate the constraint but have standing to enforce it through litigation and public pressure.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, privacy_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, privacy_advocacy_organizations, observer).

% Non-U.S. persons located abroad who are the primary targets of Section 702 collection. This reading does not extend warrant protection to their communications, but does complicate the query process for any communications involving U.S. persons, which may incidentally limit access to some foreign-target data. They are excluded from the warrant benefit but affected by its procedural consequences.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, foreign_targets_abroad, excluded,
    powerless, biographical, trapped, global).

% U.S. persons whose communications are incidentally collected because they communicate with foreign targets. Under current practice, their data is retained and searchable without warrant. This reading extends warrant protection to queries accessing their communications, whether the communication was collected incidentally or as a target.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, incidental_u_s_person_data_subjects, beneficiary,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__constitutional_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__constitutional_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform Fourth Amendment floor for all access to U.S. person communications in government databases, replacing a divided statutory regime where foreign intelligence collection escapes warrant requirements that apply to domestic criminal surveillance. The coordination is normative: it solves the problem of inconsistent privacy protection across different collection authorities by imposing constitutional constraint uniformly.
% TRANSFER_FUNCTION: Transfers from executive speed and operational secrecy to judicial oversight and transparency: every query triggering U.S. person data access must be submitted to the FISA Court for probable cause review, generating a reviewable record and delaying access. The transfer is from executive flexibility to constitutional constraint.
% ABSENT_VOICES: Foreign governments and foreign targets are excluded; they have no seat at the table, though they are affected by query delays and the complication of accessing communications involving incidental U.S. persons. Congress could speak through statutory amendment but is largely absent from the continuous application of the rule — the FISA Court and executive agencies negotiate the constraint in practice without direct Congressional oversight per query.
% DISAPPEARANCE_RATIONALE: Under this reading, if the warrant requirement disappeared overnight, the executive would regain warrantless access to all U.S. person communications in 702 databases, restoring the operational speed and secrecy preferences the current statutory reading provides. The world would not 'rearrange' but would shift to a baseline of executive discretion subject only to statutory minimization. Contestation arises because the foreign intelligence community argues the warrant requirement would degrade national security collection; privacy advocates argue the warrant requirement is constitutionally mandatory and disappearance would be illegal. A further contest: does the Constitution actually require warrants, or is disappearance of this constraint merely disappearance of a judicial policy preference not grounded in constitutional text?
% FOUNDING_PROBLEM: The statutory language of Section 702 does not explicitly require warrant authorization for queries into the database of collected communications, permitting executive agencies to search U.S. person communications without individualized judicial approval when the collection itself was lawful under the 'foreign targeting' authorization. This reading identifies the problem as a constitutional gap: statutory silence on warrant requirements should not override Fourth Amendment protections when the practical effect is unfettered access to communications.
% FOUNDING_PROBLEM_CORROBORATION: Privacy advocates and civil liberties organizations, outside the executive agencies executing 702, attest the warrant gap is a real constitutional problem requiring correction. The National Security Agency, FBI, and executive branch national security officials attest the warrant requirement would impair intelligence operations without meaningful privacy gain since the original collection was already authorized and constrained by statutory foreign-targeting requirements. Federal judges have split on whether the founding problem is genuine or mischaracterized; the FISA Court has not formally adopted this reading, though some FISA judges have expressed skepticism about unrestricted query access. Congressional intelligence committees have stated the warrant requirement is not constitutionally mandated, but this statement comes from the beneficiary side of the current arrangement.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, contested).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is LOW (0.25) because this reading functions as a constitutional protection, not an extraction mechanism. The Fourth Amendment warrant requirement does not extract value from anyone; it constrains executive extraction. The baseline extractiveness score reflects the executive's loss of unilateral access speed and secrecy — measured as a cost imposed on the executive preference set, not as a benefit flowing from the constraint to a collector. Suppression is low (0.15) because the warrant requirement does not suppress dissent; it requires transparent court review and creates a record. Theater ratio is minimal (0.05) because the constraint is substantive: every query genuinely must clear the warrant hurdle; there is little performative activity independent of the functional requirement. Accessibility of alternatives is moderate-high (0.72 at base) because statutory reinterpretation or Congressional amendment could shift the rule, and the foreign intelligence collection community has alternative (though more constrained) pathways. Resistance is high (0.68) because the executive and national security establishment actively contest this reading and argue the warrant requirement would degrade collection.
 *
 * PERSPECTIVAL GAP:
 *   Executive agencies and the foreign intelligence community: read this constraint as an extraconstitutional interference with the Foreign Intelligence Surveillance Act's statutory scheme. They argue Congress explicitly authorized foreign targeting collection without domestic-warrant constraints, and the Fourth Amendment was never intended to require warrants for foreign intelligence surveillance. From their seat, the constraint appears as a judicial overreach imposing procedural extraction (mandatory court approval) on a statutory grant of authority. Privacy advocates and civil-liberties litigators: read this constraint as enforcing the Fourth Amendment's core function — probable cause before government searches — uniformly across all search contexts, including foreign intelligence. They argue the Constitution does not contain a foreign intelligence exception, and statutory silence does not override constitutional requirements. From their seat, the constraint appears as belated protection finally applied. The FISA Court: occupies an institutional middle ground. It gains authority to approve queries but also confronts a potential institutional crisis if forced to refuse warrants the executive considers operationally essential. The Court may read this constraint as either appropriate separation-of-powers enforcement or as an unwelcome delegation of policy conflict to the judiciary.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons are the structural beneficiaries (they receive warrant protection; directionality d near 0.0). Executive agencies are the targets (they lose operational discretion; d near 1.0). The FISA Court sits near symmetric (d ≈ 0.5) — it gains institutional authority and caseload but is also burdened with additional review and potential conflict with executive branch. Congress is analytical (observer role, not directly bound). The divergence in computed types per seat arises because the executive sits as a payer facing high d (extraction of procedural constraint) while the U.S. person sits as a beneficiary with low d (protection granted). Under the executive's seat, the constraint computes as substantially extractive to their preferences; under the U.S. person's seat, it computes as protective (negative extraction). This is the core perspectival gap: executive reads 'constitutional interference with foreign intelligence'; U.S. person reads 'constitutional protection finally applied'.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy. The founding problem (warrant gap in statutory 702) is contested but not obsolete: privacy advocates argue it is live and urgent; the executive argues it is mischaracterized but does not claim the founding problem no longer exists. The constraint's justification is its function — enforcing Fourth Amendment protections — not a transitional role now outgrown. If evidence emerged that warrant pre-authorization actually reduced surveillance effectiveness without privacy gains, or conversely that it reliably protected U.S. persons without operational harm, the constraint's legitimacy would shift but not its mandate. The disappearance_verdict is contested, not defeated, which is appropriate for a reading in live legal contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fourth_amendment_foreign_intelligence_scope,
    'Does the Fourth Amendment''s text, original public meaning, or precedent establish that warrant requirements apply to foreign intelligence surveillance, or does ''the people'' and ''persons'' in the Amendment implicitly exclude non-U.S. persons or foreign intelligence contexts?',
    'Supreme Court precedent establishing or denying an historical foreign intelligence exception to the warrant requirement; originalist historical scholarship on the Framers'' intent regarding foreign-threat intelligence gathering; comparative jurisprudence from other constitutional democracies.',
    'If the Fourth Amendment does NOT extend to foreign intelligence, this reading''s core axiom (warrant_requirement_unexcepted) becomes untenable and the constraint reclassifies toward snare or scaffold (judicially-imposed extraction without constitutional mandate). If it does extend, the reading is vindicated and the constraint remains rope (constitutional protection).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fourth_amendment_foreign_intelligence_scope, conceptual, 'Whether Fourth Amendment warrant requirements apply to foreign intelligence collection.').

omega_variable(
    statutory_versus_constitutional_supremacy,
    'When statutory language (Section 702''s permission for foreign targeting collection) conflicts with constitutional reading (warrant requirements apply), does the Constitution override the statute, or does the statute''s explicit authorization of the collection practice establish that Congress intended to provide a statutory exception to warrant requirements?',
    'Judicial doctrine on statutory-constitutional conflict; legislative history and committee testimony on Section 702''s scope; empirical comparison of warrant-requirement proposals Congress considered but rejected versus those it adopted.',
    'If the statute explicitly authorizes 702 collection and Congress understood that to include warrantless queries, then this reading''s axiom (constitutional_floor_independent_of_statute) is weakened by Congressional intent. If Congress did not address warrant requirements or assumed Fourth Amendment would apply, the reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_versus_constitutional_supremacy, empirical, 'Whether Congressional intent or constitutional text takes priority when they conflict on warrant requirements.').

omega_variable(
    query_as_search_classification,
    'Is a database query into already-collected communications a Fourth Amendment ''search'' requiring warrant authorization, or is the search already concluded when the collection occurred (making the query a mere retrieval of already-lawfully-seized data)?',
    'Supreme Court precedent on when government access to stored data constitutes a search (test case: Carpenter v. United States, United States v. Jones — does the reasoning extend to queries into databases); FISA Court practice establishing whether queries are routinely treated as searches or merely as data retrievals.',
    'If queries are searches, this reading''s classification holds and warrant requirements apply. If queries are not searches but retrieval, the constraint reclassifies to piton (the rule is theater — warrants would be required for collection but not for access, creating an appearance of protection without substance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(query_as_search_classification, empirical, 'Whether database queries constitute Fourth Amendment searches.').

omega_variable(
    incidental_collection_warrant_scope,
    'If the constraint requires warrants for queries, must a warrant specify the particular U.S. persons whose incidentally-collected communications will be accessed, or can a warrant target the foreign principal with a standing authorization to access any incidentally-collected U.S. person data?',
    'FISA Court practice if this reading is adopted; comparison with warrant doctrine in domestic criminal investigations (Fourth Amendment requires particularity; does foreign intelligence collection require the same?).',
    'If particular U.S. persons must be specified, the constraint extracts substantial procedural cost (separate warrant per person/query). If a foreign-targeting warrant with general incidental-collection authorization suffices, the constraint is less extractive to executive operations. Either way, the reading holds; this omega addresses the DEPTH of the constraint''s imposition, not its validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_collection_warrant_scope, empirical, 'What level of particularity warrant requirements impose on incidentally-collected U.S. person data queries.').

omega_variable(
    reading_coexistence_institutional_fragmentation,
    'Given that this reading forecloses the sibling readings within a single legal framework, how can all three readings remain institutionally live? Are we observing genuine legal contestation within courts and Congress, or institutional fragmentation where different branches adopt incompatible readings?',
    'Institutional decision analysis: if the Supreme Court endorses this reading, do executive agencies comply or invoke state secrets privilege to shield practices? If Congress amends Section 702 to explicitly reject warrant requirements for foreign intelligence, does that legislative act foreclose this reading or trigger a constitutional challenge? What does institutional behavior reveal about whether the readings truly coexist or are in zero-sum conflict?',
    'If the readings coexist institutionally despite logical foreclosure, the constraint may be more accurately classified as tangled_rope (institutionally sustained through fragmented authority rather than as pure coordination). If institutional pressure forces a choice, the winner''s reading reclassifies toward snare (if executive) or rope (if privacy-protective), and the loser becomes piton (theater without force).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_institutional_fragmentation, conceptual, 'Whether logically foreclosed readings can remain institutionally coexistent or must resolve to fragmentation/conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(fisa_tr_t5, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(fisa_tr_t15, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 15, 0.04).
narrative_ontology:measurement(fisa_tr_t20, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(fisa_tr_t25, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 25, 0.05).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fisa_be_t5, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(fisa_be_t15, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(fisa_be_t25, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 25, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(fisa_su_t5, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 5, 0.16).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(fisa_su_t15, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement(fisa_su_t25, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 25, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__constitutional_floor_reading, 0.12).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel fisa_702_statutory_text. All three readings instantiate different structural interpretations of how Section 702 relates to Fourth Amendment warrant requirements. The constitutional_floor_reading asserts warrants are required for all U.S. person queries; the incidental_collection_reading asserts warrantless queries are permitted for incidentally-collected data; the foreign_target_strict_reading asserts minimization and inaccessibility constraints protect incidental U.S. persons without warrant requirements. Each reading produces different ε values (this reading: 0.25, reflecting constitutional constraint cost to executive preferences; incidental reading: 0.65+, reflecting executive operational benefit from warrantless access; foreign_target reading: 0.40-0.50, reflecting moderate constraints from minimization requirements). All three affect each other because they are competing interpretations of the same statutory-constitutional arrangement. The network edges model that judicial or legislative decisions on one reading create pressure and precedent affecting the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__constitutional_floor_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
