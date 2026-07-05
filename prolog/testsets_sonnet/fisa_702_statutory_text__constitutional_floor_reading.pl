% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Constitutional Floor Reading of 702 Queries as Fourth Amendment Searches
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This story instantiates the constitutional floor reading of the Section
 *   702 kernel: regardless of what the statutory text permits or how it is
 *   construed, the Fourth Amendment independently requires a probable-cause
 *   warrant before the government queries its own already-collected database
 *   for a U.S. person's communications content. On this reading, the query —
 *   not the initial foreign-targeted collection — is the operative Fourth
 *   Amendment search, and the foreign/domestic distinction that structures
 *   the collection-side statute is irrelevant to the query-side
 *   constitutional question. This is a criminal-procedure claim wearing a
 *   national-security statute's clothing. It is one of three readings of the
 *   same kernel (fisa_702_statutory_text); the foreign_target_strict_reading
 *   disputes the scope of lawful collection itself, and the
 *   incidental_collection_reading affirmatively defends warrantless querying
 *   as a permitted use of lawfully collected foreign-intelligence data. All
 *   three readings are separate constraint stories linked by
 *   network.affects_constraints; this file generates only the constitutional
 *   floor reading, cleanly, without describing or averaging over the sibling
 *   readings.
 *
 * KEY AGENTS:
 *   - us_persons_queried_without_warrant: primary target — bears the uncompensated Fourth Amendment cost of warrantless queries
 *   - criminal_defendants_facing_derivative_evidence: secondary target — inherits the constitutional defect through derivative evidence
 *   - intelligence_community_operational_tempo: primary beneficiary — retains query speed and operational secrecy
 *   - executive_branch_secrecy_interests: institutional beneficiary and agenda-setter — defends the narrower readings in litigation and reauthorization
 *   - fisa_court: agenda-setter whose institutional role would transform under this reading
 *   - civil_liberties_advocates: excluded voice — argues the floor reading but faces standing barriers
 *   - congress: observer with latent agenda-setting power — could legislate the floor but has not
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.4).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Constitutional Floor Reading of 702 Queries as Fourth Amendment Searches").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '3801cdbd-d201-4deb-9819-5c51e633c983').
narrative_ontology:cs_kernel_codification('3801cdbd-d201-4deb-9819-5c51e633c983', fixed_text).
narrative_ontology:cs_authority_grounding('3801cdbd-d201-4deb-9819-5c51e633c983', lineage).
narrative_ontology:cs_interpretation_layer_present('3801cdbd-d201-4deb-9819-5c51e633c983').
narrative_ontology:cs_reading_relation('3801cdbd-d201-4deb-9819-5c51e633c983', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('3801cdbd-d201-4deb-9819-5c51e633c983', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_axiom('3801cdbd-d201-4deb-9819-5c51e633c983', foundational, query_of_content_is_independent_fourth_amendment_search).
narrative_ontology:cs_axiom_status(query_of_content_is_independent_fourth_amendment_search, holdable).
narrative_ontology:cs_axiom_grounding('3801cdbd-d201-4deb-9819-5c51e633c983', query_of_content_is_independent_fourth_amendment_search, deontological).
narrative_ontology:cs_axiom('3801cdbd-d201-4deb-9819-5c51e633c983', secondary, statutory_foreign_intelligence_purpose_cannot_waive_warrant_clause).
narrative_ontology:cs_axiom_status(statutory_foreign_intelligence_purpose_cannot_waive_warrant_clause, holdable).
narrative_ontology:cs_axiom_grounding('3801cdbd-d201-4deb-9819-5c51e633c983', statutory_foreign_intelligence_purpose_cannot_waive_warrant_clause, deontological).
narrative_ontology:cs_reference_frame('3801cdbd-d201-4deb-9819-5c51e633c983', warrant_clause_individualized_review_baseline).
narrative_ontology:cs_drift_state('3801cdbd-d201-4deb-9819-5c51e633c983', post_snowden_reauthorization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3801cdbd-d201-4deb-9819-5c51e633c983', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, intelligence_community_operational_tempo).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_secrecy_interests).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, us_persons_queried_without_warrant).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, criminal_defendants_facing_derivative_evidence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their communications, incidentally collected under Section 702 targeting foreign persons abroad, are queried by name or identifier by FBI, NSA, or CIA analysts using U.S.-person query terms, without any individualized judicial warrant. Under this reading, each such query is itself a Fourth Amendment search that should have required probable cause ex ante. They have no visibility into whether they have been queried, no notice, and no practical mechanism to contest a search they cannot know occurred.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons_queried_without_warrant, payer,
    powerless, biographical, trapped, national).

% Face prosecution using evidence derived from warrantless 702 queries laundered through parallel construction or belated notice. Under the constitutional floor reading, this evidence chain traces to an unconstitutional search at the query stage, giving these defendants a suppression argument that the incidental-collection reading forecloses.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, criminal_defendants_facing_derivative_evidence, payer,
    powerless, biographical, trapped, national).

% NSA, FBI, and CIA analysts run millions of queries per year against the 702-collected database for foreign intelligence and, controversially, for domestic criminal and even routine vetting purposes. A probable-cause-per-query requirement would collapse this operational tempo, requiring individualized judicial process before each database lookup — the agencies treat the current query-without-warrant practice as essential to speed and secrecy.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, intelligence_community_operational_tempo, beneficiary,
    institutional, immediate, arbitrage, global).

% Successive administrations have defended the incidental-collection and foreign-target readings before Congress and the FISA Court, resisting the constitutional floor reading because it would expose query practices to adversarial judicial review and slow classified operations. It administers the reauthorization process and negotiates minimization procedures that fall short of the warrant standard this reading would require.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_secrecy_interests, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, executive_branch_secrecy_interests, agenda_setter).

% Reviews annual certifications and minimization procedures under Section 702 but, under current practice, does not conduct individualized probable-cause review of specific U.S.-person queries. Under the constitutional floor reading, its review function would need to expand to per-query adjudication, a structural transformation of its role from program-level overseer to case-by-case magistrate.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, constrained, national).

% Litigate and lobby for the warrant requirement (this reading) in cases like United States v. Hasbajrami and Wikimedia v. NSA, but lack standing in most instances because the very secrecy of the query process prevents them from establishing that a particular plaintiff was searched. Their structural argument is heard in amicus briefs and congressional testimony but rarely reaches a merits ruling that would settle the kernel contest.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% Reauthorizes Section 702 periodically and could legislate a warrant requirement for U.S.-person queries but has repeatedly declined to do so, instead layering additional minimization and reporting procedures that fall short of judicial pre-approval. It holds hearings surfacing the constitutional floor argument without adopting it into statute.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, congress, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__constitutional_floor_reading, intelligence_community_operational_tempo).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__constitutional_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The underlying 702 program coordinates lawful foreign-intelligence collection targeting non-U.S. persons abroad, solving a genuine problem: intercepting foreign threat communications without a warrant requirement that would be unworkable against foreign targets who have no Fourth Amendment rights. The constitutional floor reading does not dispute this collection-side coordination — it disputes what happens at the query stage, once U.S.-person communications are already in the government's holdings.
% TRANSFER_FUNCTION: Under this reading, what is transferred is a constitutional protection: the probable-cause safeguard that would normally stand between government and a U.S. person's communications content is displaced by treating the query as something other than a search, moving investigative convenience and operational speed to intelligence and law-enforcement agencies at the cost of the query targets' warrant right.
% ABSENT_VOICES: The individuals actually queried are structurally absent from any adjudication of this claim — they typically never learn a query occurred, so they cannot bring the as-applied challenge that would test the constitutional floor reading on its merits. Civil liberties litigants stand in for them but face standing barriers created by the same secrecy the reading contests.
% DISAPPEARANCE_RATIONALE: If courts adopted this reading tomorrow, agencies would face an immediate operational disruption requiring individualized warrants before U.S.-person queries, likely triggering a sharp drop in query volume and a scramble for emergency-exception doctrine (similar to exigent-circumstances carve-outs in ordinary Fourth Amendment law). Intelligence agencies dispute this would meaningfully change security outcomes; civil liberties advocates argue it would restore a constitutional floor that never should have eroded. Whether the world 'rearranges' or 'stays the same' is precisely what the kernel contest between the three readings is about.
% FOUNDING_PROBLEM: Section 702 was built to solve the problem of collecting foreign intelligence communications that pass through U.S. telecommunications infrastructure without requiring a warrant for surveillance of foreign persons who have no Fourth Amendment rights — a genuine gap exposed by post-9/11 restructuring of global communications routing.
% FOUNDING_PROBLEM_CORROBORATION: Intelligence agencies and the executive branch attest the original foreign-intelligence problem remains live and justifies current query practices. Independent corroboration from outside the benefiting agencies comes from the Privacy and Civil Liberties Oversight Board's 2023-2024 reporting and from federal judges in Hasbajrami and related cases, who have documented that U.S.-person query volume and use for domestic criminal purposes has expanded well beyond the foreign-intelligence problem the statute was built to solve — supporting the constitutional floor reading's claim that the query stage has become a distinct, under-scrutinized constitutional event.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, contested).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.25) because this reading's cost is not rent extraction in the economic sense but constitutional compliance cost measured in operational friction — the 'extraction' here is executive speed and secrecy displaced by procedural safeguard, exactly the ε≈0.25 anchor specified for this reading. Suppression is moderate (0.4) and rising slowly over the interval: the suppressive force is the practical unreviewability of the query practice (targets cannot know they were searched, so cannot contest it), which has intensified as query volume has grown under reauthorized certifications (2008, 2012, 2018, 2023) each preserving the practice this reading contests. Theater ratio is moderate and rising (0.15 to 0.30): minimization procedures and PCLOB reporting perform compliance without instituting the individualized review this reading would require, and that performative gap has widened as query volume and public scrutiny have both grown.
 *
 * PERSPECTIVAL GAP:
 *   From the executive/intelligence seat, the query is a continuation of already-lawful foreign intelligence collection and imposing a warrant requirement at the query stage would be redundant and operationally paralyzing. From the queried U.S. person's seat, the query is the first moment the government actually looks at their content for a purpose that may have nothing to do with the foreign target — a Fourth Amendment event that has never received the individualized process the Constitution otherwise requires for content searches. The engine computes these as structurally different experiences of the same underlying data flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, U.S. persons queried without a warrant are the clear structural targets — trapped, powerless, unable to detect or contest the query, so directionality sits near the full-target end. The intelligence community and executive branch are the structural beneficiaries: they retain operational speed and secrecy by resisting the warrant requirement, giving them low directionality (subsidized by the absence of the safeguard). The FISA Court and Congress sit in an intermediate structural position — they are agenda-setters who could impose the reading's requirement but have institutional incentives (workload, deference to executive expertise, secrecy culture) that align them closer to the beneficiary side despite their nominal oversight role.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than pure snare) reflects that a genuine coordination function exists at the collection stage — foreign intelligence gathering against non-U.S. persons is a real problem this statute solves — but the query practice this reading contests rides on that legitimate collection function to extract a constitutional cost from U.S. persons who were never the lawful target. Treating the whole 702 apparatus as either pure coordination or pure extraction would mislabel one half of the structure; this reading isolates the query-stage extraction while conceding the collection-stage coordination is real, which is precisely the seat-divergence the tangled_rope category exists to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    query_as_search_doctrinal_status,
    'Is a database query of already-lawfully-collected communications a Fourth Amendment ''search'' triggering an independent probable-cause requirement, or is it a permissible retrieval of information the government already lawfully possesses?',
    'A Supreme Court ruling squarely addressing whether querying constitutes a search independent of the collection''s legality — the question left open in United States v. Hasbajrami (2d Cir. 2019) and not yet resolved by the Court. Circuit splits or a granted cert petition would sharpen this.',
    'If queries are searches, this reading''s warrant requirement follows and the tangled_rope''s victim-side extraction becomes formally unconstitutional, likely forcing a legislative or judicial overhaul of query procedures. If queries are not independently searches, this reading collapses into the incidental_collection_reading''s framework and this constraint''s claimed extraction has no doctrinal footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(query_as_search_doctrinal_status, conceptual, 'Whether the query-as-search premise this entire reading depends on is doctrinally sound.').

omega_variable(
    standing_barrier_self_reinforcement,
    'Does the secrecy of the query process itself prevent the standing that would be needed to litigate and resolve whether the constitutional floor reading is correct?',
    'Track outcomes in cases where criminal defendants received post hoc notice of 702-derived evidence (a narrow class where standing exists) and whether courts reach the merits question there versus dismissing on other grounds.',
    'If standing is systematically unavailable, the constitutional floor reading may be structurally unresolvable through ordinary litigation regardless of its underlying merit — an omega about whether the kernel contest can even be adjudicated, not just how it would be resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standing_barrier_self_reinforcement, empirical, 'Whether the reading can ever reach a merits ruling given the secrecy that shields the practice it contests.').

omega_variable(
    cs_framing_criminal_procedure_vs_foreign_intelligence,
    'Is Section 702 properly framed, for constitutional purposes, as a foreign intelligence statute (where Fourth Amendment scrutiny is traditionally relaxed) or as a domestic criminal procedure question once U.S.-person content is queried (where the ordinary warrant standard applies)?',
    'This is a framing choice with no single dispositive resolution mechanism, but the pattern of judicial reasoning (foreign-intelligence exception cases like In re Directives vs. ordinary Fourth Amendment content-search cases like Carpenter v. United States) tracks which framing dominates in a given era.',
    'The foreign-intelligence framing supports the incidental_collection_reading; the criminal-procedure framing supports this constitutional_floor_reading. The classification of this constraint as tangled_rope versus a cleaner mountain-like ''this is simply what the Fourth Amendment requires'' depends on which framing the reader adopts — this omega documents that under-determination explicitly rather than letting it hide in the metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_criminal_procedure_vs_foreign_intelligence, conceptual, 'Alternative framing of the kernel as foreign-intelligence law versus criminal procedure, and how that choice shapes classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(fisa_tr_t2011, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2011, 0.19).
narrative_ontology:measurement(fisa_tr_t2014, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2014, 0.22).
narrative_ontology:measurement(fisa_tr_t2018, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(fisa_tr_t2021, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2021, 0.28).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2008, 0.12).
narrative_ontology:measurement(fisa_be_t2011, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2011, 0.15).
narrative_ontology:measurement(fisa_be_t2014, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2014, 0.18).
narrative_ontology:measurement(fisa_be_t2018, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2018, 0.21).
narrative_ontology:measurement(fisa_be_t2021, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2021, 0.23).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2008, 0.3).
narrative_ontology:measurement(fisa_su_t2011, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2011, 0.32).
narrative_ontology:measurement(fisa_su_t2014, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2014, 0.34).
narrative_ontology:measurement(fisa_su_t2018, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2018, 0.37).
narrative_ontology:measurement(fisa_su_t2021, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2021, 0.39).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__constitutional_floor_reading, 0.1).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the fisa_702_statutory_text kernel. constitutional_floor_reading (this file) treats the query as an independent Fourth Amendment search requiring a warrant regardless of statutory framing, with ε≈0.25 reflecting constitutional compliance cost rather than economic rent. foreign_target_strict_reading narrows lawful collection scope at the targeting stage rather than imposing a query-stage warrant. incidental_collection_reading affirmatively permits warrantless querying under the foreign-intelligence-purpose doctrine and represents the reading closest to current executive practice. Each carries a distinct ε, distinct beneficiary/victim structure, and distinct classification; they are linked here rather than merged into one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
