% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: FISA Section 702 Incidental Collection / Backdoor Search Reading
 *   domain: constitutional_law/national_security/surveillance
 *
 * SUMMARY:
 *   This story instantiates the incidental_collection_reading of the FISA
 *   Section 702 kernel: the statutory text is read to permit retention of
 *   incidentally collected U.S. person communications and warrantless query
 *   of that repository — including by the FBI for domestic investigative
 *   purposes — so long as a foreign intelligence purpose can be asserted for
 *   the original collection. Under this reading, U.S. persons who communicate
 *   with a foreign target become a de facto searchable class without
 *   individualized warrant protection, and the Fourth Amendment's warrant
 *   requirement is displaced by administrative minimization procedures that
 *   the same executive apparatus conducting the queries also drafts and
 *   self-certifies before an ex parte FISA Court. This is a distinct
 *   constraint from the foreign_target_strict_reading (which would keep
 *   incidental U.S. person data minimized and inaccessible for domestic
 *   purposes) and the constitutional_floor_reading (which holds any 702 query
 *   of U.S. person content is itself a Fourth Amendment search requiring a
 *   warrant regardless of statutory language). Each reading has a different
 *   victim set, a different ε, and a different classification; they are
 *   linked here only through the shared kernel, not merged.
 *
 * KEY AGENTS:
 *   - fbi_domestic_investigations_division: agenda_setter/beneficiary (institutional/arbitrage) — conducts and defends warrantless backdoor queries
 *   - executive_branch_national_security_apparatus: agenda_setter/beneficiary (institutional/arbitrage) — drafts and self-certifies minimization procedures
 *   - us_persons_incidentally_collected: payer (powerless/trapped) — swept into the repository with no individualized process
 *   - us_persons_subject_to_backdoor_queries: payer (powerless/trapped) — searched without a warrant during unrelated investigations
 *   - fisa_court: observer/agenda_setter (institutional/analytical) — ex parte certification review with no adversarial representation for affected U.S. persons
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.68).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA Section 702 Incidental Collection / Backdoor Search Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, 'e0ed094a-0380-4a09-9f21-9e6e375fd561').
narrative_ontology:cs_kernel_codification('e0ed094a-0380-4a09-9f21-9e6e375fd561', fixed_text).
narrative_ontology:cs_authority_grounding('e0ed094a-0380-4a09-9f21-9e6e375fd561', extraction).
narrative_ontology:cs_interpretation_layer_present('e0ed094a-0380-4a09-9f21-9e6e375fd561').
narrative_ontology:cs_reading_relation('e0ed094a-0380-4a09-9f21-9e6e375fd561', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0ed094a-0380-4a09-9f21-9e6e375fd561', fisa_702_statutory_text__constitutional_floor_reading, influences).
narrative_ontology:cs_axiom('e0ed094a-0380-4a09-9f21-9e6e375fd561', foundational, foreign_intelligence_purpose_displaces_warrant_requirement).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_displaces_warrant_requirement, holdable).
narrative_ontology:cs_axiom_grounding('e0ed094a-0380-4a09-9f21-9e6e375fd561', foreign_intelligence_purpose_displaces_warrant_requirement, instrumental).
narrative_ontology:cs_axiom('e0ed094a-0380-4a09-9f21-9e6e375fd561', secondary, administrative_minimization_satisfies_reasonableness).
narrative_ontology:cs_axiom_status(administrative_minimization_satisfies_reasonableness, holdable).
narrative_ontology:cs_axiom_grounding('e0ed094a-0380-4a09-9f21-9e6e375fd561', administrative_minimization_satisfies_reasonableness, conventional).
narrative_ontology:cs_reference_frame('e0ed094a-0380-4a09-9f21-9e6e375fd561', post_faa_2008_foreign_intelligence_authorization).
narrative_ontology:cs_drift_state('e0ed094a-0380-4a09-9f21-9e6e375fd561', post_2018_2023_reauthorization_compliance_disclosures, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e0ed094a-0380-4a09-9f21-9e6e375fd561', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations_division).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_community_analysts).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, executive_branch_national_security_apparatus).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_subject_to_backdoor_queries).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, domestic_criminal_defendants_denied_notice).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_purpose_doctrine).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, administrative_minimization_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Queries the 702 database using U.S. person identifiers during routine domestic criminal and national-security investigations, without a warrant, justified by the same foreign-intelligence authorization that collected the data incidentally. Administers its own querying standards and internal audit process, and has resisted statutory amendments that would impose a probable-cause threshold on backdoor searches.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations_division, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations_division, beneficiary).

% Rely on the retained incidental collection as a standing investigative resource — a searchable repository built from surveillance authorized for foreign targets but usable for downstream domestic leads. Collects intelligence value directly from the retention and query authority.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, intelligence_community_analysts, beneficiary,
    institutional, generational, arbitrage, global).

% Defends the statutory reading before Congress and the FISA Court, negotiates reauthorization terms, and administers minimization procedures that it also drafts. Treats broad query access as operationally essential and has framed reform proposals as threats to national security capability.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, executive_branch_national_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, executive_branch_national_security_apparatus, beneficiary).

% Communicate with a foreign target (a family member, a business contact, a journalist's source) and have their content swept into a database never subject to a warrant naming them. They have no notice their communications were collected, no practical way to know they are in the repository, and no exit — ordinary international communication is unavoidable and the collection is invisible until (if ever) surfaced in a later proceeding.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected, payer,
    powerless, biographical, trapped, national).

% Become the subject of a warrantless query against the 702 repository during a domestic investigation unrelated to any foreign intelligence purpose in the moment of the search. The Fourth Amendment's warrant requirement is displaced by administrative minimization procedures adjudicated by the same executive apparatus that conducts the queries. They cannot contest the query before it happens and often cannot learn it happened at all.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_subject_to_backdoor_queries, payer,
    powerless, biographical, trapped, national).

% Face prosecution built partly on leads derived from 702 database queries, but statutory and litigation practice around parallel construction and notice obligations often prevents them from learning that warrantless surveillance produced the evidentiary trail, foreclosing a suppression challenge they would otherwise have standing to bring.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, domestic_criminal_defendants_denied_notice, payer,
    powerless, biographical, trapped, national).

% Reviews and approves annual certifications and minimization procedures in largely ex parte proceedings without an adversarial party representing the incidentally collected U.S. persons. Has periodically found compliance violations and ordered remedial procedures, but operates from government submissions and has limited independent fact-finding capacity.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fisa_court, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, fisa_court, agenda_setter).

% Periodically reauthorizes Section 702 and holds hearings on compliance failures. Has considered but repeatedly declined to enact a warrant requirement for backdoor queries, weighing intelligence community warnings against civil liberties advocacy.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congress_reauthorization_committees, observer,
    institutional, generational, analytical, national).

% Argue for a probable-cause warrant requirement before any U.S. person query, and for individual notice when 702-derived evidence is used in prosecutions. They submit amicus briefs and testify at reauthorization hearings but have no seat in the FISA Court's ex parte certification process and no vote in reauthorization votes.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_organizations, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations_division).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables continuous, retained access to a large pool of foreign-target communications so that analysts and investigators can query for both foreign intelligence leads and, under this reading, domestic investigative leads, without re-collecting or re-authorizing for each new use.
% TRANSFER_FUNCTION: Moves investigative and evidentiary access from a warrant-gated regime (Fourth Amendment probable cause review by a neutral magistrate) to an administrative regime (minimization procedures adjudicated internally and reviewed ex parte by the FISA Court), transferring the practical cost of that shift onto U.S. persons whose communications were incidentally collected and later queried.
% ABSENT_VOICES: U.S. persons whose communications are queried have no notice and no adversarial representation before the FISA Court; civil liberties organizations and public defenders raise the objection in Congress and in litigation after the fact, but neither sits in the certification process where the reading is operationalized.
% DISAPPEARANCE_RATIONALE: If this reading were displaced by a warrant requirement for backdoor queries, FBI domestic investigations would lose routine access to the 702 repository absent individualized judicial authorization, intelligence-to-law-enforcement referral patterns would need new legal infrastructure, and a substantial volume of current investigative practice would have to be reconstructed around a probable-cause gate — a real institutional rearrangement, not a cosmetic one.
% FOUNDING_PROBLEM: Section 702 was built to allow warrantless collection of foreign intelligence communications where at least one party is a non-U.S. person reasonably believed to be located abroad, addressing the technical reality that international communications routinely traverse U.S. infrastructure and that pre-FISA-Amendments-Act practice required cumbersome individualized orders for surveillance that was substantively about foreign, not domestic, targets.
% FOUNDING_PROBLEM_CORROBORATION: The executive branch and intelligence community attest the foreign-intelligence problem remains live and that query access is operationally necessary. Independent corroboration from outside the benefiting parties is mixed: the Privacy and Civil Liberties Oversight Board and multiple FISA Court opinions documenting compliance violations attest that the querying practice has drifted well beyond the foreign-targeting rationale into a general-purpose domestic investigative tool, and several federal judges reviewing 702-derived evidence have independently flagged the gap between the statute's foreign-intelligence justification and its domestic-query use.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__incidental_collection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__incidental_collection_reading, 0.45, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.45) reflects that the statutory reading extracts Fourth Amendment protection from a defined, non-consenting population (U.S. persons who communicate internationally) and converts it into investigative capability for a different institution (domestic law enforcement) than the one the collection was originally authorized for (foreign intelligence). Suppression (0.68) is high because the mechanism depends on invisibility — affected persons typically cannot learn they were queried, and parallel construction practices can prevent even criminal defendants from raising the issue. Theater ratio (0.42) captures that minimization procedures perform compliance rigor (internal audits, court certifications) while backdoor query volume and compliance-violation findings have grown over the measured interval, suggesting the procedural performance has not kept pace with the underlying practice's expansion — consistent with a rising theater_ratio trend authored across the shared time grid. Accessibility collapse (0.72) is high: once the reading is understood, there is no practical alternative for a U.S. person who communicates internationally to avoid being incidentally collected. Resistance (0.58) is elevated because civil liberties organizations, some FISA Court judges, and oversight bodies have actively contested the practice, unlike a genuine mountain which meets little resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   The FBI, the executive branch national security apparatus, and intelligence community analysts sit near the beneficiary end: they administer the querying regime, defend it in reauthorization fights, and draw investigative value directly from it, with arbitrage-grade institutional exit (they can adjust internal procedures rather than face external constraint). U.S. persons incidentally collected and subject to backdoor queries sit at the target end: trapped exit (ordinary international communication cannot be avoided), powerless structural position, and no individualized process before the search occurs. Domestic criminal defendants denied notice are a downstream victim class whose exit is foreclosed procedurally (notice denial) rather than physically. The FISA Court and Congress are observer/partial-agenda-setter seats — they have some structural leverage to alter the reading but have largely deferred to the executive's framing across reauthorization cycles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (efficient foreign intelligence collection against non-U.S. persons abroad) remains partially live, which prevents a flat mandatrophy verdict — this is not a pure zombie mandate. But the founding_problem_status is authored contested precisely because independent oversight bodies (PCLOB, several FISA Court opinions) corroborate that the backdoor-query practice has drifted from the foreign-intelligence justification into general-purpose domestic investigative use, which the founding rationale does not obviously license. The tangled_rope classification is chosen over snare because a genuine coordination function (efficient foreign intelligence collection) persists alongside the asymmetric extraction from U.S. persons — collapsing this into pure extraction would erase the real foreign-intelligence coordination value; collapsing it into pure coordination would erase the documented, uncontested harm to a specifically identifiable, non-consenting domestic population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    backdoor_query_volume_vs_foreign_intelligence_yield,
    'What fraction of FBI backdoor queries against the 702 database produce foreign-intelligence value versus purely domestic investigative leads unrelated to the original foreign-targeting purpose?',
    'Declassified query audit statistics, PCLOB compliance reports, and FISA Court opinions documenting query purpose classification over time.',
    'A high domestic-lead fraction would confirm the reading has drifted into a general-purpose surveillance tool decoupled from its foreign intelligence justification, supporting reclassification toward snare; a low fraction closer to genuine foreign-intelligence use would support the tangled_rope coordination component more strongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backdoor_query_volume_vs_foreign_intelligence_yield, empirical, 'Whether backdoor queries function as foreign intelligence tools or domestic investigative shortcuts.').

omega_variable(
    notice_suppression_scope,
    'How often does parallel construction or notice-avoidance practice prevent criminal defendants from learning that 702 database queries contributed to their prosecution?',
    'Case-law audit of suppression motions raising 702 notice issues; DOJ disclosure practice statistics; defense bar surveys.',
    'Widespread notice suppression would indicate the suppression metric understates the constraint''s actual coercive reach, since even the judicial check on the practice is structurally evaded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(notice_suppression_scope, empirical, 'Extent to which downstream notice suppression compounds the upstream warrantless-query practice.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the incidental_collection_reading the operative controlling interpretation, or does the constitutional_floor_reading''s Fourth Amendment argument represent an unresolved, potentially controlling alternative that federal courts have not definitively foreclosed?',
    'Tracking circuit court and Supreme Court disposition of Fourth Amendment challenges to 702 backdoor queries; a definitive ruling either way would resolve which reading is legally operative going forward.',
    'If the constitutional_floor_reading were judicially adopted, this reading''s core premise (that the statutory foreign-intelligence purpose can displace warrant requirements for U.S. person queries) would be foreclosed rather than merely contested, converting the current tangled_rope structure into a legally invalidated practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether this reading remains a live, court-tolerated interpretation or is vulnerable to judicial foreclosure by the constitutional floor reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fisa_tr_t3, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement(fisa_tr_t6, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(fisa_tr_t9, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 9, 0.35).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(fisa_tr_t16, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 16, 0.42).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fisa_be_t3, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(fisa_be_t6, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(fisa_be_t9, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 9, 0.38).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(fisa_be_t16, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 16, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fisa_su_t3, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(fisa_su_t6, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(fisa_su_t9, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 9, 0.62).
narrative_ontology:measurement(fisa_su_t12, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(fisa_su_t16, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 16, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the fisa_702_statutory_text kernel decomposed per the ε-invariance principle. foreign_target_strict_reading has a much lower ε (minimization keeps U.S. person data inaccessible domestically) and would classify closer to rope or scaffold. constitutional_floor_reading treats any U.S. person query as a Fourth Amendment search requiring a warrant, which if adopted would foreclose this reading's operative premise entirely. All three share the same underlying statutory text but instantiate structurally distinct constraints with distinct victim sets and distinct ε values; they are linked here for contamination-propagation analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
