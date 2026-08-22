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
 *   human_readable: FISA Section 702 Incidental Collection and Warrantless Query Arrangement (Permissive Statutory Reading)
 *   domain: constitutional/national_security/surveillance
 *
 * SUMMARY:
 *   Section 702 of FISA authorizes the government to target non-U.S. persons
 *   abroad for foreign intelligence collection without individualized
 *   warrants. In operation, the targeting sweeps in large volumes of
 *   communications involving U.S. persons, which are retained in searchable
 *   repositories and may be queried using U.S.-person identifiers without a
 *   warrant when a foreign intelligence purpose justifies the search. This
 *   story authors that retention-and-query arrangement as it stands under the
 *   statute's permissive reading: the FBI's domestic access to the
 *   repository, the displacement of the individualized warrant requirement by
 *   administrative minimization procedures, and the position of U.S. persons
 *   who cannot exit, cannot learn of collection, and cannot contest queries
 *   prospectively. The claimed type and the authored metrics are independent
 *   facts: the claim is tangled_rope (a genuine foreign-collection
 *   coordination function with asymmetric costs borne by rights-holders
 *   inside the same structure), while the metrics describe what the
 *   arrangement's operation actually looks like from the affected
 *   population's position.
 *
 * KEY AGENTS:
 *   - nsa_cia_collectors: Agenda setter and primary beneficiary (institutional/arbitrage) — tasks collection, retains incidental U.S.-person content, disseminates product; can shift posture across legal authorities
 *   - fbi_query_agents: Secondary beneficiary (institutional/constrained) — queries the repository for domestic cases without individualized warrants; the seat the contested query access accrues to
 *   - us_persons_with_foreign_contacts: Primary target (powerless/trapped) — incidentally collected, retained, and queryable without notice, consent, or opt-out
 *   - fbi_backdoor_search_targets: Primary target (powerless/trapped) — Americans whose identifiers are queried in domestic investigations without warrants
 *   - foreign_intelligence_targets: Nominal collection object (powerless/constrained) — the intended targets abroad whose communications the coordination function exists to reach
 *   - technology_providers: Compelled intermediary (powerful/constrained) — furnishes communications under legal compulsion; bears compliance and reputational costs
 *   - congress_intelligence_committees: Agenda setter (institutional/constrained) — writes and reauthorizes the statute under classified-briefing constraints
 *   - fisc_judges: Analytical observer (institutional/analytical) — approves procedures ex parte; one-sided docket by design
 *   - civil_liberties_litigants: Excluded challenger (organized/trapped) — faces standing and secrecy barriers that keep the practice out of open adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.58).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA Section 702 Incidental Collection and Warrantless Query Arrangement (Permissive Statutory Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional/national_security/surveillance").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, '796fd606-5ef0-470c-be3d-0d81e5f54793').
narrative_ontology:cs_kernel_codification('796fd606-5ef0-470c-be3d-0d81e5f54793', fixed_text).
narrative_ontology:cs_authority_grounding('796fd606-5ef0-470c-be3d-0d81e5f54793', lineage).
narrative_ontology:cs_interpretation_layer_present('796fd606-5ef0-470c-be3d-0d81e5f54793').
narrative_ontology:cs_reading_relation('796fd606-5ef0-470c-be3d-0d81e5f54793', fisa_702_statutory_text__foreign_target_strict_reading, forecloses).
narrative_ontology:cs_reading_relation('796fd606-5ef0-470c-be3d-0d81e5f54793', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('796fd606-5ef0-470c-be3d-0d81e5f54793', foundational, foreign_intelligence_purpose_suffices).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_suffices, holdable).
narrative_ontology:cs_axiom_grounding('796fd606-5ef0-470c-be3d-0d81e5f54793', foreign_intelligence_purpose_suffices, instrumental).
narrative_ontology:cs_axiom('796fd606-5ef0-470c-be3d-0d81e5f54793', foundational, administrative_minimization_sufficient_safeguard).
narrative_ontology:cs_axiom_status(administrative_minimization_sufficient_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('796fd606-5ef0-470c-be3d-0d81e5f54793', administrative_minimization_sufficient_safeguard, conventional).
narrative_ontology:cs_reference_frame('796fd606-5ef0-470c-be3d-0d81e5f54793', statutory_foreign_intelligence_authority).
narrative_ontology:cs_drift_state('796fd606-5ef0-470c-be3d-0d81e5f54793', post_risaa_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('796fd606-5ef0-470c-be3d-0d81e5f54793', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, nsa_cia_collectors).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_query_agents).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_with_foreign_contacts).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, fbi_backdoor_search_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_targets).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, technology_providers).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_exception_doctrine).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, administrative_minimization_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Task collection under annual certifications targeting non-U.S. persons abroad, operate the repositories where incidentally collected U.S.-person communications are retained under FISC-approved minimization procedures, and disseminate intelligence product to consumers across government. If one legal channel narrows, they can shift collection posture to other authorities or adjust tasking — they hold the procedures and draft the certifications the court reviews.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, nsa_cia_collectors, agenda_setter,
    institutional, generational, arbitrage, global).

% Query the 702 repository using U.S.-person identifiers without individualized warrants — for counterintelligence leads, threat assessment, and, under the evidence-of-crime provision added in 2024, certain criminal investigations. They draft the justification memoranda their own procedures require and undergo audits; their alternative is the slower traditional route of grand jury process and Article III warrants.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_query_agents, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, fbi_query_agents, agenda_setter).

% Communicate with family, business partners, or contacts abroad over email, messaging, and voice services. When those communications transit U.S. infrastructure or involve a targeted foreign party, they are collected, retained, and made available for warrantless query. There is no notice, no consent mechanism, and no way to keep communicating across borders while opting out; the only full exit is severing cross-border communication itself.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_with_foreign_contacts, payer,
    powerless, biographical, trapped, national).

% Americans whose names, phone numbers, or email addresses are run through the 702 repository in domestic investigations — in many documented cases with no foreign-nexus finding at all. They typically learn a query occurred only if derived information surfaces in a prosecution, and even then notice practices have been partial; they cannot prevent the query or discover it prospectively.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_backdoor_search_targets, payer,
    powerless, biographical, trapped, national).

% Non-U.S. persons abroad whose communications are the collection's intended object — suspected terrorists, foreign intelligence services, cyber actors, and the networks around them. Some can encrypt, move to non-U.S. providers, or avoid U.S. infrastructure; most are embedded in communication patterns that cross U.S. switches and servers, which is precisely what the targeting authority is built to reach.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_targets, payer,
    powerless, biographical, constrained, global).

% Email, cloud, and telecom companies compelled to furnish communications and facilities assistance under directives they cannot disclose for long periods. They bear compliance engineering costs, legal exposure, and reputational damage when programs surface; they can litigate scope, harden encryption, and press for reform, but cannot exit the obligation to assist.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, technology_providers, payer,
    powerful, biographical, constrained, global).

% Write the statutory text, receive classified briefings, and run reauthorization every few years. Reform is constrained by what members can learn in classified settings with limited cleared staff, by national-security framing of the threat, and by agency resistance; the 2024 reauthorization defeated a broader warrant requirement and enacted a narrow evidence-of-crime warrant provision instead.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congress_intelligence_committees, agenda_setter,
    institutional, generational, constrained, national).

% Review government certifications and approve targeting and minimization procedures in ex parte proceedings; receive compliance reports and, since 2018, benefit from amicus curiae participation in significant interpretations. Their docket is structurally one-sided — only the government appears in the first instance — and their significant opinions are published only in redacted form, often years late.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fisc_judges, observer,
    institutional, generational, analytical, national).

% Organizations and individuals who have challenged the program in federal court since its inception. Standing doctrine requires showing their own communications were collected — something the government's secrecy prevents them from proving — and state-secrets assertions and classified dockets have repeatedly kept cases from the merits. The secrecy that maintains the practice is the same force that keeps them out of the forum.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_litigants, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, fbi_query_agents).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem that globalized communications created for foreign intelligence collection: foreign targets' communications transit U.S. switches and sit on U.S. servers, so the individualized-warrant architecture of Title III and pre-2008 FISA made programmatic foreign collection legally unworkable. The statute channels that collection through one framework — annual certifications, FISC-approved targeting, and minimization procedures — so agencies can task collection at non-U.S. persons abroad without case-by-case warrants.
% TRANSFER_FUNCTION: Moves communications content and metadata from private users and the providers carrying their traffic to the intelligence community (retention in searchable repositories) and to FBI investigators (warrantless query access), and relocates search-authorization decisions from Article III warrant magistrates to executive agencies operating under FISC-approved procedures.
% ABSENT_VOICES: The surveilled U.S. persons themselves: most never receive notice that their communications were collected or queried, so they are structurally absent from any proceeding about the practice. Civil liberties litigants are largely excluded by standing doctrine (they cannot show they were surveilled); criminal defendants historically received little or no notice when 702-derived information was used against them. Defense counsel and privacy advocates would contest the query practice but sit outside the FISC's ex parte process.
% DISAPPEARANCE_RATIONALE: If the retention-and-query permission vanished overnight, the FBI would lose warrantless access to a repository it queried millions of times a year and would need Article III warrants or grand jury process for U.S.-person identifiers; the NSA would face minimization obligations that purge retained U.S.-person content from searchable repositories; foreign collection would continue under other authorities, but the domestic exploitation pipeline — the part this arrangement specifically licenses — would reorganize around the warrant process. Current investigative practice is load-bearing on this arrangement: the world rearranges.
% FOUNDING_PROBLEM: Globalized communications: by the 2000s, foreign targets' communications transited U.S. switches and were stored on U.S. servers, so traditional FISA's individualized-warrant architecture made bulk foreign collection legally fragile — every interception risked capturing a U.S. person and triggering the warrant requirement. Section 702 was built to authorize programmatic targeting of non-U.S. persons abroad, with the question of what may be done with incidentally collected U.S.-person data left to minimization procedures.
% FOUNDING_PROBLEM_CORROBORATION: The foreign-collection problem is corroborated from outside the benefiting parties by the PCLOB's 2014 and 2023 reports and by the 2013 President's Review Group, both of which affirm the mission's reality while disputing the necessity of warrantless U.S.-person queries; FISC amicus briefs and the public 2017 and 2024 reauthorization-debate records document the dispute over the query practice. The operating agencies attest necessity from inside the benefiting set, which is why the status is contested rather than live: every outside reviewer that has examined the question has found the foreign-collection problem real and the warrantless-query necessity unproven.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__incidental_collection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__incidental_collection_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.45 from this reading's own lights: the burden on U.S. persons is real and non-exitable (retention without consent, query without warrant, notice only by accident of prosecution), but the reading assesses the foreign-intelligence justification and the FISC-approved minimization layer as genuine limiting structure, holding the assessment below pure-extraction levels. The FBI's domestic query practice is the component the reading itself concedes needs policing, which is why the 2024 reauthorization enacted a narrow warrant provision for evidence-of-crime queries. Suppression is a raw structural property and is NOT scaled by power or scope: it reflects classification, standing barriers, ex parte review, and partial notice — the machinery that keeps the arrangement out of open adjudication. Theater rises over the interval as oversight activity (transparency reports, annual certifications, compliance summaries) increasingly reports on a practice whose operative constraints are set elsewhere; the 2024 warrant provision dents it slightly. The temporal series shows a ratchet rather than smooth drift: at each reauthorization (2008, 2017-18, 2024) query access is locked in or expanded, disclosure fights surface violations, a narrow concession follows, and accumulation resumes — the cycle is driven by the reauthorization calendar and the secrecy that keeps full practice out of view between cycles. Accessibility collapse is moderate (0.55): for an affected U.S. person, alternatives collapse almost entirely once the arrangement is understood — there is no way to communicate across borders while opting out — but policy alternatives (individualized warrants, minimization-first regimes) remain live and are actively proposed, which keeps the value far from mountain-like levels. Resistance is sustained (0.60): a decade and a half of litigation, two oversight-board investigations, and repeated reauthorization fights, including floor votes that came within a handful of ballots of imposing a general warrant requirement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda/beneficiary seats compute differently from the same structure. From the collectors' and FBI's position the arrangement is a lawful, court-supervised foreign-intelligence tool whose safeguards they operate and document; from the U.S.-person seats it is a warrantless search regime with no notice, no consent, and no exit, experienced only through its outputs (a prosecution disclosure, if that). Congress experiences it as a manageable oversight object under classified constraints; the FISC experiences a one-sided docket; excluded litigants experience a forum they cannot enter. The engine computes this per-seat divergence from the structural data — power, exit, and declared position — and the authored claim does not adjudicate it. Note also the coalition failure that keeps the largest affected class at the lowest power atom: U.S. persons are numerous but diffuse, un-noticed, and unable to aggregate, so their structural power remains powerless despite their size.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation. nsa_cia_collectors sit at the beneficiary end: declared beneficiary, agenda-setting role, arbitrage exit across legal authorities. fbi_query_agents are declared beneficiaries with constrained exit — low directionality but not the floor, since they bear the compliance and audit burdens the same procedures impose. us_persons_with_foreign_contacts and fbi_backdoor_search_targets sit near the full-target end: declared victims, powerless, trapped, with no notice mechanism. foreign_intelligence_targets bear the coordination function's aimed burden and carry constrained exit (encryption and provider choice blunt but do not remove exposure). technology_providers are payers with powerful standing and constrained exit — high directionality despite institutional power, because compulsion removes their exit. congress_intelligence_committees hold the agenda-setting seat with politically constrained exit; fisc_judges occupy the analytical seat; civil_liberties_litigants are excluded and locked out of the forum rather than positioned inside the arrangement. Scope amplification applies where authored: the collectors operate at global scope, the affected U.S.-person seats at national scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what keeps the mandatrophy question honest in both directions. Reading the arrangement as pure extraction would erase the genuine coordination function — foreign-intelligence collection through U.S. infrastructure is a real problem the 1978 warrant architecture could not solve, and that founding problem remains live (terrorism, espionage, cyber operations). Reading it as pure coordination would erase the asymmetric structure: the same repository that solves the foreign-collection problem transfers warrantless access to U.S.-person communications to domestic investigators, a use the founding problem never required. The founding problem is authored as contested, not dead: the foreign-collection mission persists, but whether warrantless U.S.-person query remains necessary for it is disputed by every outside reviewer that has examined the question, and the FBI's domestic use has drifted beyond the founding problem's scope. Mandatrophy pressure is therefore real at the margin — the domestic-query increment serves a mandate the statute was not built for — and the measurement series tracks it as rising extractiveness with partial reform concessions rather than completed atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the fixed statutory kernel 50 U.S.C. 1881a (fisa_702_statutory_text): does the statute''s foreign-targeting authority reach retention and warrantless query of incidentally collected U.S.-person communications (this story''s incidental_collection_reading), or does the statutory scheme compel minimization of incidental U.S.-person data to inaccessibility (foreign_target_strict_reading), or is the question governed by an independent constitutional warrant floor (constitutional_floor_reading)?',
    'Authoritative construction of the statutory text by an appellate court or by Congress; FISC procedural approvals are not final statutory construction. The sibling stories author the strict and constitutional readings with their own epsilon, victim sets, and classifications; this story authors only the permissive reading.',
    'If the strict reading prevails, U.S.-person data leaves the queryable repository and this arrangement''s epsilon collapses toward the coordination-only core. If the constitutional reading prevails, the warrant requirement reattaches regardless of statutory text and the query practice becomes unconstitutional as operated. If this reading is confirmed, the present structure holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which reading of the fixed 702 text governs, and what each sibling reading would change structurally.').

omega_variable(
    minimization_sufficiency_in_practice,
    'Do FISC-approved minimization procedures actually substitute for individualized warrants in operation, or does the safeguard layer fail in practice?',
    'PCLOB audits, published FISC compliance opinions, and FBI query-audit disclosure. The 2021-2022 FISC opinions already documented widespread FBI query violations, including queries with no foreign-nexus finding; remediation records and the 2024 evidence-of-crime warrant provision are the corrective record.',
    'Demonstrated and repeated safeguard failure undermines this reading''s foundational axiom that administrative minimization is a sufficient substitute for warrants and pushes effective extraction upward; documented durable remediation supports the reading''s structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimization_sufficiency_in_practice, empirical, 'Whether the minimization layer performs as the warrant substitute this reading claims it is.').

omega_variable(
    standing_secrecy_feedback,
    'Does the arrangement''s secrecy (classification, standing doctrine, ex parte review) structurally shield it from the constitutional challenge that would otherwise test it, so that the permissive reading persists adjudicatively rather than on its merits?',
    'Disclosure of U.S.-person query counts, statutory notice regimes reaching affected persons, or a challenge reaching the merits on an established record (a defendant with proven 702-derived evidence, or statutory standing).',
    'If justiciability opens, the constitutional_floor_reading becomes fully live in open court and this reading''s stability must rest on its own statutory and constitutional merits; while the feedback loop holds, the reading persists partly because it cannot be completely tested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standing_secrecy_feedback, empirical, 'Secrecy-standing feedback governing whether the arrangement is ever fully adjudicated on the merits.').

omega_variable(
    fbi_query_purpose_composition,
    'What share of FBI queries using U.S.-person identifiers serve the founding foreign-intelligence problem versus ordinary domestic criminal work?',
    'Purpose-coded query audit data with independent verification; FBI internal audits and FISC review of query records could classify queries by investigative purpose.',
    'If most queries serve ordinary domestic criminal work, the arrangement''s operative mandate has outgrown its founding problem and the structure tilts toward pure extraction at the FBI seat; if foreign-intelligence purposes dominate, the coordination function covers the query practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fbi_query_purpose_composition, empirical, 'Composition of FBI query use across the foreign-intelligence and domestic-criminal purpose boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fisa_tr_t3, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 3, 0.24).
narrative_ontology:measurement(fisa_tr_t6, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(fisa_tr_t9, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 9, 0.34).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(fisa_tr_t15, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(fisa_tr_t18, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 18, 0.4).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fisa_be_t3, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(fisa_be_t6, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(fisa_be_t9, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 9, 0.45).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(fisa_be_t15, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(fisa_be_t18, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 18, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fisa_su_t3, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(fisa_su_t6, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(fisa_su_t9, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 9, 0.6).
narrative_ontology:measurement(fisa_su_t12, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(fisa_su_t15, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(fisa_su_t18, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 18, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Section 702 and the Fourth Amendment' covers three structurally distinct constraints that this corpus holds apart: the permissive statutory reading (this story), the strict statutory reading (foreign_target_strict_reading), and the constitutional warrant floor (constitutional_floor_reading). Each has its own epsilon, victim set, and classification; the readings are linked through network edges and cs_structure.reading_relations rather than merged into one story with a measurement parameter. The readings coexist as live positions across parties, but this reading's operation shapes the constitutional sibling's adjudicative environment through the secrecy-standing feedback documented in the standing_secrecy_feedback omega.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
