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
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: FISA Section 702 Incidental Collection Reading — Warrantless Query of U.S. Person Communications
 *   domain: constitutional/national_security/surveillance
 *
 * SUMMARY:
 *   This constraint story captures one reading of the FISA Section 702
 *   statutory text: the 'incidental collection reading' that permits
 *   retention and warrantless query of U.S. person communications swept up
 *   during foreign-targeted upstream collection. The reading treats the
 *   foreign intelligence purpose as a categorical justification that
 *   displaces the Fourth Amendment warrant requirement for any communication
 *   incidentally collected, even when the query targets a U.S. person for
 *   domestic investigative purposes. The constraint is claimed as a
 *   tangled_rope — it performs a genuine coordination function (foreign
 *   intelligence collection at scale) while simultaneously extracting from
 *   U.S. persons whose communications are queried without warrants. The
 *   engine will compute per-seat classifications from the structural data:
 *   intelligence community and FBI as beneficiaries/agenda_setters with low
 *   directionality; U.S. persons and criminal defendants as payers with high
 *   directionality (trapped exit, powerless). The claimed_type and metrics
 *   are authored independently — the claim is tangled_rope, the metrics
 *   describe substantially extractive, actively enforced operation with
 *   rising theater.
 *
 * KEY AGENTS:
 *   - intelligence_community: Primary agenda_setter (institutional/arbitrage) — operates collection, sets procedures
 *   - fbi: Primary beneficiary (institutional/arbitrage) — queries database for domestic investigations without warrants
 *   - us_persons_incidentally_collected: Primary payer (powerless/trapped) — communications swept up incidentally, no exit
 *   - us_persons_queried: Primary payer (powerless/trapped) — subject to warrantless backdoor searches
 *   - criminal_defendants: Payer (powerless/trapped) — prosecuted with 702-derived evidence, often unaware
 *   - fisa_court: Agenda_setter (institutional/constrained) — authorizes but cannot review individual queries
 *   - congress: Agenda_setter (institutional/arbitrage) — legislative authority, political incentives favor renewal
 *   - privacy_advocates: Excluded (organized/constrained) — would object but excluded from authorization process
 *   - federal_courts: Observer (institutional/analytical) — adjudicate challenges but constrained by standing and state secrets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.72).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA Section 702 Incidental Collection Reading — Warrantless Query of U.S. Person Communications").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional/national_security/surveillance").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, '6070ed0d-518c-4695-b019-7e9b49208127').
narrative_ontology:cs_kernel_codification('6070ed0d-518c-4695-b019-7e9b49208127', formalized).
narrative_ontology:cs_authority_grounding('6070ed0d-518c-4695-b019-7e9b49208127', extraction).
narrative_ontology:cs_interpretation_layer_present('6070ed0d-518c-4695-b019-7e9b49208127').
narrative_ontology:cs_reading_relation('6070ed0d-518c-4695-b019-7e9b49208127', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('6070ed0d-518c-4695-b019-7e9b49208127', fisa_702_statutory_text__constitutional_floor_reading, forecloses).
narrative_ontology:cs_axiom('6070ed0d-518c-4695-b019-7e9b49208127', foundational, foreign_intelligence_exception_to_warrant_requirement).
narrative_ontology:cs_axiom_status(foreign_intelligence_exception_to_warrant_requirement, holdable).
narrative_ontology:cs_axiom_grounding('6070ed0d-518c-4695-b019-7e9b49208127', foreign_intelligence_exception_to_warrant_requirement, empirically_contingent).
narrative_ontology:cs_axiom('6070ed0d-518c-4695-b019-7e9b49208127', foundational, minimization_procedures_satisfy_fourth_amendment).
narrative_ontology:cs_axiom_status(minimization_procedures_satisfy_fourth_amendment, holdable).
narrative_ontology:cs_axiom_grounding('6070ed0d-518c-4695-b019-7e9b49208127', minimization_procedures_satisfy_fourth_amendment, conventional).
narrative_ontology:cs_reference_frame('6070ed0d-518c-4695-b019-7e9b49208127', fisa_702_original_authorization).
narrative_ontology:cs_drift_state('6070ed0d-518c-4695-b019-7e9b49208127', post_2018_reauthorization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6070ed0d-518c-4695-b019-7e9b49208127', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_community).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, national_security_apparatus).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_queried).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, criminal_defendants).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_exception_to_warrant_requirement).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, minimization_procedures_satisfy_fourth_amendment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates upstream collection under Section 702 authority; designs targeting, minimization, and querying procedures; controls the technical infrastructure and retains institutional knowledge. Justifies the program as essential for foreign intelligence. Collects budgetary and operational authority from the arrangement.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, intelligence_community, agenda_setter,
    institutional, generational, arbitrage, global).

% Queries the 702 database for domestic investigations using U.S. person identifiers without obtaining warrants. Receives intelligence value from incidentally collected communications. Does not operate the collection but is a primary consumer of the query capability.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi, beneficiary,
    institutional, generational, arbitrage, national).

% Communications with foreign targets are swept up incidentally during upstream collection. Cannot avoid collection when communicating with foreigners abroad. Have no notice, no standing to challenge, and no practical exit from the surveillance net. Bear the privacy cost of retention.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected, payer,
    powerless, biographical, trapped, national).

% Subject to warrantless queries of the 702 database using their identifiers (email, phone, name) by FBI and IC analysts for foreign intelligence or domestic investigative purposes. The querying occurs without probable cause, particularized suspicion, or judicial approval. No practical exit exists short of ceasing all electronic communication with foreigners.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_queried, payer,
    powerless, biographical, trapped, national).

% May be prosecuted using evidence derived from 702 queries. Often unaware the evidence originated from warrantless surveillance due to parallel construction and notice deficiencies. Cannot suppress evidence they do not know exists. Bear the due process cost of the arrangement.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, criminal_defendants, payer,
    powerless, immediate, trapped, national).

% Reviews and approves annual certifications and targeting procedures. Issues opinions interpreting statutory terms. Constrained by ex parte nature of proceedings, classified record, and institutional deference to executive branch expertise. Cannot review individual queries.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fisa_court, agenda_setter,
    institutional, generational, constrained, national).

% Enacted and repeatedly reauthorized Section 702. Receives classified briefings but limited independent verification capacity. Political incentives favor renewal. Could amend or sunset the authority but has not done so since 2008.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Civil liberties organizations (ACLU, EFF, CDT) that challenge 702 in court and Congress. Excluded from FISA Court proceedings and classified briefings. Their objections are filtered through public advocacy and amicus briefs rather than direct participation in the authorization process.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, privacy_advocates, excluded,
    organized, generational, constrained, national).

% Adjudicate Fourth Amendment challenges to 702 evidence in criminal cases. Constrained by state secrets doctrine, standing barriers, and deference to national security judgments. Provide the only adversarial testing of the program's constitutionality.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables collection of foreign intelligence communications transiting U.S. infrastructure at scale and speed without individualized warrants for each foreign target, solving the problem of intelligence gaps against non-U.S. persons abroad who use U.S. providers and networks.
% TRANSFER_FUNCTION: Moves U.S. persons' communications content and metadata from private communications into government databases, where they are retained and made available for warrantless querying by intelligence agencies and FBI for both foreign intelligence and domestic investigative purposes.
% ABSENT_VOICES: U.S. persons whose communications are incidentally collected (cannot know or object at time of collection); criminal defendants prosecuted with 702-derived evidence (often unaware of source due to parallel construction); privacy advocates and civil liberties organizations (excluded from FISA Court proceedings and classified briefings where the program's scope is defined).
% DISAPPEARANCE_RATIONALE: If Section 702 authority vanished overnight, the intelligence community would lose its primary legal basis for upstream collection on U.S. infrastructure; the FBI would lose warrantless access to a database of communications involving U.S. persons; Congress would need to enact new authority or accept a fundamental intelligence gap; the legal architecture enabling backdoor searches would collapse.
% FOUNDING_PROBLEM: Post-9/11 intelligence failures and the technological shift of foreign communications onto U.S. infrastructure created a perceived gap: the intelligence community could not collect foreign-to-foreign communications transiting U.S. switches without individualized FISA warrants, which were too slow and narrow for modern threats.
% FOUNDING_PROBLEM_CORROBORATION: The intelligence community attests the foreign intelligence gap persists and has grown (PCLOB 2014 report, ODNI testimonies). Critics including PCLOB minority reports, FISA Court opinions (2011, 2018), Snowden disclosures, and congressional oversight hearings (Church Committee legacy, 2017-2018 reauthorization debates) corroborate that the arrangement has expanded into routine domestic querying — the founding problem has morphed into a domestic surveillance tool.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness (0.45) reflects the statutory authorization for warrantless querying of U.S. person communications — a substantial transfer of privacy interests to government agencies. Suppression (0.72) is high because the constraint's persistence depends on active enforcement: FISA Court oversight, minimization procedures, classification barriers, and standing doctrines that prevent judicial review. Theater_ratio (0.38, rising) captures the growing gap between the statutory minimization framework and actual querying practice — the procedures exist but the query volume and domestic investigative use have expanded. Accessibility_collapse (0.68) is elevated because once the statutory framework is understood, alternatives (traditional FISA warrants, criminal wiretaps) are legally foreclosed for the covered collection. Resistance (0.55) is moderate — there is litigation, congressional debate, and public advocacy, but structural barriers (classification, standing, state secrets) limit effective challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the intelligence_community and fbi seats (low directionality), the constraint appears as genuine coordination: a congressionally authorized, court-overseen framework for foreign intelligence that incidentally sweeps U.S. persons but minimizes harm. From the us_persons_incidentally_collected, us_persons_queried, and criminal_defendants seats (high directionality, trapped exit), the same structure operates as extraction: their communications are retained and searched without warrants, probable cause, or notice. The fisa_court seat experiences moderate directionality — it administers the framework but is structurally constrained from meaningful oversight. The engine computes this divergence from the declared beneficiaries/victims, power atoms, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: intelligence_community, fbi, national_security_apparatus — these agents collect intelligence value and operational authority from the arrangement. Victims declared: us_persons_incidentally_collected, us_persons_queried, criminal_defendants — these agents bear privacy and due process costs with no practical exit (trapped). The derivation chain assigns low directionality (d ~ 0.1-0.2) to beneficiaries (institutional power, arbitrage exit) and high directionality (d ~ 0.8-0.9) to victims (powerless, trapped). FBI sits at the intersection: institutional power but benefits from the extraction component specifically, so its effective extraction is amplified for the query function. No directionality_overrides needed — the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (foreign intelligence gap on U.S. infrastructure) was live in 2008. The status is now contested: the intelligence community says the gap persists; oversight bodies and courts document mission creep into domestic surveillance. The coordination function (foreign collection) remains real, but the extraction component (backdoor searches of U.S. persons) has grown without statutory amendment — the minimization procedures have not prevented the query function from becoming a domestic investigative tool. This is classic mandatrophy: a mandate (foreign intelligence) that has atrophied in practice while the constraint persists and expands into a new function (domestic querying) that was not the founding justification. The mandate has not been formally resolved; it has been displaced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the warrantless querying of incidentally collected U.S. person communications structurally necessary for the foreign intelligence coordination function, or is it mission creep that extracts from rights-holders without improving foreign targeting?',
    'Comparative analysis of intelligence products derived from backdoor searches vs. traditional foreign-target-only queries; declassification of query logs showing proportion of U.S. person queries yielding actionable foreign intelligence.',
    'If backdoor searches are unnecessary for foreign intelligence, the extraction component is pure rent-seeking on a genuine coordination function — strengthening the tangled_rope classification. If necessary, the extraction is the price of coordination, moving the constraint toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the extraction component (backdoor searches) is functionally coupled to the coordination function (foreign intelligence collection) or is severable mission creep.').

omega_variable(
    minimization_effectiveness,
    'Do the statutory minimization procedures meaningfully limit retention, dissemination, and querying of U.S. person information, or are they performative compliance that fails to prevent routine access?',
    'FISA Court compliance incident reports; PCLOB audits; whistleblower testimony; technical analysis of query audit trails.',
    'If minimization is effective, suppression is lower and the constraint''s coordination function is more credible. If performative, suppression is higher and the theater_ratio understates the gap between procedure and practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_effectiveness, empirical, 'Whether administrative minimization procedures are functional constraints or theatrical cover.').

omega_variable(
    exit_impossibility_for_us_persons,
    'Can U.S. persons meaningfully exit the surveillance net by altering communication behavior, or is the constraint structurally inescapable for anyone communicating with foreigners?',
    'Technical analysis of upstream collection points; legal analysis of whether encryption, foreign providers, or metadata obfuscation provide practical exit; survey of affected communities.',
    'If exit is structurally impossible, the trapped exit_option is validated and effective extraction for U.S. persons approaches the theoretical maximum. If partial exit exists, the constraint''s extraction is modulated by exit availability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_impossibility_for_us_persons, empirical, 'Whether U.S. persons have any practical exit from incidental collection and warrantless querying.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the incidental collection reading''s core premise — that statutory foreign intelligence purpose displaces the Fourth Amendment warrant requirement for U.S. person communications — logically foreclose the constitutional floor reading within a single legal framework?',
    'Supreme Court jurisprudence on statutory vs. constitutional authority (e.g., whether Congress can authorize what the Constitution forbids); analysis of whether a single court could consistently apply both readings.',
    'If forecloses, the two readings cannot coexist in one framework — one must displace the other. If coexists_with, they represent competing but structurally compatible positions held by different institutional actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Structural relationship between the incidental collection reading and the constitutional floor reading of the same statutory kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa702_incidental_tr_t0, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fisa702_incidental_tr_t3, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 3, 0.18).
narrative_ontology:measurement(fisa702_incidental_tr_t6, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(fisa702_incidental_tr_t9, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 9, 0.31).
narrative_ontology:measurement(fisa702_incidental_tr_t12, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(fisa702_incidental_tr_t16, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 16, 0.38).

% Extraction over time
narrative_ontology:measurement(fisa702_incidental_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fisa702_incidental_be_t3, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(fisa702_incidental_be_t6, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(fisa702_incidental_be_t9, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 9, 0.41).
narrative_ontology:measurement(fisa702_incidental_be_t12, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(fisa702_incidental_be_t16, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 16, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa702_incidental_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fisa702_incidental_su_t3, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(fisa702_incidental_su_t6, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(fisa702_incidental_su_t9, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 9, 0.71).
narrative_ontology:measurement(fisa702_incidental_su_t12, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(fisa702_incidental_su_t16, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 16, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__incidental_collection_reading, 0.1).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This reading and foreign_target_strict_reading decompose the statutory text into competing interpretations of 'targeting' and 'minimization.' The constitutional_floor_reading decomposes the constitutional question into a separate constraint about Fourth Amendment floor. All three share the kernel fisa_702_statutory_text. The incidental_collection_reading (this story) has higher extractiveness (0.45) than the foreign_target_strict_reading would claim (near 0) because it authorizes querying that the strict reading forbids. The constitutional_floor_reading would claim even higher extractiveness from a constitutional violation perspective.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
