% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: FISA 702 Incidental Collection: Retention and Warrantless Query of U.S. Person Communications
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   The FISA Section 702 statute (50 U.S.C. § 1881a) authorizes the Executive
 *   to conduct foreign intelligence surveillance targeting non-U.S. persons
 *   reasonably believed to be outside the United States. As a structural
 *   inevitability, surveillance of foreign persons inevitably collects
 *   communications with U.S. persons — foreigners contact U.S. residents,
 *   U.S. persons travel to foreign territories, and bulk collection systems
 *   have marginal accuracy. The "incidental collection reading" interprets
 *   702 to permit retention and warrantless query of these incidentally
 *   collected U.S. person communications when justified by a "reasonable
 *   foreign intelligence purpose." This reading entered practice through
 *   administrative interpretation (NSA, FBI minimization procedures) and
 *   survived constitutional challenge (Fourth Amendment claims in Clapper v.
 *   Amnesty International; Carpenter v. United States did not directly
 *   address 702). The reading is contested: a stricter reading would require
 *   warranting before querying U.S. person communications even if collected
 *   under foreign target authority; a constitutional floor reading would
 *   require probable cause and individualized warrants for any purposeful
 *   search of U.S. person data. This constraint story instantiates the
 *   incidental collection reading as a clean, ε-invariant constraint. The
 *   extractiveness value (0.48) reflects moderate asymmetric extraction:
 *   intelligence agencies benefit from unwaranted access to a database built
 *   on foreign target authorization; U.S. persons suffer Fourth Amendment
 *   erosion without consent or exit option. The suppression value (0.68)
 *   reflects high barriers: no statutory right to notice, no statutory remedy
 *   for retention or query, judicial review limited to certification adequacy
 *   (not reasonableness of individual queries), minimization procedures are
 *   administrative rather than adversarial. The theater ratio (0.55) reflects
 *   moderate performativity: FISC approval provides legitimacy ritual, but
 *   court review is asymmetric and lacks adversarial counterparty;
 *   minimization procedures are rule-governed but not externally audited in
 *   real time.
 *
 * KEY AGENTS:
 *   - U.S. Persons Incidentally Surveilled: Primary victims (powerless/trapped) — cannot opt out, cannot contest collection, cannot remedy retention or query, no Fourth Amendment protection under this reading
 *   - Intelligence Agencies (NSA, CIA, FBI): Primary beneficiaries (institutional/arbitrage) — design and control the collection system, retain indefinite access to communications collected under foreign target authority, can query for foreign intelligence purposes without warrant
 *   - Law Enforcement (FBI Domestic Investigations): Secondary beneficiary (moderate/constrained) — gain access to 702 database for criminal investigations when foreign intelligence nexus can be invoked; constrained by minimization procedures and oversight but not by warrant requirement
 *   - Foreign Intelligence Surveillance Court (FISC): Institutional reviewer (institutional/constrained) — approves targeting certifications and oversees minimization procedures; constrained by statutory text that pre-authorizes collection categories; limited visibility into operational queries
 *   - Civil Liberties Organizations and Advocacy: Secondary victim (moderate/constrained) — bear costs of Fourth Amendment erosion; constrained by judicial deference to Executive on national security; benefit from statutory text as focal point for litigation and legislative reform
 *   - Congress (Oversight and Reauthorization): Organized actor (organized/constrained) — controls reauthorization vote; constrained by classification restrictions and Executive pressure; periodic reauthorization creates scaffold sunset mechanism
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy choice (retention and warrantless query) as inevitable consequence of bulk foreign collection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.48).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.68).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA 702 Incidental Collection: Retention and Warrantless Query of U.S. Person Communications").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, '64496e42-2634-43da-8108-e824689174db').
narrative_ontology:cs_kernel_codification('64496e42-2634-43da-8108-e824689174db', formalized).
narrative_ontology:cs_authority_grounding('64496e42-2634-43da-8108-e824689174db', extraction).
narrative_ontology:cs_interpretation_layer_present('64496e42-2634-43da-8108-e824689174db').
narrative_ontology:cs_reading_relation('64496e42-2634-43da-8108-e824689174db', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('64496e42-2634-43da-8108-e824689174db', fisa_702_statutory_text__constitutional_floor_reading, forecloses).
narrative_ontology:cs_axiom('64496e42-2634-43da-8108-e824689174db', foundational, statutory_foreign_intelligence_purpose_alternative_to_warrant).
narrative_ontology:cs_axiom_status(statutory_foreign_intelligence_purpose_alternative_to_warrant, holdable).
narrative_ontology:cs_axiom_grounding('64496e42-2634-43da-8108-e824689174db', statutory_foreign_intelligence_purpose_alternative_to_warrant, empirically_contingent).
narrative_ontology:cs_axiom('64496e42-2634-43da-8108-e824689174db', secondary, incidental_collection_necessity_justifies_retention).
narrative_ontology:cs_axiom_status(incidental_collection_necessity_justifies_retention, holdable).
narrative_ontology:cs_axiom_grounding('64496e42-2634-43da-8108-e824689174db', incidental_collection_necessity_justifies_retention, instrumental).
narrative_ontology:cs_reference_frame('64496e42-2634-43da-8108-e824689174db', foreign_intelligence_surveillance_authority_without_domestic_warrant_requirement).
narrative_ontology:cs_drift_state('64496e42-2634-43da-8108-e824689174db', contemporary_post_snowden_disclosure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('64496e42-2634-43da-8108-e824689174db', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, law_enforcement_domestic_investigations).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, u_s_persons_incidentally_surveilled).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, fourth_amendment_protection_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCIDENTALLY SURVEILLED U.S. PERSON (SNARE) — No exit available. The U.S. person cannot opt out of incidental collection because the statutory authority doesn't trigger via their own actions — it triggers via targeting a foreign person. Collection occurs without warrant; minimization procedures are administrative, not judicial. Warrantless query by FBI domestic investigators becomes possible as retention extends collection window. Maximum extraction: surveillance without due process, no right to know of query, no remedy for retention.
constraint_indexing:constraint_classification(fisa_702_statutory_text__incidental_collection_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADVOCACY AND CIVIL LIBERTIES ORGANIZATIONS (TANGLED ROPE) — Constrained by judicial deference to Executive on national security grounds. But also benefit from the statute's existence as a focal point for advocacy: the statutory text and minimization procedures provide measurable standards to litigate against, unlike purely executive action. Courts apply Katz + Fourth Amendment framework, but the FISA text creates boundaries within which they can argue. Significant extraction (Fourth Amendment erosion) but also genuine coordination function (the statute enables surveillance that intelligence agencies require for foreign intelligence).
constraint_indexing:constraint_classification(fisa_702_statutory_text__incidental_collection_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTELLIGENCE AGENCIES / FOREIGN COLLECTION AUTHORITY (ROPE) — Pure coordination benefit. The 702 statute enables systematic collection against foreign targets at scale without foreign-language court warrants. Agencies can establish collection targets, retain communications for reference, and—via reasonable foreign intelligence purpose—query the database domestically. The statute solves the coordination problem: how to enable foreign intelligence collection that serves domestic law enforcement needs without requiring dual legal frameworks. Minimal experienced extraction because agencies designed the statutory framework and maintain full control over its operation.
constraint_indexing:constraint_classification(fisa_702_statutory_text__incidental_collection_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FISC (TANGLED ROPE) — Constrained by statutory text that pre-authorizes collection categories; court reviews upstream certifications but cannot block individual targets without showing the agency lied about the targeting standard. Coordination function: FISC provides procedural legitimacy and a check on Executive unilateralism. But also extraction: the court's review power is asymmetric — it rubber-stamps most applications, lacks adversarial counterparty for foreign targets, and has no visibility into how collected communications are queried domestically. Theater ratio moderating this perspective's severity: the court's approval ritual has genuine legitimacy function even if practical review is limited.
constraint_indexing:constraint_classification(fisa_702_statutory_text__incidental_collection_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESSIONAL OVERSIGHT / REAUTHORIZATION CYCLE (SCAFFOLD) — Organized agents (Congress, inspectors general, FISC itself through reporting) see 702 as a temporary authority requiring periodic reauthorization and amendment. The 2023 Section 702 reauthorization included modest reforms (FBI minimization procedures, limitations on query of U.S. persons for criminal investigations). The scaffold perception is that surveillance authority is conditional on demonstrated restraint and public legitimacy maintenance. Sunset mechanism: periodic reauthorization vote; if Congress withholds reauth, foreign collection authority sunsets. Theater-ratio gate does not trigger here (theater_ratio is 0.55 baseline, moderate, not high) — the oversight cycle itself has some functional legitimacy despite Congress's structural deference to Executive on national security.
constraint_indexing:constraint_classification(fisa_702_statutory_text__incidental_collection_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, incidental collection is an immutable consequence of bulk foreign signal intelligence: any foreign targeting system at scale will necessarily collect some U.S. person communications (foreigners contact U.S. persons, U.S. persons in foreign territories are foreign intelligence subjects, etc.). The reading treats this incidental effect as a natural law of surveillance: you cannot do bulk SIGINT without collateral. This perspective will trigger the false-summit detection because the statistical reality of incidental collection is real, but the decision to *retain and query* those communications is a policy choice, not a law of nature. The engine should identify this as naturalization.
constraint_indexing:constraint_classification(fisa_702_statutory_text__incidental_collection_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__incidental_collection_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fisa_702_statutory_text__incidental_collection_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fisa_702_statutory_text__incidental_collection_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fisa_702_statutory_text__incidental_collection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, reflecting that intelligence agencies and law enforcement receive genuine benefit (unwarranted access to communications database) while incidentally surveilled U.S. persons bear costs (Fourth Amendment erosion, indefinite retention, administrative querying without judicial process). The value is not extreme (not 0.65+) because the statutory framework creates some procedural boundaries: FISC certification requirement, minimization procedures, and reasonable foreign intelligence purpose standard do impose some constraint on pure extraction. But the boundaries are administrative rather than constitutional, and the perpetrator (intelligence agencies) controls the interpretive framing. Measurement trajectory shows extractiveness rising from 0.35 (initial 2008 authorization era, tighter operational constraints) to 0.48 (post-2013 Snowden disclosure, expanded query authorities and retention practices documented), with modest decline to 0.45 after 2023 reauthorization reforms (FBI query limitations, enhanced minimization procedures, transparency requirements). The recent plateau suggests extraction has found a stable operational equilibrium rather than continuing to expand. Suppression (0.68): High, reflecting structural barriers to exit and remedy. U.S. persons have no statutory right to notice of incidental collection, no statutory right to challenge retention, limited judicial review focused on certification adequacy rather than query reasonableness, and administrative remedies controlled by the agencies conducting surveillance. The measurement trajectory shows suppression rising from 0.62 to 0.72 during the 2008-2013 expansion period (as query authorities broadened and retention indefinitely extended), then declining modestly to 0.68 post-2023 as minimization procedures were tightened and transparency improved. Theater ratio (0.55): Moderate, reflecting that the statutory framework and FISC process create procedural legitimacy without high performative content. FISC does conduct real review (not rubber-stamp), but the review is limited and asymmetric. The measurement trajectory shows theater rising from 0.40 to 0.58 during the 2013 crisis period (when Snowden disclosures forced public justification of the program, triggering legitimacy theater), then declining to 0.55 post-reform (as transparency reduced performative necessity and procedures became more genuine).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal in this constraint. From the incidentally surveilled U.S. person's view (powerless/trapped), the constraint is a pure snare: unwarranted surveillance with no remedy. From intelligence agencies' view (institutional/arbitrage), it is pure rope: statutory coordination mechanism enabling foreign intelligence collection at scale. From advocacy organizations' view (moderate/constrained), it is tangled rope: the statute provides a litigation focal point even though Fourth Amendment protections are substantially eroded. From FISC's view (institutional/constrained), it is also tangled rope: the court has genuine oversight function but limited practical power. From Congress's view (organized/constrained), it is scaffold: reauthorization cycles provide periodic opportunity to revise the statute (2013, 2018, 2023 amendments show iterative tightening). From the analytical observer's view (analytical/analytical), it risks appearing as mountain: incidental collection is treated as inevitable consequence of bulk SIGINT, naturalizing what is actually a policy choice about retention and query authority. The engine should flag this as false summit because the incidental collection phenomenon is real and immutable, but the decision to retain indefinitely and query without warrant is contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position relative to the constraint. Incidentally surveilled U.S. persons are victims with no exit options (trapped): d≈0.95, f(d)≈1.42, experiencing maximum effective extraction. Intelligence agencies are beneficiaries with arbitrage options (can leverage 702 authority for multiple purposes): d≈0.05, f(d)≈-0.12, experiencing negative extraction (subsidy). FBI domestic investigations are beneficiaries with constrained exit (can access 702 database for criminal investigations but not for purely domestic law enforcement): d≈0.30, f(d)≈0.15, experiencing modest extraction. Civil liberties organizations are victims with constrained exit (can litigate and advocate but face judicial deference): d≈0.75, f(d)≈1.05, experiencing above-average extraction of Fourth Amendment protections. FISC is a mixed institutional actor with constrained exit (bound by statutory text, can review but not reject categories): d≈0.50, f(d)≈0.65, experiencing symmetric extraction and benefit. Congress is an organized actor with contingent exit (can reauthorize or withold, but faces Executive pressure and classified information access constraints): d≈0.55, f(d)≈0.75, experiencing moderate extraction. The analytical observer occupies the perspectival vantage (d≈0.72, canonical analytical d), experiencing the constraint's structure as reportable but not personally trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL-LEVEL MANDATROPHY RESOLUTION: The three sibling readings (incidental collection, foreign target strict, constitutional floor) represent competing interpretations of a single statutory text's ambiguity. The incidental collection reading (this constraint) maximizes intelligence agency flexibility by interpreting 'reasonable foreign intelligence purpose' broadly and permitting administrative minimization as Fourth Amendment substitute. The foreign target strict reading (sibling constraint) would narrow the query authority to contexts where the U.S. person was the actual foreign target (not just incidentally collected contact), raising the effective extraction barrier. The constitutional floor reading (sibling constraint) would require warrants for purposeful U.S. person queries even if incidentally collected. These are not three empirical claims about the same phenomenon; they are three competing readings of statutory authority with different normative consequences. The incidental collection reading assumes that statutory certification by the AG can substitute for individualized warrants (axiom: statutory_foreign_intelligence_purpose_alternative_to_warrant). The foreign target strict reading assumes that Fourth Amendment protection applies even to incidentally collected communications (axiom: fourth_amendment_applies_to_incidental_communications). The constitutional floor reading assumes that purposeful searches of U.S. person data require warrant (axiom: warrant_required_for_purposeful_u_s_person_search). These axioms coexist as different parties' commitments (DoJ/agencies adopt incidental collection reading; civil liberties organizations advocate constitutional floor reading; courts have not fully resolved the conflict). The mandatrophy is resolved by recognizing that all three readings are defensible interpretations of the statutory text, each with a coherent authority grounding (extraction for incidental collection—agencies benefit from the interpretation; distributed for the others—no single authority enforces them). The engine should flag the omega on Fourth Amendment statutory displacement as the conceptual crux: does the statutory structure achieve genuine Fourth Amendment protection or does it merely displace it?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foreign_target_boundary_manipulation,
    'Does the foreign intelligence purpose standard create ex post reclassification risk where FBI domestic investigation initiates a query knowing it targets a U.S. person, then ex post invokes ''foreign intelligence purpose'' to justify retention and use?',
    'Forensic analysis of query logs: correlation between query initiation timing, subject matter, and classification as foreign intelligence; audit trails showing whether foreign intelligence nexus was identified before or after query; comparison with parallel warrant applications for same subjects.',
    'If ex post reclassification is systematic: statutory protection is illusory (Snare), extractiveness rises to 0.65+. If foreign intelligence nexus is genuinely predicate: statutory gate has real effect (Tangled Rope remains accurate, extractiveness ~0.48).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_target_boundary_manipulation, empirical, 'Whether foreign intelligence purpose standard is applied ex ante or ex post').

omega_variable(
    retention_window_duration_effect,
    'How does the duration of retention (currently unbounded, with queries possible years after collection) affect the effective Fourth Amendment gap?',
    'Historical analysis: correlation between retention duration and number of queries per communication; comparison of immediate-use queries vs delayed-use queries; evidence on whether retention enables investigative drift (queries initiated for different purpose than original foreign intelligence basis).',
    'If retention enables systematic investigative drift: extraction rises (ε→0.55+). If retention is primarily reference function: extraction remains moderate (ε~0.48).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retention_window_duration_effect, empirical, 'Effect of indefinite retention on incidental Fourth Amendment erosion').

omega_variable(
    minimization_procedure_actual_effect,
    'Do administrative minimization procedures (NSA minimization rules, FBI query limitations post-2023) reduce the effective suppression (barriers to querying U.S. person communications) or do they provide appearance of protection while leaving structure intact?',
    'Audit of minimization compliance and enforcement: documented violations and penalties; comparison of pre- and post-2023 reauthorization query patterns; evidence on whether minimization rules actually gate queries or merely require after-the-fact documentation.',
    'If minimization is effective: suppression drops to ~0.55 and constraint moves toward Rope. If minimization is performative: suppression remains high (~0.68) and constraint remains Tangled Rope with theater component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_procedure_actual_effect, empirical, 'Actual effectiveness of administrative minimization vs procedural theater').

omega_variable(
    fourth_amendment_statutory_displacement,
    'Does the statutory warrant structure (certification by Attorney General + FISC approval) constitute a Fourth Amendment-compliant alternative to individualized warrants, or is it a statutory displacement that leaves Fourth Amendment protection eroded?',
    'Constitutional case law development: Supreme Court positioning on statutory alternatives to Katz/warrant requirement; empirical evidence on whether statutory procedure achieves warrant function (particularization, probable cause assessment); comparison with historical Fourth Amendment standards.',
    'If statutory procedure is genuine Fourth Amendment alternative: constraint is legitimate Tangled Rope with functional coordination. If statutory procedure is displacement: constraint naturalizes Fourth Amendment erosion, and false-summit detection triggers (mountain classification is naturalization of policy choice).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fourth_amendment_statutory_displacement, conceptual, 'Whether FISA certification structure satisfies Fourth Amendment warrant requirement').

omega_variable(
    incidental_collection_necessity_baseline,
    'What percentage of collected communications are actually incidental (foreign target contact with U.S. person) versus deliberate targeting of U.S. persons via foreign nexus? Does the incidental/deliberate ratio shift over time as legal boundaries are tested?',
    'NSA statistical reporting on collection types (published in transparency reports or congressional briefings); analysis of collection targeting patterns; historical comparison of designated U.S. person inclusion in foreign target definitions.',
    'If high incidental ratio (>80%): supports mountain natural law framing. If substantial deliberate component: supports reading that statutory authority enables deliberate targeting via foreign nexus classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incidental_collection_necessity_baseline, empirical, 'Ratio of genuinely incidental to deliberately targeted U.S. person communications').

omega_variable(
    contested_kernel_alternative_readings,
    'This constraint is one reading of the FISA 702 statutory text. What structural shifts occur if the foreign_target_strict_reading or constitutional_floor_reading interpretations are adopted instead?',
    'Comparative analysis of three readings'' core axioms and authority groundings; identification of which empirical facts would favor each reading; analysis of how FISC case law and reauthorization amendments shifted the balance between readings.',
    'Different reading → different extractiveness value and different beneficiary/victim structure. This omega documents the committer-frame ambiguity: the statute admits multiple readings with materially different implications for Fourth Amendment protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contested_kernel_alternative_readings, conceptual, 'Kernel-level ambiguity: competing statutory readings admit different constraint profiles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa702_ic_theater_initial, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(fisa702_ic_theater_2013_crisis, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(fisa702_ic_theater_post_2023_reform, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(fisa702_ic_extract_initial, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fisa702_ic_extract_2013_snowden, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(fisa702_ic_extract_post_2023_reform, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(fisa702_ic_suppression_initial, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(fisa702_ic_suppression_2013_peak, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(fisa702_ic_suppression_post_2023_reform, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, third_party_doctrine_telecommunications_metadata).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, minimization_procedures_legal_sufficiency).

% DUAL FORMULATION NOTE:
% The FISA 702 statutory text admits three materially different structural readings with different extractiveness values. The incidental_collection_reading (this constraint, ε≈0.48) permits indefinite retention and warrantless query of incidentally collected U.S. person communications. The foreign_target_strict_reading would restrict authority to communications where the U.S. person was deliberately targeted as foreign intelligence subject (ε≈0.30, Tangled Rope). The constitutional_floor_reading would require warrants for purposeful U.S. person queries (ε≈0.20, Rope or Scaffold depending on implementation). Each story gets its own extractiveness, beneficiary/victim structure, and perspectives. They are linked via network.affects_constraints because judicial interpretation of 702 in one reading influences the operational constraints on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__incidental_collection_reading, institutional, 0.05).
constraint_indexing:directionality_override(fisa_702_statutory_text__incidental_collection_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
