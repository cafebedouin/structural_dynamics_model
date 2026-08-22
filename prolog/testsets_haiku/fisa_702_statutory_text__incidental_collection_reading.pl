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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: Section 702 Incidental Collection and Warrantless Query Authority
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   Section 702 of the Foreign Intelligence Surveillance Act (2008)
 *   authorizes the NSA and other intelligence agencies to collect
 *   communications of foreign targets reasonably believed to be outside the
 *   United States. When a foreign target communicates with a U.S. person,
 *   that U.S. person's communications are incidentally collected and
 *   retained. Under the 'incidental collection reading' instantiated here,
 *   the statute permits the intelligence community to query this incidentally
 *   collected database for any foreign intelligence purpose — a gate
 *   interpreted expansively to include FBI domestic counterintelligence and
 *   criminal investigations when framable as foreign intelligence. This
 *   reading displaces the Fourth Amendment warrant requirement: U.S. persons
 *   subject to incidental collection have no pre-search judicial process, no
 *   opportunity to object, and no practical exit. The constraint extracts
 *   warrant-protected privacy from U.S. persons and converts it to
 *   warrantless administrative search authority. This is ONE reading of the
 *   FISA 702 kernel; sibling readings (strict foreign-target-only,
 *   constitutional-floor-warrant-required) interpret the same statute and
 *   kernel differently, producing different victim sets and different
 *   extraction profiles.
 *
 * KEY AGENTS:
 *   - U.S. person residents: subjects of incidental collection and warrantless query; trapped; cannot exit or avoid surveillance
 *   - Intelligence community (NSA/CIA/DIA): primary beneficiary; sets collection scope and query justifications; institutional power
 *   - FBI domestic investigations: secondary beneficiary; accesses incidentally collected data for domestic cases; lower query barrier than Title III
 *   - Executive foreign intelligence function: agenda-setter; authorizes collection authorities and 'foreign intelligence purpose' justifications
 *   - FISA Court: structurally sidelined under this reading; reviews initial target certification but NOT incidental queries
 *   - Congress oversight committees: post-hoc observers; can reauthorize but cannot reverse individual queries
 *   - Civil liberties advocates: excluded from operational rules; challenge through litigation and legislation
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
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "Section 702 Incidental Collection and Warrantless Query Authority").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, 'ab0f4658-aa6a-45ef-89cc-da20f1653938').
narrative_ontology:cs_kernel_codification('ab0f4658-aa6a-45ef-89cc-da20f1653938', fixed_text).
narrative_ontology:cs_authority_grounding('ab0f4658-aa6a-45ef-89cc-da20f1653938', extraction).
narrative_ontology:cs_interpretation_layer_present('ab0f4658-aa6a-45ef-89cc-da20f1653938').
narrative_ontology:cs_reading_relation('ab0f4658-aa6a-45ef-89cc-da20f1653938', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab0f4658-aa6a-45ef-89cc-da20f1653938', fisa_702_statutory_text__constitutional_floor_reading, influences).
narrative_ontology:cs_axiom('ab0f4658-aa6a-45ef-89cc-da20f1653938', foundational, incidental_data_query_accessibility_for_foreign_intelligence_purpose).
narrative_ontology:cs_axiom_status(incidental_data_query_accessibility_for_foreign_intelligence_purpose, holdable).
narrative_ontology:cs_axiom_grounding('ab0f4658-aa6a-45ef-89cc-da20f1653938', incidental_data_query_accessibility_for_foreign_intelligence_purpose, empirically_contingent).
narrative_ontology:cs_axiom('ab0f4658-aa6a-45ef-89cc-da20f1653938', foundational, warrant_requirement_displaced_by_statutory_authorization_and_administrative_minimization).
narrative_ontology:cs_axiom_status(warrant_requirement_displaced_by_statutory_authorization_and_administrative_minimization, holdable).
narrative_ontology:cs_axiom_grounding('ab0f4658-aa6a-45ef-89cc-da20f1653938', warrant_requirement_displaced_by_statutory_authorization_and_administrative_minimization, conventional).
narrative_ontology:cs_reference_frame('ab0f4658-aa6a-45ef-89cc-da20f1653938', foreign_target_warrant_exemption_framework).
narrative_ontology:cs_drift_state('ab0f4658-aa6a-45ef-89cc-da20f1653938', contemporary_incidental_query_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab0f4658-aa6a-45ef-89cc-da20f1653938', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_community).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, executive_foreign_intelligence_function).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, u_s_person_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% U.S. persons whose communications are incidentally collected when communicating with foreign targets. They cannot avoid collection by leaving the jurisdiction (territorial dragnet), cannot know collection has occurred, and have no legal mechanism to exclude their data or suppress warrantless queries except through litigation after disclosure — a costly and typically classified proceeding. Bear the extraction (surveillance without warrant, retention without notice, query access without probable cause) passively and without consent or exit option.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, u_s_person_residents, payer,
    powerless, biographical, trapped, national).

% The NSA, FBI, CIA, and Defense Intelligence Agency collect, retain, and query communications under 702 authority. They set the scope of what counts as a foreign target, determine minimization procedures, approve queries by downstream users, and define what justifies 'foreign intelligence purpose.' Collect foreign intelligence without warrant requirement on foreign targets; incidental U.S. person data is retained and accessible for queries. This reading permits them to search the incidentally collected database for domestic counterintelligence, criminal investigation, and intelligence purposes — expanding their access far beyond the original foreign target.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, intelligence_community, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, intelligence_community, agenda_setter).

% FBI field offices and counterintelligence divisions access the 702 database to investigate domestic crime and counterintelligence cases. Under this reading, they can query incidentally collected U.S. person communications without separate warrant or judicial approval if the query is framed as serving 'foreign intelligence purpose' — even when the actual investigative interest is domestic. The query gate is administrative (FBI FOIA minimization procedures) not judicial (Fourth Amendment warrant requirement), dramatically lowering the cost of search access compared to Title III wiretaps or pen registers.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations, beneficiary,
    institutional, biographical, mobile, national).

% Foreign persons who are the authorized targets of 702 collection. They benefit from the constraint in that collection directed at them is authorized by statute rather than requiring case-by-case warrant approval. They are not the focus of extraction under this reading — the constraint's asymmetry flows through U.S. persons, not through foreign targets themselves.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_targets, beneficiary,
    moderate, biographical, constrained, global).

% The President, Secretary of State, and executive officials who authorize foreign intelligence collection priorities. Under 702 and this reading, they can define what counts as foreign intelligence purpose expansively (counterterrorism, cybersecurity, sanctions enforcement, etc.) and thereby justify queries that sweep incidentally collected U.S. person data into domestic investigations. The executive sets the agenda — what targets, what minimization rules, what counts as foreign intelligence justification — with limited judicial oversight.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, foreign_policy_executive, agenda_setter,
    institutional, generational, arbitrage, global).

% Senate and House intelligence committees receive classified briefings on 702 operations and approve the bulk collection authorities. Under this reading, they are informed but have no power to reverse a query decision; their role is post-hoc review and reauthorization of the collection authorities themselves. They observe the constraint's operation but do not directly control any agent's access.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congress_oversight_committees, observer,
    institutional, generational, analytical, national).

% The Foreign Intelligence Surveillance Court reviews the government's certification that foreign targets are reasonably believed to be abroad and engaged in foreign intelligence activities. Under this reading, the FISA Court's review is limited to the initial certification of foreign targets; it does NOT review individual queries of incidentally collected U.S. person communications, which are governed by administrative minimization procedures only. The Court is structurally sidelined from the query gate — the core extraction mechanism.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fisa_court, observer,
    institutional, generational, analytical, national).

% Privacy organizations, civil rights groups, and some Congress members argue that incidental collection of U.S. person communications and warrantless query access violates the Fourth Amendment and should be prohibited or severely limited. They are excluded from the constraint's operational rules — they do not set minimization procedures, do not approve queries, do not sit on any approval chain. Their access to the constraint's mechanics is through litigation (rare, slow, typically classified) or legislative amendment (blocked by executive/intel community lobbying). They would fundamentally restructure the constraint if included.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, intelligence_community).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the U.S. foreign intelligence apparatus to collect, retain, and search communications involving foreign targets without the delay and administrative burden of obtaining separate warrants for each collection target or query — a coordination problem if foreign counterintelligence must be rapid and flexible across a large foreign target set.
% TRANSFER_FUNCTION: Moves Fourth Amendment warrant protections away from U.S. persons whose communications are incidentally collected. The transfer is from constitutional protections (warrant requirement, probable cause, particularity) to administrative procedures (minimization protocols set by the executive, not judicial review). Incidentally collected U.S. person data flows from the warrant-protected query regime to the administrative-only regime, accessible to FBI domestic investigators and executive agencies for purposes labeled 'foreign intelligence' without separate judicial process.
% ABSENT_VOICES: U.S. persons whose communications are collected incidentally cannot object — they do not know collection occurred. Foreign targets nominally included in the collection relationship are not represented in the minimization procedures. International human rights advocates and foreign governments whose nationals are U.S. residents are excluded. Telecommunications companies that handle the traffic are excluded from the query approval chain (though some see collection volume, they do not participate in query authorization).
% DISAPPEARANCE_RATIONALE: If this reading — and the statutory authority it instantiates — disappeared overnight, the FBI could not query incidentally collected U.S. person communications without obtaining warrants. Foreign intelligence collection would continue under 702 for its original purpose, but the backdoor access to incidentally collected data for domestic investigations would close. The constraint's operation directly channels incidental data into domestic use; its removal would restore Fourth Amendment gating.
% FOUNDING_PROBLEM: Post-9/11 foreign intelligence collection was conducted under legal theories that required separate warrants or certifications for each foreign target, and additional process for any incidental U.S. person data. The delays and volume constraints were seen as insufficient for rapid counterterrorism response against a geographically dispersed, communications-dependent adversary. 702 was enacted to streamline foreign collection by removing per-target warrant requirements.
% FOUNDING_PROBLEM_CORROBORATION: Executive and intelligence community officials attest the founding problem (slow foreign intelligence collection hampered counterterrorism) remains live. Congressional intelligence committees accepted this rationale in periodic reauthorizations. Civil liberties organizations and several federal courts attest the founding problem has been solved — foreign collection occurs at scale and speed — and the persistence of warrantless incidental-collection access serves executive convenience and scope expansion, not counterterrorism necessity. Academic analysis and the Snowden revelations provide outside corroboration that bulk collection has far exceeded the original threat-response scope.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high (0.45) because the constraint moves warrant protections from U.S. persons to administrative procedures, but U.S. persons themselves are not the primary intelligence target — the extraction is collateral, not the main function. This distinguishes it from a pure snare targeting U.S. persons as subjects. Suppression is high (0.72) because U.S. persons cannot know they are collected, cannot object prospectively, and face classified-proceeding barriers to post-hoc litigation. Theater is substantial (0.58) because the operative justification in practice is often foreign intelligence framing of domestic investigations — the theater lies in the foreign intelligence label masking domestic queries. The measurement series show rising extractiveness over the interval: as the database accumulated incidentally collected data and FBI access patterns expanded to more domestic investigations, the constraint's actual scope and operational extraction increased, though the statutory language remained constant. This drift is observable in disclosed FISA Court filings and NSA IG reports. The rising theater_ratio reflects the documented expansion of 'foreign intelligence purpose' to justify domestic counterintelligence and criminal investigations. Suppression stays high and stable because the opacity of collection and query was engineered at the constraint's design.
 *
 * PERSPECTIVAL GAP:
 *   The intelligence community's seat and the U.S. person victim's seat experience opposite types from the same constraint. From the intelligence seat: this is tangled_rope — they benefit from the coordination function (streamlined foreign collection) and pay nothing; they set the minimization rules; the constraint solves a genuine foreign intelligence problem. From the U.S. person seat: this is a snare with added inertia — they are trapped subjects of warrantless search, bear the extraction (no warrant), have no exit, and cannot exit even if they wanted to (territorial dragnet). The engine computes these divergent types from the structural data: the intelligence community is the agenda-setter with institutional power and direct beneficiary status (d near 0.0); U.S. persons are powerless victims trapped by nationality, with no exit option (d near 1.0). The perspectival gap is the core signal that this is a tangled_rope masking extraction — the coordination function is real (foreign intelligence collection) but asymmetric extraction is also real (warrant displacement for incidental U.S. persons).
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. person residents have directionality d ≈ 1.0 (full target): they bear the extraction (warrantless search, retained data, query access without warrant), they are trapped by citizenship/residence, and they have no exit option. Their d is unambiguous. Intelligence community and FBI have d near 0.0 (beneficiaries): they collect the direct benefit (access to foreign and incidental U.S. person communications without warrant delay), they set the agenda (minimization procedures, query justifications), and they have institutional power and mobility (they can adjust the scope and rules). The executive foreign intelligence function similarly has low d — it authorizes the scope and purpose definitions. FISA Court has d ≈ 0.5 (symmetric) — it performs a real judicial function (reviewing foreign target certification) but is structurally sidelined from the query gate, so it neither benefits from nor suffers the warrant displacement. Civil liberties advocates have high d as excluded parties — they bear the cost of being unable to challenge the constraint, but their directionality is not on the extraction axis itself (they are not subjects of surveillance), so they are not entered in the base_properties.victims array. No overrides are needed; the structural derivation from power + exit + beneficiary/victim status yields the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This story instantiates a key mandatrophy pattern: the founding mandate (foreign intelligence collection) is live and achieved — collection occurs at scale and speed. The operational mandate (incidental U.S. person query access to support intelligence operations) is also live. But the constitutional mandate — Fourth Amendment warrant protection for U.S. person communications — has been displaced and is no longer operative under this reading. The constraint persists by reframing queries of incidentally collected U.S. person data as foreign intelligence operations rather than domestic searches of U.S. persons. The classification as tangled_rope captures this hybrid: coordination is real (foreign collection is genuinely faster and less burdened); extraction is also real (U.S. persons lose warrant protection); both are structural features, not measurement artifacts. A piton classification would require theater_ratio to dominate (the foreign intelligence justification is mostly performed maintenance), but theater_ratio at 0.58 shows the foreign intelligence function is still operating — the performance is not purely theatrical, just increasingly stretched to cover domestic queries. Mandatrophy is not resolved; it is covered by reframing (foreign intelligence purpose label shields domestic queries), which is why the constraint persists despite the constitutional mandate erosion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foreign_intelligence_purpose_scope,
    'What counts as ''foreign intelligence purpose'' sufficient to justify query of incidentally collected U.S. person data? Is the scope defined by the statute, by executive policy, or by operational practice?',
    'Declassified NSA and FBI guidance documents, FISA Court opinions, congressional oversight testimonies, and statistical data on query justification categories would reveal whether the operative standard is narrow (counterterrorism, espionage) or expansive (any connection to foreign policy, sanctions, international crime).',
    'If ''foreign intelligence purpose'' is defined narrowly by statute or FISA Court, extractiveness drops substantially (near 0.25) because most incidental queries would be excluded. If defined expansively through executive interpretation or practice, extractiveness is sustained or rises (current 0.45+) because most domestic investigations can be reframed as foreign intelligence. The scope of the foreign intelligence purpose gate is the primary valve on extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreign_intelligence_purpose_scope, empirical, 'The operational definition of foreign intelligence purpose determines query access scope and thus the constraint''s effective extractiveness.').

omega_variable(
    warrant_requirement_fourth_amendment_survival,
    'Does the Fourth Amendment require a warrant (or warrant-equivalent judicial process) for 702 queries of U.S. person communications, or does the statute''s authorization suffice to satisfy constitutional requirements?',
    'Supreme Court decision on the constitutionality of 702 queries under Fourth Amendment standards; alternately, judicial recognition of the warrant requirement would force legislative remedy or policy change.',
    'If the Supreme Court holds that Fourth Amendment requires warrant, the constraint moves from tangled_rope (current reading) to snare-only or becomes enjoined entirely. Extractiveness would drop to near 0 (warrant requirement = no extraction) or the constraint is dismantled. This is the sibling ''constitutional_floor_reading'' outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warrant_requirement_fourth_amendment_survival, conceptual, 'Whether statutory authorization for incidental queries is constitutionally sufficient without separate warrant process.').

omega_variable(
    incidental_collection_scope_boundary,
    'When U.S. person A communicates with foreign target B, how much of A''s communications are legitimately ''incidental''? Does the statute''s scope permit collection of A''s separate conversations with other U.S. persons C and D, if they occur near in time or in the same communications metadata cluster as the A-B conversation?',
    'FISA Court opinions, NSA IG reports, or congressional investigation into collection scope and metadata proximity standards would establish whether incidental collection is narrowly tailored (only A-B conversation) or expansively inclusive (all communications in a temporal window or metadata cluster containing A and any foreign entity).',
    'Narrow incidental scope (only direct A-B communications) reduces extraction by limiting the size of the incidentally-collected U.S. person database and thus the query access surface. Expansive scope (metadata clustering, temporal windows) increases extractiveness to 0.55+ by growing the dataset and access opportunity. The boundary between direct incidental collection and over-collection determines the practical size of the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_collection_scope_boundary, empirical, 'Whether incidental collection is limited to direct foreign-target communications or includes clustering and temporal neighborhood effects.').

omega_variable(
    this_reading_forecloses_strict_foreign_target,
    'Does the incidental_collection_reading logically foreclose the foreign_target_strict_reading (foreign targets only, incidental data inaccessible for domestic purpose) within a single regulatory framework?',
    'Textual analysis of the statute and the readings'' core premises. If both readings can coexist as different policy choices from the same statutory language, neither forecloses. If one reading''s core premise (incidental data accessible for foreign intelligence purpose) directly contradicts the other''s (incidental data inaccessible for domestic purpose), foreclosure holds.',
    'If foreclosure holds: the readings cannot both be operative in the same legal system; the constraint''s persistence requires either this reading''s victory or the strict reading''s legislative reversal. If coexistence holds: the readings represent different factions and different interpretations; the constraint''s persistence is stable under this reading, but the strict reading remains a live alternative if policy shifts. The reading_relations field in cs_structure reports this determination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_reading_forecloses_strict_foreign_target, conceptual, 'Whether this reading''s core operational claim (incidental data accessible via FI purpose queries) logically forecloses or coexists with the strict reading.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the high measured suppression (0.72) attributable to structural barriers (lack of notice, classified proceedings, no pre-search opportunity to object) or to internalized acceptance (U.S. persons believe surveillance is justified national security, or accept the bargain as the cost of communications technology)?',
    'Survey data, post-disclosure behavior, and litigation posture studies would reveal whether suppression persists after disclosure (internalized) or collapses when alternatives are available (structural). The Snowden revelations provided a natural experiment: U.S. persons learned of incidental collection and query access — if suppression is internalized, litigation rates and privacy-protective technology adoption should remain low; if structural, they should spike. Data shows mixed results, suggesting suppression is partially structural (cannot practically exclude from territorial surveillance) and partially internalized (acceptance of national security rationale).',
    'If suppression is primarily structural, the constraint''s extractiveness is anchored to the technical architecture and territorial sovereignty; reform requires policy change to permit-exit or alternative collection methods. If suppression is partially internalized, the constraint''s persistence depends on continued acceptance of the national security rationale; cognitive or ideological shift (framing change, trust erosion) could weaken suppression significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether high suppression is structural (cannot exit) or internalized (accept the surveillance).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(fisa_tr_t0, observed).
narrative_ontology:measurement(fisa_tr_t3, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 3, 0.46).
narrative_ontology:measurement_basis(fisa_tr_t3, observed).
narrative_ontology:measurement(fisa_tr_t6, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 6, 0.49).
narrative_ontology:measurement_basis(fisa_tr_t6, observed).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 12, 0.54).
narrative_ontology:measurement_basis(fisa_tr_t12, observed).
narrative_ontology:measurement(fisa_tr_t18, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 18, 0.56).
narrative_ontology:measurement_basis(fisa_tr_t18, observed).
narrative_ontology:measurement(fisa_tr_t25, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(fisa_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(fisa_be_t0, observed).
narrative_ontology:measurement(fisa_be_t3, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 3, 0.36).
narrative_ontology:measurement_basis(fisa_be_t3, observed).
narrative_ontology:measurement(fisa_be_t6, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 6, 0.39).
narrative_ontology:measurement_basis(fisa_be_t6, observed).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement_basis(fisa_be_t12, observed).
narrative_ontology:measurement(fisa_be_t18, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 18, 0.44).
narrative_ontology:measurement_basis(fisa_be_t18, observed).
narrative_ontology:measurement(fisa_be_t25, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement_basis(fisa_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(fisa_su_t0, observed).
narrative_ontology:measurement(fisa_su_t3, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 3, 0.67).
narrative_ontology:measurement_basis(fisa_su_t3, observed).
narrative_ontology:measurement(fisa_su_t6, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 6, 0.69).
narrative_ontology:measurement_basis(fisa_su_t6, observed).
narrative_ontology:measurement(fisa_su_t12, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(fisa_su_t12, observed).
narrative_ontology:measurement(fisa_su_t18, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement_basis(fisa_su_t18, observed).
narrative_ontology:measurement(fisa_su_t25, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(fisa_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__incidental_collection_reading, 0.18).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% The FISA 702 statutory text is a contested kernel with three incompatible readings: foreign_target_strict_reading (incidental data inaccessible for domestic purpose; ε≈0.15), constitutional_floor_reading (fourth amendment warrant required; ε≈0.05), and this constraint incidental_collection_reading (incidental data accessible for foreign intelligence purpose queries; ε≈0.45). All three stories share the same statutory text and institutional actors but instantiate different constraints with different victim sets and extraction profiles. They are linked via network.affects_constraints and constrain each other: the persistence of this reading limits the operational scope of the strict reading (queries that this reading permits are forbidden under the strict reading), and both readings are theoretically vulnerable to constitutional override by the constitutional_floor_reading. The decomposition follows ε-invariance: each reading instantiates a distinct constraint with stable, reading-indexed ε values; no single 'FISA 702 constraint' exists with measurement-dependent ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
