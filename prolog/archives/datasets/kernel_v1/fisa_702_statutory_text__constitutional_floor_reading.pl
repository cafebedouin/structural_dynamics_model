% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_constitutional_floor, []).

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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: FISA 702 Statutory Text as Constitutional Floor: Fourth Amendment Warrant Requirement for Content Queries
 *   domain: constitutional_law/national_security/surveillance
 *
 * SUMMARY:
 *   FISA Section 702 authorizes the government to conduct broad surveillance
 *   of foreign intelligence targets' communications without individualized
 *   warrants. The constitutional floor reading interprets this statute as
 *   subordinate to Fourth Amendment constraints: any search of U.S. person
 *   communications content requires probable cause and warrant, regardless of
 *   statutory text. This reading treats 702 database queries as Fourth
 *   Amendment searches triggering warrant requirement independent of the
 *   foreign/domestic distinction. The constraint measures the extractive
 *   force of the warrantless query regime: how much government power flows
 *   from treating queries as administrative functions rather than searches.
 *   The structural data shows this reading instantiates a Snare from the
 *   perspective of trapped U.S. persons and a high-suppression extraction
 *   mechanism from the beneficiary's absent perspective (no beneficiaries
 *   exist under this reading—the constraint is pure one-sided extraction).
 *   The measurement trajectory tracks rising suppression (state secrets
 *   privilege, FISA Court opacity, classified dockets) and rising theater
 *   ratio (FISA Court approval rates above 99%, rubber-stamp authorization
 *   without meaningful review). The constraint is a kernel reading: one
 *   interpretation of FISA's contested statutory text. The sibling readings
 *   (incidental_collection_reading, foreign_target_strict_reading) interpret
 *   the same kernel differently, arriving at different constraints with
 *   different ε values. This reading's structural delta is that 702 queries
 *   are Fourth Amendment searches; the other readings argue the searches
 *   occurred during collection, not querying, or that foreign intelligence
 *   targets fall outside Fourth Amendment protection.
 *
 * KEY AGENTS:
 *   - U.S. Persons in 702 Database: Primary victims (powerless/trapped) — searchable without warrant or knowledge; no exit mechanism
 *   - Government Intelligence Agencies (NSA, FBI, CIA): Asymmetric beneficiaries (institutional/constrained) — experience 702 as coordination enabling foreign intelligence collection, but under constitutional floor reading face extraction cost (warrant requirement) they have externalized to subjects
 *   - FISA Court: Nominal warrant authority (institutional/arbitrage) — designed to conduct independent review but operates with 99.97% approval rate; experiences piton degradation (coordination function atrophied)
 *   - Privacy Advocates / Fourth Amendment Litigants: Secondary victims (moderate/constrained) — constrained exit (litigation faces standing doctrine, state secrets, FISA Court secrecy)
 *   - Congressional Oversight: Distributed authority (organized/constrained) — balances foreign intelligence need against constitutional constraint but is suppressed by classification requirements and executive secrecy
 *   - Supreme Court: Analytical authority (analytical/analytical) — potential resolver of the kernel ambiguity; has not yet classified 702 queries as Fourth Amendment searches in binding precedent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.68).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.72).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, snare).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "FISA 702 Statutory Text as Constitutional Floor: Fourth Amendment Warrant Requirement for Content Queries").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '817aa5fa-f234-4813-923d-1771b7155221').
narrative_ontology:cs_kernel_codification('817aa5fa-f234-4813-923d-1771b7155221', formalized).
narrative_ontology:cs_authority_grounding('817aa5fa-f234-4813-923d-1771b7155221', lineage).
narrative_ontology:cs_interpretation_layer_present('817aa5fa-f234-4813-923d-1771b7155221').
narrative_ontology:cs_reading_relation('817aa5fa-f234-4813-923d-1771b7155221', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('817aa5fa-f234-4813-923d-1771b7155221', fisa_702_statutory_text__foreign_target_strict_reading, forecloses).
narrative_ontology:cs_axiom('817aa5fa-f234-4813-923d-1771b7155221', foundational, fourth_amendment_floor_inviolable).
narrative_ontology:cs_axiom_status(fourth_amendment_floor_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('817aa5fa-f234-4813-923d-1771b7155221', fourth_amendment_floor_inviolable, deontological).
narrative_ontology:cs_axiom('817aa5fa-f234-4813-923d-1771b7155221', foundational, query_as_independent_search).
narrative_ontology:cs_axiom_status(query_as_independent_search, holdable).
narrative_ontology:cs_axiom_grounding('817aa5fa-f234-4813-923d-1771b7155221', query_as_independent_search, empirically_contingent).
narrative_ontology:cs_reference_frame('817aa5fa-f234-4813-923d-1771b7155221', criminal_procedure_constitutional_lineage).
narrative_ontology:cs_drift_state('817aa5fa-f234-4813-923d-1771b7155221', contemporary_post_snowden_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('817aa5fa-f234-4813-923d-1771b7155221', '2026-02-26T14:32:15Z').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, u_s_persons_subject_to_702_queries).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_protection_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: U.S. PERSON IN 702 DATABASE (SNARE) — Trapped within the surveillance apparatus with no structural exit. Communications content searchable by government agents without individualized warrant or probable cause review. Full extraction with zero agency. The constraint operates invisibly — the person does not know they are in the database or subject to queries. No alternative exists except geographic exit (emigration), which is prohibitively costly.
constraint_indexing:constraint_classification(fisa_702_statutory_text__constitutional_floor_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIVACY ADVOCATE (SNARE) — Can challenge 702 in litigation (constrained exit: high legal/political cost) but faces suppression mechanisms: state secrets privilege, Youngstown standing doctrine, FISA Court opacity, classified briefings limiting public advocacy. Attempting exit through litigation triggers counter-extraction (government compels disclosure of litigation strategy through surveillance itself). Moderate power + constrained exit + high suppression = snare.
constraint_indexing:constraint_classification(fisa_702_statutory_text__constitutional_floor_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE INTELLIGENCE AGENCY (TANGLED ROPE) — Experiences 702 as coordination mechanism: it solves the genuine problem of foreign intelligence collection without requiring warrants for foreign targets. However, under constitutional floor reading, the mechanism includes asymmetric extraction: the coordination benefit (fast, efficient foreign targeting) is maintained, but the extraction cost (warrantless U.S. person querying) is imposed on subjects without consent or compensation. Active enforcement required to maintain the query authority. Exit is constrained: agencies cannot abandon 702 without congressional action, but they can lobby for statutory amendment or administrative interpretation widening the scope. The coordination (foreign targeting) is genuine; the extraction (U.S. person access) is embedded in the mechanism.
constraint_indexing:constraint_classification(fisa_702_statutory_text__constitutional_floor_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FISA COURT (ROPE) — Experiences constitutional floor reading as pure coordination: the court's statutory role is to balance foreign intelligence needs against constitutional constraints. Under constitutional floor reading, the FISA Court has clear coordinating authority: it must conduct individualized probable cause review before U.S. person content queries, establishing a transparent warrant alternative. The court has arbitrage exit (can reinterpret the statute; can appeal to higher courts). From the court's structural position, the constraint is a solving mechanism, not extractive. However, the FISA Court's actual institutional trajectory (high rubber-stamp rate, secret docket, limited transparency) suggests the rope classification may be aspirational—see Piton perspective below.
constraint_indexing:constraint_classification(fisa_702_statutory_text__constitutional_floor_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FISA COURT HISTORICAL PRACTICE (PITON) — The FISA Court's role as independent warrant authority is substantially degraded. Theater_ratio reflects the performative character of FISA authorization: the Court approves ~99.97% of government applications; judicial review is secret; the government's legal theories are not publicly tested; the Court has largely adopted executive agency framings of constitutional constraints. The Rope coordination function (independent review) persists through institutional form (judges, docket, written opinions) but functions minimally. This is piton: former Rope that atrophied through institutional capture, maintained through inertia and theatrical legitimacy.
constraint_indexing:constraint_classification(fisa_702_statutory_text__constitutional_floor_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the Fourth Amendment is a foundational fixed text. The constitutional floor reading treats the warrant requirement and probable cause standard as immutable features of the legal structure: no government interpretation or statutory text can override a constitutional guarantee. This perspective sees the constraint as a mountain—unchangeable by legislation or administrative action. However, this reading is a FALSE SUMMIT: the 'immutability' of the constitutional floor depends entirely on the interpretive lineage chosen (see axioms). Alternative readings (incidental_collection_reading, foreign_target_strict_reading) invoke the same constitutional text but arrive at different constraints. The mountain classification naturalizes one interpretive choice and obscures its contingency.
constraint_indexing:constraint_classification(fisa_702_statutory_text__constitutional_floor_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fisa_702_statutory_text__constitutional_floor_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fisa_702_statutory_text__constitutional_floor_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, TR),
    TR >= 0.70.

:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68): High. Under constitutional floor reading, government agents can query 702 database for any U.S. person communications without probable cause, warrant, or oversight. The extraction is the asymmetric power: government gains warrantless access; subjects lose Fourth Amendment protection. The measurement trajectory (0.42→0.58→0.68) tracks the accumulation of 702 querying as a normalized practice post-Snowden. Initial value (0.42) reflects pre-Snowden relative obscurity; post-Snowden (0.58) reflects public revelation of scale; contemporary (0.68) reflects administrative normalization despite litigation and reform proposals. If constitutional floor reading prevails, extractiveness drops sharply (warrant requirement imposes cost on government), but the current reading measures the constraint AS IT EXISTS under executive interpretation, which is ε≈0.68. Suppression (0.72): High. Multiple suppression mechanisms maintain the constraint: state secrets privilege prevents full disclosure of 702 programs; FISA Court docket is classified; agency legal opinions are not published; whistleblowers face criminal prosecution; Congress receives only classified briefings; standing doctrine prevents public litigation. The suppression trajectory (0.55→0.68→0.72) reflects post-Snowden hardening of classification protocols and expansion of legal theories justifying secrecy. Theater ratio (0.58): Moderate-high. FISA Court approval rate of 99.97% indicates the warrant process is performative: the court's rubber-stamp function differs dramatically from the adversarial warrant process in ordinary criminal procedure. However, theater is not as extreme as judicial review in national security contexts (some FISA opinion language addresses constitutional constraints). The measurement trajectory shows slight degradation (0.48→0.54→0.58) reflecting the court's increasing adoption of executive agency framing without apparent independent scrutiny.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps in this constraint reveal the interpretive kernel's structural ambiguity. The powerless U.S. person sees pure extraction (Snare)—they are trapped without knowledge or exit. The intelligence agency sees coordination (Tangled Rope)—702 solves their genuine foreign intelligence problem, but under constitutional floor reading, the solution includes asymmetric extraction (warrantless queries). The FISA Court experiences its nominal role as coordination (Rope), but the historical piton perspective shows the coordination function has atrophied. The analytical observer risks naturalizing one interpretive choice (constitutional floor = mountain) when alternative readings would classify the same statutory text as a different constraint entirely. The gap between Rope (FISA Court as warrant authority) and Piton (FISA Court as rubber-stamp) reveals how institutional capture transforms a coordination mechanism into a performative constraint maintained by inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   The constitutional floor reading declares no beneficiaries. Under this reading's interpretation, 702 is pure extraction—the government extracts power from U.S. person querying without providing coordination benefit or compensation to subjects. The victims are U.S. persons subject to queries and the Fourth Amendment protection regime itself (abstract collective). Directionality flows entirely toward the government (d approaching 1.0 from subject perspectives). The FISA Court's Rope classification derives from its nominal role (independent warrant authority) even though institutional piton degradation shows the mechanism functions minimally. The intelligence agency's Tangled Rope classification recognizes genuine coordination (foreign intelligence collection) embedded within asymmetric extraction (warrantless U.S. person access). No beneficiary/victim declarations appear in base_properties because the reading treats the constraint as zero-sum extraction, not mixed coordination. This absence is structurally meaningful—it signals that alternative readings would populate beneficiaries (foreign intelligence agencies, national security state) and potentially shift the constraint toward Rope or Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED through kernel reading structure. The apparent contradiction—is 702 a foreign intelligence coordination mechanism or a warrantless search violation?—dissolves when we recognize these are TWO DIFFERENT CONSTRAINTS. The constitutional floor reading answers the second question (ε≈0.68, Snare). The foreign_target_strict_reading answers the first question (ε≈0.15, Rope). Both readings interpret the same FISA Section 702 statutory text but route to different constraints because they adopt different foundational axioms about constitutional supremacy and the scope of foreign intelligence exceptions. The engine resolves mandatrophy by showing: (1) both readings are coherent; (2) they disagree on what constraint 702 instantiates; (3) the disagreement is at the kernel level (constitutional interpretation), not at the metrics level; (4) no single metric can adjudicate between readings because the readings define which metrics matter. This reading does not resolve mandatrophy by discovering the 'true' constraint—it resolves by mapping the disjunction of readings across the kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_text_constitutional_override,
    'Can FISA statutory text (Section 702) authorize government action that the Fourth Amendment floor prohibits? Is statutory text subordinate to constitutional floor or co-equal?',
    'Supreme Court precedent on constitutional supremacy vs statutory interpretation deference; analysis of Youngstown three-zone framework (explicit authorization vs twilight vs claimed authority against constitutional constraint)',
    'If statutory text subordinate: constitutional floor applies (this reading, ε≈0.68). If statutory text can redefine what counts as Fourth Amendment search: extraction drops sharply (ε≈0.25, shifts toward foreign_target_strict_reading). This is THE axiomatic gap between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_text_constitutional_override, conceptual, 'Whether statutory text can override constitutional floor constraints').

omega_variable(
    foreign_intelligence_exception_scope,
    'Does Fourth Amendment warrant requirement apply differently in foreign intelligence context? Is the foreign/domestic distinction constitutionally significant or merely policy-relevant?',
    'Historical analysis of United States v. Truong precedent and its relationship to post-FISA doctrine; examination of whether foreign intelligence agents are treated as ''federal officers'' subject to Fourth Amendment at all',
    'If foreign intelligence exception is constitutionally permissible: extraction increases (statutory foreign/domestic distinction valid), supporting incidental_collection_reading (ε≈0.15). If no foreign intelligence exception: constitutional floor applies universally, supporting this reading (ε≈0.68).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_intelligence_exception_scope, conceptual, 'Constitutional scope of foreign intelligence exception to warrant requirement').

omega_variable(
    query_as_search_definition,
    'Does searching a pre-collected communications database constitute a Fourth Amendment ''search'' triggering warrant requirements? Or is the search only the initial collection from the wire?',
    'Comparison of Karo/Carpenter/Kyllo definitions of Fourth Amendment search; analysis of whether database access differs structurally from wiretap content access; empirical analysis of how query execution relates to collection execution',
    'If query is separate search: constitutional floor reading applies, warrant required before query (ε≈0.68, this reading). If query is mere retrieval of lawfully collected data: no Fourth Amendment search triggered (ε≈0.08, shifts toward incidental_collection_reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(query_as_search_definition, conceptual, 'Whether 702 database queries constitute Fourth Amendment searches').

omega_variable(
    individualized_probable_cause_practicability,
    'Can FISA Court conduct meaningful individualized probable cause review of all 702 queries? Or does the volume/scale of 702 querying make per-query warrant review structurally impossible?',
    'Empirical data on query volume and FISA Court capacity; analysis of whether batch procedures or threshold-based pre-authorization could substitute for per-query review; comparison to analogous bulk data systems (NSA phone metadata)',
    'If practicable: constitutional floor reading is implementable, extraction remains high (ε≈0.68) but legitimated through procedure. If impracticable: reading becomes aspirational (extraction stays ε≈0.68 but justified differently) or reading is overruled by structural limits favoring foreign_target_strict_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individualized_probable_cause_practicability, empirical, 'Feasibility of per-query probable cause review under 702 scale').

omega_variable(
    reading_kernel_ambiguity,
    'Which interpretive lineage does FISA 702 statutory text instantiate: the foreign intelligence statutes lineage (United States v. Truong, FISA 1978 original intent, executive discretion) or the criminal procedure/constitutional lineage (Fourth Amendment doctrine, Karo/Carpenter, warrant requirement as bright line)?',
    'Historical legislative analysis of FISA 1978 vs 2008 amendments; comparison of statutory text cross-references (FISA references Fourth Amendment minimization but does not explicitly authorize waiver); analysis of whether 702 ''targets'' language presupposes constitutional permissibility or merely statutory authorization',
    'If foreign intelligence lineage: this reading is foreclosed by the kernel''s foundational design (ε→0.15, supporting foreign_target_strict_reading). If constitutional lineage: this reading is the baseline (ε≈0.68). If ambiguous kernel: all three sibling readings coexist as interpretive options (affects mandate of reading_relations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Which interpretive tradition (foreign intelligence vs criminal procedure) governs FISA 702 kernel').

omega_variable(
    false_summit_mountain_naturalization,
    'Does the ''constitutional floor'' classification as Mountain naturalize one interpretive choice as though it were inevitable law of nature, rather than one reading of a contested kernel?',
    'Comparison of this reading''s mountain classification to alternative readings'' classifications; examination of whether the mountain perspective relies on assuming the constitutional floor axiom is already settled (circular reasoning) vs deriving it from independent textual/doctrinal analysis',
    'If false summit confirmed: mountain classification is signature of interpretive lock-in, not description of structural immutability. The piton perspective becomes diagnostically central—it shows how the mountain framing masks institutional capture of the FISA Court.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_mountain_naturalization, conceptual, 'Whether constitutional floor reading naturalizes contested kernel interpretation as immutable law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa702cf_theater_initial, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(fisa702cf_theater_post_snowden, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 8, 0.54).
narrative_ontology:measurement(fisa702cf_theater_contemporary, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(fisa702cf_epsilon_initial, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fisa702cf_epsilon_post_snowden, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(fisa702cf_epsilon_contemporary, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 16, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fisa702cf_suppression_initial, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fisa702cf_suppression_post_snowden, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(fisa702cf_suppression_contemporary, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 16, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_amendment_reauthorization_2023).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_digital_surveillance_scope).

% DUAL FORMULATION NOTE:
% The FISA 702 kernel decomposes into three structurally distinct constraints via reading relations. This constraint (constitutional_floor) measures extraction from the Fourth Amendment floor perspective. The incidental_collection_reading measures extraction from the foreign intelligence authorization perspective. The foreign_target_strict_reading measures extraction from the foreign intelligence exception perspective. All three share the same statutory text (702); all three are kernel readings of the same kernel (fisa_702_statutory_text); but they define different constraints because they adopt different foundational axioms about constitutional scope. Network links enable contamination propagation analysis: if Supreme Court precedent resolves the kernel ambiguity in favor of one reading, the engine can automatically reclassify all three constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
