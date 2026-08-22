% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: FISA 702 Incidental Collection / Backdoor Search Reading
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   This story instantiates the incidental_collection_reading of the FISA 702
 *   statutory text kernel: the standing operational practice by which
 *   communications of U.S. persons, acquired incidentally during
 *   foreign-target collection, are retained in a searchable repository and
 *   queried by domestic agencies including the FBI without a warrant, on the
 *   theory that the foreign intelligence purpose justifying the original
 *   collection also justifies subsequent domestic access. This reading treats
 *   the statute's silence on U.S. person query procedure as permissive, and
 *   treats the administrative minimization regime as an adequate
 *   constitutional substitute for judicial process. It is one of three
 *   readings of the same statutory kernel; the foreign_target_strict_reading
 *   holds that incidental U.S. person data must remain inaccessible for
 *   domestic purposes, and the constitutional_floor_reading holds that a
 *   probable-cause warrant is required for any government search of U.S.
 *   person communications content regardless of the statutory text. Those are
 *   separate constraints with their own ε values, not alternative
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - nsa_foreign_intelligence_directorate: sets the collection and minimization architecture (institutional/analytical) — beneficiary via mission continuity
 *   - fbi_domestic_investigations_division: queries the repository without a warrant (institutional/arbitrage) — direct beneficiary of investigative access
 *   - us_persons_incidentally_collected: bear the extraction with no notice and no exit (powerless/trapped)
 *   - fisa_court: reviews certifications and reports compliance violations but cannot pre-review individual queries (institutional/analytical)
 *   - congress_intelligence_committees: holds reauthorization leverage but has not enacted a warrant requirement across repeated cycles (organized/constrained)
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
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA 702 Incidental Collection / Backdoor Search Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, 'e279340b-4190-4bcb-ad7a-78237c040f38').
narrative_ontology:cs_kernel_codification('e279340b-4190-4bcb-ad7a-78237c040f38', fixed_text).
narrative_ontology:cs_authority_grounding('e279340b-4190-4bcb-ad7a-78237c040f38', extraction).
narrative_ontology:cs_interpretation_layer_present('e279340b-4190-4bcb-ad7a-78237c040f38').
narrative_ontology:cs_reading_relation('e279340b-4190-4bcb-ad7a-78237c040f38', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('e279340b-4190-4bcb-ad7a-78237c040f38', fisa_702_statutory_text__constitutional_floor_reading, influences).
narrative_ontology:cs_axiom('e279340b-4190-4bcb-ad7a-78237c040f38', foundational, collection_authorization_extends_to_query_access).
narrative_ontology:cs_axiom_status(collection_authorization_extends_to_query_access, holdable).
narrative_ontology:cs_axiom_grounding('e279340b-4190-4bcb-ad7a-78237c040f38', collection_authorization_extends_to_query_access, conventional).
narrative_ontology:cs_axiom('e279340b-4190-4bcb-ad7a-78237c040f38', secondary, administrative_minimization_satisfies_privacy_interest).
narrative_ontology:cs_axiom_status(administrative_minimization_satisfies_privacy_interest, holdable).
narrative_ontology:cs_axiom_grounding('e279340b-4190-4bcb-ad7a-78237c040f38', administrative_minimization_satisfies_privacy_interest, instrumental).
narrative_ontology:cs_reference_frame('e279340b-4190-4bcb-ad7a-78237c040f38', foreign_intelligence_collection_efficiency_framework).
narrative_ontology:cs_drift_state('e279340b-4190-4bcb-ad7a-78237c040f38', post_2018_reauthorization_compliance_disclosures, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e279340b-4190-4bcb-ad7a-78237c040f38', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations_division).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, nsa_foreign_intelligence_directorate).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, intelligence_community_leadership).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, domestic_criminal_defendants_queried).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, journalists_and_attorneys_communicating_abroad).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_purpose_doctrine).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, administrative_minimization_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Targets non-U.S. persons abroad for foreign intelligence collection under Section 702 certifications approved by the FISA Court, and in the ordinary operation of that targeting incidentally acquires and retains communications involving U.S. persons. Sets and administers the minimization procedures that govern retention and access, and defends the practice as an unavoidable byproduct of legitimate foreign surveillance rather than domestic surveillance of citizens.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, nsa_foreign_intelligence_directorate, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, nsa_foreign_intelligence_directorate, beneficiary).

% Queries the already-collected 702 database using U.S. person identifiers during domestic criminal and counterintelligence investigations, without obtaining a warrant, on the theory that the data was lawfully collected under the foreign-targeting authority and a subsequent query is not a new 'search.' Gains investigative leads and evidentiary access it could not obtain through ordinary criminal process without probable cause.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations_division, beneficiary,
    institutional, biographical, arbitrage, national).

% Communicate with people abroad, or are discussed by people abroad, in the ordinary course of personal, professional, or political life, with no way to know whether or when their communications are swept into a foreign-target's collection stream. Cannot consent to or refuse the collection, cannot learn of the query that touched their communications, and have no judicial process to invoke before their data is searched by a domestic agency.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_persons_incidentally_collected, payer,
    powerless, biographical, trapped, national).

% Become subjects of criminal investigation or prosecution after their communications are located via a warrantless query of the 702 repository, often without contemporaneous notice that this occurred, complicating any pretrial motion to suppress or otherwise contest the search's legality.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, domestic_criminal_defendants_queried, payer,
    powerless, immediate, trapped, national).

% Maintain professional communications with foreign sources, clients, or contacts as a structural requirement of their work, which places privileged and confidential communications at elevated and disproportionate risk of incidental collection and subsequent domestic query, chilling the practice of the profession itself.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, journalists_and_attorneys_communicating_abroad, payer,
    moderate, biographical, constrained, global).

% Reviews and approves annual 702 certifications and minimization procedures in a largely ex parte, non-adversarial proceeding, and periodically finds and reports compliance violations in query practices, but has no mechanism to review individual queries before they occur and limited power to compel structural reform beyond procedural revision.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fisa_court, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, fisa_court, agenda_setter).

% Holds reauthorization power over Section 702 and receives classified compliance reports, but operates with information asymmetry relative to the agencies it oversees and has repeatedly failed, across multiple reauthorization cycles, to enact a warrant requirement for U.S. person queries despite bipartisan proposals to do so.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congress_intelligence_committees, excluded,
    organized, generational, constrained, national).

% Litigate and lobby against warrantless backdoor searches, but are structurally excluded from the classified certification and query-approval process itself; their objections surface in public reauthorization debate and amicus briefs rather than in any operational chokepoint.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_advocacy_groups, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigations_division).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables continuous, unbroken foreign intelligence collection against non-U.S. persons abroad without requiring the collecting agency to filter out every communication that happens to involve a U.S. person in real time, and allows the resulting lawfully-acquired repository to be queried by other agencies for their own missions rather than duplicating collection.
% TRANSFER_FUNCTION: Moves the practical burden and constitutional protection of a probable-cause warrant away from U.S. persons whose communications are incidentally collected, and moves investigative and evidentiary access to FBI and other domestic agencies who query that repository without the process ordinarily required to reach U.S. person communications content.
% ABSENT_VOICES: The individual U.S. persons whose communications are queried have no notice, no opportunity to object, and typically no way to ever learn a query occurred, unless they become criminal defendants and counsel identifies the 702 provenance during discovery — which itself depends on prosecutorial disclosure practices that have been criticized as inconsistent. Congress and civil liberties groups object publicly but are excluded from the operational approval chain for individual queries.
% DISAPPEARANCE_RATIONALE: If warrantless backdoor queries disappeared and a probable-cause warrant were required before an agency could search 702-collected data for U.S. person identifiers, FBI domestic investigations would lose a distinct evidentiary pathway, minimization procedures would need to be rebuilt around query-time judicial review rather than collection-time certification, and the practical boundary between foreign intelligence collection and domestic criminal investigation — which the statute currently allows to blur — would have to be reconstructed with a warrant gate in between.
% FOUNDING_PROBLEM: Section 702 was built to let intelligence agencies collect communications of foreign targets located abroad efficiently, without the individualized FISA warrant process designed for a pre-internet, circuit-based communications environment, on the premise that non-U.S. persons abroad have no Fourth Amendment claim and that incidental collection of U.S. person communications was an unavoidable side effect requiring only after-the-fact minimization.
% FOUNDING_PROBLEM_CORROBORATION: The Office of the Director of National Intelligence and successive FISA Court opinions attest the foreign intelligence collection function remains live and operationally essential. Independent corroboration from outside the benefiting agencies exists in the form of the Privacy and Civil Liberties Oversight Board's 2023-era findings and repeated FISA Court compliance opinions documenting tens of thousands of improper U.S. person queries, which read the founding collection-efficiency problem as solved but the query-access practice as having drifted into a distinct, unauthorized domestic-investigation use never contemplated by the original certification framework.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.45 for this reading — substantial but not maximal — because the underlying collection retains a genuine foreign intelligence coordination function (targeting non-U.S. persons abroad is not itself extractive under this reading), while the query-and-retain practice layered on top of it is where the extraction from U.S. persons is concentrated. Suppression is authored higher (0.68) than extractiveness because U.S. persons subject to a query have no notice mechanism, no adversarial process, and in most cases no eventual knowledge that a query occurred at all — the suppression is near-total even where the extraction from any single query may be modest. Theater ratio rises across the interval (0.18 to 0.42) tracking the growing gap between the minimization-procedure compliance apparatus (audits, congressional reporting, FISA Court opinions) and documented continued improper query volume — the paperwork of compliance has scaled faster than the practice's conformity to it. Accessibility collapse is high (0.72): once a person's communications enter the repository, there is no remaining practical alternative or notice mechanism by which they could contest or avoid the subsequent query.
 *
 * PERSPECTIVAL GAP:
 *   From the NSA/FBI agenda-setter and beneficiary seats, the practice reads as a lawful, minimized, and oversight-reviewed extension of legitimate foreign intelligence collection. From the U.S. persons who are queried without notice, the same structure reads as government search of their communications with no warrant and no meaningful process — the engine computes this divergence from the trapped exit option and powerless power atom on the payer side versus the institutional/arbitrage seat on the beneficiary side; the story does not assert which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   FBI domestic investigations division and NSA's foreign intelligence directorate sit at the beneficiary end: they collect, retain, and query without bearing the query's cost. US persons incidentally collected sit at the target end and are directionality-locked as trapped: they cannot know when their communications entered the repository, cannot exit the risk of being queried by continuing ordinary international communication, and have essentially no individualized recourse. Journalists and attorneys are moderate power but constrained exit — their profession structurally requires the foreign communications that expose them to elevated incidental collection risk, so 'exit' would mean abandoning international practice, not a real option.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — efficient foreign intelligence collection without individualized warrants for non-U.S. persons abroad — remains largely live per ODNI and the FISA Court's own framing. But the query practice that grew on top of that collection, allowing domestic agencies warrantless access to the resulting U.S. person data, addresses a different and newer problem (investigative convenience for domestic agencies) that was never the statute's stated purpose. The founding_problem_status is authored as contested rather than dead precisely because the coordination function (foreign collection) is real and current, while the extraction component (domestic warrantless query) has drifted from that founding purpose without ever being separately authorized — this is the tangled_rope signature: genuine coordination and asymmetric extraction riding the same statutory structure, distinguishable only by decomposing collection from query, which this reading declines to do because the statute itself does not distinguish them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    query_vs_search_characterization,
    'Is a backdoor query of already-collected 702 data a new ''search'' under the Fourth Amendment, or merely an act of accessing data already lawfully in government possession?',
    'Appellate or Supreme Court resolution of the query-as-search question, currently split across circuits and unresolved by FISA Court of Review precedent; alternatively, a statutory amendment explicitly defining query as a search event.',
    'If query is a search, the incidental_collection_reading''s core premise (that no new process is triggered at query time) collapses and this reading converges toward the constitutional_floor_reading''s warrant requirement. If query is not a search, this reading''s structural basis is reinforced and the practice remains outside Fourth Amendment scrutiny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(query_vs_search_characterization, conceptual, 'Whether querying incidentally-collected data constitutes a Fourth Amendment search.').

omega_variable(
    administrative_minimization_adequacy,
    'Do the intelligence community''s administrative minimization procedures function as an adequate substitute for judicial warrant process, or are they self-certified and effectively unreviewable in individual cases?',
    'Comparative empirical study of minimization procedure compliance rates against documented improper query volumes reported by PCLOB and the FISA Court; independent audit access beyond classified self-reporting.',
    'If minimization is empirically adequate, the tangled_rope''s coordination component is stronger than authored here. If minimization is largely self-certifying with weak external check, the extraction component dominates and the reading trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_minimization_adequacy, empirical, 'Whether minimization procedures provide real oversight or largely formal compliance.').

omega_variable(
    statutory_silence_as_permission,
    'Does the statute''s silence on domestic query procedure constitute affirmative permission for warrantless query, or an unaddressed gap that courts should fill with a warrant requirement by default?',
    'This is the precise interpretive fork separating this reading from the foreign_target_strict_reading — resolvable only by congressional clarification via reauthorization amendment or definitive appellate ruling on statutory construction.',
    'Determines whether this reading or the foreign_target_strict_reading is the operative one going forward; the two readings cannot both govern actual practice simultaneously even though both remain live in current legal and political discourse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(statutory_silence_as_permission, conceptual, 'Whether statutory silence on query procedure should be read as permission or as a gap requiring a warrant default.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(fisa_tr_t2012, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(fisa_tr_t2016, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2016, 0.28).
narrative_ontology:measurement(fisa_tr_t2018, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2018, 0.32).
narrative_ontology:measurement(fisa_tr_t2021, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2008, 0.22).
narrative_ontology:measurement(fisa_be_t2012, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2012, 0.28).
narrative_ontology:measurement(fisa_be_t2016, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2016, 0.34).
narrative_ontology:measurement(fisa_be_t2018, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement(fisa_be_t2021, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2021, 0.42).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement(fisa_su_t2012, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2012, 0.55).
narrative_ontology:measurement(fisa_su_t2016, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(fisa_su_t2018, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2018, 0.63).
narrative_ontology:measurement(fisa_su_t2021, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2021, 0.66).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__incidental_collection_reading, 0.1).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the single natural-language label 'FISA Section 702' into structurally distinct claims sharing one statutory kernel (fisa_702_statutory_text): this story (incidental_collection_reading, tangled_rope, ε≈0.45) treats warrantless domestic query of incidentally collected data as statutorily permitted; foreign_target_strict_reading treats the same text as requiring U.S. person data to remain inaccessible absent separate authorization (lower ε, narrower victim set); constitutional_floor_reading treats the Fourth Amendment as displacing the statutory question entirely, requiring a warrant regardless of how the statute is construed (distinct victim/beneficiary structure grounded in constitutional rather than statutory analysis). Each carries its own ε, its own stakeholder set, and its own classification; they are linked via affects_constraints because the readings compete for the same interpretive space and downstream legislative or judicial resolution of one materially changes the operative status of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
