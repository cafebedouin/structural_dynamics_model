% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__institutional_reallocation_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne: Institutional Reallocation of Printing Rights
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the institutional_reallocation_reading
 *   of the statute_of_anne_ip_foundation kernel. It treats the Statute of
 *   Anne (1710) not as the conceptual invention of copyright, but as a
 *   reallocation of existing control over texts from the Stationers' Company
 *   guild to authorsâa reallocation that publishers immediately captured
 *   through standard assignment, leaving the 'author as proprietor' framework
 *   as largely nominal. The Stationers' Company, which had enjoyed a monopoly
 *   under royal patent, is the structural victim of the reallocation;
 *   publishers, who received assignments from authors, are the effective
 *   beneficiaries. The claim/metric independence is maintained: the reading
 *   claims the statute is a tangled rope (coordination through monopoly-break
 *   plus extraction through publisher capture) while the metrics describe the
 *   actual operation of the regime over its first century.
 *
 * KEY AGENTS:
 *   - stationers_company: Primary victim (institutional/trapped) â loses monopoly control over printing.
 *   - authors: Nominal beneficiary (moderate/constrained) â granted statutory rights but immediately assign them under economic pressure.
 *   - publishers: Effective beneficiary (powerful/mobile) â capture reallocated rights via author assignment and dominate the trade.
 *   - parliament: Agenda-setter (institutional/analytical) â enacts the reallocation to break the Stationers' monopoly.
 *   - reading_public: Excluded beneficiary (organized/constrained) â would gain from open access but was absent from legislative design.
 *   - legal_historians: Observer (analytical) â provides external corroboration and contested interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.65).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.55).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne: Institutional Reallocation of Printing Rights").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, '47559c8f-0d76-4681-b224-1616f00a0b66').
narrative_ontology:cs_kernel_codification('47559c8f-0d76-4681-b224-1616f00a0b66', fixed_text).
narrative_ontology:cs_authority_grounding('47559c8f-0d76-4681-b224-1616f00a0b66', lineage).
narrative_ontology:cs_interpretation_layer_present('47559c8f-0d76-4681-b224-1616f00a0b66').
narrative_ontology:cs_reading_relation('47559c8f-0d76-4681-b224-1616f00a0b66', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('47559c8f-0d76-4681-b224-1616f00a0b66', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('47559c8f-0d76-4681-b224-1616f00a0b66', foundational, authors_as_original_proprietors).
narrative_ontology:cs_axiom_status(authors_as_original_proprietors, holdable).
narrative_ontology:cs_axiom_grounding('47559c8f-0d76-4681-b224-1616f00a0b66', authors_as_original_proprietors, conventional).
narrative_ontology:cs_axiom('47559c8f-0d76-4681-b224-1616f00a0b66', foundational, stationers_monopoly_subject_to_reallocation).
narrative_ontology:cs_axiom_status(stationers_monopoly_subject_to_reallocation, holdable).
narrative_ontology:cs_axiom_grounding('47559c8f-0d76-4681-b224-1616f00a0b66', stationers_monopoly_subject_to_reallocation, empirically_contingent).
narrative_ontology:cs_reference_frame('47559c8f-0d76-4681-b224-1616f00a0b66', author_occupancy_of_ip_space).
narrative_ontology:cs_drift_state('47559c8f-0d76-4681-b224-1616f00a0b66', publisher_capture_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('47559c8f-0d76-4681-b224-1616f00a0b66', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, author_as_origin_of_ip).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__institutional_reallocation_reading, limited_term_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held a perpetual monopoly over English printing through royal patent and guild regulation. The statute abolished their exclusive control, requiring them to compete for titles and ending their ability to suppress competing editions through copy ownership.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company, payer,
    institutional, generational, trapped, national).

% Granted statutory ownership of their works for limited terms for the first time. In practice, economic necessity and publisher standard-form contracts led most to assign entire copyrights immediately for a one-time payment, leaving them with nominal rights but no ongoing control.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Captured the reallocated rights through widespread assignment from authors. Retained effective control of the book trade, transitioning from Stationers' monopoly licensees to contractual rights-holders under the new author-centric framework.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers, beneficiary,
    powerful, generational, mobile, national).

% Enacted the statute to break the Stationers' monopoly, promote learning, and prevent the perpetuation of exclusive control over knowledge. Framed the reform as author-centric but left the assignment mechanism unregulated.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament, agenda_setter,
    institutional, civilizational, analytical, national).

% Would have benefited from open competition and shorter terms, but was excluded from legislative design and had no voice in setting the terms of access to printed knowledge.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, excluded,
    organized, civilizational, constrained, national).

% Analyze whether the statute genuinely empowered authors or merely laundered publisher control through a new legal fiction; provide external corroboration for the founding problem from outside the benefiting parties.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reorganize the English book trade by dismantling the Stationers' perpetual monopoly and replacing it with a limited-term property system initiated by authors, ostensibly to promote learning and regulate the press.
% TRANSFER_FUNCTION: Moves the legal entitlement to print and copy from the Stationers' Company guild to individual authors, who then transfer effective control to publishers through standard assignment contracts.
% ABSENT_VOICES: Small booksellers and the broad reading public, who might have preferred shorter terms or open competition, were excluded from parliamentary debate; also authors who wished to retain rights but lacked market power to negotiate with publishers.
% DISAPPEARANCE_RATIONALE: If the statutory reallocation vanished, the Stationers' monopoly would either reassert or the market would fragment; the entire edifice of Anglo-American copyright (author as legal origin, publisher assignment, limited term) would collapse, requiring fundamental reorganization of publishing economics.
% FOUNDING_PROBLEM: The Stationers' Company held a perpetual monopoly over English printing that restricted the spread of learning, limited competition, and concentrated control over knowledge in a single guild.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary preambles and contemporary pamphlets (e.g., 'Some Thoughts on the Present State of the Printing and Bookselling Trade') attest the monopoly problem from outside the Stationers' interest; modern legal historians such as Patterson and Rose corroborate that the monopoly was the target, though they dispute whether the statute solved it or merely recreated it in new hands.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects that the reallocation created a new property form that was rapidly captured by commercial publishers through adhesive contracts, generating rents despite the anti-monopoly intent. Suppression (0.55) captures the active enforcement needed to sustain the new author-publisher rights against both residual Stationers' claims and unauthorized printers. Theater ratio (0.45) registers the growing gap between the statute's author-centric rhetoric and the publisher-dominated reality. Accessibility collapse (0.75) measures the disappearance of the Stationers' monopoly model as a viable alternative legal framework once the statute was enacted. Resistance (0.50) accounts for initial Stationers' lobbying and ongoing friction from competing printers.
 *
 * PERSPECTIVAL GAP:
 *   From Parliament's seat, the statute is a progressive reform breaking a harmful monopoly. From the Stationers' seat, it is an expropriation of their established business model. From the publisher seat, it is a new legal instrument for maintaining control, laundered through author assignment. From the author seat, it is nominal empowerment under structural constraint. The engine will compute different directionalities and effective extraction for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Stationers_company is the declared victim (high d, full target): the constraint extracts their monopoly and transfers it. Authors are nominal beneficiaries (low d) but with constrained exit, meaning their effective extraction is damped though not inverted. Publishers are the effective beneficiaries (low d, near full subsidy): they collect the reallocated rights and the resulting rents. Parliament is agenda_setter/analytical (d neutral). The directionality chain derives these from the beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâbreaking the Stationers' monopolyâwas substantially achieved within decades, yet the legal framework persisted and expanded. The R5 genealogy interview (founding_problem_status: dead, disappearance_verdict: world_rearranges) flags this as a zombie constraint: it no longer solves the problem it was built for, but persists because it has been captured by publishers who benefit from its continuation. The Tangled Rope classification captures this precisely: it prevents misreading the statute as pure coordination (Rope) by insisting on the victim seat (Stationers, later authors via capture) and active enforcement, while also preventing misreading it as pure Snare by acknowledging the genuine coordination function of ending the perpetual monopoly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_conceptual_priority,
    'Is the Statute of Anne better understood as an institutional reallocation of existing rights from Stationers to authors, or as the conceptual emergence of a new limited regulatory tool for learning?',
    'Comparative historiographical analysis weighing the statutory text''s property language against its regulatory framing; examination of whether ''rights'' language predates the statute in Stationers'' discourse.',
    'If the conceptual framing dominates, the beneficiary/victim structure shifts from a zero-sum reallocation to a public-interest regulatory innovation, lowering extractiveness and changing the directionality of victimhood.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_conceptual_priority, conceptual, 'Priority of institutional reallocation vs conceptual emergence in interpreting the Statute of Anne.').

omega_variable(
    author_publisher_assignment_inevitability,
    'Was the immediate assignment of author rights to publishers an inevitable structural consequence of the reallocation, or a contingent feature of the 18th-century book trade?',
    'Economic history of author-publisher contracts in the decades following 1710; comparison with jurisdictions where authorial retention was more common.',
    'If inevitable, publishers are the true structural beneficiaries and the statute''s author-centric framing is largely theater; if contingent, the reallocation genuinely empowered authors before later capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_publisher_assignment_inevitability, empirical, 'Whether publisher capture via assignment was structurally inevitable or historically contingent.').

omega_variable(
    stationers_pre_statute_rights_status,
    'Did the Stationers'' Company hold legally cognizable ''rights'' in copies before 1710, or merely a revocable guild monopoly and royal privilege?',
    'Legal-historical analysis of pre-1710 Stationers'' litigation, licensing regimes, and the legal status of copy ownership under the Stationers'' internal registers.',
    'If the Stationers held genuine property-like rights, the statute was a taking (reallocation) with Stationers as victims; if they held only revocable privileges, there was no victim in the reallocation and the statute created rights de novo.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stationers_pre_statute_rights_status, empirical, 'Whether the Stationers'' pre-1710 position constituted rights or revocable privileges.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soa_inst_tr_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soa_inst_tr_t20, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(soa_inst_tr_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(soa_inst_tr_t60, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(soa_inst_tr_t80, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(soa_inst_tr_t100, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(soa_inst_be_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(soa_inst_be_t20, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(soa_inst_be_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(soa_inst_be_t60, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(soa_inst_be_t80, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 80, 0.64).
narrative_ontology:measurement(soa_inst_be_t100, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(soa_inst_su_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(soa_inst_su_t20, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(soa_inst_su_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(soa_inst_su_t60, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(soa_inst_su_t80, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(soa_inst_su_t100, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, resource_allocation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the statute_of_anne_ip_foundation kernel, decomposed per the epsilon-invariance principle. The institutional_reallocation_reading isolates the shift in rights-holding parties; the conceptual_emergence_reading isolates the ideological framing; the entangled_event_reading denies their separability. Each carries a distinct epsilon and stakeholder structure, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
