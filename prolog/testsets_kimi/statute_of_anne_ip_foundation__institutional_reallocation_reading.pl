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
 *   human_readable: Statute of Anne (1710) â Institutional Reallocation of Print Monopoly
 *   domain: legal/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the institutional_reallocation_reading
 *   of the statute_of_anne_ip_foundation kernel. It treats the 1710 Statute
 *   of Anne not as a conceptual invention of copyright ex nihilo, nor as an
 *   inseparable entangled event, but as a reallocation of existing
 *   reproduction rights from the Stationers' Company to authors (and by
 *   assignment to publishers). The structural delta is that the occupied
 *   institutional space changed hands: the Stationers' monopoly was
 *   expropriated, authors became nominal rights-holders, and publishers
 *   became the effective beneficiaries through assignment.
 *
 * KEY AGENTS:
 *   - parliament: Agenda-setter (institutional/analytical) â enacts and enforces the statutory reallocation
 *   - stationers_company: Primary payer/victim (organized/constrained) â loses perpetual monopoly over print
 *   - authors: Nominal beneficiary (moderate/constrained) â receives legal rights but assigns them to publishers
 *   - publishers: Effective beneficiary (powerful/mobile) â captures reallocated rights via assignment
 *   - reading_public: Excluded voice (moderate/constrained) â invoked rhetorically but absent from institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.58).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.53).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.53).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne (1710) â Institutional Reallocation of Print Monopoly").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'f60a0aa9-9c95-4592-bdd5-b170908f856f').
narrative_ontology:cs_kernel_codification('f60a0aa9-9c95-4592-bdd5-b170908f856f', fixed_text).
narrative_ontology:cs_authority_grounding('f60a0aa9-9c95-4592-bdd5-b170908f856f', lineage).
narrative_ontology:cs_interpretation_layer_present('f60a0aa9-9c95-4592-bdd5-b170908f856f').
narrative_ontology:cs_reading_relation('f60a0aa9-9c95-4592-bdd5-b170908f856f', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f60a0aa9-9c95-4592-bdd5-b170908f856f', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('f60a0aa9-9c95-4592-bdd5-b170908f856f', foundational, statutory_reallocation_is_primary_origin).
narrative_ontology:cs_axiom_status(statutory_reallocation_is_primary_origin, holdable).
narrative_ontology:cs_axiom_grounding('f60a0aa9-9c95-4592-bdd5-b170908f856f', statutory_reallocation_is_primary_origin, conventional).
narrative_ontology:cs_reference_frame('f60a0aa9-9c95-4592-bdd5-b170908f856f', statutory_reallocation_of_print_rights).
narrative_ontology:cs_drift_state('f60a0aa9-9c95-4592-bdd5-b170908f856f', publisher_capture_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f60a0aa9-9c95-4592-bdd5-b170908f856f', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the 1710 statute to break the Stationers' monopoly and reallocate reproduction rights to authors, asserting statutory authority over the English print trade and justifying the act as an encouragement of learning.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, parliament, agenda_setter,
    institutional, civilizational, analytical, national).

% Held a perpetual monopoly over English printing through royal charter and guild regulation until the statute stripped them of exclusive control; resisted the reallocation through political lobbying and legal challenges but could not prevent the shift of institutional space to authors and publishers.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company, payer,
    organized, generational, constrained, national).

% Were granted statutory reproduction rights in the 1710 act but lacked capital and distribution networks to exploit them independently; typically assigned rights to publishers for lump-sum payments, remaining structurally dependent on the publishing trade.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Acquired reproduction rights from authors through standard-form assignment, replacing the Stationers' guild monopoly with a publisher-dominated statutory market; were the effective beneficiaries of the reallocated rights despite the statute's author-centered rhetoric.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers, beneficiary,
    powerful, generational, mobile, national).

% Stood to benefit from increased competition and eventual public-domain access as statutory terms expired, but were not represented in drafting or early enforcement; their interests were invoked in the preamble while the institutional design centered on publishers and authors.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__institutional_reallocation_reading, publishers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaced a private guild monopoly with a statutory limited-term rights regime, creating a transferable legal framework for governing reproduction of books and enabling non-guild participants to enter the print market under uniform rules.
% TRANSFER_FUNCTION: Moved the legal right to control book reproduction from the Stationers' Company (perpetual guild monopoly) to individual authors (limited statutory term), who then assigned those rights to publishers in exchange for production and distribution support.
% ABSENT_VOICES: The reading public and independent printers who stood to gain from competitive entry and lower prices were not at the drafting table; later generations of authors who found themselves with nominal rights but no bargaining power were also structurally absent from the original arrangement.
% DISAPPEARANCE_RATIONALE: If the statute vanished, the prior Stationers' monopoly might reassert by default or a legal vacuum would open in print regulation; the modern author-publisher-rights structure would lose its statutory foundation and the book trade would reorganize around whatever common-law or guild principles remained.
% FOUNDING_PROBLEM: The Stationers' Company exercised a perpetual monopoly over English printing, suppressing competition, controlling prices, and operating a private censorship regime; the statute was built to dismantle this monopoly and reallocate reproduction rights to authors for the encouragement of learning.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians Patterson and Rose corroborate the monopoly-breaking narrative from outside the publisher beneficiary set; however, the Stationers' own petitions and parliamentary drafting records show significant publisher-guild influence, leaving the provenance contested with no fully disinterested corroboration.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.58) reflects a genuine zero-sum reallocation: the Stationers' monopoly was taken and reassigned to a new class. Suppression (0.53) captures the statutory enforcement needed to override entrenched guild privilege and police the new rights boundary. Theater_ratio (0.30) registers the growing gap between the statute's author-centered rhetoric and the publisher-dominated market reality. Accessibility_collapse (0.45) models the narrowing of alternatives to statutory copyright once the common-law perpetual right was rejected. Resistance (0.55) registers Stationers' opposition and the legal friction of transition. The metrics and claim are authored independently: the constraint is claimed as tangled_rope because it combines a real coordination function (new statutory rights framework) with asymmetric extraction (Stationers as victims, publishers as beneficiaries), but the metrics are descriptive and not tuned to match that classification.
 *
 * PERSPECTIVAL GAP:
 *   From Parliament's seat, the statute is a public-spirited coordination mechanism breaking a harmful monopoly. From the Stationers' seat, it is state-enforced expropriation. From the publishers' seat, it is a beneficial reallocation that preserves their market position through author assignment. From the authors' seat, it is nominal empowerment with constrained practical benefit. The engine computes these divergences from the structural data â power, exit options, and beneficiary/victim roles â without requiring the authored claim to adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament (agenda_setter, analytical exit) sits near the neutral/coordination pole with low effective extraction. Publishers (beneficiary, mobile exit) sit at the beneficiary pole (low d, damped Ï). Stationers (payer, constrained exit) sit at the target pole (high d, amplified Ï). Authors (beneficiary in name but constrained exit and weak power) sit nearer symmetric than full beneficiary because their exit is limited and gains are largely captured by publishers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â the Stationers' monopoly â was substantially dead by mid-century, yet the statutory framework persisted and evolved. However, because it continued to coordinate (providing a legal framework for the book trade) and was not purely inertial or performative, it does not classify as piton. The R5 genealogy flags a live-to-contested transition, preventing mislabeling as pure coordination (rope) or pure extraction (snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_conceptual_priority,
    'Is the Statute of Anne structurally a reallocation of existing monopoly rights to a new occupant class, or a conceptual invention of limited copyright independent of prior guild property?',
    'Comparative legal-historical analysis of pre-1710 Stationers'' registers versus post-1710 statutory assignments; if rights were continuous and merely reassigned, the institutional reading holds; if a new conceptual object was created, the conceptual reading holds.',
    'If conceptual emergence is primary, the constraint''s extractiveness is lower (a new coordination device invented); if institutional reallocation is primary, extractiveness is higher (zero-sum transfer from Stationers to publishers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_conceptual_priority, conceptual, 'Priority of institutional reallocation versus conceptual emergence framing').

omega_variable(
    author_benefit_vs_publisher_capture,
    'Did authors structurally benefit from the statutory grant of rights, or were those rights immediately and systematically captured by publishers through standard-form assignment?',
    'Quantitative analysis of 18th-century publication contracts and author remuneration relative to publisher profits; prosopographic study of author economic standing pre- and post-1710.',
    'If publishers captured all gains, the beneficiary set narrows to publishers alone and the coordination function for authors was largely performative, raising theater_ratio. If authors benefited, the constraint has a broader coordination base.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_benefit_vs_publisher_capture, empirical, 'Whether authors were genuine beneficiaries or nominal conduits to publishers').

omega_variable(
    stationers_resistance_vs_acquiescence,
    'Did the Stationers'' Company resist the statutory reallocation through active legal and political opposition, or did they internalize the new regime and attempt capture from within?',
    'Archival evidence of Stationers'' litigation and lobbying expenditure 1710-1760; high resistance implies structural suppression, low resistance with internal pivot implies capture.',
    'High resistance raises the suppression metric and confirms the victim structure; internalized capture would reclassify the constraint toward coordination-with-capture rather than active extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationers_resistance_vs_acquiescence, empirical, 'Whether Stationers'' response was active resistance or internalized capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statute_anne_inst_tr_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(statute_anne_inst_tr_t10, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(statute_anne_inst_tr_t20, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(statute_anne_inst_tr_t30, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(statute_anne_inst_tr_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(statute_anne_inst_tr_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(statute_anne_inst_be_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(statute_anne_inst_be_t10, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(statute_anne_inst_be_t20, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(statute_anne_inst_be_t30, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(statute_anne_inst_be_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(statute_anne_inst_be_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(statute_anne_inst_su_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(statute_anne_inst_su_t10, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(statute_anne_inst_su_t20, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(statute_anne_inst_su_t30, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(statute_anne_inst_su_t40, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(statute_anne_inst_su_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 50, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, resource_allocation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the statute_of_anne_ip_foundation kernel, decomposed per the Îµ-invariance principle. The institutional reallocation reading isolates the shift in rights-holding from Stationers to authors/publishers as the structurally decisive fact, while the sibling readings treat the conceptual and entangled dimensions as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
