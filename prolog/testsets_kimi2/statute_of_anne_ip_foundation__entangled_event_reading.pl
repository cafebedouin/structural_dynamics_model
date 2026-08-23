% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__entangled_event_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: statute_of_anne_ip_foundation__entangled_event_reading
 *   human_readable: Statute of Anne â Entangled Event Reading
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the entangled_event_reading of the
 *   statute_of_anne_ip_foundation kernel. It treats the Statute of Anne
 *   (1710) not as a pure conceptual innovation or a simple institutional
 *   reallocation, but as a single synthetic event in which the 'becoming
 *   thinkable' of copyright and the 'first holding' of copyright rights
 *   occurred simultaneously and inseparably. Under this reading, the
 *   statute's ambiguity is structural: authors are nominal beneficiaries
 *   while publishers are practical beneficiaries, and the cost of this
 *   entanglement falls on conceptual clarity and the reading public. The
 *   reading is one of three sibling readings; the others treat the statute as
 *   conceptual emergence or institutional reallocation respectively. The
 *   engine will compute per-seat classifications from the structural data;
 *   the claim of tangled_rope reflects the authored view that the statute
 *   coordinated the book trade while asymmetrically extracting value through
 *   publisher capture.
 *
 * KEY AGENTS:
 *   - stationers_company: Primary practical beneficiary (organized/constrained) â captures the statutory privilege through assignment contracts and controls the book trade.
 *   - authors: Nominal beneficiary / practical payer (moderate/constrained) â receive statutory rights but must assign them to publishers, bearing the structural cost of the nominal-practical split.
 *   - general_public: Primary payer (organized/constrained) â pays higher prices and bears restricted access despite the statute's encouragement-of-learning rhetoric.
 *   - parliament: Agenda-setter (institutional/constrained) â enacts the statute but cannot disentangle its conceptual and institutional dimensions.
 *   - legal_historians: Analytical observer (analytical/analytical) â traces the entanglement as a historiographical problem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.72).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.64).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne â Entangled Event Reading").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, '553738ed-ebd1-4e5b-a0af-ea7ac723e88b').
narrative_ontology:cs_kernel_codification('553738ed-ebd1-4e5b-a0af-ea7ac723e88b', formalized).
narrative_ontology:cs_authority_grounding('553738ed-ebd1-4e5b-a0af-ea7ac723e88b', lineage).
narrative_ontology:cs_interpretation_layer_present('553738ed-ebd1-4e5b-a0af-ea7ac723e88b').
narrative_ontology:cs_reading_relation('553738ed-ebd1-4e5b-a0af-ea7ac723e88b', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('553738ed-ebd1-4e5b-a0af-ea7ac723e88b', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_axiom('553738ed-ebd1-4e5b-a0af-ea7ac723e88b', foundational, conceptual_institutional_inseparability).
narrative_ontology:cs_axiom_status(conceptual_institutional_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('553738ed-ebd1-4e5b-a0af-ea7ac723e88b', conceptual_institutional_inseparability, empirically_contingent).
narrative_ontology:cs_axiom('553738ed-ebd1-4e5b-a0af-ea7ac723e88b', foundational, synthetic_ip_genesis).
narrative_ontology:cs_axiom_status(synthetic_ip_genesis, holdable).
narrative_ontology:cs_axiom_grounding('553738ed-ebd1-4e5b-a0af-ea7ac723e88b', synthetic_ip_genesis, empirically_contingent).
narrative_ontology:cs_reference_frame('553738ed-ebd1-4e5b-a0af-ea7ac723e88b', unified_statutory_event).
narrative_ontology:cs_drift_state('553738ed-ebd1-4e5b-a0af-ea7ac723e88b', post_enactment_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('553738ed-ebd1-4e5b-a0af-ea7ac723e88b', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, general_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, authors).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__entangled_event_reading, limited_term_copyright_doctrine).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__entangled_event_reading, encouragement_of_learning_rhetoric).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and enacted the 1710 statute to resolve the book-trade crisis following the lapse of press licensing; it created a new statutory privilege nominally vested in authors, but could not disentangle the conceptual justification for that privilege from the institutional settlement that reallocated commercial control.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, parliament, agenda_setter,
    institutional, generational, constrained, national).

% Received the first statutory copyright in their writings, but lacked the physical means of production and distribution; to see their work in print they were required to assign the statutory privilege to publishers, making the nominal benefit structurally flow immediately to the Stationers.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, authors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, authors, payer).

% Controlled the presses, bookselling networks, and assignment contracts; accepted the statute because it supplied legal enforceability against competitors, and through standard-form contracts captured the authorial privilege while presenting the arrangement as a new rights-based regime.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company, beneficiary,
    organized, generational, constrained, national).

% Constituted the rhetorical beneficiary of the statute's encouragement-of-learning purpose, but in practice paid higher prices and faced restricted access as publisher-captured statutory monopolies narrowed the channels through which knowledge circulated.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, general_public, payer,
    organized, generational, constrained, national).

% Analyze the statute as a synthetic event in which the conceptual possibility of limited-term copyright and the institutional reallocation of printing privileges occurred simultaneously and resist analytical separation.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, legal_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated the post-licensing book trade by replacing the collapsed Stationers' Company private monopoly with a statutory privilege system that created a regulated channel between authors, publishers, and the reading public.
% TRANSFER_FUNCTION: Moves the legal privilege of reproduction from an informal guild monopoly to a statutory author right, which is immediately transferred via contract to publishers; simultaneously transfers the cost of statutory ambiguity to the reading public and to later jurists who inherit a conceptually entangled foundation.
% ABSENT_VOICES: Scottish booksellers and provincial printers structurally excluded from the London trade; later jurists and legal theorists who would have preferred a clean conceptual foundation for literary property but were given an institutionally muddled one.
% DISAPPEARANCE_RATIONALE: Without the Statute of Anne, the book trade would have continued under the Stationers' Company private regime or collapsed into unregulated competition; the modern author-publisher-public triad and the concept of limited-term copyright would not have emerged in this form.
% FOUNDING_PROBLEM: The lapse of the Licensing Act in 1694 ended press censorship and the Stationers' de facto perpetual monopoly, creating chaos in the book trade and threatening the economic model of London publishing.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by parliamentary debate records and later historical scholarship (e.g., Patterson 1968, Deazley 2004) from outside the immediate beneficiary set; the Stationers' own petitions to Parliament attest the problem, but their testimony is self-interested.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__entangled_event_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72 at interval end) is high because the statute decoupled the nominal beneficiary (authors) from the practical capture of value (publishers), creating a persistent extraction channel. Suppression (0.64) reflects the active legal enforcement against competing printers and Scottish booksellers, as well as the statutory barriers to alternative publishing arrangements. Theater_ratio (0.28) captures the growing gap between the statute's 'encouragement of learning' rhetoric and its actual operation as a publisher-enforcement tool. Accessibility_collapse (0.45) is moderate: alternatives (unregulated printing, perpetual Stationer monopoly, or no copyright at all) were partially imaginable but legally collapsed once the statutory framework was established. Resistance (0.40) reflects the Scottish bookseller challenges and competing stationer opposition that culminated in litigation through the interval. The measurement series runs on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The Stationers' Company seat experiences the statute as a coordination mechanism that secures their trade against piracy; the author and public seats experience it as an extractive arrangement where nominal rights are captured by intermediaries. Parliament experiences it as a necessary legislative compromise. The engine computes this divergence from the structural data without requiring authorial reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Stationers_company is the structural beneficiary (low d, subsidy through legal privilege). Authors are structurally ambiguous: declared as beneficiaries (nominal rights) but also payers (forced assignment, weak bargaining), yielding a mid-range d that the engine will amplify toward target due to their constrained exit. General_public is a declared payer with constrained exit (high d). Parliament sits near symmetric (sets the constraint, bears political cost). Legal_historians are analytical (no d contribution).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â the collapse of press licensing and Stationer monopoly â was substantially dead by the mid-18th century, yet the arrangement persisted and deepened. This prevents misclassification as a rope (pure coordination) because the coordination function became subordinate to publisher extraction. It prevents misclassification as a snare because the statute did solve a genuine coordination problem (post-licensing trade chaos), even if the solution was captured. The tangled_rope classification captures the simultaneous presence of genuine coordination and asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    entanglement_intentionality,
    'Was the conceptual-institutional entanglement a deliberate legislative strategy by the Stationers'' Company to preserve monopoly rents under the guise of authorial rights, or an unintended byproduct of the political compromise necessary to pass the statute?',
    'Archival analysis of Stationers'' Company lobbying records and parliamentary drafting history; comparison with other post-guild regulatory statutes of the period.',
    'If deliberate, the extraction metric should be revised upward toward snare territory; if unintended, the entanglement reads as institutional drift and the classification remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entanglement_intentionality, empirical, 'Whether entanglement was strategic or accidental').

omega_variable(
    authorial_benefit_verification,
    'Did the Statute of Anne produce material improvements in authorial bargaining power or income, or were authors immediate and systematic victims of publisher capture?',
    'Economic history of author-publisher contracts 1710-1774; probate records and correspondence where available.',
    'If authors genuinely benefited, the victim set should shrink and directionality for authors should shift toward beneficiary; if capture was immediate, the current payer classification for authors is sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authorial_benefit_verification, empirical, 'Whether authors were genuine beneficiaries or immediate captives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(stat_tr_t9, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 9, 0.17).
narrative_ontology:measurement(stat_tr_t18, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 18, 0.19).
narrative_ontology:measurement(stat_tr_t27, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 27, 0.21).
narrative_ontology:measurement(stat_tr_t36, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 36, 0.23).
narrative_ontology:measurement(stat_tr_t45, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 45, 0.25).
narrative_ontology:measurement(stat_tr_t54, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 54, 0.27).
narrative_ontology:measurement(stat_tr_t64, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 64, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(stat_be_t9, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 9, 0.56).
narrative_ontology:measurement(stat_be_t18, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 18, 0.59).
narrative_ontology:measurement(stat_be_t27, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 27, 0.62).
narrative_ontology:measurement(stat_be_t36, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 36, 0.65).
narrative_ontology:measurement(stat_be_t45, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 45, 0.68).
narrative_ontology:measurement(stat_be_t54, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 54, 0.7).
narrative_ontology:measurement(stat_be_t64, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 64, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(stat_su_t9, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 9, 0.43).
narrative_ontology:measurement(stat_su_t18, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 18, 0.47).
narrative_ontology:measurement(stat_su_t27, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 27, 0.5).
narrative_ontology:measurement(stat_su_t36, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 36, 0.54).
narrative_ontology:measurement(stat_su_t45, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 45, 0.57).
narrative_ontology:measurement(stat_su_t54, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 54, 0.61).
narrative_ontology:measurement(stat_su_t64, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 64, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This story is one of three readings of the statute_of_anne_ip_foundation kernel, decomposed per the Îµ-invariance principle because the natural-language label 'the Statute of Anne as foundation of IP' conflates structurally distinct claims: conceptual emergence, institutional reallocation, and entangled synthetic event. Each reading carries its own Îµ, stakeholders, and classification. The kernel is not one constraint viewed from multiple angles but three constraints linked by shared historiographical material.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
