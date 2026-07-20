% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: statute_of_anne_ip_foundation__entangled_event_reading
 *   human_readable: Statute of Anne (Entangled Event Reading): IP Concept and Institution as Inseparable Origin
 *   domain: legal/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne 1710 is conventionally identified as the origin point
 *   of modern copyright. The entangled_event_reading treats this origin not
 *   as a conceptual innovation separable from an institutional power shift,
 *   nor as a mere reallocation of existing guild privileges, but as a single
 *   event in which conceptual and institutional dimensions mutated together.
 *   Under this reading, the statute is a coordination mechanism for the
 *   post-monopoly book trade that simultaneously extracts conceptual clarity
 *   from later jurists by fusing the idea of authorial property with the
 *   practice of statutory registration and limited-term monopoly. The
 *   constraint persists through centuries of reinterpretation, with
 *   publishers capturing the practical benefits of a framework nominally
 *   centered on authors, and legal scholars bearing the epistemic cost of an
 *   origin that cannot be cleanly parsed.
 *
 * KEY AGENTS:
 *   - early_modern_authors: Nominal beneficiary / practical payer (moderate/constrained) â granted rights they could not enforce and routinely assigned to publishers.
 *   - publishers_and_booksellers: Practical beneficiary (organized/mobile) â captured statutory value through assignment and controlled enforcement networks.
 *   - jurists_and_scholars: Payer (moderate/constrained) â bear the epistemic cost of conceptual entanglement in statutory interpretation.
 *   - parliament: Agenda setter (institutional/mobile) â enacted the framework and retained authority to amend it.
 *   - legal_historians: Analytical observer (analytical/analytical) â evaluate the entanglement as a contested historiographical site.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.72).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.58).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne (Entangled Event Reading): IP Concept and Institution as Inseparable Origin").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, '1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22').
narrative_ontology:cs_kernel_codification('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22', fixed_text).
narrative_ontology:cs_authority_grounding('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22', lineage).
narrative_ontology:cs_interpretation_layer_present('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22').
narrative_ontology:cs_reading_relation('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_axiom('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22', foundational, conceptual_institutional_entanglement).
narrative_ontology:cs_axiom_status(conceptual_institutional_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22', conceptual_institutional_entanglement, conventional).
narrative_ontology:cs_axiom('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22', secondary, no_pure_conceptual_or_institutional_origin).
narrative_ontology:cs_axiom_status(no_pure_conceptual_or_institutional_origin, holdable).
narrative_ontology:cs_axiom_grounding('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22', no_pure_conceptual_or_institutional_origin, conventional).
narrative_ontology:cs_reference_frame('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22', anne_statutory_origin).
narrative_ontology:cs_drift_state('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22', post_modern_ip_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1ea3a4f4-c21e-4408-a3d6-dfeed0f78c22', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, early_modern_authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, publishers_and_booksellers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, jurists_and_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, early_modern_authors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nominally granted exclusive rights in their works under the 1710 statute, but lacked the capital and legal infrastructure to enforce those rights in practice. Routinely assigned rights to publishers in exchange for publication, making the statutory grant a formal entry point into a system that immediately transferred value upward. Registration requirements and London-centric enforcement further constrained their effective exit.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, early_modern_authors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, early_modern_authors, payer).

% Practical beneficiaries of the statutory framework. Captured the value of exclusive rights through assignment contracts, controlled registration and enforcement networks, and maintained oligopolistic control over the book trade. The statute replaced the old Stationers' licensing monopoly with a new statutory monopoly that flowed to them through commercial practice.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, publishers_and_booksellers, beneficiary,
    organized, generational, mobile, national).

% Bear the epistemic cost of the statute's entangled design. Cannot cleanly separate the conceptual justification for copyright (authorial property, limited term, encouragement of learning) from its institutional history (publisher capture, trade reorganization, statutory registration). Every attempt to derive doctrinal clarity founders on the inseparability of these dimensions.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, jurists_and_scholars, payer,
    moderate, generational, constrained, national).

% Enacted the statute to stabilize the book trade after the collapse of the Licensing Act and to appease Whig anti-monopoly sentiment. Retained formal authority to amend or repeal the framework, but the entangled structure persisted and expanded through subsequent reinterpretation and term extension.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, parliament, agenda_setter,
    institutional, generational, mobile, national).

% Occupy an analytical seat evaluating the statute as a contested historiographical site. Their interpretive commitments determine whether the entanglement of concept and institution is treated as an ontological feature of legal origin or as an analytical failure to separate dimensions that could in principle be distinguished.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, legal_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__entangled_event_reading, publishers_and_booksellers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__entangled_event_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilized the English book trade after the lapse of the Licensing Act by creating a statutory, limited-term, transferable exclusive right in printed works, replacing guild licensing with a nominally author-centered legal framework that allowed commerce to continue.
% TRANSFER_FUNCTION: Transferred the legal basis of control over texts from the Stationers' Company monopoly to a state-granted right that immediately flowed to publishers through assignment; transferred the burden of conceptual clarity from legislators to later jurists and scholars who must interpret an origin that fuses concept and institution.
% ABSENT_VOICES: Common law jurists who argued for perpetual authorial rights independent of statute; provincial and non-London authors who could not navigate the registration system; the unlearned public who were nominally the beneficiaries of the 'encouragement of learning' but had no seat in drafting.
% DISAPPEARANCE_RATIONALE: If the statute had not been enacted, the book trade would have reorganized around common law, contract, or guild custom rather than statutory copyright. The specific entanglement of IP concept with IP institution would not exist as the origin point of modern copyright, and later jurists would lack this particular epistemic obstacle.
% FOUNDING_PROBLEM: The collapse of the Stationers' Company monopoly after the lapse of the Licensing Act in 1695 created legal uncertainty and trade instability in the English book market; the statute was built to provide a new enforceable foundation for exclusive printing while accommodating Whig opposition to perpetual monopoly.
% FOUNDING_PROBLEM_CORROBORATION: Whig parliamentarians and anti-monopoly pamphleteers attested the need to break the Stationers' monopoly from outside the trade. The Stationers' Company themselves petitioned for new legislation, corroborating the trade-instability reading but from inside the benefiting party. Modern legal historians (Rose, Deazley) from outside the original beneficiary set attest that the 'encouragement of authors' was substantially rhetorical cover for publisher-driven trade reorganization.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.72) is substantial because the statute created a transferable monopoly right that concentrated in publishers despite nominal authorial grant. Suppression (0.58) is moderate: the constraint suppressed alternative arrangements (perpetual common law copyright, open printing) through statutory preemption and registration requirements, but less violently than a pure snare. Theater_ratio (0.55) is moderate-high: the 'encouragement of learning' rhetoric and author-centric framing performed a legitimating function increasingly disconnected from publisher-centric practice. Accessibility_collapse (0.65) is moderately high because the statute's entangled origin became the taken-for-granted foundation of IP discourse, making alternative genealogies harder to articulate. Resistance (0.45) is moderate: the Stationers initially resisted the loss of licensing monopoly, and later jurists resisted the conceptual confusion, but neither overturned the framework. The measurement series tracks gradual intensification of extraction and theater across successive reinterpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the publisher's seat, the statute is a coordination mechanism that brought order to the book trade after the Licensing Act's collapse. From the author's seat, it is a promise of rights that was structurally captured by those with capital and enforcement capacity. From the jurist's seat, it is an epistemic trap: any attempt to derive clean doctrinal principles from the statute founders on its inseparable conceptual and institutional dimensions. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers_and_booksellers derive low d (near beneficiary) because they collect the practical rents of the statutory framework and have mobile exit. Early_modern_authors derive a mixed d: they are declared beneficiaries but their constrained exit and secondary payer role push d upward toward the target end; the automatic derivation from beneficiary declaration alone would understate their practical extraction. Jurists_and_scholars derive high d (near target) because they bear the cost of conceptual entanglement without collecting rents. Parliament sits near symmetric, as both the source of the constraint and the actor that could amend it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the statute as pure rope (which would ignore the publisher capture and conceptual obscurity) or pure snare (which would deny the genuine coordination function of stabilizing the book trade after 1695). The founding problem â trade instability after the lapse of the Licensing Act â was real and live, but the solution entangled a conceptual innovation with an institutional reallocation that benefited publishers more than authors. The mandatrophy is not resolved because the arrangement persists beyond its original trade-stabilization function, now serving as the origin myth for a vastly expanded IP architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_separability,
    'Can the conceptual_emergence_reading and institutional_reallocation_reading be held independently, or does the entangled_event_reading demonstrate that any separation is an analytical artifact?',
    'Comparative historiographical analysis assessing whether the sibling readings can each account for the statute''s full structure without remainder.',
    'If the sibling readings cannot be held independently, the entangled_event_reading acquires structural priority; if they can, the kernel remains genuinely underdetermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_separability, conceptual, 'Whether the kernel''s sibling readings are separable or analytical artifacts').

omega_variable(
    publisher_capture_magnitude,
    'To what extent did publishers and booksellers capture the statutory rights nominally granted to authors under the Statute of Anne?',
    'Quantitative analysis of assignment records and litigation patterns in the 18th-century book trade.',
    'High capture would confirm the asymmetric extraction dimension of the tangled rope; low capture would suggest the coordination function was more symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publisher_capture_magnitude, empirical, 'Extent of publisher capture of nominally authorial rights').

omega_variable(
    conceptual_clarity_victim,
    'Is the loss of conceptual clarity an unavoidable cost of legal-institutional genesis, or a contingent feature of this specific statute?',
    'Comparative legal history examining whether other foundational statutes exhibit similar entanglement or achieve cleaner conceptual-institutional separation.',
    'If unavoidable, the entanglement is a structural feature of legal origin and the victim set generalizes; if contingent, the statute is a locally defective coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptual_clarity_victim, conceptual, 'Whether conceptual unclarity is structural or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t50, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(stat_tr_t100, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement(stat_tr_t150, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 150, 0.45).
narrative_ontology:measurement(stat_tr_t200, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 200, 0.5).
narrative_ontology:measurement(stat_tr_t250, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 250, 0.53).
narrative_ontology:measurement(stat_tr_t300, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 300, 0.55).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t50, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(stat_be_t100, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 100, 0.5).
narrative_ontology:measurement(stat_be_t150, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 150, 0.55).
narrative_ontology:measurement(stat_be_t200, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 200, 0.62).
narrative_ontology:measurement(stat_be_t250, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 250, 0.68).
narrative_ontology:measurement(stat_be_t300, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 300, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(stat_su_t50, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(stat_su_t100, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(stat_su_t150, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 150, 0.6).
narrative_ontology:measurement(stat_su_t200, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 200, 0.65).
narrative_ontology:measurement(stat_su_t250, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 250, 0.7).
narrative_ontology:measurement(stat_su_t300, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 300, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the statute_of_anne_ip_foundation kernel, alongside conceptual_emergence_reading and institutional_reallocation_reading. The entangled_event_reading asserts inseparability; decomposition into separate conceptual and institutional constraints would violate its core premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
