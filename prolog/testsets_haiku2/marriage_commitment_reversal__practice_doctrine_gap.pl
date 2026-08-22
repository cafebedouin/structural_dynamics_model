% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Principle Doctrine-Practice Gap (1890-1904)
 *   domain: religious/institutional/political
 *
 * SUMMARY:
 *   Between 1890 and 1904, the institutional leadership maintained plural
 *   marriage as a preserved doctrine (Section 132, framed as revealed and
 *   eternal) while suspending its public practice to meet federal
 *   anti-polygamy enforcement. The constraint is the structural ambiguity
 *   itself: membership identity becomes contingent on interpreting doctrine
 *   as eternally true while practice as temporarily suspended — not
 *   repudiated, but no longer binding. This reading instantiates the
 *   practice-doctrine gap as a live constraint, not as a transient
 *   negotiating position or a doctrinal innovation. The beneficiary is
 *   institutional survival through ambiguity; the victims are those whose
 *   identity was constituted by doctrine-practice coherence. The measurement
 *   series tracks the intensification of the gap from the initial practice
 *   suspension (year 0, 1890) through the deepening of doctrinal preservation
 *   without practice (year 14, 1904), showing theater_ratio rising as
 *   performative doctrinal affirmation replaces lived practice.
 *
 * KEY AGENTS:
 *   - Institutional leadership: preserves doctrine while suspending practice, collects institutional flexibility and survival
 *   - General membership: bears identity fragmentation and cognitive dissonance from the gap
 *   - Fundamentalist factions: resist the gap, either schism or face suppression
 *   - Federal authorities: enforce anti-polygamy laws, validate the gap by enforcement of practice suspension
 *   - Territorial communities: continue practice covertly, structurally excluded from public institutional voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.82).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.76).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Principle Doctrine-Practice Gap (1890-1904)").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious/institutional/political").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '368aa14d-157c-440c-9615-106ceadaab43').
narrative_ontology:cs_kernel_codification('368aa14d-157c-440c-9615-106ceadaab43', fixed_text).
narrative_ontology:cs_authority_grounding('368aa14d-157c-440c-9615-106ceadaab43', extraction).
narrative_ontology:cs_interpretation_layer_present('368aa14d-157c-440c-9615-106ceadaab43').
narrative_ontology:cs_reading_relation('368aa14d-157c-440c-9615-106ceadaab43', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('368aa14d-157c-440c-9615-106ceadaab43', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('368aa14d-157c-440c-9615-106ceadaab43', foundational, doctrine_practice_separation_permissible).
narrative_ontology:cs_axiom_status(doctrine_practice_separation_permissible, holdable).
narrative_ontology:cs_axiom_grounding('368aa14d-157c-440c-9615-106ceadaab43', doctrine_practice_separation_permissible, deontological).
narrative_ontology:cs_axiom('368aa14d-157c-440c-9615-106ceadaab43', foundational, institutional_survival_justifies_ambiguity).
narrative_ontology:cs_axiom_status(institutional_survival_justifies_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('368aa14d-157c-440c-9615-106ceadaab43', institutional_survival_justifies_ambiguity, instrumental).
narrative_ontology:cs_reference_frame('368aa14d-157c-440c-9615-106ceadaab43', plural_marriage_as_eternal_principle).
narrative_ontology:cs_drift_state('368aa14d-157c-440c-9615-106ceadaab43', federal_enforcement_period_1890_1904, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('368aa14d-157c-440c-9615-106ceadaab43', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, general_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the doctrinal commitment to plural marriage (Section 132) as revealed principle while publicly suspending practice to meet federal pressure. Sets the official narrative that doctrine and practice can diverge under external duress without invalidating either. Collects institutional survival and legitimacy from the flexibility this ambiguity enables. Operates simultaneously across two jurisdictional zones: territory (where practice continues under cover) and federal (where public compliance is performed).
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, continental).

% Bears the cognitive cost of a doctrine-practice split: the principle they were taught is sacred but now unenforceable; the institutional authority that taught it now denies enforcing it. Experiences betrayal (promised eternal practice no longer delivered) and bewilderment (unclear what commitment actually binds). Membership identity is constituted through the doctrine; exit means leaving the faith community entirely, not merely disagreeing with leadership.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    powerless, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, general_membership, beneficiary).

% Reject the practice suspension as a betrayal of the claimed-eternal doctrine. They interpret the doctrine-practice gap not as necessary flexibility but as false witness — the institution claiming fidelity to the principle while abandoning its practice. Either schism from the main institution or face suppression of their continued practice. Their exit is more structured than the general membership's (they can form alternative communities) but comes at the cost of losing institutional resources and legitimacy.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions, payer,
    organized, biographical, constrained, continental).

% Enforce the anti-polygamy statutes that create the external pressure the institution responds to. They assess compliance by observable practice, not doctrine. The doctrine-practice gap is the institutional response they motivated; they validate compliance by the gap's existence (practice suspension) while the institution validates doctrinal purity by the gap's continuation (doctrine preserved).
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_authorities, observer,
    institutional, generational, analytical, national).

% Practice plural marriage under the doctrine in territorial jurisdictions where federal enforcement is weak. They would attest that the practice-doctrine gap is a false appearance — the doctrine is lived, the principle is operative — but their voice is structurally excluded from public institutional narratives, which must perform federal compliance. Their continued practice contradicts the public institutional narrative while the institution benefits from both the territorial practice (maintaining doctrine's viability) and the federal appearance (avoiding legal jeopardy).
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, territorial_communities, excluded,
    moderate, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves institutional identity and doctrinal continuity under external legal threat: maintains both the claimed-eternal principle (doctrine) and the institution's survival (practice suspension) as simultaneously true in different contexts, solving the coordination problem of 'how to remain faithful to revealed truth while submitting to law.'
% TRANSFER_FUNCTION: Transfers authority and interpretive power from the membership to the leadership: members lose the right to claim the doctrine as binding on their own practice while leadership retains the right to preserve it as doctrine. Transfers institutional flexibility and survival benefit to leadership; transfers cognitive dissonance and membership-identity fragmentation to the general membership.
% ABSENT_VOICES: Territorial practitioners and women in plural marriages (whose family stability and marital security are directly contingent on the practice-doctrine coherence) are structurally excluded from public institutional discourse, which must narrate the gap as acceptable. They would testify that the gap is a lie; their testimony is unavailable in the spaces where the constraint's legitimacy is adjudicated (federal courts, public institutional statements).
% DISAPPEARANCE_RATIONALE: If the doctrine-practice gap closed (either doctrine abandoned or practice resumed publicly), the institution would either lose doctrinal coherence (if doctrine revoked) or legal jeopardy (if practice resumed). The contradiction itself enables the institution's dual-track survival. Without it, either the institution must schism, face legal suppression, or abandon the doctrine entirely — the membership would reorganize around a clear commitment rather than a split identity.
% FOUNDING_PROBLEM: How can an institution founded on a revealed doctrinal principle survive when external law prohibits that principle's practice? The founding problem is institutional survival under legal attack, not doctrinal truth or moral justification.
% FOUNDING_PROBLEM_CORROBORATION: Federal authorities and legal historians attest the external threat is real and that practice suspension was forced response, not voluntary reinterpretation. Fundamentalist factions and territorial practitioners attest that the institution is choosing the gap rather than resolving it. The institutional leadership itself attests that doctrine is preserved as principle — all parties corroborate the gap's existence; the contest is over whether it is a necessary solution (leadership, federal reading) or a disguised capitulation (fundamentalist, territorial reading).
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the constraint extracts the membership's right to live the doctrine as taught — the principle is preserved but its practice is suspended, and membership cannot claim entitlement to practice without institutional permission. Suppression is high (0.76) because the gap is maintained by active denial: the institution must actively prevent territorial practice from surfacing in public narrative, must suppress fundamentalist attempts to resume practice, and must suppress the membership's cognitive coherence by insisting both the doctrine and the suspension are true. Theater is the highest metric (0.68) because the constraint's primary function shifts from coordination (unified belief-practice) to performance: doctrinal affirmation becomes theater, a public action divorced from lived function. The measurement series shows all three metrics rising in the first 7 years (1890-1897: panic, intensifying enforcement, leadership doubling down on doctrinal preservation while practice is suppressed), plateauing toward the end (1897-1904: the gap normalizes, members learn to interpret doctrine as non-binding in practice, institutional theater becomes routinized). The rising theater_ratio and suppression_requirement are diagnostic of a constraint whose coordination function has atrophied but whose extraction function (leadership's control over membership interpretation) has intensified.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat, the doctrine-practice gap is a survival mechanism: doctrine is preserved as principle (institutional fidelity to revelation) while practice is suspended (obedience to law). This seat computes the constraint as rope — coordination of dual loyalty (God's principle and Caesar's law). From the membership and fundamentalist seats, the gap is extraction: leadership preserved its own doctrinal authority while suspending the membership's right to practice. These seats compute the constraint as snare — the doctrine binds the membership's identity but not their practice; leadership collects obedience while denying entitlement. The engine will compute per-seat types from the structural data; the claimed_type (tangled_rope) sits between them, capturing both the coordination function (institutional survival requires leadership to maintain doctrine while obeying law) and the asymmetric extraction (membership bears the cognitive cost and identity fragmentation).
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional leadership is the clear beneficiary: they collects institutional survival, doctrinal authority, and interpretive power from the gap. The membership pays in cognitive dissonance and identity fragmentation. Fundamentalists pay by facing suppression or schism. Territorial communities are both trapped payers (their practice is denied and suppressed) and excluded, making them the highest-d targets. General membership is identity_locked — they cannot exit without leaving the faith — and thus experience high extractiveness even though the constraint's surface is doctrinal, not legal. The directionality data from these structural positions will show the leadership as d ≈ 0.0 (beneficiary with high exit: they can exit the leadership role and still retain institutional membership, but they choose not to, so exit is arbitrage), membership as d ≈ 0.95 (fully trapped by identity), fundamentalists as d ≈ 0.85 (organized but constrained), and territorial communities as d ≈ 1.0 (fully trapped by geography and institutional denial of voice). No overrides are needed; the beneficiary/victim declarations and exit_options drive the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is institutional survival under legal attack. At the interval start (1890), the problem is live and acute: federal enforcement is active, practice must be suspended, doctrine is in jeopardy. At the interval end (1904), the problem's status becomes contested: the institution claims survival is assured and doctrine is preserved, but fundamentalists attest that survival has been purchased through doctrinal betrayal, and the gap itself demonstrates the problem remains unresolved (doctrine cannot be lived). The measured theater_ratio rising from 0.22 to 0.68 indicates the constraint's function migrating from coordination (unified belief-practice) to performance (dual affirmation without coherence). This is classic mandatrophy: the constraint's founding problem (institutional survival) outlives its coordination function (membership coherence), and leadership must perform the doctrine without enabling its practice to maintain both institutional survival and doctrinal claim. The doctrine-practice gap is the mandatrophic structure itself — a constraint that solves for institutional survival at the cost of sacrificing membership coherence and doctrinal integrity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression primarily structural (institutional authority actively preventing practice and suppressing fundamentalist dissent) or internalized (membership internalizing the doctrine-practice split and suppressing their own entitlement claims)?',
    'Post-exit trajectory analysis: if membership who leave the institution continue to experience cognitive dissonance about the gap (internalized suppression persists), the suppression is partially internalized. If fundamentalist communities that schism quickly normalize alternative practice (structural suppression was the primary factor), the suppression is primarily structural.',
    'If internalized, the constraint''s effective extraction is higher than the measured 0.82 suggests — membership carries the suppression with them after exit. If structural, the constraint''s extraction is localized to the institutional context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of the doctrine-practice coherence is maintained by institutional coercion or by membership self-suppression.').

omega_variable(
    doctrine_preservation_vs_doctrine_falsification,
    'Is the preserved doctrine (Section 132 maintained as revealed principle) a genuine doctrinal commitment, or is it a false affirmation maintained for legitimacy while the institution''s real operative doctrine is the practice suspension?',
    'Internal institutional record analysis and subsequent doctrinal evolution: does the preserved doctrine reappear in later revisions, or is it permanently superseded? If permanently superseded by doctrine explicitly revising the principle, the preserved doctrine was theater. If it resurfaces as a binding principle (even if never re-practiced), preservation was real.',
    'If false affirmation, the constraint is pure snare (extraction disguised as coordination); the claimed tangled_rope type would be reclassified. If genuine preservation, the constraint remains tangled_rope (real coordination problem of institutional survival + real asymmetric extraction on membership).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_preservation_vs_doctrine_falsification, conceptual, 'Whether the preserved doctrine is a sincere institutional commitment or a performative claim maintained only for legitimacy.').

omega_variable(
    beneficiary_location_ambiguity,
    'Does the institutional leadership benefit as a unified seat, or does the constraint differentially benefit some leadership factions (pragmatists accepting federal pressure) while extracting from others (fundamentalist sympathizers within leadership)?',
    'Leadership internal schism analysis: if some leadership members promote the doctrine-practice gap while others resist it, the gap is not uniformly beneficial to ''institutional leadership'' as a monolith. The constraint''s beneficiary location shifts to ''pragmatist leadership'' (more narrow) with ''fundamentalist leadership'' becoming a victim.',
    'If the constraint''s benefit is not uniformly distributed within leadership, the directionality computation for institutional leadership must account for internal disagreement, and the piton dynamics (theater without clear beneficiary) become more plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_location_ambiguity, empirical, 'Whether institutional leadership is a unified beneficiary or is itself split by the constraint.').

omega_variable(
    kernel_reading_coexistence_test,
    'Can all three sibling readings (practice_doctrine_gap, exogenous_override_reading, endogenous_reinterpretation_reading) be simultaneously true within a single interpretive framework, or do they foreclose each other?',
    'Narrative coherence test: can an institution coherently hold that practice was overridden by external pressure AND reinterpreted by internal revelation AND maintained as ambiguous doctrine simultaneously, or does adopting one reading logically preclude the others?',
    'If they coexist within a single framework (all three describe different aspects of the same reversal), the readings are genuinely coexisting siblings. If they foreclose each other (adoption of one precludes the others), the kernel contains logical contradictions that force institutional choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_test, conceptual, 'Whether the three readings of the marriage-commitment reversal kernel coexist as live positions or foreclose each other logically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0, 0.22).
narrative_ontology:measurement(marr_tr_t2, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 2, 0.35).
narrative_ontology:measurement(marr_tr_t4, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 4, 0.48).
narrative_ontology:measurement(marr_tr_t7, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 7, 0.61).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 10, 0.66).
narrative_ontology:measurement(marr_tr_t14, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 14, 0.68).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t2, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(marr_be_t4, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(marr_be_t7, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 7, 0.78).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(marr_be_t14, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 14, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0, 0.51).
narrative_ontology:measurement(marr_su_t2, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 2, 0.62).
narrative_ontology:measurement(marr_su_t4, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(marr_su_t7, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 7, 0.73).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(marr_su_t14, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 14, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, attachment_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__practice_doctrine_gap, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_commitment_reversal kernel. The three readings (practice_doctrine_gap, exogenous_override_reading, endogenous_reinterpretation_reading) all describe the 1890-1904 practice suspension but instantiate it as different constraints with different ε values and victim structures. The practice_doctrine_gap reading treats the structural ambiguity itself as the constraint (high extraction because membership coherence is sacrificed for flexibility), with ε ≈ 0.82. The exogenous_override_reading treats external legal pressure as the primary causal mechanism (moderate-high extraction, different causation), and the endogenous_reinterpretation_reading treats divine revelation as the mechanism (lower extraction if reinterpretation is accepted as genuine, higher if seen as false). All three readings share the surface reversal of practice but diverge on whether the constraint is the gap itself, the external pressure, or the internal reinterpretation. Each should be authored as a separate constraint story with its own beneficiary/victim structure and ε value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
