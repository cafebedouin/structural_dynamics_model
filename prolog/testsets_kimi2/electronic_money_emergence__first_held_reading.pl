% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: Digital Money Emergence: First Institutional Holding Reading
 *   domain: economic/monetary_history
 *
 * SUMMARY:
 *   This constraint story models the institutional-legal arrangement that
 *   defines digital money by the act of a licensed institutional bearer
 *   holding dematerialized currency in a form legally distinguishable from
 *   physical notes. It is one reading (first_held_reading) of the contested
 *   kernel electronic_money_emergence, which asks when digital money
 *   historically emerged. This reading treats emergence as a discrete
 *   ontological threshold tied to regulatory recognition, contrasting with
 *   readings that locate emergence in conceptual thinkability or statistical
 *   measurement artifact. The constraint coordinates the modern payment
 *   system by establishing a legally enforceable category of non-physical
 *   money, while asymmetrically concentrating money-creation and settlement
 *   privileges in licensed banking institutions.
 *
 * KEY AGENTS:
 *   - Central banks and regulators (agenda_setter): define and enforce the institutional-bearer boundary
 *   - Licensed commercial banks (beneficiary): hold dematerialized monetary claims with exclusive settlement finality
 *   - Non-bank public (payer): uses institutional digital money without direct access to its creation or reserve layer
 *   - Non-bank payment innovators (excluded): issue functional dematerialized claims denied money-status
 *   - Monetary scholars (observer): historiographical and analytical seat debating emergence readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.68).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.8).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, tangled_rope).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Digital Money Emergence: First Institutional Holding Reading").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic/monetary_history").

domain_priors:requires_active_enforcement(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, 'fe5cc351-8973-4a2f-94fa-f18f96484d0d').
narrative_ontology:cs_kernel_codification('fe5cc351-8973-4a2f-94fa-f18f96484d0d', formalized).
narrative_ontology:cs_authority_grounding('fe5cc351-8973-4a2f-94fa-f18f96484d0d', lineage).
narrative_ontology:cs_interpretation_layer_present('fe5cc351-8973-4a2f-94fa-f18f96484d0d').
narrative_ontology:cs_reading_relation('fe5cc351-8973-4a2f-94fa-f18f96484d0d', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe5cc351-8973-4a2f-94fa-f18f96484d0d', electronic_money_emergence__m4_m5_collapse_reading, forecloses).
narrative_ontology:cs_axiom('fe5cc351-8973-4a2f-94fa-f18f96484d0d', foundational, institutional_recognition_constitutes_money).
narrative_ontology:cs_axiom_status(institutional_recognition_constitutes_money, holdable).
narrative_ontology:cs_axiom_grounding('fe5cc351-8973-4a2f-94fa-f18f96484d0d', institutional_recognition_constitutes_money, conventional).
narrative_ontology:cs_axiom('fe5cc351-8973-4a2f-94fa-f18f96484d0d', foundational, legal_finality_requires_institutional_bearer).
narrative_ontology:cs_axiom_status(legal_finality_requires_institutional_bearer, holdable).
narrative_ontology:cs_axiom_grounding('fe5cc351-8973-4a2f-94fa-f18f96484d0d', legal_finality_requires_institutional_bearer, conventional).
narrative_ontology:cs_reference_frame('fe5cc351-8973-4a2f-94fa-f18f96484d0d', institutional_legal_recognition_framework).
narrative_ontology:cs_drift_state('fe5cc351-8973-4a2f-94fa-f18f96484d0d', contemporary_digital_currency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe5cc351-8973-4a2f-94fa-f18f96484d0d', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, licensed_commercial_banks).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, non_bank_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the legal criteria for institutional bearer status, license banks, and enforce the boundary between money and non-money liabilities. They maintain the monetary ontology through regulation, settlement system oversight, and lender-of-last-resort frameworks.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_banks_and_regulators, agenda_setter,
    institutional, civilizational, analytical, global).

% Hold reserve deposits and customer liabilities in dematerialized form that the law recognizes as money. They collect intermediation spreads, payment fees, and seigniorage-like benefits from the exclusive right to issue demand deposits that settle at par with central bank money.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, licensed_commercial_banks, beneficiary,
    powerful, generational, constrained, national).

% Use digital money for everyday transactions and savings but cannot issue monetary liabilities or hold central bank reserves directly. They bear the costs of financial intermediation and must accept the institutional definition of money as a condition of economic participation.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, non_bank_public, payer,
    powerless, biographical, constrained, national).

% Issue dematerialized value claims that function as money in practice but lack institutional bearer status. They are structurally excluded from the regulatory money category and must partner with licensed banks to achieve settlement finality.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, non_bank_payment_innovators, excluded,
    moderate, biographical, trapped, national).

% Document and debate the historical threshold of digital money emergence, comparing institutional, conceptual, and measurement-based readings of the same historical record without bearing the constraint's costs or collecting its rents.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, licensed_commercial_banks).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legally enforceable standard for what counts as digital money, enabling interbank settlement, payment finality, and public confidence in non-physical value storage across a dispersed economy.
% TRANSFER_FUNCTION: Moves the authority to create, hold, and settle dematerialized monetary claims from the non-institutional public to licensed depository institutions and central banks; transfers seigniorage and intermediation rents to licensed bearers.
% ABSENT_VOICES: Non-bank fintech issuers, cryptocurrency communities, and mutual credit system operators are structurally excluded from the institutional-bearer category; they would argue that their dematerialized liabilities also function as money but are denied recognition.
% DISAPPEARANCE_RATIONALE: If the institutional-bearer definition vanished overnight, dematerialized claims would lose legal settlement finality, payment systems would fragment, and the boundary between money and non-money credit would dissolve — the monetary order would reorganize around whatever functional claims parties accepted.
% FOUNDING_PROBLEM: How to legally recognize and secure value in dematerialized form after the decay of metallic currency standards, so that non-physical claims could circulate with the same finality as physical notes.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and banking historians attest the founding problem was achieving legal certainty for book-entry money. Heterodox economists and monetary archaeologists attest the problem was already solved by private clearing systems and the institutional overlay was a capture mechanism; corroboration from outside the benefiting parties is mixed.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.30 to 0.68 over the interval as banking intermediation deepens and alternative digital claims are systematically denied money-status. Suppression rises from 0.40 to 0.80 as legal tender laws, licensing requirements, and settlement finality rules harden against private and decentralized alternatives. Theater ratio rises to 0.45, reflecting growing performative compliance and regulatory ritual surrounding the institutional-bearer boundary even as the underlying coordination function (payment clearing) becomes more automated. Accessibility collapse is high (0.82) because once the institutional definition is accepted, non-institutional alternatives appear legally and conceptually illegitimate. Resistance is moderate (0.48) from cryptocurrency movements, local currency advocates, and historical free-banking scholarship.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (central banks) experiences the constraint as necessary monetary infrastructure guaranteeing finality; the payer seat (non-bank public) experiences it as an unchangeable background condition of economic life; the excluded seat (non-bank innovators) experiences it as an arbitrary gate blocking functional claims from recognition. The engine computes divergent per-seat classifications from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Licensed commercial banks are structural beneficiaries (low d) because the institutional-bearer definition directly subsidizes their intermediation rents and settlement privileges. The non-bank public are structural targets (high d) because they bear the cost of intermediation without reciprocal privilege. Non-bank innovators are near full-target (high d, trapped exit) because the constraint's existence actively suppresses their claims. Central banks sit near symmetric but with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — achieving legal certainty for dematerialized value — is contested as to whether it remains live. The divergence between founding_problem_status=contested and disappearance_verdict=world_rearranges signals that the arrangement may have outlived its original problem but persists because the institutional structure now depends on it. This prevents mislabeling the constraint as pure coordination (Rope) by showing that its persistence is partly driven by beneficiary capture rather than ongoing problem-solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_measure_threshold,
    'Is the first-held threshold a genuine ontological transition in money''s form, or a retroactive legal classification imposed on pre-existing dematerialized claims?',
    'Archival discovery of the earliest institutional ledger entries and their regulatory treatment; analysis of whether pre-institutional dematerialized claims functioned as money in practice.',
    'If pre-institutional claims functioned as money, the first-held reading overstates the threshold and the constraint is more constructed than ontological; if not, the institutional event marks a genuine emergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_measure_threshold, conceptual, 'Whether the emergence threshold is ontological or classificatory.').

omega_variable(
    institutional_bearer_extraction_ambiguity,
    'Does the institutional-bearer definition of digital money coordinate payment systems efficiently, or does it asymmetrically extract by reserving money-creation and direct central-bank access to licensed entities?',
    'Comparative analysis of payment costs and access in jurisdictions with varying bank-privilege levels; assessment of whether non-bank digital settlement alternatives achieve equivalent coordination.',
    'If the institutional gate extracts asymmetrically, the constraint computes as tangled_rope; if the coordination is symmetric and necessary, it may compute as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_bearer_extraction_ambiguity, empirical, 'Whether the bearer privilege is extractive or purely coordinative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emergence_firstheld_tr_t0, electronic_money_emergence__first_held_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(emergence_firstheld_tr_t10, electronic_money_emergence__first_held_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(emergence_firstheld_tr_t20, electronic_money_emergence__first_held_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(emergence_firstheld_tr_t30, electronic_money_emergence__first_held_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(emergence_firstheld_tr_t40, electronic_money_emergence__first_held_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(emergence_firstheld_tr_t50, electronic_money_emergence__first_held_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(emergence_firstheld_tr_t60, electronic_money_emergence__first_held_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(emergence_firstheld_be_t0, electronic_money_emergence__first_held_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(emergence_firstheld_be_t10, electronic_money_emergence__first_held_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(emergence_firstheld_be_t20, electronic_money_emergence__first_held_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(emergence_firstheld_be_t30, electronic_money_emergence__first_held_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(emergence_firstheld_be_t40, electronic_money_emergence__first_held_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(emergence_firstheld_be_t50, electronic_money_emergence__first_held_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(emergence_firstheld_be_t60, electronic_money_emergence__first_held_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(emergence_firstheld_su_t0, electronic_money_emergence__first_held_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(emergence_firstheld_su_t10, electronic_money_emergence__first_held_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(emergence_firstheld_su_t20, electronic_money_emergence__first_held_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(emergence_firstheld_su_t30, electronic_money_emergence__first_held_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(emergence_firstheld_su_t40, electronic_money_emergence__first_held_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(emergence_firstheld_su_t50, electronic_money_emergence__first_held_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement(emergence_firstheld_su_t60, electronic_money_emergence__first_held_reading, suppression_requirement, 60, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, resource_allocation).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% The electronic_money_emergence kernel decomposes into three structurally distinct readings: first_held_reading (ontological institutional threshold), became_thinkable_reading (conceptual threshold), and m4_m5_collapse_reading (measurement artifact). Each reading has a different referent, epsilon, and stakeholder structure. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
