% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Electronic Money Emergence: First Institutional Held Reading
 *   domain: economic/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint story models the 'first held reading' of the
 *   electronic_money_emergence kernel: the claim that digital money emerged
 *   as a discrete institutional event when the first licensed bearer held
 *   dematerialized currency in a form legally distinguishable from physical
 *   notes. The reading formalizes monetary emergence as a legal-institutional
 *   threshold rather than a conceptual, technical, or statistical event. It
 *   functions as a commitment system that coordinates monetary historiography
 *   and regulatory jurisdiction while asymmetrically concentrating
 *   definitional authority in central banks and licensed depositories.
 *
 * KEY AGENTS:
 *   - central_banks: Agenda-setter (institutional/global) â administers the legal threshold for what counts as money.
 *   - licensed_depository_institutions: Beneficiary (institutional/global) â gains historical legitimacy as the necessary condition for electronic money's existence.
 *   - cryptocurrency_networks: Payer (organized/global) â functions as money but is excluded from the historical and legal narrative by the institutional-bearer requirement.
 *   - early_digital_cash_innovators: Payer (moderate/global) â historiographically trapped; their pre-institutional experiments are classified as non-monetary.
 *   - monetary_legal_scholars: Observer (analytical/global) â interprets and transmits the legal-institutional tradition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.55).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.62).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, tangled_rope).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Electronic Money Emergence: First Institutional Held Reading").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic/monetary_theory/technology_studies").

domain_priors:requires_active_enforcement(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, 'ffb89492-7cd4-4cd5-9aeb-01f9bbda8378').
narrative_ontology:cs_kernel_codification('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378', formalized).
narrative_ontology:cs_authority_grounding('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378', lineage).
narrative_ontology:cs_interpretation_layer_present('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378').
narrative_ontology:cs_reading_relation('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378', foundational, institutional_recognition_precedes_moneyhood).
narrative_ontology:cs_axiom_status(institutional_recognition_precedes_moneyhood, holdable).
narrative_ontology:cs_axiom_grounding('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378', institutional_recognition_precedes_moneyhood, conventional).
narrative_ontology:cs_axiom('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378', foundational, ontological_transition_is_observable_event).
narrative_ontology:cs_axiom_status(ontological_transition_is_observable_event, holdable).
narrative_ontology:cs_axiom_grounding('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378', ontological_transition_is_observable_event, empirically_contingent).
narrative_ontology:cs_reference_frame('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378', legal_recognition_standard).
narrative_ontology:cs_drift_state('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378', post_bitcoin_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ffb89492-7cd4-4cd5-9aeb-01f9bbda8378', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, licensed_depository_institutions).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, cryptocurrency_networks).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, early_digital_cash_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the legal and regulatory framework that defines when dematerialized currency constitutes 'money'. Set monetary aggregates, oversee payment systems, and determine which institutions qualify as legitimate bearers. Their authority derives from continuity with legal-institutional monetary theory and international regulatory coordination. Exit is constrained by treaty obligations, mandate, and the global monetary order.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_banks, agenda_setter,
    institutional, generational, constrained, global).

% Hold dematerialized currency on behalf of clients and are recognized as the necessary institutional vehicle for electronic money's historical emergence. Benefit from definitional legitimacy that anchors monetary origins in their custody function. Exit is constrained by licensing requirements and dependence on central bank recognition.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, licensed_depository_institutions, beneficiary,
    institutional, biographical, constrained, global).

% Operate decentralized digital value-transfer systems that function as money without institutional bearer status in the traditional sense. Are structurally excluded from the 'first held' narrative and often denied legal-tender or money-status by regulators who apply the institutional threshold. Exit is constrained by global regulatory convergence around the institutional definition.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, cryptocurrency_networks, payer,
    organized, biographical, constrained, global).

% Developed pre-Bitcoin digital cash systems that operated without licensed institutional custody. Historiographically classified as experimental or non-monetary because no regulated institution held them in a form distinguished from physical notes. Their exit is trapped: the historical record is fixed by the definitional framework.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, early_digital_cash_innovators, payer,
    moderate, biographical, trapped, global).

% Interpret and transmit the legal-institutional tradition of monetary theory, articulating why institutional recognition constitutes the valid threshold for money's emergence. Occupies an analytical seat but is institutionally embedded in law schools and central bank research departments that favor the first-held reading.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legally determinate threshold for when dematerialized currency transitions from bookkeeping entry to 'money', enabling coordinated monetary historiography, regulatory jurisdiction, and macroeconomic aggregation across institutions and nations.
% TRANSFER_FUNCTION: Moves definitional legitimacy and historical priority from pre-institutional and non-institutional digital value-transfer experiments to licensed institutional custodians and central banks; moves the authority to recognize or deny money-status from operational social practice to legal and regulatory institutions.
% ABSENT_VOICES: Early digital cash innovators (e.g. Chaum's DigiCash) whose systems functioned as digital value transfer without institutional bearer status; cryptocurrency networks and stablecoin issuers that operate as money without traditional depository institutions; anthropologists and heterodox economists who theorize money as emergent from social credit relations rather than legal fiat.
% DISAPPEARANCE_RATIONALE: If the first-held threshold disappeared, monetary history would lose its institutional anchor; central bank authority over digital money definitions would weaken; alternative money systems could claim historical continuity and legitimacy; regulatory jurisdiction over early digital experiments would shift; and macroeconomic aggregates would require reconceptualization.
% FOUNDING_PROBLEM: How to preserve legal certainty, state monetary sovereignty, and regulatory jurisdiction as bank accounting dematerialized from physical notes to electronic book entries, preventing a vacuum in monetary definition.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and international monetary institutions (BIS, IMF) attest the problem remains live, citing financial stability and legal clarity. Cryptocurrency advocates and critical monetary scholars attest the problem was constructed to preserve institutional gatekeeping and is now used to suppress non-state money forms; no corroborating source fully outside the benefiting institutional complex exists.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.55) is moderate-to-high because the constraint extracts definitional legitimacy and regulatory authority from non-institutional money forms, concentrating it in the institutional core. Suppression (0.62) reflects the active enforcement of the threshold through legal tender definitions, monetary aggregate classification, and academic curriculum â alternative ontologies are structurally excluded from official recognition. Theater_ratio (0.45) captures the increasing performative dimension: as technology makes institutional custody less functionally necessary, the threshold's maintenance becomes more ceremonial. Accessibility_collapse (0.78) is high because once the legal-institutional framework is accepted, the possibility of money existing without institutional recognition becomes nearly unthinkable within orthodox discourse. Resistance (0.48) comes from cryptocurrency advocates and heterodox scholars who contest the institutional monopoly on monetary definition. The measurement series shows a ratchet: extraction and suppression were modest when the threshold was first articulated but intensified after 2008 as non-institutional digital money challenged the framework.
 *
 * PERSPECTIVAL GAP:
 *   The central bank seat experiences the constraint as necessary coordination: without a clear legal threshold, monetary sovereignty and macroeconomic measurement would dissolve into ambiguity. The cryptocurrency and early-innovator seats experience the same structure as extraction: their functional money is denied ontological status by a definitional fiat that privileges institutional form over operational reality. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and licensed institutions are structurally positioned as beneficiaries (low directionality): the constraint subsidizes their authority and historical role. Cryptocurrency networks and early digital cash innovators are targets (high directionality): the constraint extracts legitimacy from them and redirects it to the institutional core. Monetary legal scholars sit near analytical neutrality but their institutional embeddedness biases them toward the beneficiary pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve a genuine coordination problem â how to maintain legal certainty and regulatory jurisdiction as accounting dematerialized â making it a scaffold or rope at origin. However, it has accumulated extractive function: it now persists to gatekeep monetary innovation and exclude functional non-institutional money forms. The founding problem (legal certainty in dematerialized accounting) is contested as to whether it remains live; meanwhile the constraint's steady-state operation increasingly serves institutional authority rather than coordination. This prevents mislabeling it as pure rope (the coordination is real but layered with extraction) or pure snare (the extraction rides on a genuine historical coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_versus_operational_moneyhood,
    'Does the legal-institutional threshold capture a genuine ontological distinction, or does it merely formalize a power relation that privileges incumbent custodians?',
    'Comparative historical analysis of jurisdictions that recognize non-institutional digital assets as legal tender or payment instruments; if functional equivalence precedes legal recognition, the threshold is power-laden rather than ontologically necessary.',
    'If operational moneyhood is sufficient, the constraint''s extractiveness is higher than its coordination value and the reading drifts toward snare; if institutional custody is genuinely necessary for the money-function in complex economies, the tangled-rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_versus_operational_moneyhood, conceptual, 'Whether the institutional threshold is ontological or power-laden').

omega_variable(
    first_holder_empirical_identification,
    'Can the ''first institutional bearer'' be identified as a determinate historical fact, or is the threshold a retrospective legal fiction constructed after the fact?',
    'Archival and documentary research tracing the first regulatory recognition of dematerialized currency holdings; discovery of ambiguous or multiple candidate institutions would undermine the discrete-event premise.',
    'If the first holder is indeterminate, the constraint''s coordination function (shared threshold) weakens and its theater_ratio rises, pushing toward piton; if determinate, the coordination claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_holder_empirical_identification, empirical, 'Whether the first institutional bearer is a determinate historical fact or retrospective fiction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emergence_fh_tr_t0, electronic_money_emergence__first_held_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(emergence_fh_tr_t8, electronic_money_emergence__first_held_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(emergence_fh_tr_t16, electronic_money_emergence__first_held_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(emergence_fh_tr_t24, electronic_money_emergence__first_held_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(emergence_fh_tr_t32, electronic_money_emergence__first_held_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(emergence_fh_tr_t40, electronic_money_emergence__first_held_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(emergence_fh_be_t0, electronic_money_emergence__first_held_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(emergence_fh_be_t8, electronic_money_emergence__first_held_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(emergence_fh_be_t16, electronic_money_emergence__first_held_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(emergence_fh_be_t24, electronic_money_emergence__first_held_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(emergence_fh_be_t32, electronic_money_emergence__first_held_reading, base_extractiveness, 32, 0.48).
narrative_ontology:measurement(emergence_fh_be_t40, electronic_money_emergence__first_held_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(emergence_fh_su_t0, electronic_money_emergence__first_held_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(emergence_fh_su_t8, electronic_money_emergence__first_held_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(emergence_fh_su_t16, electronic_money_emergence__first_held_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(emergence_fh_su_t24, electronic_money_emergence__first_held_reading, suppression_requirement, 24, 0.46).
narrative_ontology:measurement(emergence_fh_su_t32, electronic_money_emergence__first_held_reading, suppression_requirement, 32, 0.54).
narrative_ontology:measurement(emergence_fh_su_t40, electronic_money_emergence__first_held_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% The electronic_money_emergence kernel decomposes into three structurally distinct constraints because the natural-language label 'when did digital money emerge?' conflates an intellectual-history threshold (thinkability), a legal-institutional threshold (first holding), and a statistical-constructivist threshold (M4/M5 category collapse). Each reading has a different epsilon, beneficiary structure, and ontological commitment. This story models the legal-institutional reading; the siblings model the conceptual and statistical readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
