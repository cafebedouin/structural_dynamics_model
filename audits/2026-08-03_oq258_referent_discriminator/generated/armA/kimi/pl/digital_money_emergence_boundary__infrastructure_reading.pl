% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence: Infrastructure Reading (Interbank Electronic Transfer Boundary)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint is the infrastructure_reading of the contested kernel
 *   digital_money_emergence_boundary. It posits that digital money emerged
 *   when banks gained the operational capability to transfer value
 *   electronically (1967 ATMs, 1972 ACH, 1977 SWIFT), irrespective of
 *   theoretical formalization or consumer direct access. This reading
 *   concentrates definitional authority and operational rents with banking
 *   infrastructure providers while rendering retail holders and non-bank
 *   innovators structurally invisible. The arrangement coordinates genuine
 *   interbank settlement but asymmetrically extracts fees and definitional
 *   control from member banks and excluded innovators.
 *
 * KEY AGENTS:
 *   - banking_infrastructure_providers (SWIFT, ACH operators): agenda_setter/beneficiary — control the rails and set the definitional boundary
 *   - member_banks: payer — depend on rails for interoperability, bear fees and compliance costs
 *   - non_bank_innovators: excluded — locked out of interbank infrastructure and the digital money category
 *   - monetary_economists: observer — analyze the boundary collapse in monetary aggregates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.62).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.48).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence: Infrastructure Reading (Interbank Electronic Transfer Boundary)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'b32b92f8-4f57-4996-8789-3c263477578e').
narrative_ontology:cs_kernel_codification('b32b92f8-4f57-4996-8789-3c263477578e', implicit).
narrative_ontology:cs_authority_grounding('b32b92f8-4f57-4996-8789-3c263477578e', practice).
narrative_ontology:cs_reading_relation('b32b92f8-4f57-4996-8789-3c263477578e', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('b32b92f8-4f57-4996-8789-3c263477578e', digital_money_emergence_boundary__consumer_holdings_reading, forecloses).
narrative_ontology:cs_axiom('b32b92f8-4f57-4996-8789-3c263477578e', foundational, operational_capability_defines_digital_money).
narrative_ontology:cs_axiom_status(operational_capability_defines_digital_money, holdable).
narrative_ontology:cs_axiom_grounding('b32b92f8-4f57-4996-8789-3c263477578e', operational_capability_defines_digital_money, empirically_contingent).
narrative_ontology:cs_axiom('b32b92f8-4f57-4996-8789-3c263477578e', foundational, interbank_ledger_supersedes_retail_holdings).
narrative_ontology:cs_axiom_status(interbank_ledger_supersedes_retail_holdings, holdable).
narrative_ontology:cs_axiom_grounding('b32b92f8-4f57-4996-8789-3c263477578e', interbank_ledger_supersedes_retail_holdings, conventional).
narrative_ontology:cs_reference_frame('b32b92f8-4f57-4996-8789-3c263477578e', operational_interbank_settlement_framework).
narrative_ontology:cs_drift_state('b32b92f8-4f57-4996-8789-3c263477578e', post_retail_digital_money_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b32b92f8-4f57-4996-8789-3c263477578e', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, member_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, non_bank_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the proprietary messaging and settlement networks (SWIFT, national ACH systems, early ATM switches) that enable interbank electronic transfers. Set membership rules, technical protocols, message formats, and fee schedules. Collect transaction and membership fees from participating banks. Control the operational boundary of what counts as digital money by owning and administering the rails.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers, agenda_setter,
    institutional, generational, arbitrage, global).

% Rely on SWIFT and ACH networks to settle payments, clear funds, and maintain correspondent relationships. Pay network fees and comply with technical and reporting standards. Cannot unilaterally exit without losing interoperability with the global banking system. Bear the cost of infrastructure rents embedded in the prevailing definition of digital money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, member_banks, payer,
    powerful, biographical, constrained, global).

% Develop alternative payment technologies, e-money prototypes, and retail-facing digital instruments. Are excluded from interbank infrastructure membership and from the 'digital money' category as defined by this reading, which requires bank-operated electronic transfer rails. Their products are not classified as digital money because they lack access to the core banking networks.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, non_bank_innovators, excluded,
    moderate, biographical, trapped, national).

% Analyze monetary aggregates and debate the M4/M5 boundary collapse triggered by the proliferation of electronic bank deposits. Observe that the infrastructure reading privileges bank-centric, operational definitions of digital money over retail-accessible or theoretically-grounded alternatives.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables reliable, standardized electronic interbank transfer and settlement, replacing physical correspondent banking, paper checks, and telex messages with automated clearing and messaging protocols.
% TRANSFER_FUNCTION: Moves definitional authority over the boundary of 'digital money' to infrastructure operators; moves fees and compliance costs from member banks to network operators in exchange for access to the electronic rails.
% ABSENT_VOICES: Retail consumer advocates, non-bank payment innovators, and proponents of the consumer-holdings reading are structurally excluded from the definitional conversation; they would argue that digital money requires direct public accessibility, not merely interbank operational capability.
% DISAPPEARANCE_RATIONALE: If the interbank electronic transfer infrastructure and its definitional authority vanished overnight, correspondent banking would revert to slower physical and telegraphic methods, monetary aggregates would require reclassification, and the conceptual boundary between 'digital' and 'physical' money would shift to alternative thresholds.
% FOUNDING_PROBLEM: Physical correspondent banking and paper-based settlement were slow, error-prone, illiquid, and costly; international and even domestic clearing required multi-day delays and physical document transport.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and financial historians attest to the pre-digital settlement problem. Independent economists and fintech historians attest that the founding problem has been solved and the infrastructure arrangement now persists as a rent-bearing definitional gate; corroboration exists from outside the benefiting party set.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the combination of network fees and definitional gatekeeping power exercised by infrastructure operators. Suppression (0.48) captures the marginalization of alternative payment rails and non-bank definitions through network effects and standardization. Theater ratio (0.22) is low because the coordination function (interbank settlement) is heavily utilized and genuinely functional, though a modest share of narrative activity defends the definitional boundary against retail challengers. Accessibility collapse (0.58) reflects the dominance of SWIFT and ACH as de facto standards. Resistance (0.35) comes from bank complaints about fees and early fintech attempts to bypass the rails.
 *
 * PERSPECTIVAL GAP:
 *   Infrastructure providers experience the constraint as necessary coordination they built and maintain; member banks experience it as a costly but unavoidable toll on participation in the payment system; non-bank innovators experience it as an exclusionary wall that denies them definitional recognition. The engine should compute divergent seat types from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Banking infrastructure providers are the structural beneficiaries (low d, subsidized by the constraint's rent extraction). Member banks and non-bank innovators are structural targets (high d, paying the constraint's costs in fees and exclusion). Monetary economists sit at the analytical pole with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — slow, physical, error-prone settlement — was genuinely solved by electronic infrastructure. However, the arrangement has outlived the acute phase of that problem and now functions partly as a definitional gate that accumulates extraction. The founding problem status is contested, which prevents simple mandatrophy resolution and supports the tangled_rope classification over scaffold or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_naturalness,
    'Is the infrastructure-reading boundary a natural feature of monetary evolution or a constructed narrative that privileges incumbent infrastructure operators?',
    'Comparative historical analysis of alternative monetary systems where retail access preceded interbank infrastructure, or where interbank networks failed to achieve definitional dominance.',
    'If constructed, the reading is a tangled rope or snare serving infrastructure incumbents; if natural, it approaches a scaffold or rope for interbank coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_naturalness, conceptual, 'Natural vs constructed boundary ambiguity').

omega_variable(
    retail_exclusion_mechanism,
    'Does the infrastructure reading structurally exclude retail digital money by definitional fiat, or merely describe a sequential historical phase?',
    'Analysis of whether the reading is used to gate regulatory treatment, monetary aggregation, and access to central bank facilities for non-bank payment providers.',
    'If used for gating, the exclusion is an active suppression mechanism raising suppression metrics; if merely descriptive, it is a neutral historical boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_exclusion_mechanism, empirical, 'Whether the reading actively suppresses retail alternatives').

omega_variable(
    committer_relation_ambiguity,
    'Does the infrastructure reading genuinely foreclose the consumer_holdings reading, or do they merely describe different scope conditions (wholesale vs retail digital money)?',
    'Examination of whether monetary scholars treat these as competing answers to one question or as answers to distinct sub-questions.',
    'If scope-differentiated, relation should be coexists_with; if mutually exclusive, forecloses is correct and the kernel is more sharply contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_relation_ambiguity, conceptual, 'Ambiguity in structural relation to sibling reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(digi_tr_t2, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement(digi_tr_t5, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(digi_tr_t7, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 7, 0.18).
narrative_ontology:measurement(digi_tr_t10, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 10, 0.22).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(digi_be_t2, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(digi_be_t5, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(digi_be_t7, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 7, 0.55).
narrative_ontology:measurement(digi_be_t10, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(digi_su_t2, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2, 0.3).
narrative_ontology:measurement(digi_su_t5, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(digi_su_t7, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 7, 0.45).
narrative_ontology:measurement(digi_su_t10, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
