% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence Boundary (Infrastructure Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the emergence of digital money from the
 *   perspective of the underlying financial infrastructure. It posits that
 *   digital money became a reality when the technical systems (like ATMs,
 *   ACH, and SWIFT) allowed banks to move value electronically, even if
 *   end-users didn't directly 'hold' digital currency. This reading
 *   emphasizes the functional capability of the financial system as the
 *   defining characteristic of digital money's arrival. It is one reading of
 *   the 'digital_money_emergence_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.15).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.05).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, mountain).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence Boundary (Infrastructure Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '9f3e2502-a775-4dc3-940f-146234a7c7f7').
narrative_ontology:cs_kernel_codification('9f3e2502-a775-4dc3-940f-146234a7c7f7', formalized).
narrative_ontology:cs_authority_grounding('9f3e2502-a775-4dc3-940f-146234a7c7f7', expertise).
narrative_ontology:cs_interpretation_layer_present('9f3e2502-a775-4dc3-940f-146234a7c7f7').
narrative_ontology:cs_reading_relation('9f3e2502-a775-4dc3-940f-146234a7c7f7', digital_money_emergence_boundary__conceptualization_reading, influences).
narrative_ontology:cs_reading_relation('9f3e2502-a775-4dc3-940f-146234a7c7f7', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('9f3e2502-a775-4dc3-940f-146234a7c7f7', foundational, money_is_what_banks_can_move).
narrative_ontology:cs_axiom_status(money_is_what_banks_can_move, holdable).
narrative_ontology:cs_axiom_grounding('9f3e2502-a775-4dc3-940f-146234a7c7f7', money_is_what_banks_can_move, conventional).
narrative_ontology:cs_axiom('9f3e2502-a775-4dc3-940f-146234a7c7f7', foundational, electronic_transfer_is_digital_movement).
narrative_ontology:cs_axiom_status(electronic_transfer_is_digital_movement, holdable).
narrative_ontology:cs_axiom_grounding('9f3e2502-a775-4dc3-940f-146234a7c7f7', electronic_transfer_is_digital_movement, empirically_contingent).
narrative_ontology:cs_reference_frame('9f3e2502-a775-4dc3-940f-146234a7c7f7', interbank_electronic_settlement).
narrative_ontology:cs_drift_state('9f3e2502-a775-4dc3-940f-146234a7c7f7', contemporary_crypto_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('9f3e2502-a775-4dc3-940f-146234a7c7f7', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities (e.g., SWIFT, ACH operators) provide the rails for electronic money movement between banks. Their existence and growth are directly tied to the emergence of digital money at this infrastructural level. They benefit from the increased volume and necessity of electronic transfers.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Central banks define and regulate the monetary system. The emergence of digital money at the infrastructural level forces them to adapt their definitions of money supply (e.g., M4/M5 collapse) and consider new regulatory frameworks. They are the ultimate arbiters of what counts as 'money'.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter,
    institutional, generational, analytical, national).

% Commercial banks are the primary users of the electronic transfer infrastructure. They pay fees to infrastructure providers but also benefit immensely from the efficiency and reach of digital money, allowing them to offer new services and reduce operational costs. Their deposits become the core of digital money at this stage.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, beneficiary).

% Academics and researchers who study the evolution of financial systems. They analyze the historical records of technological adoption and policy changes to define the 'moment' of digital money's emergence. Their work is to interpret and classify these historical shifts.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, financial_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common understanding and capability for interbank electronic transfer, allowing financial institutions to move value efficiently and reliably across distances, forming the backbone of modern financial systems.
% TRANSFER_FUNCTION: Facilitates the electronic transfer of monetary value between financial institutions, moving 'money' from one bank's ledger to another's, enabling the settlement of transactions without physical cash.
% ABSENT_VOICES: Early proponents of purely conceptual digital money (e.g., cryptographers like David Chaum) might argue that true digital money requires cryptographic unlinkability and user control, which this infrastructural stage does not fully provide. Their vision of digital money is not yet realized or fully integrated into this system.
% DISAPPEARANCE_RATIONALE: If the infrastructure for electronic transfer (ATMs, ACH, SWIFT) had never emerged, the global financial system would be fundamentally different, relying on physical settlement or much slower, less efficient methods. The modern economy, with its rapid transactions and global reach, would not exist as we know it.
% FOUNDING_PROBLEM: The problem of efficiently and securely transferring monetary value between geographically dispersed financial institutions, beyond the limitations of physical cash or paper-based instruments.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and financial institutions universally attest that the problem of efficient and secure interbank transfer remains live, constantly requiring upgrades and new protocols to meet evolving demands for speed, security, and global reach. This is corroborated by ongoing investments in payment system modernization and the continuous evolution of standards like ISO 20022.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_emergence_boundary__infrastructure_reading),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because the emergence of this infrastructure was a natural progression of technological capability, fundamentally altering the landscape of what was possible in finance. Its extractiveness is low (0.15) because the infrastructure primarily enables coordination and efficiency, with costs largely reflecting operational overhead rather than rent-seeking. Suppression is minimal (0.05) as the shift was driven by technological advancement and market demand, not coercion. The 'beneficiaries' (infrastructure providers) are a consequence of this emergence, not its cause, and their benefits are largely from providing a necessary service.
 *
 * PERSPECTIVAL GAP:
 *   Different readings of the 'digital_money_emergence_boundary' kernel will emphasize different criteria (conceptualization, consumer holdings, or infrastructure), leading to different 'emergence' dates and different classifications of the underlying constraints. This infrastructure reading focuses on the functional capacity of the financial system, which is a relatively objective, 'mountain-like' development, whereas other readings might highlight more contested or constructed aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   Banking infrastructure providers are beneficiaries as their services become essential. Central banks act as agenda-setters, adapting definitions and regulations to the new reality. Commercial banks are both payers (for infrastructure services) and beneficiaries (from increased efficiency). Financial historians are observers, analyzing the structural shift.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_emergence,
    'Is the emergence of digital money infrastructure a ''natural'' technological progression (Mountain) or a ''constructed'' outcome of policy and institutional choices (Rope/Tangled Rope)?',
    'Comparative historical analysis of other technological adoptions in regulated industries, examining the interplay of innovation, market forces, and regulatory intervention. If similar infrastructure emerged in less regulated contexts, it supports natural progression; if it required specific policy mandates, it supports construction.',
    'If more constructed, the extractiveness and suppression metrics might be higher, and the classification could shift towards a Rope or Tangled Rope, reflecting the role of active enforcement and benefit capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_emergence, conceptual, 'Ambiguity regarding the naturalness of infrastructural development versus its policy-driven construction.').

omega_variable(
    m4_m5_collapse_causality,
    'To what extent did the emergence of electronic transfer infrastructure directly cause the ''collapse'' or redefinition of broader money supply aggregates like M4/M5, versus merely reflecting pre-existing conceptual shifts?',
    'Econometric analysis correlating the adoption rates of electronic transfer systems with changes in monetary aggregate definitions and usage patterns, controlling for other economic and conceptual factors.',
    'Strong causal linkage would reinforce this reading''s claim of a structural boundary shift. Weak linkage would suggest the infrastructural development was a symptom rather than a primary driver of the conceptual redefinition, potentially shifting the ''true'' emergence boundary to the conceptualization reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m4_m5_collapse_causality, empirical, 'Causal relationship between infrastructure emergence and changes in monetary definitions.').

omega_variable(
    kernel_reading_focus,
    'Is this ''infrastructure_reading'' the most appropriate lens for defining the emergence of digital money, or do the ''conceptualization_reading'' or ''consumer_holdings_reading'' offer a more fundamental boundary?',
    'This is a conceptual omega. Resolution depends on the analytical goals of the observer: if the focus is on functional capacity of the financial system, this reading is primary. If on theoretical possibility or end-user experience, other readings are more salient. No single empirical test resolves this; it''s a framing choice.',
    'Adopting a different reading would instantiate a different constraint with potentially different metrics and classification, reflecting a shift in the defining criteria for ''digital money''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_focus, conceptual, 'Under-determination of the ''true'' emergence boundary across different readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 1977).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.0).
narrative_ontology:measurement(digi_tr_t1972, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1972, 0.0).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.0).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.1).
narrative_ontology:measurement(digi_be_t1972, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1972, 0.13).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.03).
narrative_ontology:measurement(digi_su_t1972, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1972, 0.04).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
