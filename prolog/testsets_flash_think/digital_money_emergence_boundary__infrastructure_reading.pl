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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   This constraint represents the 'infrastructure reading' of the emergence
 *   of digital money, positing that digital money became a reality when the
 *   underlying electronic transfer infrastructure (like ATMs, ACH, SWIFT)
 *   enabled banks to move funds electronically. This reading emphasizes the
 *   operational capabilities of the financial system over theoretical
 *   conceptualizations or direct consumer access. It defines a historical
 *   boundary that centers the role of banking infrastructure providers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.6).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.5).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence Boundary (Infrastructure Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'd41e2e4e-da95-41a8-86a7-dabce50419b1').
narrative_ontology:cs_kernel_codification('d41e2e4e-da95-41a8-86a7-dabce50419b1', formalized).
narrative_ontology:cs_authority_grounding('d41e2e4e-da95-41a8-86a7-dabce50419b1', practice).
narrative_ontology:cs_interpretation_layer_present('d41e2e4e-da95-41a8-86a7-dabce50419b1').
narrative_ontology:cs_reading_relation('d41e2e4e-da95-41a8-86a7-dabce50419b1', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('d41e2e4e-da95-41a8-86a7-dabce50419b1', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('d41e2e4e-da95-41a8-86a7-dabce50419b1', foundational, digital_money_is_interbank_liability).
narrative_ontology:cs_axiom_status(digital_money_is_interbank_liability, holdable).
narrative_ontology:cs_axiom_grounding('d41e2e4e-da95-41a8-86a7-dabce50419b1', digital_money_is_interbank_liability, conventional).
narrative_ontology:cs_axiom('d41e2e4e-da95-41a8-86a7-dabce50419b1', foundational, infrastructure_enables_monetary_function).
narrative_ontology:cs_axiom_status(infrastructure_enables_monetary_function, holdable).
narrative_ontology:cs_axiom_grounding('d41e2e4e-da95-41a8-86a7-dabce50419b1', infrastructure_enables_monetary_function, empirically_contingent).
narrative_ontology:cs_reference_frame('d41e2e4e-da95-41a8-86a7-dabce50419b1', interbank_electronic_transfer_standard).
narrative_ontology:cs_drift_state('d41e2e4e-da95-41a8-86a7-dabce50419b1', contemporary_digital_asset_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d41e2e4e-da95-41a8-86a7-dabce50419b1', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, central_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, early_digital_money_theorists).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, consumers).
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

% These entities (e.g., SWIFT, ACH operators) built and maintain the electronic transfer networks. They define the operational boundaries of digital money by controlling the rails through which it moves, collecting fees and asserting their definition of 'digital money' as interbank liabilities.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the clarity and control offered by a definition of digital money centered on interbank transfers. This framework facilitates monetary policy, oversight, and financial stability, even if they don't directly operate the infrastructure.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, beneficiary,
    institutional, generational, analytical, national).

% Are required to use these infrastructures for electronic transfers, incurring fees. However, they also benefit immensely from the efficiency and standardization these systems provide, enabling them to offer digital services to customers.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, beneficiary).

% Their conceptualizations of digital money, often predating or diverging from the infrastructure-centric view (e.g., focusing on cryptographic protocols or consumer-held digital cash), are sidelined by this dominant definition of emergence. They are not part of the operational definition-setting.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, early_digital_money_theorists, excluded,
    analytical, biographical, analytical, global).

% Initially, consumers could not directly hold or transact with this 'digital money' (interbank liabilities), experiencing it only indirectly through bank accounts. They bear indirect costs through bank fees that incorporate infrastructure costs, and their direct access to digital instruments came much later than this emergence point.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, consumers, payer,
    powerless, biographical, constrained, local).

% Observe and interpret the evolution of digital money, often influenced by the operational definitions set by infrastructure. They can propose new regulations but operate within the framework established by existing financial systems.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, financial_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, standardized operational boundary for what constitutes 'digital money' within the formal financial system, enabling efficient and secure interbank electronic transfers across diverse institutions and geographies.
% TRANSFER_FUNCTION: Transfers definitional authority and economic value (via transaction fees and control over financial flows) to the operators of the electronic transfer infrastructure, from those whose alternative definitions are sidelined or who must use the infrastructure.
% ABSENT_VOICES: Early digital money theorists who focused on conceptual breakthroughs or consumer-centric digital instruments are largely absent from the historical narrative shaped by this reading. They would argue for a broader or different definition of emergence, emphasizing theoretical possibility or direct user agency.
% DISAPPEARANCE_RATIONALE: If this infrastructure-centric definition of digital money's emergence vanished, the historical narrative of financial innovation would be fundamentally rewritten. The perceived 'start date' and foundational nature of digital money would shift, altering economic analyses, regulatory frameworks, and the understanding of how money evolved beyond physical forms. The role of banking infrastructure in shaping the very concept of digital money would be diminished.
% FOUNDING_PROBLEM: The increasing need for efficient, standardized, and secure interbank electronic transfer to support a growing global economy, moving beyond the limitations of physical cash and paper checks for large-scale transactions.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and financial institutions continue to invest heavily in maintaining and upgrading these infrastructures, corroborating the ongoing need for efficient interbank transfer. Historians of technology and finance also document the critical role of these systems in the evolution of modern finance, providing external validation for the problem's persistence.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.6) reflects the value captured by infrastructure providers who control the essential rails for electronic transfers, and the way this definition prioritizes their role. Suppression (0.5) arises from the definitional power that sidelines alternative views of digital money's emergence. Theater ratio is low (0.1) as this is a historical claim about a functional development, not a performative one. Accessibility collapse (0.4) is moderate, as other conceptualizations of digital money's emergence still exist, but this one gained significant traction in financial history. Resistance (0.5) comes from proponents of alternative emergence narratives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of banking infrastructure providers and central banks, this reading accurately captures the moment digital money became a practical reality within the formal financial system. However, from the perspective of early theorists or consumers, this definition might be seen as too narrow, overlooking earlier conceptual breakthroughs or the later development of direct consumer-facing digital instruments.
 *
 * DIRECTIONALITY LOGIC:
 *   Banking infrastructure providers are the agenda setters and primary beneficiaries, as this reading validates their historical role and the value of their systems. Central banks also benefit from the clarity and control this framework provides. Early digital money theorists and consumers are victims, as their perspectives on digital money's emergence (conceptual or direct-holding) are not centered by this definition, and consumers initially lacked direct access to this form of 'digital money'.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitional_scope_ambiguity,
    'Is the emergence of ''digital money'' best defined by the operational capabilities of interbank infrastructure, or by earlier theoretical conceptualizations or later direct consumer access?',
    'Historical and economic analysis that weighs the impact of each proposed emergence point on the broader financial system and public understanding of money. Resolution would involve a consensus on the most impactful ''first'' moment.',
    'If resolved towards conceptualization, this constraint''s extractiveness might be re-evaluated as lower, as the ''value'' of the infrastructure would be seen as a consequence, not the definition itself. If resolved towards consumer holdings, this reading would be seen as an intermediate step, not the ultimate emergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_scope_ambiguity, conceptual, 'Ambiguity in the defining moment of digital money''s emergence.').

omega_variable(
    infrastructure_control_vs_innovation,
    'Does defining digital money''s emergence by existing infrastructure inadvertently constrain future financial innovation by privileging established players and technologies?',
    'Longitudinal study of financial innovation trajectories in jurisdictions with different historical narratives of digital money''s origins. If jurisdictions emphasizing conceptual or consumer-centric origins show faster or more diverse innovation, it suggests a constraining effect.',
    'If a constraining effect is found, the ''suppression'' metric for this reading might be re-evaluated as higher, reflecting the subtle suppression of alternative innovation paths by a dominant historical narrative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(infrastructure_control_vs_innovation, empirical, 'Impact of historical definition on future innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1960, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(digi_tr_t1965, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(digi_tr_t1970, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(digi_tr_t1975, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(digi_tr_t1980, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1980, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(digi_be_t1965, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(digi_be_t1970, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(digi_be_t1975, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(digi_be_t1980, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1980, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(digi_su_t1965, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(digi_su_t1970, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement(digi_su_t1975, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(digi_su_t1980, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1980, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'digital_money_emergence_boundary' kernel, each focusing on a different aspect of digital money's historical origin. This reading emphasizes the role of interbank electronic transfer infrastructure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
