% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Infrastructure Boundary (1967–1977)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the infrastructure-boundary reading of
 *   digital money emergence: money became digital (and subject to
 *   infrastructure operator governance) when banks could electronically
 *   transfer funds between institutions, beginning with ATM networks (1967),
 *   automated clearing houses (1972), and international wire standards
 *   (SWIFT, 1977). This reading differs fundamentally from sibling readings
 *   that locate the boundary at theoretical conceptualization (1960s) or
 *   consumer-facing digital holdings (1990s+). The infrastructure boundary is
 *   a middle position: money is already digital in this reading even if
 *   consumers cannot directly hold or control digital instruments — the
 *   digitality lies in the transfer capability, not in consumer access. This
 *   reading's core claim is that digital money's emergence is grounded in the
 *   infrastructure operators' technical capability and regulatory authority
 *   to define what counts as transferable money, not in consumer capability
 *   or theoretical formalization.
 *
 * KEY AGENTS:
 *   - Banking infrastructure operators (ACH, SWIFT, ATM networks): agenda-setters controlling the technical definition and access rules
 *   - Commercial and central banks: beneficiaries gaining settlement capabilities; central banks retain coercive control via access gatekeeping
 *   - Non-bank payment actors (excluded): fintech, private money, alternative networks barred from direct infrastructure access
 *   - Consumer account holders (powerless payers): no direct access to infrastructure; subject to fees and bank routing decisions
 *   - International traders (organized payers): dependent on SWIFT; face vendor lock-in and no routing alternatives
 *   - Monetary theorists (analytical observers): name and analyze the boundary claim itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.68).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.52).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Infrastructure Boundary (1967–1977)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '42f5831f-1492-4bf4-ac6a-35c2b32014dd').
narrative_ontology:cs_kernel_codification('42f5831f-1492-4bf4-ac6a-35c2b32014dd', fixed_text).
narrative_ontology:cs_authority_grounding('42f5831f-1492-4bf4-ac6a-35c2b32014dd', extraction).
narrative_ontology:cs_interpretation_layer_present('42f5831f-1492-4bf4-ac6a-35c2b32014dd').
narrative_ontology:cs_reading_relation('42f5831f-1492-4bf4-ac6a-35c2b32014dd', digital_money_emergence_boundary__conceptualization_reading, influences).
narrative_ontology:cs_reading_relation('42f5831f-1492-4bf4-ac6a-35c2b32014dd', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('42f5831f-1492-4bf4-ac6a-35c2b32014dd', foundational, infrastructure_capability_constitutes_digitality).
narrative_ontology:cs_axiom_status(infrastructure_capability_constitutes_digitality, holdable).
narrative_ontology:cs_axiom_grounding('42f5831f-1492-4bf4-ac6a-35c2b32014dd', infrastructure_capability_constitutes_digitality, conventional).
narrative_ontology:cs_axiom('42f5831f-1492-4bf4-ac6a-35c2b32014dd', secondary, bank_intermediation_is_regulatory_necessity).
narrative_ontology:cs_axiom_status(bank_intermediation_is_regulatory_necessity, holdable).
narrative_ontology:cs_axiom_grounding('42f5831f-1492-4bf4-ac6a-35c2b32014dd', bank_intermediation_is_regulatory_necessity, instrumental).
narrative_ontology:cs_reference_frame('42f5831f-1492-4bf4-ac6a-35c2b32014dd', bank_to_bank_electronic_clearing_standard).
narrative_ontology:cs_drift_state('42f5831f-1492-4bf4-ac6a-35c2b32014dd', contemporary_fintech_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('42f5831f-1492-4bf4-ac6a-35c2b32014dd', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, central_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, non_bank_payment_actors).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, consumer_account_holders).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, international_traders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the technical backbone: automated clearing houses (ACH), wire protocols (SWIFT), and ATM networks. Set technical standards for what counts as electronically transferable money, which jurisdictions and counterparties can connect, and fee structures for participation. Collect rents on every transaction routed through their infrastructure.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain the ability to settle obligations electronically without physical cash movement, reducing operational risk and enabling 24/7 implied-liquidity markets. Must pay infrastructure fees and comply with operator standards. Large banks have leverage to negotiate; smaller banks face take-it-or-leave-it pricing.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, payer).

% Benefit from the ability to control money supply and transmission through the electronic infrastructure layer. Can mandate who accesses settlement networks and on what terms. Gain insight into transaction flows and can implement policy at the clearing layer. Trade off: lose direct control if non-bank actors gain clearing access.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter).

% Fintech firms, private money operators, and alternative payment networks are structurally barred from direct access to the infrastructure. Must route through authorized banks (who extract a margin) or operate parallel systems that cannot achieve the settlement finality the infrastructure offers. Would compete on fees and speed if admitted.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, non_bank_payment_actors, excluded,
    moderate, biographical, constrained, global).

% Hold accounts in banks and initiate transfers. Cannot directly access the infrastructure; transfers clear through the bank's connection to the rail. Subject to fees imposed at each layer (bank fee, infrastructure operator fee). No choice of operator or routing protocol.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, consumer_account_holders, payer,
    powerless, biographical, constrained, local).

% Depend on SWIFT and similar rails for cross-border settlement. Must accept infrastructure operator's fees, delay structures, and technical standards. Cannot route around the infrastructure without returning to slower, riskier physical settlement. Face vendor lock-in: switching costs are high.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, international_traders, payer,
    organized, biographical, constrained, global).

% Analyze what constitutes digital money and how the emergence of electronic infrastructure reshapes monetary aggregates. This reading instantiates the infrastructure-boundary claim; they are the analytical seat naming it.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of multi-party settlement without physical presence: central clearing of mutual obligations, final confirmation of ownership transfer, synchronization across time zones and political boundaries. The infrastructure makes these synchronization points possible at scale and speed that cash settlement cannot achieve.
% TRANSFER_FUNCTION: Routes financial flows through a controlled infrastructure layer; transfers rents (fees, access control, data collection) from users and non-bank actors to the infrastructure operator and the banks that control access to it. Central banks retain coercive control via the ability to grant or deny access to settlement.
% ABSENT_VOICES: Non-bank payment innovators and alternative clearing systems would argue for open-access infrastructure and competitive fee structures; they are excluded by design. Private money proponents would argue that the infrastructure boundary is arbitrary and excludes legitimate monetary competitors; they are kept out by regulatory fiat tied to the infrastructure definition.
% DISAPPEARANCE_RATIONALE: If the electronic infrastructure and its exclusionary structure vanished, international settlement would return to correspondent banking and physical cash movement; transaction speeds would drop from minutes to days; cross-border trade finance would become substantially more expensive. The global financial system would reorganize around slower, bilateral settlement patterns or alternative non-bank rails.
% FOUNDING_PROBLEM: Physical cash settlement was slow, risky (theft, loss in transit), geographically constrained, and could not scale to the volume of international trade. Banks needed a synchronous, trustworthy way to clear obligations electronically without meeting face-to-face.
% FOUNDING_PROBLEM_CORROBORATION: All institutional seats (central banks, commercial banks, infrastructure operators) attest the founding problem persists. Independent financial historians and infrastructure analysts confirm that the speed and scale benefits are real and would be lost if the rails were removed. However, this corroboration does NOT address whether the infrastructure boundary is the only defensible definition of digital money — it only confirms that the technical coordination problem the infrastructure solves is genuine.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extraction rises from 0.35 (1967, pre-infrastructure) to 0.68 (1977, SWIFT operational) as the infrastructure consolidates and fee structures harden. Suppression remains moderate (0.52) because the infrastructure is technically necessary — suppression is not primarily coercive but structural (no alternatives exist until much later). Theater is low-to-moderate (0.28) because most activity is genuinely functional clearing; however, by 1977 a growing share of suppression activity defends access exclusivity rather than clearing per se. The measurements use a shared time grid (six points, all metrics at each point) with mixed observed/projected basis: 1967–1970 are projected (pre-SWIFT, infrastructure being built); 1972 (ACH operational) and 1977 (SWIFT operational) are observed anchors; intermediate points interpolate. The rising trajectory reflects extraction accumulation as the infrastructure becomes the mandatory chokepoint for money transfer.
 *
 * PERSPECTIVAL GAP:
 *   The infrastructure operator and central bank seats see this as genuine coordination with real beneficiaries (faster settlement, global reach, systemic stability). Consumer and trader seats see it as enforced extraction: they have no choice of infrastructure, no ability to audit the operator's costs, and no exit except through slower alternatives. The analytical observer seat names the boundary claim itself — this reading's entire purpose is to instantiate the infrastructure-boundary definition as opposed to alternatives. The engine should compute divergent types across these seats precisely because they experience the same technical infrastructure through radically different structural relationships (one controls it, one depends on it with no exit).
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure operators are near d=0.0 (beneficiaries: they control rule-setting, face no exit pressure, collect rents). Central banks are near d=0.15 (moderate beneficiary: gain settlement capability but retain coercive control, so extraction is mediated through their own authority). Commercial banks are near d=0.35 (mixed: gain settlement speed, pay fees, but have leverage in negotiating terms; large banks trade off coordination benefit against extraction, small banks face higher extraction). Non-bank actors are d=1.0 (full target: structurally excluded, would compete if admitted, bearing the cost of slower/more expensive alternatives). Consumer account holders and traders are high-d targets (0.85–0.95) because they have trapped exit and no leverage. The override mechanism is not needed here: the structural derivation from beneficiary/victim + exit should produce these values cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is claimed as tangled_rope (genuine coordination + asymmetric extraction). The founding problem (fast multi-party settlement) is live and would re-emerge if the infrastructure vanished. However, mandatrophy risk exists: if the infrastructure became so efficient that it was no longer necessary to enforce exclusivity (if open-access alternative rails emerged with comparable speed and security), the enforcement flag could flip to false. At present (1977 snapshot), enforcement is still necessary because the technical barrier to entry for alternative infrastructure is very high. The theater_ratio is rising but still moderate, indicating the functional coordination component is real, not yet mostly performance. A piton diagnosis would require theater_ratio > 0.5 and beneficiary absence; neither holds here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_necessity_vs_exclusivity,
    'Is the infrastructure operator''s control over access exclusionary by technical necessity or by regulatory/contractual choice?',
    'Counterfactual analysis: could the same electronic settlement infrastructure be built with open-access routing (any actor, not just banks, can connect)? Historical examination of regulatory decisions that mandated banking intermediation versus technical decisions.',
    'If open-access is technically feasible but was chosen for rent extraction, the extraction component is higher and mandatrophy risk increases. If exclusivity is technically necessary for settlement finality and fraud prevention, more of the measured extraction is coordination cost. This resolves the core uncertainty about whether the constraint is genuine tangled_rope (coordination + extraction) or mostly snare (extraction wearing coordination cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_necessity_vs_exclusivity, empirical, 'Whether banking-intermediation exclusivity is technically required or policy-chosen.').

omega_variable(
    central_bank_rent_capture,
    'Do central banks benefit from infrastructure operator rent-seeking, or are they aligned with open-access and only allow operator monopoly for regulatory reach?',
    'Archival evidence from central bank policy discussions (1970s Federal Reserve, Bank of England, BIS) about SWIFT and ACH access; analysis of whether central banks lobbied for or against private-operator infrastructure versus public clearing systems.',
    'If central banks capture part of the operator rents (via regulatory capture or shared governance), the beneficiary set is more concentrated and mandatrophy risk is lower (more powerful actor defending status quo). If they are indifferent or would prefer open-access, the operator''s extraction is less defended and mandatrophy risk is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_bank_rent_capture, empirical, 'Whether central bank and operator interests are aligned or adversarial.').

omega_variable(
    kernel_boundary_underdetermination,
    'Does this reading''s boundary claim (infrastructure-capability as the defining moment) rest on objective technical criteria, or does it embed a conceptual choice about what counts as ''digital money''?',
    'Formalize each sibling reading''s boundary criterion (conceptualization, infrastructure, consumer access) and test whether they can be satisfied independently. If they can coexist in a single account of money''s evolution, the boundary is conceptually chosen; if they contradict (one reading''s money is not the other''s), they are genuinely different constraints.',
    'If the boundary is objective, this reading''s ε is stable across observers. If it is conceptually chosen, ε should carry an omega noting the choice; different readings would then be NOT alternate framings of one constraint but genuinely distinct constraints (per ε-invariance principle). This affects whether the corpus treats them as network-linked siblings or as separate constraints entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_boundary_underdetermination, conceptual, 'Whether the infrastructure-boundary definition is objective or embedded in a conceptual framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 1977).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement_basis(digi_tr_t1967, projected).
narrative_ontology:measurement(digi_tr_t1970, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1970, 0.16).
narrative_ontology:measurement_basis(digi_tr_t1970, projected).
narrative_ontology:measurement(digi_tr_t1972, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1972, 0.19).
narrative_ontology:measurement_basis(digi_tr_t1972, observed).
narrative_ontology:measurement(digi_tr_t1974, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1974, 0.24).
narrative_ontology:measurement_basis(digi_tr_t1974, observed).
narrative_ontology:measurement(digi_tr_t1975, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1975, 0.26).
narrative_ontology:measurement_basis(digi_tr_t1975, observed).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.28).
narrative_ontology:measurement_basis(digi_tr_t1977, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement_basis(digi_be_t1967, projected).
narrative_ontology:measurement(digi_be_t1970, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement_basis(digi_be_t1970, projected).
narrative_ontology:measurement(digi_be_t1972, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1972, 0.54).
narrative_ontology:measurement_basis(digi_be_t1972, observed).
narrative_ontology:measurement(digi_be_t1974, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1974, 0.61).
narrative_ontology:measurement_basis(digi_be_t1974, observed).
narrative_ontology:measurement(digi_be_t1975, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1975, 0.64).
narrative_ontology:measurement_basis(digi_be_t1975, observed).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.68).
narrative_ontology:measurement_basis(digi_be_t1977, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement_basis(digi_su_t1967, projected).
narrative_ontology:measurement(digi_su_t1970, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1970, 0.38).
narrative_ontology:measurement_basis(digi_su_t1970, projected).
narrative_ontology:measurement(digi_su_t1972, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1972, 0.45).
narrative_ontology:measurement_basis(digi_su_t1972, observed).
narrative_ontology:measurement(digi_su_t1974, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1974, 0.49).
narrative_ontology:measurement_basis(digi_su_t1974, observed).
narrative_ontology:measurement(digi_su_t1975, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement_basis(digi_su_t1975, observed).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.52).
narrative_ontology:measurement_basis(digi_su_t1977, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.22).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested digital_money_emergence_boundary kernel. The three readings (conceptualization, infrastructure, consumer_holdings) are separate constraints with different ε values, different beneficiary/victim structures, and different types. They are linked here because they all attempt to fix the boundary moment for what counts as digital money. The infrastructure_reading (this file) locates the boundary at bank-to-bank electronic transfer capability (1967–1977). The sibling readings push the boundary earlier (theoretical conceptualization) or much later (consumer access). Each reading is ε-invariant internally; they differ because they measure different observables (when the infrastructure works, when theory makes it thinkable, when consumers can hold it). The network edges record the genealogical relationship: understanding why different parties locate the boundary differently requires engaging all three readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
