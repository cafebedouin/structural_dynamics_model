% ============================================================================
% CONSTRAINT STORY: fda_single_trial_drug_approval
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fda_single_trial_drug_approval, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fda_single_trial_drug_approval
 *   human_readable: FDA Proposal for Single-Trial Drug Approvals
 *   domain: regulatory/healthcare
 *
 * SUMMARY:
 *   The FDA has proposed a rule change that would no longer strictly require
 *   two large, controlled clinical trials for new drug approvals, allowing
 *   one to suffice in some cases. The stated purpose is to accelerate the
 *   delivery of new medicines, particularly for rare and serious diseases
 *   where patient recruitment is challenging. However, this policy
 *   structurally transfers risk: the financial risk of a failed second trial
 *   is removed from the manufacturer, while the health risk of a potentially
 *   ineffective or unsafe drug is transferred to patients and the broader
 *   healthcare system.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — Save hundreds of millions in R&D costs and achieve faster market access and revenue.
 *   - Patients: Primary victim (powerless/trapped) — Bear the ultimate risk of receiving treatments approved on a weaker evidence base.
 *   - FDA: Institutional regulator (institutional/constrained) — Balances dual pressures of ensuring public safety and facilitating medical innovation.
 *   - Public Citizen & other watchdogs: Analytical observers (organized/analytical) — Argue the change dangerously weakens consumer protection.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fda_single_trial_drug_approval, 0.68).
domain_priors:suppression_score(fda_single_trial_drug_approval, 0.75).
domain_priors:theater_ratio(fda_single_trial_drug_approval, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fda_single_trial_drug_approval, extractiveness, 0.68).
narrative_ontology:constraint_metric(fda_single_trial_drug_approval, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fda_single_trial_drug_approval, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fda_single_trial_drug_approval, tangled_rope).
narrative_ontology:human_readable(fda_single_trial_drug_approval, "FDA Proposal for Single-Trial Drug Approvals").
narrative_ontology:topic_domain(fda_single_trial_drug_approval, "regulatory/healthcare").

domain_priors:requires_active_enforcement(fda_single_trial_drug_approval).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fda_single_trial_drug_approval, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(fda_single_trial_drug_approval, biotech_investors).
narrative_ontology:constraint_victim(fda_single_trial_drug_approval, patients_receiving_drugs).
narrative_ontology:constraint_victim(fda_single_trial_drug_approval, public_and_private_insurers).
narrative_ontology:constraint_victim(fda_single_trial_drug_approval, prescribing_physicians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT (SNARE) — For an individual patient, especially one with a serious illness, the regulatory system is a black box they are trapped within. Lowering the evidence standard transfers the unquantified risk of an ineffective or harmful drug directly onto them. The potential for faster access is offset by the risk of receiving a treatment approved on weaker data. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.97.
constraint_indexing:constraint_classification(fda_single_trial_drug_approval, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PHARMACEUTICAL MANUFACTURER (ROPE) — From the manufacturer's viewpoint, this rule change removes a costly, time-consuming barrier (the second trial), representing a pure coordination win that accelerates revenue. The financial benefit is immense and direct. They can arbitrage this streamlined process against stricter regulatory regimes. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09.
constraint_indexing:constraint_classification(fda_single_trial_drug_approval, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: FDA (TANGLED ROPE) — The FDA is caught between its mandate to ensure drug safety/efficacy (coordination) and pressure from industry and patient advocacy groups for faster approvals (which enables extraction). As the sole national regulator, it cannot exit this role. The policy attempts to serve both goals, embodying the hybrid nature of a Tangled Rope.
constraint_indexing:constraint_classification(fda_single_trial_drug_approval, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL (TANGLED ROPE) — This is the system's objective classification. The policy has a genuine coordination function (speeding access to potentially life-saving drugs) but is structured to facilitate massive, asymmetric extraction by shifting the financial and health risks of uncertainty from producers to consumers. The high base extraction (ε=0.68) and suppression (0.75) confirm its hybrid nature.
constraint_indexing:constraint_classification(fda_single_trial_drug_approval, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fda_single_trial_drug_approval_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fda_single_trial_drug_approval, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fda_single_trial_drug_approval, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fda_single_trial_drug_approval, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fda_single_trial_drug_approval_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68) is high because the value of the transferred risk (avoided trial costs for pharma, potential harm/inefficacy for patients) is substantial. Suppression (0.75) is high because patients have no alternative to the regulated drug market and must trust the FDA's seal of approval; they cannot opt into a 'two-trial-only' system.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The manufacturer sees the removal of a 'redundant' hurdle, a pure coordination improvement (Rope). The patient, who relies on that hurdle for safety, experiences the change as a hidden danger they are trapped by (Snare). The FDA, caught between these interests, operates within a Tangled Rope, attempting to achieve a public good (access) through a mechanism that creates private, extractive opportunities. The analytical view aligns with the FDA's structural position, identifying the constraint as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the structural relationships. Beneficiaries (pharma) have arbitrage exit and reap huge financial gains, giving them a very low, even negative, effective extraction (χ). Victims (patients) are trapped and bear the costs, resulting in a very high derived directionality (d) and a high effective extraction, classifying their experience as a Snare. This gap is the core of the political conflict over the rule.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a canonical example of potential mandatrophy. The policy is presented as a coordination mechanism (Scaffold/Rope) to 'help patients' by accelerating access. However, its primary structural effect is extractive, shifting risk and cost away from corporations. The Tangled Rope classification correctly identifies this duality, preventing the system from misinterpreting a highly extractive policy as a purely benevolent act of coordination. The lack of a sunset clause further invalidates the 'Scaffold' claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_tradeoff,
    'Does the net public health benefit from faster access to some effective drugs outweigh the societal cost of approving more ineffective or harmful ones?',
    'Long-term, post-market surveillance data comparing health outcomes and total cost of care for drugs approved under single-trial vs. two-trial standards.',
    'If the net benefit is negative, the constraint''s coordination function is illusory, making it a pure Snare from a societal viewpoint. If positive, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_tradeoff, empirical, 'Net public health benefit of faster access versus lower evidence standards.').

omega_variable(
    regulatory_capture,
    'To what extent is this rule change a product of regulatory capture by the pharmaceutical industry versus a good-faith effort to solve access problems for rare diseases?',
    'Analysis of industry lobbying expenditures, the ''revolving door'' between the FDA and pharmaceutical companies, and internal agency documents detailing the justification for the rule change.',
    'If evidence points strongly to capture, the claimed ''coordination'' function is purely theatrical, and the constraint is functionally a Snare designed to subsidize industry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture, conceptual, 'Whether the rule change stems from public interest or industry capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fda_single_trial_drug_approval, 2025, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fda__tr_t0, fda_single_trial_drug_approval, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fda__tr_t5, fda_single_trial_drug_approval, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fda__tr_t10, fda_single_trial_drug_approval, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(fda__be_t0, fda_single_trial_drug_approval, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(fda__be_t5, fda_single_trial_drug_approval, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(fda__be_t10, fda_single_trial_drug_approval, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fda_single_trial_drug_approval, resource_allocation).
narrative_ontology:affects_constraint(fda_single_trial_drug_approval, drug_pricing_negotiation).
narrative_ontology:affects_constraint(fda_single_trial_drug_approval, intellectual_property_patents).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
