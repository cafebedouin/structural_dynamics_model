% ============================================================================
% CONSTRAINT STORY: irreversible_policy_commitment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irreversible_policy_commitment, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: irreversible_policy_commitment
 *   human_readable: The Burned Bridge Protocol
 *   domain: political/economic
 *
 * SUMMARY:
 *   The Burned Bridge Protocol represents a structural constraint where
 *   policy decisions generate irreversible institutional changes that
 *   eliminate the possibility of return to prior equilibrium. Once
 *   implemented, the cost of policy reversal becomes so high — involving
 *   societal disruption, institutional collapse, or political crisis — that
 *   reversal is effectively impossible even when the policy's original
 *   benefits have vanished. This constraint exhibits a characteristic six-way
 *   perspectival split: the policy-initiating coalition experiences
 *   coordination and lock-in benefit; displaced populations experience pure
 *   extraction with no exit; moderate actors bear mixed costs and some
 *   benefits; international observers see temporary lock-in with eventual
 *   sunset mechanisms; the bureaucratic apparatus performs elaborate
 *   justifications for immutability; and the analytical observer risks
 *   naturalizing a contingent institutional arrangement as an immutable law.
 *   The constraint's evolution shows increasing theater_ratio and
 *   extractiveness over time, reflecting that the policy's original
 *   functional purpose (solving a real coordination problem) has degraded
 *   into institutional inertia and rent defense. The suppression level (0.72)
 *   reflects high barriers to policy reversal: sunk capital, institutional
 *   dependencies, political coalitions, and path-dependent expectations.
 *
 * KEY AGENTS:
 *   - Policy Initiating Coalition: Primary beneficiary (institutional/arbitrage) — political parties, dominant industries, regional interests that locked in the policy; controls reversal costs through political gatekeeping
 *   - Displaced Populations: Primary victim (powerless/trapped) — communities directly harmed by irreversible changes (asset seizures, territorial loss, industrial decimation); cannot exit or organize effective reversal
 *   - Policy Adjustment Constituency: Secondary victim/moderate beneficiary (moderate/constrained) — businesses, labor organizations, regional governments with some benefits from lock-in but significant adjustment costs; constrained exit options
 *   - International Reform Coalition: Organized actor (organized/constrained) — supranational institutions, treaty mechanisms, development organizations building alternative reversal pathways; creating structural sunset
 *   - Regulatory Apparatus: Institutional defender (institutional/constrained) — bureaucratic machinery that enforces the policy and performs elaborate justifications for its immutability; exhibits high theater
 *   - Future Generations: Powerless victim (powerless/trapped) — born into a policy regime whose reversal was decided by previous generations; cannot exercise voice in the original commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irreversible_policy_commitment, 0.58).
domain_priors:suppression_score(irreversible_policy_commitment, 0.72).
domain_priors:theater_ratio(irreversible_policy_commitment, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irreversible_policy_commitment, extractiveness, 0.58).
narrative_ontology:constraint_metric(irreversible_policy_commitment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(irreversible_policy_commitment, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irreversible_policy_commitment, tangled_rope).
narrative_ontology:human_readable(irreversible_policy_commitment, "The Burned Bridge Protocol").
narrative_ontology:topic_domain(irreversible_policy_commitment, "political/economic").

domain_priors:requires_active_enforcement(irreversible_policy_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irreversible_policy_commitment, policy_initiating_coalition).
narrative_ontology:constraint_beneficiary(irreversible_policy_commitment, institutional_lock_in_beneficiaries).
narrative_ontology:constraint_victim(irreversible_policy_commitment, displaced_populations).
narrative_ontology:constraint_victim(irreversible_policy_commitment, future_policy_flexibility).
narrative_ontology:constraint_victim(irreversible_policy_commitment, exit_constrained_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED POPULATION (SNARE) — Communities affected by irreversible policy decisions (asset seizures, territorial reallocation, industrial decimation, pension elimination) cannot exit or organize effective resistance. The constraint operates with maximum suppression: attempting to reverse the policy incurs prohibitive political and economic costs. No alternatives available; no escape path. Maximum experienced extraction — born into a policy regime whose reversal is structurally impossible.
constraint_indexing:constraint_classification(irreversible_policy_commitment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLICY ADJUSTMENT CONSTITUENCY (TANGLED ROPE) — Moderate-power actors (businesses, labor organizations, regional governments) benefit from some aspects of the locked-in policy (subsidies, market protection, regulatory stability) while bearing costs from others (sunk capital in obsolete infrastructure, constrained adaptation options). Extraction is asymmetric but not total — these actors have some agency through regulatory reinterpretation, coalitional leverage, or relocation, but their exit options are genuinely constrained by the policy's irreversibility.
constraint_indexing:constraint_classification(irreversible_policy_commitment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POLICY INITIATING COALITION (ROPE) — Political actors and institutional beneficiaries (political parties, labor unions, regulatory agencies, incumbent firms) experience the irreversible commitment as a pure coordination mechanism: it solves the problem of maintaining coalition stability by making defection or reversal prohibitively costly. The constraint coordinates their interests through lock-in. High arbitrage capacity — they can exit the coalition's original jurisdiction or leverage the lock-in for international negotiating power. The constraint functions as pure coordination from their vantage point.
constraint_indexing:constraint_classification(irreversible_policy_commitment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL REFORM COALITION (SCAFFOLD) — Global governance actors (supranational institutions, treaty mechanisms, development organizations) see the irreversible commitment as a temporary coordination failure with a structural sunset: international precedent, treaty obligations, and cross-border effects are gradually creating pathways for 'conditional reversal' or 'managed transition' that reduce the effective cost of adjustment. Low effective extraction because organized international pressure can reframe reversal as compliance with higher obligations. Theater_ratio is relatively low here — the sunset mechanism is becoming functionally real through treaty renegotiation and supranational enforcement.
constraint_indexing:constraint_classification(irreversible_policy_commitment, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY APPARATUS (PITON) — The bureaucratic machinery that enforces and defends the policy exhibits high theater: elaborate justifications for irreversibility, procedural defenses against challenge, administrative theater around 'why it cannot be changed.' The regulation has degraded from its original functional purpose (solving a real coordination problem) into a performative defense of institutional inertia. The apparatus exists largely to prevent reversal rather than to achieve the policy's original goal. Theater ratio >= 0.70. Piton classification derives from institutional self-perpetuation through performative justification.
constraint_indexing:constraint_classification(irreversible_policy_commitment, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From the civilizational/universal perspective, some policy commitments appear to be 'immutable' or 'locked in by nature': once sufficient irreversible changes propagate through the institutional ecosystem, reversal becomes theoretically possible but practically synonymous with societal collapse. This perspective risks naturalizing a contingent institutional arrangement as an immutable law of political economy. The engine's false summit detector will identify this as naturalization rather than discovery of a genuine natural law. Accessibility_collapse and resistance values will be moderate, not meeting the mountain threshold for genuine NL certification.
constraint_indexing:constraint_classification(irreversible_policy_commitment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irreversible_policy_commitment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irreversible_policy_commitment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irreversible_policy_commitment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irreversible_policy_commitment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irreversible_policy_commitment, TR),
    TR >= 0.70.

:- end_tests(irreversible_policy_commitment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting strong asymmetric benefits for the initiating coalition and concentrated costs for displaced populations and constrained actors. The value is below 0.66 (snare threshold) because some policy adjustments are possible within the lock-in, and moderate-power actors retain partial agency. The metric has increased from 0.42 to 0.58 over the 20-year interval as sunk capital makes reversal progressively more costly and the original coordination benefit has eroded. Suppression (0.72): High, reflecting substantial barriers to policy reversal — sunk institutional capital, political coalitions, path-dependent expectations, and the sheer coordination costs of reversing a major policy. These are not primarily coercive suppression (active enforcement) but structural suppression (exit barriers). Theater ratio (0.48): Moderate, showing that the policy originally had real functional content (solving a genuine coordination problem) but increasingly relies on performative defense and institutional inertia. The ratio has increased from 0.25 to 0.48 as the bureaucratic apparatus has elaborated justifications for irreversibility.
 *
 * PERSPECTIVAL GAP:
 *   The original research group sees coordination (Rope) because the irreversible commitment solves their political coordination problem — coalition members cannot defect without destroying the arrangement. The open science coalition (international reform) sees a temporary problem with sunset (Scaffold) — supranational institutions are gradually creating mechanisms for 'conditional reversal' through treaty renegotiation and supranational enforcement. The regulatory apparatus sees its own degraded ritual (Piton) — the justifications for immutability have become performative as the original coordination benefit has eroded, but the apparatus persists through institutional inertia. Replication groups (moderate actors) see mixed coordination and extraction (Tangled Rope) — they benefit from the policy's stability but are constrained by adjustment costs and sunk capital. The displaced population sees pure extraction (Snare) — they bear indefinite costs with no exit option or voice in the reversal decision. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — but the structural data reveals this as a false summit: the 'irreversibility' is a product of specific institutional arrangements and political coalitions, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position relative to the lock-in mechanism. The policy-initiating coalition has low d (0.15–0.30) — they are beneficiaries with arbitrage options (can relocate, exit the jurisdiction, leverage the lock-in internationally). Displaced populations have high d (0.90–0.95) — they are victims with trapped exit options; the lock-in was imposed without their consent and they bear indefinite costs. Moderate-power actors have intermediate d (0.55–0.65) — they have some benefit from the policy's stability but also significant adjustment costs; their exit options are constrained but not totally eliminated. The international coalition has d around 0.70 (observer-with-agency) — they see the lock-in from outside and have leverage through treaty mechanisms and supranational pressure. The regulatory apparatus has d around 0.40 (institutional defender) — they benefit from the policy's persistence through institutional authority but are also constrained by the need to defend it. The analytical observer has d around 0.72 (analytical perspective) — seeing the full structure but at risk of naturalizing contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how the same irreversible commitment can be classified as coordination (Rope) for beneficiaries, mixed coordination-extraction (Tangled Rope) for moderate actors, pure extraction (Snare) for displaced populations, and temporary lock-in (Scaffold) for international actors. The mandatrophy is resolved by recognizing that indexical classification reveals whose interests the 'irreversibility' serves. For the initiating coalition, the commitment is purely coordinative — it locks in their interests. For displaced populations, it is purely extractive — they bear costs without exit. For moderate actors, it is mixed — they coordinate some interests while being extracted from on others. For international observers, it is temporary — supranational mechanisms gradually reduce reversal costs. The false summit (mountain perspective) attempts to naturalize what is actually a contingent institutional arrangement maintained by political coalitions. The accessibility_collapse for the mountain perspective would be moderate (around 0.60), not the ≥0.85 required for genuine NL certification — revealing that 'irreversibility' is not a natural law but a political-economic structure. This decomposition prevents mislabeling of institutional lock-in as natural necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversal_cost_threshold,
    'At what cost-to-reversal threshold does an irreversible policy commitment transition from strategically contingent to genuinely structurally immutable?',
    'Historical case analysis: track documented reversal attempts and their actual costs vs. predicted costs; identify examples where ''impossible'' reversals were ultimately executed',
    'If threshold is moderate: many locked-in policies are reversible at political cost (reclassification to snare/tangled_rope from multiple perspectives). If threshold is very high: most policy locks are genuinely irreversible (mountain appearance becomes more defensible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversal_cost_threshold, empirical, 'Cost threshold distinguishing strategic lock-in from structural immutability').

omega_variable(
    coalition_benefit_distribution,
    'How do benefits from the irreversible commitment distribute across the initiating coalition, and does the distribution remain stable over time?',
    'Political economy analysis of who captures rents from lock-in; tracking of coalition defections and compensatory side-payments; measurement of benefit concentration',
    'If benefits concentrate: coalition stability is high and extraction mechanisms are strong (snare appearance strengthens). If benefits disperse: coalition degrades and reversal pressure increases (scaffold/rope appearance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_benefit_distribution, empirical, 'Distribution of lock-in benefits across initiating coalition').

omega_variable(
    intergenerational_cost_accumulation,
    'Do the costs of irreversible commitment accumulate across generations, or do they stabilize once the initial adjustment period ends?',
    'Longitudinal measurement of policy costs over 50+ year horizons; tracking of generational attitudes toward reversal; identification of cost-escalation vs cost-stabilization regimes',
    'If costs accumulate: generational conflict intensifies and organized opposition to lock-in builds (scaffold/organized perspectives strengthen, snare perspective intensifies). If costs stabilize: the policy becomes ''normalized'' and reversal pressure declines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_cost_accumulation, empirical, 'Whether lock-in costs accumulate or stabilize intergenerationally').

omega_variable(
    supranational_override_mechanism,
    'Can international law, treaty obligations, or supranational governance structures override domestic irreversible commitments, effectively creating a higher-order reversal mechanism?',
    'Legal precedent analysis; tracking of supranational enforcement against locked-in domestic policies; examination of treaty renegotiation outcomes',
    'If overridable: scaffold perspective gains structural reality, sunset mechanism becomes functionally real, and effective irreversibility is lower than base metrics suggest. If not overridable: international constraints are purely performative and the lock-in is more absolute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_override_mechanism, conceptual, 'Whether supranational governance can functionally override domestic lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irreversible_policy_commitment, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irrev_tr_t0, irreversible_policy_commitment, theater_ratio, 0, 0.25).
narrative_ontology:measurement(irrev_tr_t10, irreversible_policy_commitment, theater_ratio, 10, 0.38).
narrative_ontology:measurement(irrev_tr_t20, irreversible_policy_commitment, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(irrev_be_t0, irreversible_policy_commitment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(irrev_be_t10, irreversible_policy_commitment, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(irrev_be_t20, irreversible_policy_commitment, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irreversible_policy_commitment, enforcement_mechanism).
narrative_ontology:affects_constraint(irreversible_policy_commitment, regulatory_capture).
narrative_ontology:affects_constraint(irreversible_policy_commitment, sunk_cost_fallacy_institutionalized).
narrative_ontology:affects_constraint(irreversible_policy_commitment, path_dependent_lock_in).

% DUAL FORMULATION NOTE:
% The Burned Bridge Protocol is upstream of specific policy lock-in cases (e.g., currency union irreversibility, territorial partition, asset seizure). The protocol itself represents the structural mechanism by which temporary policies become permanent institutional constraints. Downstream constraints show how this lock-in manifests in specific domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irreversible_policy_commitment, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
