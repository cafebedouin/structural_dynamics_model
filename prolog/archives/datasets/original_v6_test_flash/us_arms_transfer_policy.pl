% ============================================================================
% CONSTRAINT STORY: us_arms_transfer_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_arms_transfer_policy, []).

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
 *   constraint_id: us_arms_transfer_policy
 *   human_readable: US Arms Transfer Policy
 *   domain: political
 *
 * SUMMARY:
 *   The US Arms Transfer Policy involves a complex interplay of factors,
 *   including national security interests, economic benefits, and
 *   geopolitical strategy. It also entails significant ethical considerations
 *   regarding human rights and regional stability. The policy governs the
 *   sale and distribution of weapons and military technology by the United
 *   States to other nations. Different stakeholders experience it in diverse
 *   ways, leading to a range of classifications.
 *
 * KEY AGENTS:
 *   - US Defense Industry: Primary beneficiary (institutional/arbitrage)
 *   - US Government: Secondary beneficiary (institutional/constrained)
 *   - Regional Stability: Primary victim (moderate/constrained)
 *   - Human Rights: Secondary victim (powerless/trapped)
 *   - Civilian Populations: Target (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_arms_transfer_policy, 0.6).
domain_priors:suppression_score(us_arms_transfer_policy, 0.7).
domain_priors:theater_ratio(us_arms_transfer_policy, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_arms_transfer_policy, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_arms_transfer_policy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_arms_transfer_policy, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_arms_transfer_policy, tangled_rope).
narrative_ontology:human_readable(us_arms_transfer_policy, "US Arms Transfer Policy").
narrative_ontology:topic_domain(us_arms_transfer_policy, "political").

domain_priors:requires_active_enforcement(us_arms_transfer_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_arms_transfer_policy, us_defense_industry).
narrative_ontology:constraint_beneficiary(us_arms_transfer_policy, us_government).
narrative_ontology:constraint_victim(us_arms_transfer_policy, regional_stability).
narrative_ontology:constraint_victim(us_arms_transfer_policy, human_rights).
narrative_ontology:constraint_victim(us_arms_transfer_policy, civilian_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Global Civilian Populations (Snare) - Trapped in conflict zones, bear the brunt of arms proliferation with no exit option. The policy extracts stability and safety from these populations.
constraint_indexing:constraint_classification(us_arms_transfer_policy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Regional Stability (Tangled Rope) - Constrained by arms flows which can escalate conflicts, but also benefits from US security guarantees and deterrence. Extraction is significant, but not absolute, reflecting a mixed benefit-cost relationship.
constraint_indexing:constraint_classification(us_arms_transfer_policy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective 3: US Defense Industry (Rope) - Benefits from arms sales contracts, which enhance profitability and secure jobs. The policy facilitates coordination and expansion opportunities.
constraint_indexing:constraint_classification(us_arms_transfer_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: US Government (Tangled Rope) - Benefits from enhanced geopolitical influence and strategic alliances, but constrained by reputational risks and potential blowback from arms transfers. The policy provides instruments of power projection, but also imposes restraints. 
constraint_indexing:constraint_classification(us_arms_transfer_policy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 5: International Norms on Arms Control (Piton) - The US arms transfer policy can degrade international norms and treaties, with its performance being theatrical rather than impactful. This perspective reflects a piton classification due to the inertia and limited current functional impact of these norms.
constraint_indexing:constraint_classification(us_arms_transfer_policy, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 6: The Analytical Observer (Tangled Rope) - Sees the policy as a blend of coordination (alliance building) and extraction (regional destabilization, human rights costs). Assesses net effect and long-term implications of the transfers.
constraint_indexing:constraint_classification(us_arms_transfer_policy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_arms_transfer_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_arms_transfer_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_arms_transfer_policy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_arms_transfer_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_arms_transfer_policy, TR),
    TR >= 0.70.

:- end_tests(us_arms_transfer_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): Reflects the policy's potential to exacerbate conflicts, destabilize regions, and contribute to human rights abuses. The arms transfer policy's impacts are broad and can often contribute to high levels of extractiveness from affected communities and populations. Suppression (0.70): Indicates a high degree of constraint on alternative courses of action due to legal frameworks, international agreements, and the dominance of US arms in the global market. This suggests limited space for change. Theater Ratio (0.30): Reflects that performative actions, like stating the importance of human rights, do not necessarily translate into practical impact.
 *
 * PERSPECTIVAL GAP:
 *   The US defense industry benefits economically (Rope), while regional stability suffers (Snare). The US government navigates a complex trade-off (Tangled Rope). The Analytical observer must account for these different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries like the US Defense industry (arbitrage) experience low extraction and see the policy as coordination (rope). Victims like regional stability and civilian populations (trapped) experience high extraction (snare). The US government, in a constrained position, experiences mixed benefits and costs (tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   The policy blends coordination (alliance building, security provision) with extraction (conflict escalation, human rights abuses). The tangled rope classification reflects this mixed character, preventing the policy from being mislabelled as purely extractive (snare) or purely coordinative (rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    criteria_enforcement_strength,
    'How strictly are human rights and regional stability criteria enforced in arms transfer decisions?',
    'Audits of arms transfer review processes; comparative analysis of arms transfer decisions versus documented human rights records.',
    'Stringent enforcement reduces extraction (closer to Rope). Lax enforcement increases extraction (closer to Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criteria_enforcement_strength, empirical, 'Strength of enforcement for human rights and stability criteria').

omega_variable(
    unintended_use_probability,
    'What is the probability that transferred arms will be used for unintended purposes (e.g., internal repression, diversion to non-state actors)?',
    'Post-transfer tracking of arms; analysis of conflict event data; risk assessments based on recipient country characteristics.',
    'High probability increases extraction from civilian populations (Snare). Low probability shifts towards coordination (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unintended_use_probability, empirical, 'Likelihood of unintended use or diversion').

omega_variable(
    alternative_supplier_availability,
    'To what extent can recipient countries readily obtain arms from alternative suppliers if the US restricts transfers?',
    'Market analysis of global arms trade; evaluation of recipient country relationships with alternative suppliers.',
    'High availability weakens the US influence (less enforcement capacity) and shifts towards rope. Limited availability strengthens US leverage (more snare-like).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_supplier_availability, empirical, 'Availability of alternative arms suppliers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_arms_transfer_policy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_a_tr_t0, us_arms_transfer_policy, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_a_tr_t10, us_arms_transfer_policy, theater_ratio, 10, 0.25).
narrative_ontology:measurement(us_a_tr_t20, us_arms_transfer_policy, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(us_a_be_t0, us_arms_transfer_policy, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(us_a_be_t10, us_arms_transfer_policy, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(us_a_be_t20, us_arms_transfer_policy, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_arms_transfer_policy, enforcement_mechanism).
narrative_ontology:affects_constraint(us_arms_transfer_policy, international_arms_control_treaties).
narrative_ontology:affects_constraint(us_arms_transfer_policy, us_foreign_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
