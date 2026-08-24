% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support as Universality Paradox
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   Unconditional income support functions as a politically ambiguous Trojan
 *   horse. The term 'universality' carries two structurally distinct
 *   meanings: a universal demogrant (everyone receives the same cash payment)
 *   and a universal credit (everyone receives the same net benefit after
 *   tax-back). These are fiscally equivalent per decades of tax-benefit
 *   modeling, but politically incommensurable. The ambiguity is not
 *   accidental — it is the load-bearing element that allows a single policy
 *   vehicle to carry mutually exclusive normative commitments (freedom floor
 *   vs. streamlined administration). Political entrepreneurs exploit this to
 *   build coalitions; policy designers provide the technical equivalence that
 *   makes the ambiguity sustainable. The victims are ideological clarity (the
 *   ambiguity prevents coherent evaluation) and targeted program recipients
 *   (universality becomes a rhetorical vehicle for cutting means-tested
 *   programs).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.25).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.4).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support as Universality Paradox").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, 'a85e7637-7931-4df5-81df-aa0af2674ba7').
narrative_ontology:cs_kernel_codification('a85e7637-7931-4df5-81df-aa0af2674ba7', distributed).
narrative_ontology:cs_authority_grounding('a85e7637-7931-4df5-81df-aa0af2674ba7', extraction).
narrative_ontology:cs_interpretation_layer_present('a85e7637-7931-4df5-81df-aa0af2674ba7').
narrative_ontology:cs_reading_relation('a85e7637-7931-4df5-81df-aa0af2674ba7', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('a85e7637-7931-4df5-81df-aa0af2674ba7', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('a85e7637-7931-4df5-81df-aa0af2674ba7', foundational, universality_ambiguity_is_load_bearing).
narrative_ontology:cs_axiom_status(universality_ambiguity_is_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('a85e7637-7931-4df5-81df-aa0af2674ba7', universality_ambiguity_is_load_bearing, conventional).
narrative_ontology:cs_axiom('a85e7637-7931-4df5-81df-aa0af2674ba7', secondary, tax_back_equivalence_converges_outcomes).
narrative_ontology:cs_axiom_status(tax_back_equivalence_converges_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('a85e7637-7931-4df5-81df-aa0af2674ba7', tax_back_equivalence_converges_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('a85e7637-7931-4df5-81df-aa0af2674ba7', post_war_welfare_consensus).
narrative_ontology:cs_drift_state('a85e7637-7931-4df5-81df-aa0af2674ba7', contemporary_ubi_debate, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('a85e7637-7931-4df5-81df-aa0af2674ba7', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, policy_evaluators).
narrative_ontology:constraint_vindicates(unconditional_income_support__universality_paradox_reading, tax_back_equivalence).
narrative_ontology:constraint_vindicates(unconditional_income_support__universality_paradox_reading, political_coalition_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploit the ambiguity between universal demogrant and universal credit to build cross-ideological coalitions. They frame universality as either a freedom floor (appealing to left) or a streamlined negative income tax (appealing to right), never specifying which. Their careers advance by keeping the coalition intact through strategic ambiguity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary).

% Gain professional standing and rhetorical flexibility by designing tax-back mechanisms that make universal demogrant fiscally equivalent to targeted transfers. They publish models showing convergence of outcomes, which lets them avoid taking a normative position on whether universality means 'same check for everyone' or 'same net benefit after tax'. Their expertise is valued precisely because it sustains the ambiguity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    organized, biographical, mobile, national).

% Lose political protection when universality becomes the dominant frame. Means-tested programs that once had dedicated constituencies get folded into universal schemes where the political logic shifts to 'everyone gets the same' — making it easier to cut benefits across the board. They cannot exit the constraint because they depend on the transfers for survival.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, local).

% Bear the cost of evaluative incoherence. The ambiguity prevents coherent cost-benefit analysis, longitudinal tracking, or clear accountability metrics. When every design claims 'universality' but means different things, evaluation becomes a moving target. They are professionally constrained — their methods assume a stable policy object to evaluate.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_evaluators, payer,
    moderate, biographical, constrained, national).

% Advocate for universality as an autonomy-enabling floor that decommodifies labor and eliminates stigma. They are excluded from the design table when the ambiguity is resolved toward tax-back equivalence, because their vision requires a genuine demogrant without clawback. Their identity is fused to the 'freedom' reading of universality.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, freedom_floor_advocates, excluded,
    organized, generational, identity_locked, national).

% Oppose unconditional support as incentive-distorting and upwardly redistributive. They are excluded when the ambiguity is resolved toward universal demogrant, because their critique only bites if the transfer is truly unconditional. Their identity is fused to the 'dependency' reading — they cannot accept any unconditional transfer as legitimate.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, dependency_trap_advocates, excluded,
    organized, generational, identity_locked, national).

% Observes the structural entanglement: the same policy vehicle carries mutually exclusive normative commitments because the ambiguity is politically load-bearing. Sees fiscal equivalence research confirming that design choice is political, not economic. No stake in any reading's victory.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Builds cross-ideological coalitions around universal cash transfers by exploiting ambiguity about whether universality means 'everyone gets the same check' (demogrant) or 'everyone gets the same net benefit after tax' (universal credit). The ambiguity lets left universalists and right negative-income-tax proponents vote for the same bill.
% TRANSFER_FUNCTION: Moves rhetorical flexibility and coalition-building capacity to political entrepreneurs and policy designers; moves evaluative clarity and targeted program integrity off the table. The fiscal equivalence of demogrant+tax-back vs targeted transfer means the distributional outcome is identical — only the political meaning differs.
% ABSENT_VOICES: Targeted program recipients who lose political protection when universality becomes the frame; fiscal conservatives who would support negative income tax but oppose universal demogrant; left universalists who would support universal basic services but get bundled with cash-transfer universalism. All are structurally excluded because their presence would force specification of which universality is meant.
% DISAPPEARANCE_RATIONALE: If the ambiguity vanished overnight, the cross-ideological coalition would fracture. Left universalists would demand a genuine demogrant; right proponents would retreat to negative income tax; targeted program advocates would reassert means-testing. The policy vehicle would split into its component readings, each with different distributional politics.
% FOUNDING_PROBLEM: Post-war welfare state needed to extend coverage beyond contributory insurance without creating poverty traps or stigmatizing means tests. Universality was the political solution — but it contained an unresolved ambiguity: universal as 'same benefit for all' vs universal as 'same net benefit after tax'.
% FOUNDING_PROBLEM_CORROBORATION: OECD tax-benefit modeling (1980s-present) shows negative income tax achieves similar distributional outcomes to universal demogrant at lower gross cost. Political scientists (Pierson, Hacker, etc.) document coalition fracture when design specifics are debated. The ambiguity persists because resolving it breaks the coalition that sustains the policy.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because fiscal/distributional outcomes converge across designs — the constraint does not extract resources so much as extract evaluative capacity. Suppression is moderate (0.4) because the constraint actively suppresses design specification: when a proposal specifies 'demogrant' or 'tax-back', the coalition fractures. Theater ratio is moderately high (0.55) because the performance of 'universality' replaces substantive design debate. Accessibility collapse is moderate (0.4) — alternatives (negative income tax, targeted transfers, universal basic services) exist but are marginalized by the universality frame. Resistance is substantial (0.6) because both excluded readings (freedom floor, dependency trap) mount active resistance when design specifics threaten their version of universality.
 *
 * PERSPECTIVAL GAP:
 *   From the political entrepreneur's seat, the ambiguity is a feature — it enables coalition maintenance. From the targeted recipient's seat, the same ambiguity is a threat — it erases their specific claims. From the policy designer's seat, the fiscal equivalence is a technical truth that resolves the dispute; from the evaluator's seat, that same equivalence is a methodological nightmare. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs are structural beneficiaries (d near 0.0) — they collect coalition-building capacity and agenda control. Policy designers are beneficiaries (d ~ 0.2) — they gain professional standing from maintaining the technical equivalence literature. Targeted program recipients are targets (d near 1.0) — they bear the political cost of universality becoming a cut vector. Policy evaluators are targets (d ~ 0.7) — they bear the professional cost of evaluative incoherence. Freedom floor and dependency trap advocates are excluded (identity_locked) — their exit is blocked by identity fusion to their reading of universality. The analytical observer sits at d=0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (extending coverage without poverty traps/stigma) is contested — some argue tax credits solved it, others say stigma persists. The constraint persists not because the problem is solved, but because the ambiguity itself became politically valuable. This is mandatrophy: the original coordination function (universal coverage) has been displaced by a new function (coalition maintenance via ambiguity). The constraint is a tangled rope because it genuinely coordinates (universal coverage consensus) AND extracts (evaluative capacity, targeted program integrity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_mechanism,
    'Is the universality ambiguity actively maintained by political entrepreneurs, or does it emerge from genuine conceptual confusion about what ''universal'' means in a tax-transfer system?',
    'Process-tracing of legislative debates and design documents: if actors explicitly strategize ambiguity, it''s maintained; if they genuinely conflate the two meanings, it''s emergent confusion.',
    'If maintained, the constraint is a deliberate snare-like coordination; if emergent, it''s a genuine tangled rope where coordination and extraction are inseparable. Affects whether the constraint could be resolved by better communication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_mechanism, empirical, 'Whether the ambiguity is a strategic political resource or an epistemic limitation').

omega_variable(
    tax_back_equivalence_boundary,
    'Does fiscal equivalence of demogrant+tax-back vs targeted transfer hold under behavioral responses (labor supply, take-up, political sustainability)?',
    'Dynamic microsimulation with behavioral elasticities; natural experiments from jurisdictions that implemented one design vs the other.',
    'If equivalence breaks under behavior, the design choice becomes economically consequential, not just political. The paradox reading''s claim that ''design is political not economic'' would be falsified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tax_back_equivalence_boundary, empirical, 'Whether the fiscal equivalence claim survives behavioral realism').

omega_variable(
    kernel_framing_ambiguity,
    'Does the kernel ''unconditional income support'' refer to a single policy design space with multiple readings, or to a family of distinct constraints (demogrant, negative income tax, universal credit) that share only a label?',
    'Compare the structural parameters (beneficiaries, victims, epsilon, type) across the three declared readings. If they diverge significantly, the kernel is a label over multiple constraints.',
    'If the kernel decomposes into distinct constraints, the ''universality paradox'' is a category error — there is no single constraint with ambiguity, but three constraints sharing a name. This would require splitting into separate constraint stories per the epsilon-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel is one constraint with multiple readings or multiple constraints with a shared label').

omega_variable(
    suppression_mechanism,
    'Is the suppression of design specification structural (institutional rules, coalition discipline) or internalized (advocates self-censor to preserve coalition)?',
    'Interview advocates across the coalition: do they report external pressure to stay vague, or internal reluctance to fracture the coalition?',
    'If internalized, the constraint''s effective suppression is higher than institutional measures suggest — the coalition carries the suppression with it. Affects whether resolving the ambiguity requires institutional reform or coalition fracture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism, empirical, 'Structural vs internalized suppression of design specification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t1945, unconditional_income_support__universality_paradox_reading, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(unco_tr_t1970, unconditional_income_support__universality_paradox_reading, theater_ratio, 1970, 0.4).
narrative_ontology:measurement(unco_tr_t1990, unconditional_income_support__universality_paradox_reading, theater_ratio, 1990, 0.5).
narrative_ontology:measurement(unco_tr_t2000, unconditional_income_support__universality_paradox_reading, theater_ratio, 2000, 0.52).
narrative_ontology:measurement(unco_tr_t2010, unconditional_income_support__universality_paradox_reading, theater_ratio, 2010, 0.54).
narrative_ontology:measurement(unco_tr_t2020, unconditional_income_support__universality_paradox_reading, theater_ratio, 2020, 0.55).
narrative_ontology:measurement(unco_tr_t2025, unconditional_income_support__universality_paradox_reading, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(unco_be_t1945, unconditional_income_support__universality_paradox_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(unco_be_t1970, unconditional_income_support__universality_paradox_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(unco_be_t1990, unconditional_income_support__universality_paradox_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(unco_be_t2000, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2000, 0.24).
narrative_ontology:measurement(unco_be_t2010, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(unco_be_t2020, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2020, 0.25).
narrative_ontology:measurement(unco_be_t2025, unconditional_income_support__universality_paradox_reading, base_extractiveness, 2025, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t1945, unconditional_income_support__universality_paradox_reading, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(unco_su_t1970, unconditional_income_support__universality_paradox_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(unco_su_t1990, unconditional_income_support__universality_paradox_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(unco_su_t2000, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(unco_su_t2010, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(unco_su_t2020, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(unco_su_t2025, unconditional_income_support__universality_paradox_reading, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__universality_paradox_reading, 0.15).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form a constraint family decomposing the 'unconditional income support' kernel. The universality_paradox_reading claims the ambiguity between demogrant and tax-back equivalence is load-bearing; freedom_floor_reading claims universality as decommodifying floor; dependency_trap_reading claims universality as upward redistribution. All three share the referent 'unconditional income support' but instantiate different constraints with different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__universality_paradox_reading, organized, 0.15).
constraint_indexing:directionality_override(unconditional_income_support__universality_paradox_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
