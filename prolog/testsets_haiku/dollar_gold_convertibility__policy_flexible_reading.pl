% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility as Conditional Obligation (Policy Flexible Reading)
 *   domain: international_political_economy/monetary_policy
 *
 * SUMMARY:
 *   Under the Bretton Woods system (1944–1973), the U.S. Treasury committed
 *   to convert dollars into gold at a fixed parity of 35 dollars per fine
 *   ounce, and all other signatories pegged their currencies to the dollar.
 *   This reading frames convertibility as a CONDITIONAL commitment
 *   subordinate to U.S. domestic economic needs: the U.S. claims the right to
 *   suspend or redefine convertibility if domestic inflation, unemployment,
 *   or balance-of-payments stress demands it. This differs from the
 *   strict_convertibility_reading (which treats convertibility as an absolute
 *   legal constraint on U.S. monetary policy) and from the
 *   triffin_structural_reading (which treats convertibility as an inherently
 *   unsustainable design). Under the policy-flexible reading, the U.S. gains
 *   monetary autonomy (beneficiary seat) while foreign dollar holders lose
 *   the certainty of redemption (victim seat); extraction is asymmetric and
 *   enforced through the U.S. government's control of gold supplies and
 *   convertibility suspension.
 *
 * KEY AGENTS:
 *   - united_states_government: primary agenda-setter; controls the convertibility definition and can suspend it unilaterally
 *   - foreign_central_banks: institutional payers; hold dollar reserves and bear devaluation risk
 *   - dollar_privileged_users: beneficiaries of dollar stability and transaction advantages; can adjust faster than central banks if devaluation occurs
 *   - non_reserve_currency_economies: powerless payers; trapped in a dollar-denominated system without voice in U.S. policy decisions
 *   - bretton_woods_institution_architects: excluded; designed the system but lack enforcement power over the dominant issuer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.62).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.71).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility as Conditional Obligation (Policy Flexible Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_policy").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, 'b2d56b05-8ef8-4f27-abf3-a5346e93b8f0').
narrative_ontology:cs_kernel_codification('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0', fixed_text).
narrative_ontology:cs_authority_grounding('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0', extraction).
narrative_ontology:cs_interpretation_layer_present('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0').
narrative_ontology:cs_reading_relation('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0', foundational, domestic_stability_hierarchy).
narrative_ontology:cs_axiom_status(domestic_stability_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0', domestic_stability_hierarchy, instrumental).
narrative_ontology:cs_axiom('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0', foundational, unilateral_us_policy_authority).
narrative_ontology:cs_axiom_status(unilateral_us_policy_authority, holdable).
narrative_ontology:cs_axiom_grounding('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0', unilateral_us_policy_authority, deontological).
narrative_ontology:cs_reference_frame('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0', dollar_gold_parity_maintained).
narrative_ontology:cs_drift_state('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0', post_1970_systemic_pressure, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('b2d56b05-8ef8-4f27-abf3-a5346e93b8f0', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, united_states_government).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, dollar_privileged_users).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, dollar_reserve_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, non_reserve_currency_economies).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.35 (1944, fresh commitment, low suspicion of U.S. flexibility) to 0.62 (1973, after Nixon shock). This trajectory reflects growing evidence that the U.S. was treating convertibility as optional — using monetary expansion (1960s) that violated the commitment's spirit, ultimately forced into explicit suspension. Suppression rises from 0.40 to 0.71 because maintaining the appearance of convertibility (while printing dollars that couldn't be backed) required active enforcement: capital controls, gold sales management, informal pressure on central banks not to redeem. Theater ratio climbs from 0.20 to 0.48 because by the late 1960s, the constraint's main function was performative — defending the fiction of convertibility rather than enabling it. The coercion grid shows suppression rising fastest at the STRUCTURAL level (0.42→0.78) — the Bretton Woods institution itself had to suppress the logical consequences of dual mandates (maintain parity AND expand money supply). Resistance rises at every level but fastest at the structural level (0.15→0.72), driven by Triffin's analysis, French gold accumulation, and run-on-the-bank dynamics (London Gold Pool pressure, late-1960s). The grid shows a DIVERGING multi-level picture: suppression increases fastest at structural/organizational levels (the international system must maintain theater); resistance increases fastest at structural/organizational levels too (central banks and economists mount pressure). Individual-level resistance and suppression lag because average citizens in foreign economies did not directly perceive the devaluation risk — it was an institutional artifact.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. government's seat, this constraint is a tool for preserving monetary autonomy while gaining seigniorage benefits (printing money other nations hold as reserves). From foreign central banks' seats, it is a trap: they are obligated to hold dollars by the international system, but those dollars may be devalued unilaterally. From non-reserve economies' seats, it is pure exclusion — they depend on dollar-denominated export revenues but have no vote on the dollar's fate. The engine should compute markedly different d values: U.S. government near d=0.0 (beneficiary), foreign central banks near d=0.85+ (target), non-reserve economies near d=0.9+ (trapped target). The perspectival gap is structural: the same constraint has opposite directionalities depending on where you stand.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. government benefits from the arrangement (collects seigniorage, retains monetary autonomy despite formal commitment) — d near 0.1 → negative or minimal extraction in χ. Foreign central banks are the primary targets: they hold a depreciating asset (dollars redeemable in a fixed gold quantity when the U.S. is printing above the gold stock's ability to back) — d near 0.85 → high extraction in χ. Dollar-privileged users have mixed directionality (~0.4): they benefit from the dollar's stability and prevalence, but face exchange-rate losses if devaluation occurs. They have better exit options (can hedge, diversify, adjust faster) so their d is lower than central banks'. Non-reserve economies are trapped (d near 0.95): their exports are priced in dollars, they cannot influence the convertibility decision, and they absorb inflation losses if the U.S. devalues. No directionality overrides are necessary; the structural derivation from beneficiary/victim + exit options produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII need for stable exchange rates and rules-based order) was LIVE in 1944–1958. By 1960, the Triffin dilemma made the founding problem CONTESTED: was the problem the lack of rules, or was the problem the incompatible demands placed on the U.S. (maintain gold parity while serving as world banker)? By 1971, the problem was DEAD: the U.S. had decided (unilaterally, despite the formal commitment) that domestic stability took precedence, and the commitment was abandoned. This MISMATCH (founding_problem_status=dead, disappearance_verdict=world_rearranges) is a MANDATROPHY signal: the arrangement persists as a zombie constraint (1971–1973, the 'Smithsonian interval') even after its mandate has expired and its founding problem is resolved by U.S. abandonment of the commitment. The constraint should not be classified as PITON because it does not persist from institutional inertia — it is explicitly abandoned by design. However, it fits TANGLED_ROPE because it coordinates (fixed-rate system enables predictable trade) AND extracts (U.S. collects seigniorage, foreign holders lose optionality), and the extraction persists through ACTIVE ENFORCEMENT (capital controls, gold management, institutional pressure) until the moment the U.S. chooses to exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_autonomy_vs_credibility,
    'Is the convertibility commitment genuinely subordinate to domestic economic stability, or does the existence of the commitment constrain U.S. policy choices to the point of becoming binding?',
    'Examine counterfactual: what would U.S. monetary policy have been absent the convertibility obligation? Comparison with Federal Reserve decisions 1964–1971 against stated policy preferences reveals whether the constraint was treated as optional or as an external limit.',
    'If the constraint was genuinely perceived as subordinate (optional, breakable at will), the U.S. was extracting benefits with low suppression (option-value asymmetry). If the constraint was binding despite formal reserve power, the U.S. was a victim of its own commitment — extractiveness reverses, and the reading mis-categorizes the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_autonomy_vs_credibility, empirical, 'Whether the policy-flexibility framing describes actual U.S. behavior or is retrospective rationalization.').

omega_variable(
    reading_contest_asymmetry,
    'Is this reading (policy flexibility) distinguishable from the strict_convertibility_reading by genuine structural difference, or are they competing narratives of identical facts?',
    'Examine whether the two readings predict different policy trajectories or different agent behaviors under stress. If both readings describe the same historical path (Nixon''s 1971 decision) identically, they are narrative frames over one constraint, not structurally different constraints.',
    'If readings are truly distinct ε-values (one treats the commitment as binding, one as conditional), this story stands; if readings are identical structures with different rhetoric, the decomposition is premature and the stories should be merged into one constraint with an omega about interpretive contestation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_asymmetry, conceptual, 'Whether kernel decomposition reflects genuine structural difference or rhetorical framing difference.').

omega_variable(
    victim_vs_beneficiary_measurement,
    'Are foreign dollar holders genuinely victims of this constraint, or are they compensated (through interest, seigniorage-sharing, or other mechanisms) such that their net position is beneficiary?',
    'Measure real returns on dollar reserves held by foreign central banks, including interest income, against the counterfactual: what would they have earned in gold, sterling, or a diversified portfolio? Accounting for the devaluation event (1971–1973).',
    'If foreign holders received below-counterfactual returns, they are victims (victim set correct, extractiveness confirmed). If they received above-counterfactual returns, they are disguised beneficiaries (victim set misspecified, extractiveness inflated, constraint may be rope not tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_vs_beneficiary_measurement, empirical, 'Whether foreign central banks were extractively targeted or compensated participants.').

omega_variable(
    kernel_reading_alternative_framing,
    'Could this constraint be read under a DIFFERENT kernel (not dollar_gold_convertibility) that would produce different ε?',
    'Test alternative kernels: (1) international monetary authority and its legitimacy; (2) U.S. monetary hegemony and its constraints; (3) the gold standard itself as a kernel. Do any alternative kernels produce materially different readings with different ε values?',
    'If an alternative kernel yields a cleaner reading with lower ambiguity, the current kernel decomposition may be mis-identified. The constraint might be better understood through a different theoretical frame, invalidating the sibling set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Whether dollar_gold_convertibility is the correct kernel or whether a different kernel better decomposes the constraint family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1944, 0.2).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1950, 0.28).
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1958, 0.35).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1965, 0.42).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.52).
narrative_ontology:measurement(doll_tr_t1973, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1973, 0.48).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1958, 0.51).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.65).
narrative_ontology:measurement(doll_be_t1973, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1973, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1944, 0.4).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1958, 0.58).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1965, 0.67).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.75).
narrative_ontology:measurement(doll_su_t1973, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1973, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__policy_flexible_reading, 0.22).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the dollar_gold_convertibility kernel. The policy_flexible_reading treats the commitment as conditional on domestic U.S. economic needs; the strict_convertibility_reading treats it as binding law; the triffin_structural_reading treats it as structurally unsustainable. Ε values differ across the readings: policy_flexible (this story) has moderate-high extractiveness (0.62) because the U.S. can abandon convertibility at will, making foreign holders victims. Strict_convertibility would have lower U.S. extractiveness (the commitment constrains U.S. monetary policy) and shift victim status to U.S. domestic constituencies hurt by monetary constraint. Triffin_structural would have even higher structural extractiveness (the system is rigged to fail, extracting from whoever absorbs the adjustment when it collapses). All three readings describe the same historical period (1944–1973) but predict different agent situations and different policy trajectories. They form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
