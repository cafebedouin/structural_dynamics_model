% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__coordination_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__coordination_scaffold_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: statutory_debt_ceiling__coordination_scaffold_reading
 *   human_readable: Statutory Debt Ceiling as Treasury Operational Coordination Mechanism
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This story authors the coordination-scaffold reading of the statutory
 *   debt ceiling kernel: the view that the aggregate limit is best understood
 *   as a procedural substitute for the pre-1917 practice of authorizing each
 *   federal bond issuance individually, reducing transaction costs for
 *   Treasury while preserving a periodic congressional checkpoint. Under this
 *   reading, the vast majority of ceiling adjustments historically have been
 *   routine, and episodes of brinkmanship are treated as a distinct,
 *   separable pathology rather than as the mechanism's true function. This is
 *   a deliberately narrow, low-extraction reading: it authors ε at 0.22,
 *   reflecting the theater and friction costs the routine-adjustment process
 *   still carries (periodic political posturing, media cycles, occasional
 *   short-term uncertainty) without asserting the systemic hostage-taking
 *   dynamic the sibling extraction_snare_reading documents.
 *
 * KEY AGENTS:
 *   - treasury_department: Primary administrator/beneficiary (institutional/constrained) — gains issuance flexibility, coordinates timing with Congress
 *   - congress_appropriations_committees: Primary agenda-setter (institutional/mobile) — retains periodic aggregate review without per-bond votes
 *   - bond_market_participants: Secondary beneficiary (organized/mobile) — benefits from predictable, rules-based issuance
 *   - federal_program_beneficiaries: Downstream observer (powerless/trapped) — experiences no disruption under this reading's routine-case assumption
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — assesses the mechanism's structural function independent of episodic misuse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__coordination_scaffold_reading, 0.22).
domain_priors:suppression_score(statutory_debt_ceiling__coordination_scaffold_reading, 0.15).
domain_priors:theater_ratio(statutory_debt_ceiling__coordination_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(statutory_debt_ceiling__coordination_scaffold_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__coordination_scaffold_reading, scaffold).
narrative_ontology:human_readable(statutory_debt_ceiling__coordination_scaffold_reading, "Statutory Debt Ceiling as Treasury Operational Coordination Mechanism").
narrative_ontology:topic_domain(statutory_debt_ceiling__coordination_scaffold_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:has_sunset_clause(statutory_debt_ceiling__coordination_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__coordination_scaffold_reading, '26de279a-9ddd-4abb-bd90-029ca46f4767').
narrative_ontology:cs_kernel_codification('26de279a-9ddd-4abb-bd90-029ca46f4767', formalized).
narrative_ontology:cs_authority_grounding('26de279a-9ddd-4abb-bd90-029ca46f4767', practice).
narrative_ontology:cs_interpretation_layer_present('26de279a-9ddd-4abb-bd90-029ca46f4767').
narrative_ontology:cs_reading_relation('26de279a-9ddd-4abb-bd90-029ca46f4767', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_reading_relation('26de279a-9ddd-4abb-bd90-029ca46f4767', statutory_debt_ceiling__constitutional_nullity_reading, influences).
narrative_ontology:cs_axiom('26de279a-9ddd-4abb-bd90-029ca46f4767', foundational, aggregate_authorization_reduces_transaction_costs_without_ceding_control).
narrative_ontology:cs_axiom_status(aggregate_authorization_reduces_transaction_costs_without_ceding_control, holdable).
narrative_ontology:cs_axiom_grounding('26de279a-9ddd-4abb-bd90-029ca46f4767', aggregate_authorization_reduces_transaction_costs_without_ceding_control, instrumental).
narrative_ontology:cs_axiom('26de279a-9ddd-4abb-bd90-029ca46f4767', secondary, periodic_review_checkpoint_retains_meaningful_congressional_oversight).
narrative_ontology:cs_axiom_status(periodic_review_checkpoint_retains_meaningful_congressional_oversight, holdable).
narrative_ontology:cs_axiom_grounding('26de279a-9ddd-4abb-bd90-029ca46f4767', periodic_review_checkpoint_retains_meaningful_congressional_oversight, conventional).
narrative_ontology:cs_reference_frame('26de279a-9ddd-4abb-bd90-029ca46f4767', administrative_efficiency_substitution_1917).
narrative_ontology:cs_drift_state('26de279a-9ddd-4abb-bd90-029ca46f4767', post_2011_debt_ceiling_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('26de279a-9ddd-4abb-bd90-029ca46f4767', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__coordination_scaffold_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, congress_appropriations_committees).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__coordination_scaffold_reading, bond_market_participants).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, legislative_control_of_borrowing_authority).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__coordination_scaffold_reading, periodic_congressional_review_of_aggregate_debt).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and manages federal debt within the aggregate ceiling set by Congress, using extraordinary measures to smooth cash management around ceiling adjustments. Benefits from a single aggregate authorization rather than needing item-by-item borrowing approval for every appropriation Congress has already enacted. Cannot unilaterally raise the ceiling and must coordinate timing with Congress before headroom exhausts.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__coordination_scaffold_reading, treasury_department, agenda_setter).

% Sets the aggregate ceiling periodically as a single vote rather than authorizing each individual bond issuance or spending program's financing separately. Retains a checkpoint for reviewing the cumulative fiscal trajectory that spending and tax votes already created, without re-litigating each appropriation's financing mechanics.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, congress_appropriations_committees, agenda_setter,
    institutional, generational, mobile, national).

% Rely on predictable, rules-based Treasury issuance to price and hold federal debt. Benefit when the ceiling functions as routine housekeeping — resolved with reasonable lead time — because it preserves confidence in orderly market operations; they are exposed to volatility only when the coordination function breaks down, which this reading treats as a distinct pathology rather than a design feature.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, bond_market_participants, beneficiary,
    organized, biographical, mobile, global).

% Depend on continued federal payments (benefits, contracts, salaries) that flow through Treasury operations governed by the ceiling. Under the coordination-scaffold reading, routine and timely ceiling adjustments mean this population experiences no disruption — their situation here is the baseline case, not the crisis case documented in the sibling extraction reading.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, federal_program_beneficiaries, observer,
    powerless, immediate, trapped, national).

% Study the ceiling's operation as one instance of Congress's Article I borrowing-authorization power, comparing pre-1917 individual bond authorizations to the aggregate-limit statute. Assess whether the aggregate mechanism preserves meaningful congressional control while reducing transaction costs, independent of episodes where the mechanism has been used coercively.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__coordination_scaffold_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__coordination_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__coordination_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prior to 1917, Congress authorized federal borrowing bond-issue by bond-issue, requiring a separate act for each instrument. The aggregate ceiling replaced that with a single periodic cap, letting Treasury manage issuance mechanics (maturities, timing, instruments) without returning to Congress for each transaction, while preserving a periodic congressional checkpoint on the total.
% TRANSFER_FUNCTION: In this reading, no systematic transfer occurs: the mechanism reallocates administrative labor from repeated congressional bond-by-bond votes to periodic aggregate votes, moving procedural burden rather than resources between parties. Treasury gains issuance flexibility; Congress retains and periodically exercises a check on total borrowing.
% ABSENT_VOICES: Legislative minorities who would use the periodic vote as leverage for unrelated policy demands are not silenced by this reading — they are simply outside its scope; this reading brackets episodes of weaponized brinkmanship as a distinct phenomenon (see extraction_snare_reading) rather than treating them as evidence against the coordination function.
% DISAPPEARANCE_RATIONALE: If the aggregate ceiling vanished, Treasury could issue debt within appropriated levels without a separate borrowing-authorization step, and the periodic congressional review moment would disappear. Whether this counts as 'the world rearranging' is itself contested within the kernel: the coordination-scaffold reading holds the review function could be replicated by other means (e.g., built into the budget resolution process), making disappearance largely administrative; other readings hold the leverage point itself is the point and its disappearance would be structurally significant. This reading records the split rather than resolving it.
% FOUNDING_PROBLEM: In 1917, financing World War I required rapid, large-scale, flexible bond issuance. Requiring Congress to authorize every individual bond series was administratively unworkable at wartime borrowing volumes, so Congress substituted a single aggregate limit for the previous instrument-by-instrument authorization system.
% FOUNDING_PROBLEM_CORROBORATION: Treasury officials and congressional budget staff (interviews, GAO reports on debt issuance mechanics) attest the administrative-coordination problem remains live — Treasury still benefits from not needing per-instrument authorization. Independent fiscal historians and organizations outside Treasury (e.g. Committee for a Responsible Federal Budget, Government Accountability Office) attest that whatever the founding administrative problem, the ceiling has since the 1970s functioned mainly as a periodic political flashpoint rather than as active fiscal restraint, since it does not constrain future spending or taxing decisions already enacted.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__coordination_scaffold_reading, contested).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__coordination_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__coordination_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__coordination_scaffold_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).
:- end_tests(statutory_debt_ceiling__coordination_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) and rising only modestly across the interval, reflecting this reading's core empirical claim: most historical ceiling adjustments (78+ instances since 1917) were routine, bipartisan, and low-conflict. The rising theater_ratio (0.1 to 0.4) captures a real drift this reading acknowledges even while defending the coordination function: since the 1970s, and especially after 2011, the periodic vote increasingly attracts performative brinkmanship, raising the proportion of activity that is symbolic/leveraging rather than substantive fiscal review. Suppression is low (0.15) because within this reading no party is coerced into compliance beyond ordinary legislative process — Treasury operates within a limit Congress set and can revise. Accessibility_collapse is moderate (0.35): once the mechanism is understood, alternative designs (built-in debt authorization tied to appropriations, as most other democracies use) become visible, so this is not a mountain-grade collapse of alternatives, consistent with a scaffold rather than a natural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury and Congress both sit near the beneficiary end: the mechanism reduces their respective transaction costs (per-bond authorization for Treasury; per-bond voting burden for Congress) relative to the pre-1917 baseline. Bond market participants benefit from the predictability the routine-case pattern provides. Federal program beneficiaries are structurally exposed to disruption if the coordination function fails, but under this reading's own terms that failure mode is bracketed as exceptional rather than definitional — hence their exit_options are authored as 'trapped' (reflecting real dependency) while the extraction metric stays low (reflecting this reading's claim that the trap is rarely sprung).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (per-bond authorization unworkable at wartime borrowing scale) is genuinely dead as a live technical constraint — modern Treasury has electronic issuance infrastructure that would trivially handle per-appropriation authorization if required. This creates a mandatrophy tension internal to the coordination-scaffold reading itself: the aggregate-ceiling *form* persists past the *administrative* problem it solved, even though this reading maintains the periodic-review *function* remains valuable as an independent check. The scaffold classification (with sunset-clause-equivalent: periodic legislative renewal) is the right frame for this internal tension — it flags the coordination function as needing to justify itself at each renewal rather than being grandfathered as settled infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modal_case_vs_tail_risk_framing,
    'Is the debt ceiling correctly characterized by its modal historical operation (routine, low-conflict adjustment, ~90% of instances) or by its tail-risk operation (weaponized brinkmanship episodes: 1995-96, 2011, 2013, 2023), given that the tail episodes carry disproportionate real-world cost (2011 credit rating downgrade, market volatility)?',
    'Quantify the asymmetry: compare aggregate administrative cost savings from routine adjustments against the realized and counterfactual costs of brinkmanship episodes (credit rating impact, borrowing cost premia, market volatility spillover). If tail costs dominate expected value, the coordination framing understates the constraint''s true operation even if modally accurate.',
    'If tail risk dominates the expected-value calculation, this reading''s low ε (0.22) understates the mechanism''s true extractive potential, and the extraction_snare_reading becomes the more descriptively accurate lens for policy purposes even though this reading remains historically defensible for the median case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modal_case_vs_tail_risk_framing, conceptual, 'Whether modal-case or tail-risk framing is the correct basis for classifying the ceiling''s function.').

omega_variable(
    coordination_function_separability,
    'Is the periodic congressional review function this reading credits as valuable actually separable from the aggregate-limit mechanism, or does the review function require the leverage/crisis-potential that the extraction reading identifies as the point?',
    'Comparative analysis of alternative designs (e.g., automatic debt-limit adjustment tied to enacted appropriations, used in some other democracies) that would preserve a review checkpoint without creating a binding, breachable limit. If such designs demonstrably preserve congressional oversight without crisis risk, the current mechanism''s crisis-proneness is not functionally necessary to its coordination value.',
    'If separable, the coordination_scaffold_reading''s defense of the current statutory form (rather than of some review function in the abstract) is weakened — the current mechanism''s specific hostage-taking-enabling structure would be shown as inessential to the coordination benefit this reading identifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_separability, conceptual, 'Whether the review/coordination function requires the specific breachable-limit design or could be achieved by a non-crisis-prone alternative.').

omega_variable(
    reading_selection_evidentiary_basis,
    'What determines which of the three kernel readings (coordination scaffold, extraction snare, constitutional nullity) is the operative frame for a given administration or Congress at a given moment?',
    'Track which reading''s predictions (routine adjustment vs. brinkmanship vs. litigation over 14th Amendment authority) actually materialize across administrations, and whether the party control configuration (unified vs. divided government) predicts which reading manifests.',
    'If divided government reliably predicts extraction-reading dynamics while unified government reliably predicts coordination-reading dynamics, the three readings are not just interpretive stances but empirically distinguishable regimes triggered by an identifiable structural variable — which would argue for decomposing this kernel further by government-configuration rather than treating the readings as purely interpretive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, empirical, 'Whether party-control configuration is the hidden variable selecting which kernel reading manifests in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__coordination_scaffold_reading, 1917, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(stat_tr_t1960, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(stat_tr_t2013, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2013, 0.4).
narrative_ontology:measurement(stat_tr_t2023, statutory_debt_ceiling__coordination_scaffold_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1917, 0.08).
narrative_ontology:measurement(stat_be_t1960, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2011, 0.2).
narrative_ontology:measurement(stat_be_t2013, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2013, 0.2).
narrative_ontology:measurement(stat_be_t2023, statutory_debt_ceiling__coordination_scaffold_reading, base_extractiveness, 2023, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statutory_debt_ceiling__coordination_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__coordination_scaffold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__coordination_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, extraction_snare_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__coordination_scaffold_reading, constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the statutory_debt_ceiling kernel: this story (coordination_scaffold_reading, scaffold, ε=0.22), extraction_snare_reading (tangled_rope or snare, high ε, victims = federal beneficiaries/bond markets under crisis conditions), and constitutional_nullity_reading (mountain-adjacent or void classification under 14th Amendment analysis, ε near zero if the nullity claim is correct since no valid constraint would exist to extract). All three read the identical statutory text (31 U.S.C. 3101) but diverge on whether the modal case or tail case is definitional, and whether the statute is even constitutionally operative. Each is authored as a separate ε-invariant story per the decomposition principle; this file does not attempt to average or reconcile the three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
