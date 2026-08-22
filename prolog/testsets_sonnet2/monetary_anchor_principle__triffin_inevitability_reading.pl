% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__triffin_inevitability_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma as Structural Inevitability of Bretton Woods Collapse
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   Robert Triffin's 1960 formalization holds that a country whose currency
 *   serves as the world's reserve asset under fixed gold convertibility faces
 *   two demands that cannot both be met indefinitely: (1) it must run
 *   balance-of-payments deficits to supply the growing world economy with the
 *   liquidity a reserve currency is expected to provide, and (2) it must
 *   maintain enough gold reserves relative to outstanding claims on that
 *   currency to keep the fixed convertibility promise credible. As world
 *   trade and world dollar holdings grew through the 1960s, the ratio of gold
 *   reserves to foreign dollar claims fell steadily, and by the late 1960s
 *   convertibility at $35/oz was sustainable only through increasingly
 *   extraordinary measures (the London Gold Pool, swap lines, moral suasion
 *   on allied central banks not to redeem). This reading holds that the
 *   arithmetic made abandonment a matter of timing, not choice — the system's
 *   founding requirements were jointly unsatisfiable once dollar liquidity
 *   provision outpaced US gold stock growth, a gap that was visible and
 *   quantifiable years before Nixon's August 1971 announcement.
 *
 * KEY AGENTS:
 *   - bretton_woods_institutional_framework: the arrangement itself, structurally incapable of satisfying both its liquidity-supply and convertibility-fixation requirements simultaneously as world dollar claims grew
 *   - united_states_treasury_and_federal_reserve: the reserve issuer whose deficit-financed liquidity provision was mandated by the system's design, not merely by its own policy preference under this reading
 *   - foreign_central_banks_holding_dollar_reserves: holders of the claims whose growth relative to the fixed gold stock constituted the exhaustion
 *   - monetary_economists_of_record: Triffin and successors, an analytical observer seat that formalized the contradiction prior to the event
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.06).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.04).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma as Structural Inevitability of Bretton Woods Collapse").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, 'c1b5a928-4aeb-433b-b2cb-2b17f3b4891c').
narrative_ontology:cs_kernel_codification('c1b5a928-4aeb-433b-b2cb-2b17f3b4891c', distributed).
narrative_ontology:cs_authority_grounding('c1b5a928-4aeb-433b-b2cb-2b17f3b4891c', diffuse_epistemic).
narrative_ontology:cs_reading_relation('c1b5a928-4aeb-433b-b2cb-2b17f3b4891c', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1b5a928-4aeb-433b-b2cb-2b17f3b4891c', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('c1b5a928-4aeb-433b-b2cb-2b17f3b4891c', foundational, reserve_liquidity_and_fixed_convertibility_are_jointly_unsatisfiable_under_growth).
narrative_ontology:cs_axiom_status(reserve_liquidity_and_fixed_convertibility_are_jointly_unsatisfiable_under_growth, holdable).
narrative_ontology:cs_axiom_grounding('c1b5a928-4aeb-433b-b2cb-2b17f3b4891c', reserve_liquidity_and_fixed_convertibility_are_jointly_unsatisfiable_under_growth, empirically_contingent).
narrative_ontology:cs_axiom('c1b5a928-4aeb-433b-b2cb-2b17f3b4891c', secondary, structural_arithmetic_determines_timing_bound_not_agent_choice).
narrative_ontology:cs_axiom_status(structural_arithmetic_determines_timing_bound_not_agent_choice, holdable).
narrative_ontology:cs_axiom_grounding('c1b5a928-4aeb-433b-b2cb-2b17f3b4891c', structural_arithmetic_determines_timing_bound_not_agent_choice, empirically_contingent).
narrative_ontology:cs_reference_frame('c1b5a928-4aeb-433b-b2cb-2b17f3b4891c', fixed_gold_dollar_convertibility_at_bretton_woods_parity).
narrative_ontology:cs_drift_state('c1b5a928-4aeb-433b-b2cb-2b17f3b4891c', id_1971_suspension_of_convertibility, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('c1b5a928-4aeb-433b-b2cb-2b17f3b4891c', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, foreign_central_banks_holding_dollar_reserves).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The fixed-convertibility gold-dollar arrangement that structurally required the reserve issuer to run persistent balance-of-payments deficits to supply world liquidity, while those same deficits mathematically eroded the gold-to-dollar-claims ratio underwriting convertibility. Not an agent that chooses; a design whose two requirements — being the liquidity source and maintaining fixed gold backing — became jointly unsatisfiable as world trade and dollar claims outgrew the fixed gold stock. It has no exit because it is the arrangement being described, not a party within it.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).

% As reserve issuer, obligated by the system's own logic to run the deficits that supplied global dollar liquidity; those same deficits accumulated foreign dollar claims against a gold stock that could not expand to match them. Under this reading, US policy choices (Vietnam spending, domestic fiscal stance) are downstream noise on an arithmetic collapse, not levers that could have avoided it — so the actor most visibly present at the closing of the gold window is analytically excluded from causal responsibility in this reading's account, which is exactly the point of contest with the sibling readings.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, united_states_treasury_and_federal_reserve, excluded,
    institutional, generational, trapped, global).

% Accumulated dollar claims as the system's own liquidity-provision mechanism required, while the redeemability of those claims into gold at a fixed rate became less credible each year the claims grew relative to the US gold stock. They could not individually resolve the underlying arithmetic by any policy choice of their own — their exposure was a structural byproduct of holding the reserve asset the system itself designated.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, foreign_central_banks_holding_dollar_reserves, payer,
    institutional, generational, trapped, global).

% Robert Triffin and successors described the logical structure in the 1960s, prior to the 1971 event, from outside any government mandate. They observe and formalize the contradiction; they collect nothing from the collapse and bear none of its costs, and their prior publication of the dilemma is central to this reading's claim that the outcome was foreseeable/necessary rather than a discretionary policy swap.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, monetary_economists_of_record, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods gold-dollar standard coordinated global trade settlement and reserve holding around a single, fixed-value anchor, avoiding the coordination costs of a multi-currency float and giving smaller economies a stable unit for reserves and invoicing.
% TRANSFER_FUNCTION: Under this reading, nothing is transferred between parties in the extractive sense — the arrangement's two constitutive requirements (supply enough dollars for world liquidity growth; keep gold convertibility fixed) draw down against each other until the joint requirement is mathematically unsatisfiable. What is 'transferred' is credibility: from the convertibility promise to the foreign reserve holders who priced their claims on that promise.
% ABSENT_VOICES: No party is silenced or excluded from a table in this reading, because there is no negotiation to attend — the claim is that the contradiction is arithmetic, not a bargain any coalition could have renegotiated its way out of. The sibling readings (composite, punctuated-swap) would object that this framing erases the real choices US policymakers made (financing Vietnam and Great Society spending via deficits rather than tax increases, delaying suspension for political reasons) that shaped the timing and severity of the exhaustion, even if not its ultimate occurrence.
% DISAPPEARANCE_RATIONALE: If 'the constraint disappeared overnight' is read as 'if the Triffin dilemma were not a real logical structure,' the world would be substantially different: a fixed-convertibility reserve standard could persist indefinitely without forced abandonment, and 1971 would need a different explanation. Under this reading's own lights that counterfactual is incoherent (the arithmetic is what it is), which is why the verdict is contested rather than a clean world_rearranges/world_unchanged — the dispute is precisely whether the 1971 outcome tracks back to this structural necessity or to the discretionary choices the sibling readings foreground.
% FOUNDING_PROBLEM: The Bretton Woods system was built to give the post-WWII world a stable, gold-anchored reserve currency that would avoid the competitive devaluations and monetary chaos of the interwar period, while still allowing enough dollar liquidity to expand with growing world trade.
% FOUNDING_PROBLEM_CORROBORATION: Robert Triffin's own 1960 analysis (Gold and the Dollar Crisis), written from an academic seat with no institutional stake in Bretton Woods' survival, corroborates that the founding problem's dual requirements were jointly unsatisfiable well before the 1971 suspension — an analysis made and published from outside the US Treasury, the IMF, and the foreign central banks whose reserves were at stake.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, contested).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, ExtMetricName, E),
    domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.06) because under this reading nothing is being extracted by an agent from another agent — the failure is a joint infeasibility of two design requirements, not a rent or a transfer engineered by a party who benefits. Suppression is near-zero (0.04): no coercive apparatus is needed to produce the exhaustion of gold reserves; the arithmetic does the work regardless of enforcement. Accessibility collapse is authored high (0.90) because, on this reading's own terms, there is no policy alternative available once the joint requirement becomes infeasible — the reading's entire claim is that no amount of political will or alternative macro policy could have kept fixed convertibility and adequate liquidity provision reconciled indefinitely. Resistance is low (0.08): the sibling readings resist the CLAIM that the outcome was structurally forced, but the mathematics of gold-stock-versus-claims ratio itself meets no resistance — it is exogenous to political preference. Measurements are flat and low across the interval because this reading treats the underlying contradiction as present and building from the late 1950s onward (Triffin published in 1960) rather than as a metric that spikes at a discrete decision point — the low, gently rising trajectory reflects a maturing but never-forced-by-any-agent arithmetic gap, in contrast to how a punctuated-swap reading would author a discontinuity at 1971 itself.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary is declared because this reading holds the outcome to be a system-level failure with no party positioned to collect from it — the framework itself pays the cost of its own internal contradiction becoming manifest, which is why bretton_woods_institutional_framework is listed as a non-agent victim rather than any beneficiary being named. The US Treasury and foreign central banks are declared payer/excluded rather than beneficiary because, on this reading, their exposure is a structural byproduct of occupying the roles the system assigned them (reserve issuer, reserve holder), not a return they sought or captured. This is the central directionality claim distinguishing this reading from the sibling readings, which would locate identifiable choosers (US policymakers weighing Vietnam financing against gold discipline) whose decisions could in principle have gone otherwise.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists being classified as mandatrophy (a mandate that outlived its function but persists by inertia) because the founding_problem's death (dead, not contested, under this reading) is not attributed to institutional drift or captured administration — it is attributed to the founding design itself being internally contradictory from very early in its operation. There is no agenda_setter who could have 'fixed' the arrangement cheaply; the fixing_cost is prohibitive in the strict sense that no policy adjustment within the fixed-convertibility framework could resolve the joint infeasibility — only abandoning one of the two founding requirements (fixed convertibility or liquidity provision) could. This is why the claimed type is mountain rather than piton or tangled_rope: the constraint is not being maintained past its usefulness by any party's interest, it simply describes a logical/arithmetic ceiling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_necessity_vs_policy_choice_framing,
    'Is the 1971 suspension of gold convertibility better modeled as an arithmetic inevitability (this reading) or as a discrete institutional choice among live alternatives at a specific moment (the punctuated_swap_reading), or as one of several jointly sufficient causes with no single dominant structural factor (the overdetermined_composite_reading)?',
    'Counterfactual economic-historical analysis of whether alternative US fiscal/monetary policy paths in the mid-to-late 1960s (e.g., tax increases to finance Vietnam without dollar outflow growth, earlier gold price revaluation, tighter capital controls) could have preserved convertibility indefinitely, versus whether such measures could only have delayed exhaustion by a bounded number of years given trade growth trajectories.',
    'If delay-only is correct, this reading''s inevitability claim is strongly supported and the type properly sits at mountain with near-zero extraction. If meaningful indefinite-preservation paths existed, the constraint is better modeled as policy-contingent, which would push toward the punctuated_swap or composite readings and raise this reading''s own claimed_type into question (it would look more like a scaffold or tangled rope than a mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_necessity_vs_policy_choice_framing, conceptual, 'Whether the Triffin dynamic constitutes strict mathematical inevitability or a severely constrained-but-not-unique policy trajectory — the load-bearing ambiguity separating this reading from its siblings.').

omega_variable(
    no_beneficiary_completeness,
    'Is it true that no agent structurally benefited from the Bretton Woods gold-dollar arrangement''s eventual collapse, or did some actor (e.g., US exporters who gained from post-1971 dollar depreciation, or the Federal Reserve gaining discretionary monetary policy freedom) receive a de facto windfall from the transition that this reading''s ''no beneficiary'' framing omits?',
    'Historical analysis of US trade balance, monetary policy autonomy, and asset price effects in the years immediately following August 1971, isolated from confounding effects of the 1973 oil shock.',
    'If a clear beneficiary of the TRANSITION (as distinct from the pre-1971 arrangement) is identified, this reading''s mountain classification for the transition event itself (as opposed to the pre-existing constraint) may need separate treatment — this story''s ε and stakeholder set describe the arithmetic contradiction, not the post-1971 regime, and that boundary should not be blurred.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(no_beneficiary_completeness, empirical, 'Whether the declared absence of a beneficiary holds for the full episode or only for the pre-collapse structural arrangement this story is scoped to.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1958, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1958, 0.03).
narrative_ontology:measurement(mone_tr_t1961, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1961, 0.03).
narrative_ontology:measurement(mone_tr_t1964, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1964, 0.04).
narrative_ontology:measurement(mone_tr_t1967, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1967, 0.045).
narrative_ontology:measurement(mone_tr_t1969, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1969, 0.05).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.05).

% Extraction over time
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1958, 0.02).
narrative_ontology:measurement(mone_be_t1961, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1961, 0.03).
narrative_ontology:measurement(mone_be_t1964, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1964, 0.04).
narrative_ontology:measurement(mone_be_t1967, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1967, 0.05).
narrative_ontology:measurement(mone_be_t1969, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1969, 0.055).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monetary_anchor_principle__triffin_inevitability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__triffin_inevitability_reading, 0.15).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the monetary_anchor_principle kernel decomposing the natural-language claim 'the Bretton Woods system collapsed in 1971.' triffin_inevitability_reading (this file) authors the event as mountain-type structural necessity with near-zero ε and no beneficiary. punctuated_swap_reading authors it as a discrete institutional choice with identifiable choosers and live alternatives. overdetermined_composite_reading authors it as a multi-causal convergence in which Triffin's arithmetic is one contributing factor among several (Vietnam deficits, Keynesian consensus, capital mobility) with no single factor claimed as sufficient alone. Each reading carries its own claimed_type, ε, and stakeholder set per the ε-invariance principle; they are linked here rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
