% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Triffin Dilemma as Structural Inevitability of Gold-Standard Collapse
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the 'triffin_inevitability_reading' of the
 *   contested 'monetary_anchor_principle' kernel: the claim that the 1971
 *   collapse of the gold-dollar convertibility system was mathematically
 *   forced by the Triffin dilemma alone — a reserve-currency issuer under
 *   fixed gold convertibility must run balance-of-payments deficits to supply
 *   the world with liquidity, and those deficits mechanically deplete gold
 *   reserves relative to outstanding dollar claims until convertibility
 *   becomes physically impossible to sustain. Under this reading the
 *   transition is a mountain: a logical/arithmetic limit, not a policy
 *   choice, not an extraction mechanism, and not attributable to any single
 *   decision-maker's discretion. Sibling readings of the same kernel (not
 *   authored here) treat the same historical transition as an overdetermined
 *   composite of multiple contingent pressures, or as a discrete
 *   institutional swap decided on a specific date — those are different
 *   constraints with different ε profiles, linked via
 *   network.affects_constraints, not folded into this one.
 *
 * KEY AGENTS:
 *   - bretton_woods_institutional_framework: structural victim (institutional/trapped) — bears the full cost of the contradiction it cannot resolve
 *   - united_states_treasury: reserve issuer administering the eventual reckoning (institutional/constrained) — inherits, does not create, the bind
 *   - foreign_central_banks: reserve holders facing an escalating collective-action problem (powerful/constrained)
 *   - monetary_economists: analytical observers who formalized and predicted the contradiction (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.08).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.05).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma as Structural Inevitability of Gold-Standard Collapse").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '98b37df0-d592-43b0-bdb0-34f3ba50cdc5').
narrative_ontology:cs_kernel_codification('98b37df0-d592-43b0-bdb0-34f3ba50cdc5', distributed).
narrative_ontology:cs_authority_grounding('98b37df0-d592-43b0-bdb0-34f3ba50cdc5', distributed).
narrative_ontology:cs_reading_relation('98b37df0-d592-43b0-bdb0-34f3ba50cdc5', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('98b37df0-d592-43b0-bdb0-34f3ba50cdc5', monetary_anchor_principle__punctuated_swap_reading, influences).
narrative_ontology:cs_axiom('98b37df0-d592-43b0-bdb0-34f3ba50cdc5', foundational, single_issuer_fixed_convertibility_is_mathematically_unsustainable).
narrative_ontology:cs_axiom_status(single_issuer_fixed_convertibility_is_mathematically_unsustainable, holdable).
narrative_ontology:cs_axiom_grounding('98b37df0-d592-43b0-bdb0-34f3ba50cdc5', single_issuer_fixed_convertibility_is_mathematically_unsustainable, empirically_contingent).
narrative_ontology:cs_axiom('98b37df0-d592-43b0-bdb0-34f3ba50cdc5', secondary, policy_discretion_affects_only_timing_not_terminal_outcome).
narrative_ontology:cs_axiom_status(policy_discretion_affects_only_timing_not_terminal_outcome, holdable).
narrative_ontology:cs_axiom_grounding('98b37df0-d592-43b0-bdb0-34f3ba50cdc5', policy_discretion_affects_only_timing_not_terminal_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('98b37df0-d592-43b0-bdb0-34f3ba50cdc5', fixed_gold_dollar_convertibility_at_35_per_ounce).
narrative_ontology:cs_drift_state('98b37df0-d592-43b0-bdb0-34f3ba50cdc5', post_1971_nixon_shock, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('98b37df0-d592-43b0-bdb0-34f3ba50cdc5', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, united_states_treasury).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, foreign_central_banks).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, triffin_dilemma_formalization).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, reserve_currency_structural_contradiction_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The fixed-parity gold-dollar exchange system was built to anchor postwar reconstruction and trade. As global trade and reserve demand grew, the system had no internal mechanism to reconcile the issuer's need to run deficits (supplying liquidity) with the fixed convertibility promise (requiring gold sufficiency). The framework bears the full cost of this contradiction: it cannot adjust either requirement without dissolving itself, and it has no exit from the arithmetic that undoes it.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework, payer,
    institutional, generational, trapped, global).

% As reserve issuer, obligated to supply dollars for global liquidity while nominally maintaining gold convertibility at $35/oz. Running the deficits the system required steadily depleted gold reserves relative to outstanding dollar liabilities. It administered the eventual suspension (August 1971) but did not create the underlying arithmetic; it inherited a structural bind and could only choose the timing and manner of the reckoning, not whether one would occur.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, united_states_treasury, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__triffin_inevitability_reading, united_states_treasury, agenda_setter).

% Held mounting dollar reserves as global trade expanded, and depended on the credibility of dollar-gold convertibility to hold those reserves rather than gold itself. As the gap between outstanding dollar claims and available U.S. gold widened, they faced an escalating collective-action problem: any bank redeeming dollars for gold accelerated the reserve depletion for all others, but holding dollars exposed them to the eventual devaluation the arithmetic implied.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, foreign_central_banks, payer,
    powerful, generational, constrained, global).

% The scale of postwar trade and cross-border capital growth is the demand-side driver behind the liquidity requirement the Triffin dilemma formalizes; it is not itself a decision-making agent but the structural condition against which the gold-dollar system's arithmetic became untenable.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, international_trade_and_capital_flows, excluded,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, international_trade_and_capital_flows).

% Robert Triffin and successors formalized the logical structure showing that any single national currency serving as global reserve asset under a fixed convertibility rule faces an irreconcilable conflict between liquidity provision and reserve confidence. They observe and document the contradiction; they do not administer or benefit from the system's operation.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None at the level of this reading — the constraint identifies a logical/arithmetic contradiction in the gold-dollar system's design, not a coordination mechanism anyone is running. The prior Bretton Woods arrangement did solve a coordination problem (postwar exchange stability), but the Triffin dilemma names the mathematical limit that arrangement could not escape, not a service it renders.
% TRANSFER_FUNCTION: No party-to-party transfer occurs at this level; the arithmetic itself transfers nothing between agents. What is 'transferred' is the depletion of a finite stock (gold reserves) against a growing flow (dollar liabilities) until the fixed ratio the system promised becomes physically unsustainable.
% ABSENT_VOICES: There is no excluded human party whose objection was silenced — the constraint under this reading is a structural/mathematical fact, not a decision anyone could have voted differently. To the extent a voice is absent, it is the counterfactual designer who might have built a multi-currency or SDR-based reserve system from the outset, avoiding single-issuer dependency; that path was foreclosed by 1944-era institutional choices outside this constraint's scope.
% DISAPPEARANCE_RATIONALE: This reading claims the Triffin dilemma is a structural/logical necessity, not a policy choice. If 'it' disappeared, that would mean the underlying arithmetic (finite gold stock vs. growing reserve-currency liability) stopped applying — which is not a coherent counterfactual for a fixed-convertibility single-reserve-currency system. The world does not rearrange because nothing agentic is holding this in place; it is the condition itself. (Compare: the Bretton Woods framework's disappearance, addressed structurally via the victim declaration, is a separate, real historical event this reading explains rather than causes.)
% FOUNDING_PROBLEM: Under this reading there is no 'founding problem' the constraint was built to solve — the Triffin dilemma is not an institution someone constructed for a purpose; it is the diagnosed logical consequence of a design choice (fixed gold convertibility plus single reserve-currency liquidity provision) made for other reasons (postwar exchange-rate stability, dollar credibility).
% FOUNDING_PROBLEM_CORROBORATION: There is no founding problem in the constructed-institution sense to corroborate; the closest analogue is Robert Triffin's own 1959-1960 congressional testimony predicting the contradiction before it fully manifested, corroborated ex post by the actual 1971 gold-window suspension and by subsequent economic historiography (Eichengreen, Bordo) written well outside any party that benefited from the gold-dollar system's continuation. The status is 'dead' in the sense that the gold-anchored version of the problem no longer exists post-1971; the structural pattern (reserve-currency liquidity/confidence conflict) persists in modified form under fiat, which is precisely the boundary this reading does not extend past.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_unchanged).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.08, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored very low (0.08 at interval end) because under this reading nothing is being extracted by anyone from anyone — the arithmetic imposes a physical/logical constraint, not a rent. Suppression is low (0.05) because no coercive apparatus enforces the contradiction; it simply obtains given the fixed parameters (finite gold stock, growing dollar liability, fixed convertibility rate). Accessibility collapse is high (0.88): once the Triffin arithmetic is understood, there is no alternative reading under which the gold-dollar system could have continued indefinitely without either abandoning fixed convertibility or abandoning the reserve-currency liquidity role — the alternatives collapse to zero within this reading's own premises. Resistance is low (0.15): historical resistance to the *outcome* was intense (foreign banks, policymakers, markets), but resistance to the underlying arithmetic itself is not coherent — you cannot resist a contradiction, only delay its manifestation, which is what the 1960s gold-pool interventions and swap-line arrangements did.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap authored here by design — a mountain, correctly classified, should compute similarly close to mountain from most seats, because the constraint is a shared arithmetic fact rather than a structure some seats experience as coordination and others as extraction. Where divergence would appear is between this reading's seats and the sibling readings' seats (e.g., the punctuated_swap_reading's Treasury seat would carry much higher agency/discretion), which is exactly why those are separate constraint stories rather than alternate observables on this one.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary is declared because this reading holds the transition to be a system-level structural failure with no party positioned to profit from the contradiction itself (distinguish sharply from the historical fact that some parties profited from delaying or from the eventual devaluation — those are downstream effects, not what this constraint measures). Bretton Woods as an institutional framework is named victim because it is the entity whose design the contradiction dissolves; it has no exit (trapped) because an institutional framework cannot itself relocate or arbitrage away from its own founding arithmetic. The U.S. Treasury and foreign central banks are payers with constrained exit because each faced real costs from the unwind, but their situations are downstream administration of the mountain, not extraction by it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists the temptation to read Bretton Woods' collapse as mandatrophy (an arrangement whose founding problem died but which persisted anyway) — under this reading, the founding problem (postwar exchange stability via fixed gold-dollar convertibility) did not die of obsolescence; it died of an internal mathematical contradiction that was present from the design's inception and became binding as global trade volume grew. Classifying this as mountain rather than piton or snare prevents mislabeling a structural inevitability as either institutional inertia (piton) or extraction (snare) — the corpus needs this distinction available precisely because the sibling readings (composite, punctuated-swap) risk collapsing the same historical event into contingent-choice narratives that would wrongly imply an alternative policy path existed within the gold-standard framework itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gold_scarcity_vs_policy_choice_ambiguity,
    'Is the depletion of U.S. gold reserves relative to dollar liabilities a pure arithmetic/physical necessity (mountain), or did discretionary U.S. fiscal and monetary policy choices (deficit spending scale, monetary expansion rate) determine the TIMING and SEVERITY of the exhaustion in ways that make this partly a constructed outcome rather than a pure structural fact?',
    'Counterfactual economic modeling: hold global trade growth and reserve demand constant, vary only U.S. fiscal/monetary policy parameters (deficit size, gold-pool interventions, capital controls) across the 1958-1971 period, and observe whether gold-window suspension remains inevitable within a plausible range of alternative policy paths, or whether some policy paths meaningfully forestall it beyond a generation.',
    'If the timing/severity is substantially policy-sensitive, this reading''s mountain classification would need qualification — the underlying Triffin logic (any single-issuer fixed-convertibility reserve system eventually faces the contradiction) may remain a mountain, but the SPECIFIC 1971 date and manner would shift toward the punctuated_swap_reading''s territory. This is precisely the ambiguity the kernel''s multiple readings exist to hold separately rather than resolve within one story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_scarcity_vs_policy_choice_ambiguity, conceptual, 'Whether the Triffin dilemma determines only the eventual necessity of collapse (mountain) or also its specific historical timing (which would import policy contingency).').

omega_variable(
    beneficiary_absence_ambiguity,
    'Does the declared absence of beneficiaries hold even at finer historical resolution — did U.S. seigniorage benefits from dollar-as-reserve-currency status during the 1944-1971 period constitute a beneficiary structure that this reading''s mountain framing suppresses by focusing only on the terminal contradiction?',
    'Historical accounting of U.S. seigniorage gains and ''exorbitant privilege'' benefits during the Bretton Woods period, compared against the costs the same period imposed on foreign reserve holders, to determine whether a genuine beneficiary/victim asymmetry existed alongside the terminal structural contradiction.',
    'If a real beneficiary (the U.S., via seigniorage) is identifiable across the full interval rather than only at the point of collapse, this would push the constraint toward a false-summit-mountain concern (FSM) — a structure presented as pure natural necessity that in fact had an identifiable beneficiary throughout its operation. This story deliberately declares no beneficiaries under the terminal-collapse framing; the omega flags that a full-interval framing might differ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_absence_ambiguity, empirical, 'Whether U.S. seigniorage benefits during 1944-1971 constitute a suppressed beneficiary structure inconsistent with pure mountain classification.').

omega_variable(
    kernel_framing_choice_ambiguity,
    'Given that this kernel supports at least three structurally distinct readings (pure inevitability/mountain, overdetermined composite, punctuated institutional swap), what determines which reading an analyst selects, and is that selection itself value-laden (e.g., structuralist vs. agency-centered historiographical commitments)?',
    'This is a conceptual/framing question rather than an empirically resolvable one; it would be informed by comparing which reading each school of international political economy (structuralist IPE vs. decision-theoretic diplomatic history) tends to adopt, and whether the choice tracks prior theoretical commitments rather than the historical evidence itself.',
    'If reading-selection tracks prior theoretical commitment rather than evidence, all three readings should be maintained as permanently coexisting rather than one being resolved as ''more correct'' — supporting the coexists_with relations declared in cs_structure rather than any foreclosure between them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_ambiguity, conceptual, 'Whether the choice among the kernel''s readings is evidence-driven or theory-driven, bearing on whether the readings should ever be expected to converge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1944, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(mone_tr_t1950, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1958, 0.06).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1968, 0.09).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(mone_be_t1944, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1944, 0.03).
narrative_ontology:measurement(mone_be_t1950, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1950, 0.04).
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1958, 0.05).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1965, 0.06).
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1968, 0.07).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monetary_anchor_principle__triffin_inevitability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the 'monetary_anchor_principle' kernel (the natural-language label 'why the gold standard collapsed in 1971' conflates them). 'triffin_inevitability_reading' (this story) claims mountain: pure structural/arithmetic necessity, negligible extraction, no beneficiary, victim is the institutional framework itself. 'overdetermined_composite_reading' claims a higher-ε, multi-causal account where Vietnam deficits, Keynesian policy consensus, and capital mobility compound with Triffin pressure — plausibly tangled_rope or scaffold given identifiable policy beneficiaries (U.S. fiscal expansion beneficiaries) alongside the coordination function. 'punctuated_swap_reading' claims a discrete agentic decision (the August 15, 1971 Nixon Shock) with identifiable decision-makers and possibly beneficiaries/victims from the specific manner of the swap — likely scaffold or tangled_rope given deliberate policy discretion. Each carries its own ε and its own stakeholder set; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
