% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__composite_overdetermination_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: 1971 Gold-Fiat Transition as Convergent Structural Overdetermination (not a discrete policy swap)
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This story reads the 1971 gold-fiat transition not as a single policy
 *   swap enacted by Nixon but as the convergence of several
 *   independently-evolving structural changes: telecommunications
 *   infrastructure enabling near-instant cross-border capital movement
 *   (maturing through the 1960s Eurodollar market), the slow-motion collapse
 *   of Bretton Woods peg viability as national reserve positions diverged, a
 *   shift in labor's bargaining power relative to capital, and the gradual
 *   case-law maturation of legal-tender enforcement that made a purely fiat
 *   currency legally and practically sustainable. On this reading, the Nixon
 *   Shock announcement was a symbolic marker that made visible a transition
 *   already substantially underway on multiple independent timelines — not a
 *   causal node from which the transition can be said to originate. The
 *   theater_ratio spike at 1971 in the measurement series reflects this
 *   directly: the announcement generated a large burst of
 *   symbolic/performative significance (a discrete 'moment' the public and
 *   subsequent historiography could point to) that substantially exceeds its
 *   actual causal weight in the underlying structural shifts, which were
 *   already in motion before and continued after.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.42).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.31).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "1971 Gold-Fiat Transition as Convergent Structural Overdetermination (not a discrete policy swap)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'a7a10e73-5f7a-4b39-a214-4795f58aade5').
narrative_ontology:cs_kernel_codification('a7a10e73-5f7a-4b39-a214-4795f58aade5', distributed).
narrative_ontology:cs_authority_grounding('a7a10e73-5f7a-4b39-a214-4795f58aade5', distributed).
narrative_ontology:cs_reading_relation('a7a10e73-5f7a-4b39-a214-4795f58aade5', gold_fiat_transition_mechanism__automatic_constraint_reading, influences).
narrative_ontology:cs_reading_relation('a7a10e73-5f7a-4b39-a214-4795f58aade5', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_axiom('a7a10e73-5f7a-4b39-a214-4795f58aade5', foundational, causal_overdetermination_denies_singular_mechanism).
narrative_ontology:cs_axiom_status(causal_overdetermination_denies_singular_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('a7a10e73-5f7a-4b39-a214-4795f58aade5', causal_overdetermination_denies_singular_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('a7a10e73-5f7a-4b39-a214-4795f58aade5', secondary, symbolic_events_are_not_causal_nodes).
narrative_ontology:cs_axiom_status(symbolic_events_are_not_causal_nodes, holdable).
narrative_ontology:cs_axiom_grounding('a7a10e73-5f7a-4b39-a214-4795f58aade5', symbolic_events_are_not_causal_nodes, empirically_contingent).
narrative_ontology:cs_reference_frame('a7a10e73-5f7a-4b39-a214-4795f58aade5', bretton_woods_par_value_system).
narrative_ontology:cs_drift_state('a7a10e73-5f7a-4b39-a214-4795f58aade5', post_1971_floating_era_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a7a10e73-5f7a-4b39-a214-4795f58aade5', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, eurodollar_market_participants).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuing_treasury).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, multinational_capital_holders).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_pensioners).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, bretton_woods_peripheral_states).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, domestic_wage_earners_pre_indexation).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, monetary_regime_change_is_multiply_caused).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, single_event_causal_attribution_is_underdetermined).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Offshore dollar banking had already grown large enough by the late 1960s that instant telecommunications-enabled capital flows were arbitraging fixed parities faster than central banks could defend them. These actors profited from the pre-existing drift toward floating rates regardless of what Nixon announced in August 1971; the announcement ratified a capital-mobility reality they were already operating inside.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, eurodollar_market_participants, beneficiary,
    institutional, generational, arbitrage, global).

% The US Treasury retained the capacity to issue the world's reserve asset without a convertibility constraint, but this capacity emerged from decades of maturing legal-tender enforcement and accumulated reserve-currency network effects, not from a single decision. It administers the fiat regime but did not single-handedly cause the conditions that made the regime viable.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuing_treasury, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuing_treasury, agenda_setter).

% Firms and wealth holders able to move capital across borders benefited from the deregulation of exchange rates that followed the convergence of forces, gaining hedging and arbitrage opportunities. Their gains track the composite of technology, peg collapse, and legal changes together, not any one of them.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, multinational_capital_holders, beneficiary,
    organized, generational, mobile, global).

% Retirees and others living on fixed nominal payments bore the erosion from the inflationary decade that followed, an outcome causally entangled with labor bargaining shifts, oil shocks, and monetary discretion together. They could not reallocate into inflation-protected assets in time and had no exit from the currency they were paid in.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_pensioners, payer,
    powerless, biographical, trapped, national).

% Smaller economies pegged to the dollar system absorbed the volatility of the transition without having caused any of the four convergent structural shifts (they controlled none of the telecom infrastructure, peg architecture, labor markets, or legal-tender enforcement of the core states). Their currencies and reserves were repriced by forces entirely external and simultaneous, not by a single US policy act they could have anticipated or negotiated against.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, bretton_woods_peripheral_states, payer,
    moderate, generational, constrained, national).

% Workers whose wages were not indexed to inflation lost real income as the composite of forces (weakening peg discipline, capital mobility undermining bargaining leverage, discretionary monetary policy) unfolded together across the 1970s. No single actor targeted them; the effect emerged from the interaction of independent structural shifts.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, domestic_wage_earners_pre_indexation, payer,
    powerless, biographical, trapped, national).

% Economic historians who argue the transition cannot be pinned to August 15, 1971 as a causal node, pointing to telecommunications-enabled Eurodollar arbitrage from the mid-1960s, the slow unwinding of Bretton Woods parities through the late 1960s, labor's shifting bargaining position, and gradual legal-tender case law — all proceeding on independent timelines that merely became visible together at the Nixon announcement.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, monetary_historians_convergence_school, observer,
    analytical, civilizational, analytical, global).

% Both the automatic-constraint camp and the creditor-discipline camp attribute the transition to a single decisive mechanism centered on the Nixon Shock. They are not structurally excluded from discourse, but this reading excludes their causal framework specifically: if convergence is correct, both single-mechanism accounts misattribute overdetermined change to one causal node.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, single_event_causal_narrative_holders, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No single coordination problem was solved by one mechanism; rather, several independent coordination arrangements (Eurodollar clearing conventions, floating-rate exchange infrastructure, wage-setting norms, and legal-tender case law) each solved a distinct local coordination problem and their simultaneous maturation is what produced the appearance of one transition.
% TRANSFER_FUNCTION: Value moved unevenly across many channels at once: from creditor nations to the reserve-issuing state via seigniorage, from fixed-income holders to debtors via inflation, from labor to capital via weakened bargaining leverage under capital mobility, and from peripheral pegged states to core financial centers via repricing shocks. No single transfer channel accounts for the whole.
% ABSENT_VOICES: The single-event causal narrative holders (both automatic-constraint and creditor-discipline camps) are not literally excluded from the historical record, but their framing is challenged by this reading's core claim; peripheral-state central bankers of the era, whose internal deliberations show they were responding to multiple uncoordinated pressures rather than one US decision, are underrepresented in the dominant Nixon-centric narrative.
% DISAPPEARANCE_RATIONALE: If 'the transition' as a singular event were shown never to have existed as a causal node, the automatic-constraint and creditor-discipline narratives would need substantial revision, but the underlying distributional outcomes (inflation redistribution, capital mobility gains, peripheral-state exposure) would not rearrange, since those outcomes were produced by the independently-operating structural forces regardless of how the historical narrative labels them. The world of outcomes is unchanged; the world of causal attribution is rearranged.
% FOUNDING_PROBLEM: This reading was constructed to solve a historiographical problem: existing accounts treat August 15, 1971 as a discrete causal event, but the underlying structural changes (telecom-enabled capital flows, peg erosion, labor bargaining shifts, legal-tender maturation) demonstrably predate and postdate that date on independent timelines, making single-node causal attribution empirically strained.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the beneficiary set (academic monetary historians with no stake in either the automatic-constraint or creditor-discipline framings, e.g. scholars of Eurodollar market development and Bretton Woods archival research) corroborate that Eurodollar market growth and peg erosion substantially predate 1971; this corroboration comes from analytical observers rather than from any party that gains materially from the convergence framing itself, which is a structural strength of this reading's evidentiary basis.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, contested).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).
:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.42 at interval end) because the composite reading identifies real but diffuse and unevenly distributed transfers — no single extraction channel dominates, and no single beneficiary captures the bulk of the effect, which is exactly what a genuinely overdetermined structural shift should look like as distinct from a designed extraction mechanism. Suppression is moderate-low (0.31) and essentially flat across the interval, reflecting that this reading does not depend on active coercive enforcement of one particular narrative; the various sub-mechanisms (capital mobility, peg abandonment, labor shifts, legal-tender case law) each had their own local enforcement dynamics that this composite reading does not conflate into one suppression figure. Theater ratio is authored with a pronounced spike at 1971 specifically to model the historiographical Goodhart-style substitution this reading diagnoses: the discrete symbolic event (Nixon's announcement) substitutes for and obscures the actual multi-track causal structure in subsequent public and political memory.
 *
 * PERSPECTIVAL GAP:
 *   The reserve-currency-issuing treasury and the analytical observer seat should compute quite differently: from the Treasury's institutional position, the fiat capacity looks like an achieved, administrable authority it now exercises (closer to agenda-setting beneficiary); from the convergence-school historian's analytical seat, that same capacity is the emergent byproduct of forces the Treasury did not singularly cause and only partially controls. This divergence is exactly what the composite-overdetermination reading is built to surface, in contrast to the two single-mechanism sibling readings which each locate the causal weight (and therefore the classification) in one seat's decision.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries here are heterogeneous and none is a designed architect of a unified transition: eurodollar market participants and multinational capital holders benefited from capital mobility they did not design as a coordinated policy, and the reserve-currency-issuing treasury benefited from network effects and legal-tender maturation that long preceded any 1971 decision. Victims are similarly heterogeneous: fixed-income pensioners and non-indexed wage earners suffered from the inflationary consequences of the composite shift, and peripheral pegged states absorbed volatility they had no hand in causing. This diffuseness of both benefit and cost is the central structural signature this reading claims — a designed extraction mechanism would show a concentrated beneficiary; this reading's evidence shows dispersed, overlapping distributional effects tracking several independent causal streams.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy misclassification in a specific way: because no single mechanism or beneficiary can be shown to have caused or captured the whole transition, it is harder to mistake the composite outcome for either pure coordination (a rope) or pure extraction (a snare) engineered by an identifiable party. The tangled_rope claim reflects that real coordination functions existed within each sub-mechanism (Eurodollar clearing norms, exchange infrastructure, wage-bargaining institutions, legal-tender case law) while asymmetric costs landed on parties with no voice in any of the four independent processes — the extraction is real but was not designed by any coordinating party, which is a structurally different mandatrophy risk than either sibling reading's single-actor causal claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_singularity_versus_convergence,
    'Is ''the gold-fiat transition'' a single causal event admitting of competing single-mechanism explanations (as the automatic_constraint and creditor_discipline readings assume), or is the very idea of a unified transition event a retrospective historiographical construction imposed on a convergence of independent processes?',
    'Fine-grained timeline reconstruction comparing the onset dates and independent variance of Eurodollar market growth, individual Bretton Woods peg failures, labor bargaining power indices, and legal-tender case law maturation — if these processes show low temporal correlation and distinct proximate causes, singularity is undermined; if they show tight co-movement triggered by a common shock, singularity is supported.',
    'If convergence is correct, both sibling readings misattribute the effects of independent processes to a single mechanism, and the correct policy and historical lesson is that no single 1971 decision was decisive — reversing or replicating the Nixon announcement would not by itself have prevented or caused the underlying structural shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_singularity_versus_convergence, conceptual, 'Whether the transition kernel names one causal event or a retrospectively unified label over several independent processes.').

omega_variable(
    distributional_diffuseness_versus_hidden_concentration,
    'Is the beneficiary/victim structure genuinely as diffuse as this reading claims, or does deeper analysis reveal a concentrated beneficiary (e.g., the reserve-currency-issuing state specifically) that the composite framing obscures by spreading credit/blame across four mechanisms?',
    'Distributional accounting tracing seigniorage revenue, inflation-tax incidence, and capital-mobility gains to specific institutional actors over 1965-1985, testing whether gains concentrate disproportionately in reserve-currency-adjacent actors despite the multi-mechanism causal story.',
    'If gains concentrate strongly in reserve-currency-issuing and capital-mobile actors, this reading''s moderate extractiveness score and diffuse-beneficiary claim would understate the case and the creditor_discipline_reading''s concentrated-beneficiary account would be closer to correct despite this reading''s causal-plurality argument being separately valid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributional_diffuseness_versus_hidden_concentration, empirical, 'Whether diffuse causal attribution implies diffuse distributional outcomes, or whether these are separable.').

omega_variable(
    nixon_shock_symbolic_weight_measurement,
    'How much of the theater_ratio spike at 1971 reflects genuine contemporaneous overreaction to a symbolic event versus later historiographical simplification imposed retrospectively?',
    'Contemporary press, central bank communications, and market reaction data from August-December 1971 compared against later (post-1980) historical textbook treatments, to separate real-time overreaction from retrospective narrative construction.',
    'If the theater spike is mostly retrospective historiographical artifact rather than contemporaneous market/political overreaction, the theater_ratio measurement series would need re-dating to later time points reflecting when the simplified narrative solidified, not 1971 itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nixon_shock_symbolic_weight_measurement, empirical, 'Whether the symbolic-marker theater effect was contemporaneous or retrospectively constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1958, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1958, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement_basis(gold_tr_t1958, observed).
narrative_ontology:measurement(gold_tr_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement_basis(gold_tr_t1965, observed).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1971, 0.65).
narrative_ontology:measurement_basis(gold_tr_t1971, observed).
narrative_ontology:measurement(gold_tr_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1975, 0.55).
narrative_ontology:measurement_basis(gold_tr_t1975, observed).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1980, 0.5).
narrative_ontology:measurement_basis(gold_tr_t1980, observed).
narrative_ontology:measurement(gold_tr_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement_basis(gold_tr_t1985, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t1958, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1958, 0.18).
narrative_ontology:measurement_basis(gold_be_t1958, observed).
narrative_ontology:measurement(gold_be_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1965, 0.27).
narrative_ontology:measurement_basis(gold_be_t1965, observed).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1971, 0.36).
narrative_ontology:measurement_basis(gold_be_t1971, observed).
narrative_ontology:measurement(gold_be_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement_basis(gold_be_t1975, observed).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement_basis(gold_be_t1980, observed).
narrative_ontology:measurement(gold_be_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement_basis(gold_be_t1985, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1958, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement_basis(gold_su_t1958, observed).
narrative_ontology:measurement(gold_su_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1965, 0.28).
narrative_ontology:measurement_basis(gold_su_t1965, observed).
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1971, 0.32).
narrative_ontology:measurement_basis(gold_su_t1971, observed).
narrative_ontology:measurement(gold_su_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement_basis(gold_su_t1975, observed).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1980, 0.31).
narrative_ontology:measurement_basis(gold_su_t1980, observed).
narrative_ontology:measurement(gold_su_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1985, 0.31).
narrative_ontology:measurement_basis(gold_su_t1985, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.15).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the gold_fiat_transition_mechanism kernel. The automatic_constraint_reading and creditor_discipline_reading each locate the transition's causal weight in a single mechanism (removal of a physical reserve constraint; elimination of creditor veto power, respectively) and treat the Nixon Shock as the causal node. This composite_overdetermination_reading instead treats the label 'the transition' as covering four independently-timed structural processes and treats Nixon's announcement as a symbolic marker rather than a cause. Rather than foreclosing the siblings (their distributional claims about who benefited from the eventual regime may still hold), this reading exerts influence on both by undermining the singular causal node each depends on for its clean pre/post narrative — it does not deny that reserve constraints were removed or that creditor veto power declined, only that either removal can be dated to, or solely attributed to, one 1971 event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
