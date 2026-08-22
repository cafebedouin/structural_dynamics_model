% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Gold-to-Fiat Transition as Convergent Structural Overdetermination
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This story instantiates the composite-overdetermination reading of the
 *   gold-fiat transition kernel: rather than a single decisive policy act
 *   (Nixon's August 1971 suspension of gold convertibility) causing the shift
 *   from metal-anchored to fiat money, the reading holds that at least four
 *   independent structural processes converged over roughly two decades —
 *   telecommunications infrastructure enabling near-instant cross-border
 *   capital movement, the accumulating structural exhaustion of the Bretton
 *   Woods peg system (dollar overhang, triffin dilemma dynamics), a shift in
 *   labor's bargaining position that fed wage-price dynamics independent of
 *   the monetary anchor, and the gradual legal and institutional maturation
 *   of fiat legal tender enforcement mechanisms. The Nixon Shock announcement
 *   is treated here as a symbolic marker that crystallized public attention
 *   around a transition already substantially underway, not as the causal
 *   hinge the other two kernel readings treat it as.
 *
 * KEY AGENTS:
 *   - multinational_capital_holders: primary beneficiary of capital mobility (institutional/arbitrage) — gains predate and exceed any single 1971 policy act
 *   - reserve_currency_issuing_state: agenda-setter and beneficiary (institutional/arbitrage) — administers legal tender enforcement amid forces it did not fully control
 *   - fixed_income_savers: primary target (powerless/trapped) — bears inflation costs traceable to no single mechanism
 *   - developing_country_commodity_exporters: secondary target (powerless/constrained) — bears exchange volatility from converging causes
 *   - domestic_wage_labor: dual position (moderate/constrained) — simultaneously a causal contributor (bargaining shift) and cost-bearer
 *   - economic_historians_singularity_school: analytical observer whose frameworks structurally exclude the composite reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.42).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.35).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-to-Fiat Transition as Convergent Structural Overdetermination").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'b60055c4-62f5-4afc-a1c5-7cb9e7c44e73').
narrative_ontology:cs_kernel_codification('b60055c4-62f5-4afc-a1c5-7cb9e7c44e73', distributed).
narrative_ontology:cs_authority_grounding('b60055c4-62f5-4afc-a1c5-7cb9e7c44e73', distributed).
narrative_ontology:cs_reading_relation('b60055c4-62f5-4afc-a1c5-7cb9e7c44e73', gold_fiat_transition_mechanism__automatic_constraint_reading, influences).
narrative_ontology:cs_reading_relation('b60055c4-62f5-4afc-a1c5-7cb9e7c44e73', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_axiom('b60055c4-62f5-4afc-a1c5-7cb9e7c44e73', foundational, no_single_causal_node_produced_the_transition).
narrative_ontology:cs_axiom_status(no_single_causal_node_produced_the_transition, holdable).
narrative_ontology:cs_axiom_grounding('b60055c4-62f5-4afc-a1c5-7cb9e7c44e73', no_single_causal_node_produced_the_transition, empirically_contingent).
narrative_ontology:cs_axiom('b60055c4-62f5-4afc-a1c5-7cb9e7c44e73', foundational, kernel_singularity_is_a_retrospective_narrative_artifact).
narrative_ontology:cs_axiom_status(kernel_singularity_is_a_retrospective_narrative_artifact, holdable).
narrative_ontology:cs_axiom_grounding('b60055c4-62f5-4afc-a1c5-7cb9e7c44e73', kernel_singularity_is_a_retrospective_narrative_artifact, conventional).
narrative_ontology:cs_reference_frame('b60055c4-62f5-4afc-a1c5-7cb9e7c44e73', multi_causal_structural_convergence).
narrative_ontology:cs_drift_state('b60055c4-62f5-4afc-a1c5-7cb9e7c44e73', post_1980s_monetarist_consensus_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b60055c4-62f5-4afc-a1c5-7cb9e7c44e73', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, multinational_capital_holders).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuing_state).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_intermediation_sector).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, developing_country_commodity_exporters).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, domestic_wage_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, domestic_wage_labor).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, monetary_regime_change_is_multiply_caused).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, single_causal_node_narratives_are_underdetermined).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained from telecommunications-enabled instant capital mobility that predates and outlives any single 1971 announcement; can move capital across jurisdictions in response to interest-rate and currency differentials that a gold-anchored system would have dampened. Their advantage derives from the technology and deregulation convergence, not from a discretionary-authority swap alone.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, multinational_capital_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Administers legal tender enforcement, presides over the collapse of the Bretton Woods peg system it could no longer sustain, and benefits from seigniorage and reserve-currency demand that arose from a confluence of forces — declining gold reserves relative to dollar liabilities, allied nations' reluctance to force redemption, and the maturing infrastructure of dollar-denominated trade settlement.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuing_state, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuing_state, beneficiary).

% Built new hedging, derivatives, and cross-border settlement business on floating exchange rate volatility that only became profitable once telecommunications infrastructure, legal tender enforcement, and peg abandonment converged; no single actor engineered this opportunity, but the sector captured it comprehensively.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_intermediation_sector, beneficiary,
    organized, generational, mobile, global).

% Bore the cost of 1970s inflation as the nominal anchor loosened, but the causal chain running to their losses passes through labor bargaining shifts, legal tender enforcement maturation, and peg collapse simultaneously — no single decision point can be pointed to as the mechanism that impoverished them.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers, payer,
    powerless, biographical, trapped, national).

% Experienced volatile terms of trade once commodity prices, previously stabilized by fixed exchange rates, began floating against a dollar whose value depended on multiple converging domestic and international forces rather than a single policy lever they might have lobbied against.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, developing_country_commodity_exporters, payer,
    powerless, biographical, constrained, global).

% Labor bargaining power shifts were themselves one of the independent structural changes constituting the transition, not merely its consequence — wage-price dynamics fed back into the very inflation the transition is often blamed for, making labor simultaneously a causal contributor and a bearer of the resulting instability.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, domestic_wage_labor, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, domestic_wage_labor, beneficiary).

% Historians and economists committed to the automatic-constraint or creditor-discipline readings treat the Nixon Shock as the causal hinge; their frameworks structurally exclude the possibility that no single hinge exists, so this reading's evidence base is often talked past rather than engaged.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, economic_historians_singularity_school, excluded,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No single coordination function exists because no single mechanism exists — telecommunications infrastructure coordinated capital markets, Bretton Woods coordinated fixed exchange rates until it could not, labor bargaining coordinated wage-price expectations, and legal tender laws coordinated domestic currency acceptance. Each solved a distinct coordination problem independently.
% TRANSFER_FUNCTION: Multiple distinct transfers occurred concurrently and are frequently conflated: capital mobility transferred bargaining leverage from states to mobile capital; peg collapse transferred exchange-rate risk from governments to private actors; legal tender maturation transferred monetary discretion from a metal-backed rule to central bank judgment; labor bargaining shifts transferred real income between capital and labor independent of the monetary regime.
% ABSENT_VOICES: Proponents of the automatic-constraint and creditor-discipline readings are not absent from the historical record but are structurally unable, within their own frameworks, to register that the changes they attribute to a single policy decision were already underway via independent channels — their framing forecloses the composite view rather than merely disagreeing with it.
% DISAPPEARANCE_RATIONALE: If one insists the 'transition' vanished, the singularity readings say the world reorganizes around a restored automatic gold constraint or restored creditor discipline; the composite reading says nothing coherent disappears because there was no unified mechanism to remove — the telecommunications infrastructure, peg dynamics, labor shifts, and legal tender enforcement would each require separate, unrelated reversals. The verdict is contested precisely because the three readings disagree about what 'it' even refers to.
% FOUNDING_PROBLEM: The kernel narrative was built to explain a genuinely confusing multi-year period (roughly 1968-1976) in which fixed exchange rates ended, gold convertibility was suspended, inflation accelerated, and central bank discretion visibly expanded — observers sought a single origin story to organize these simultaneous developments.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside any single beneficiary group (e.g., Barry Eichengreen's work on the Bretton Woods system's structural exhaustion, and international relations scholarship on the trilemma of fixed exchange rates, capital mobility, and monetary autonomy) corroborate that the underlying pressures were operating independently well before August 1971; this is not asserted only by parties who benefit from diffusing responsibility for the transition's costs.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, contested).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate (0.42 at interval end) because the composite reading denies any single extractive mechanism concentrated enough to produce high ε — the costs and benefits are distributed across multiple uncoordinated processes with different winners and losers, none of which alone rises to a snare-level extraction. Suppression is moderate-low (0.35): no single party actively suppresses alternative accounts through coercion, though the analytical suppression of the composite view within rival scholarly and political frameworks is real and is captured structurally, not metrically. Theater ratio rises sharply around 1971 (0.6) reflecting the outsized symbolic/performative weight placed on the Nixon Shock announcement relative to its actual causal contribution — the announcement performed a decisiveness the underlying process did not possess, then settles to a still-elevated 0.53-0.55 as the singular-event narrative persists in textbooks and political rhetoric despite the underlying multi-causal reality. Accessibility collapse is moderate (0.5): the composite explanation remains available to careful historians but has substantially collapsed in popular and political discourse in favor of the simpler single-event narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (mobile capital, the reserve-currency state, financial intermediaries) sit toward the low-d end because multiple independent tailwinds (technology, peg collapse, enforcement maturation) compounded in their favor without requiring active extraction from any specific victim group. Victims (fixed-income savers, commodity exporters, wage labor in its cost-bearing aspect) sit toward higher d because the convergent instability was imposed on them by forces outside their control, though the diffusion of causation across four mechanisms means the extraction is less concentrated and more attributable to systemic drift than to a single coercive actor — this is why the reading claims moderate, not high, ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading resists mandatrophy-style mislabeling in a specific way: it denies that a single 'mandate' (gold-backing-as-constraint, or creditor-discipline-as-constraint) was ever the operative mechanism, so it cannot claim that mechanism has outlived its purpose in the way the other two readings might. Instead, it identifies each of the four sub-processes as having its own founding problem and its own obsolescence trajectory — telecommunications infrastructure was never a 'mandate' to begin with, legal tender enforcement matured rather than was founded-then-outlived. This reading's genealogical claim is therefore narrower and more defensible: not 'the mandate died and the shell persists' but 'there was no single mandate to die.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_singularity_versus_composite_causation,
    'Is ''the gold-fiat transition'' a single structural event admitting of one dominant causal mechanism (as the automatic-constraint and creditor-discipline readings assume), or is it better modeled as a label retrospectively applied to four independent, differently-timed structural processes that happen to have become visible around the same historical moment?',
    'Fine-grained historical dating of each sub-process (telecommunications capital-mobility infrastructure rollout, Bretton Woods peg stress accumulation, labor bargaining power indices, legal tender enforcement case law) against the presumed 1971 hinge date; if the sub-processes show materially different onset and maturation timelines uncorrelated with August 1971, the composite reading is supported. If they cluster tightly around the Nixon announcement with clear causal dependency on it, a singular reading is supported instead.',
    'If composite causation is vindicated, both sibling readings (automatic_constraint_reading and creditor_discipline_reading) misattribute causality to a non-existent unified transition and their extraction/beneficiary claims should be re-read as partial, mechanism-specific claims rather than accounts of ''the'' transition. If a singular mechanism is vindicated, this reading''s moderate ε and diffuse-beneficiary structure would need substantial revision toward whichever sibling reading''s concentrated mechanism proves dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_singularity_versus_composite_causation, conceptual, 'Whether the kernel names one event or retrospectively bundles several.').

omega_variable(
    diffuse_versus_concentrated_beneficiary_structure,
    'Does the absence of a single dominant beneficiary group (as claimed by this reading) genuinely reflect distributed, uncoordinated gains, or does it mask a coordinated set of beneficiaries (financial capital, reserve-currency state) whose gains are simply harder to attribute to any one mechanism because they profited from all four converging processes simultaneously?',
    'Wealth and income concentration data cross-referenced against exposure to each of the four sub-processes (capital mobility exposure, peg-collapse exposure, bargaining-power exposure, legal-tender-enforcement exposure) — if the same actors show high exposure-linked gains across all four channels, apparent diffusion may be concentration in disguise.',
    'If beneficiaries turn out to be substantially the same actors across all four channels, this reading''s ε should rise and its claimed_type should be reconsidered toward tangled_rope-with-concentrated-beneficiary or even snare, undermining the composite reading''s core distinguishing claim of no-single-beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffuse_versus_concentrated_beneficiary_structure, empirical, 'Whether apparent beneficiary diffusion is real or an artifact of multi-channel attribution difficulty.').

omega_variable(
    cs_framing_kernel_versus_no_kernel,
    'Is there a genuine contested kernel here at all (a single stabilized commitment that different readings interpret differently), or does the composite reading''s core claim — that no unified mechanism exists — actually deny that a ''kernel'' in the CS sense was ever present, making the entire kernel-reading framing itself a category error for this constraint?',
    'Compare this case structurally against clearer kernel cases (constitutional text, doctrinal commitments) where a genuine fixed commitment is read differently; assess whether ''the 1971 transition'' functions analogously (a fixed reference point multiple parties interpret) or whether it is simply a historical period with no canonical textual or institutional anchor.',
    'If no genuine kernel exists, this reading''s framing as ''one reading among siblings'' is itself an artifact of the generation manifest rather than a structural fact about the domain — the three readings would be better modeled as three competing historical explanations of a period, not three readings of one kernel. This would not change ε but would change how the network relationships and cs_structure fields should be interpreted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_versus_no_kernel, conceptual, 'Whether ''gold_fiat_transition_mechanism'' is a genuine CS kernel or a manifest-imposed framing over an ordinary historical-causal dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1958, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1958, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement(gold_tr_t1962, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement(gold_tr_t1966, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1966, 0.32).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1971, 0.6).
narrative_ontology:measurement(gold_tr_t1974, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1974, 0.53).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1980, 0.55).

% Extraction over time
narrative_ontology:measurement(gold_be_t1958, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1958, 0.22).
narrative_ontology:measurement(gold_be_t1962, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1962, 0.27).
narrative_ontology:measurement(gold_be_t1966, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1966, 0.31).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1971, 0.36).
narrative_ontology:measurement(gold_be_t1974, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1974, 0.4).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1980, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gold_fiat_transition_mechanism__composite_overdetermination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_discipline_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the gold_fiat_transition_mechanism kernel. automatic_constraint_reading locates the mechanism in a physical-to-discretionary authority shift (material constraint eliminated, institutional discretion substituted). creditor_discipline_reading locates it in a geopolitical power shift (creditor veto eliminated, reserve-issuer flexibility gained). This composite_overdetermination_reading denies either single mechanism is sufficient, authoring instead four independent converging processes with moderate, diffuse ε and no concentrated beneficiary. The three stories share no beneficiary/victim overlap by design — each reading's structural claims are internally coherent but mutually exclusive at the level of causal attribution, even though they may agree on many surface-level facts (that fixed exchange rates ended, that central bank discretion expanded).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
