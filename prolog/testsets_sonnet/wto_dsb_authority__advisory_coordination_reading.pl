% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__advisory_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__advisory_coordination_reading, []).

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
 *   constraint_id: wto_dsb_authority__advisory_coordination_reading
 *   human_readable: WTO Dispute Settlement Body as Advisory Coordination Mechanism
 *   domain: international_law/trade_governance
 *
 * SUMMARY:
 *   This story instantiates the advisory-coordination reading of the WTO
 *   Dispute Settlement Body kernel: DSB panels are treated as expert bodies
 *   whose reports inform and legitimize a subsequent bilateral negotiation,
 *   with member states retaining ultimate policy discretion to comply,
 *   compensate, or accept authorized retaliation. This is one of three
 *   structurally distinct readings of the same institutional kernel (the
 *   DSB's authority) — the binding_referee_reading treats panel rulings as
 *   legally obligatory under treaty commitments with surrendered sovereignty,
 *   and the judicial_activism_reading treats the same panels as having
 *   exceeded their mandate through interpretive overreach. Each reading has
 *   its own epsilon, its own beneficiary/victim structure, and its own
 *   classification; they are linked here only via network edges, not merged
 *   into one story.
 *
 * KEY AGENTS:
 *   - member_states_generally: primary beneficiary/agenda-setter (institutional/mobile) — bring disputes, retain policy discretion
 *   - export_dependent_economies: beneficiary (moderate/constrained) — gain negotiating leverage from expert findings
 *   - large_trading_powers: beneficiary/payer (powerful/arbitrage) — can absorb non-compliance costs, benefit from advisory framing when losing
 *   - trade_law_secretariat: agenda_setter (institutional/analytical) — administers process, no independent enforcement power
 *   - domestic_industries_seeking_protection: payer (moderate/constrained) — bear eventual cost of negotiated settlements
 *   - small_states_without_negotiating_leverage: excluded (powerless/trapped) — formal access without effective leverage
 *   - international_trade_scholars: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, 0.22).
domain_priors:suppression_score(wto_dsb_authority__advisory_coordination_reading, 0.15).
domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__advisory_coordination_reading, rope).
narrative_ontology:human_readable(wto_dsb_authority__advisory_coordination_reading, "WTO Dispute Settlement Body as Advisory Coordination Mechanism").
narrative_ontology:topic_domain(wto_dsb_authority__advisory_coordination_reading, "international_law/trade_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__advisory_coordination_reading, 'd3fa7252-e1ac-4d4b-8835-96824881c8bb').
narrative_ontology:cs_kernel_codification('d3fa7252-e1ac-4d4b-8835-96824881c8bb', fixed_text).
narrative_ontology:cs_authority_grounding('d3fa7252-e1ac-4d4b-8835-96824881c8bb', practice).
narrative_ontology:cs_interpretation_layer_present('d3fa7252-e1ac-4d4b-8835-96824881c8bb').
narrative_ontology:cs_reading_relation('d3fa7252-e1ac-4d4b-8835-96824881c8bb', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3fa7252-e1ac-4d4b-8835-96824881c8bb', wto_dsb_authority__judicial_activism_reading, influences).
narrative_ontology:cs_axiom('d3fa7252-e1ac-4d4b-8835-96824881c8bb', foundational, sovereignty_preserved_over_legalization).
narrative_ontology:cs_axiom_status(sovereignty_preserved_over_legalization, holdable).
narrative_ontology:cs_axiom_grounding('d3fa7252-e1ac-4d4b-8835-96824881c8bb', sovereignty_preserved_over_legalization, conventional).
narrative_ontology:cs_axiom('d3fa7252-e1ac-4d4b-8835-96824881c8bb', foundational, compliance_is_negotiated_not_compelled).
narrative_ontology:cs_axiom_status(compliance_is_negotiated_not_compelled, holdable).
narrative_ontology:cs_axiom_grounding('d3fa7252-e1ac-4d4b-8835-96824881c8bb', compliance_is_negotiated_not_compelled, instrumental).
narrative_ontology:cs_reference_frame('d3fa7252-e1ac-4d4b-8835-96824881c8bb', gatt_era_diplomatic_bargaining_norm).
narrative_ontology:cs_drift_state('d3fa7252-e1ac-4d4b-8835-96824881c8bb', post_appellate_body_paralysis_2019, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d3fa7252-e1ac-4d4b-8835-96824881c8bb', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, member_states_generally).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, export_dependent_economies).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, trade_law_secretariat).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, large_trading_powers).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, large_trading_powers).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, domestic_industries_seeking_protection).
narrative_ontology:constraint_vindicates(wto_dsb_authority__advisory_coordination_reading, sovereign_policy_discretion_doctrine).
narrative_ontology:constraint_vindicates(wto_dsb_authority__advisory_coordination_reading, negotiated_settlement_preference).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bring disputes to DSB panels to obtain expert, reasoned analysis of contested trade measures. Use the panel's findings as an input to bilateral negotiation rather than as a directly enforceable order. Retain the ability to maintain a contested measure, offer compensation, or accept retaliation as alternatives to compliance — the panel's opinion informs the bargain but does not by itself compel an outcome.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, member_states_generally, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, member_states_generally, agenda_setter).

% Rely on the DSB process to get a credible, expert reading of whether a larger trading partner's measure violates shared understandings, which strengthens their negotiating hand even without binding force. Benefit from the coordination function — a shared expert forum — while accepting that ultimate leverage still tracks relative bargaining power, not the ruling itself.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, export_dependent_economies, beneficiary,
    moderate, biographical, constrained, global).

% Participate in panel proceedings when convenient and can absorb the reputational or retaliatory cost of non-compliance more easily than smaller states. Benefit from a forum that legitimizes their preferred outcomes when they win, and retain policy discretion to decline implementation when they lose, subject only to authorized retaliation they can often out-negotiate or absorb.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, large_trading_powers, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, large_trading_powers, payer).

% Administers the panel process, selects panelists, and drafts procedural rules, but has no independent enforcement capacity — its authority rests entirely on member states choosing to treat panel reports as useful inputs. Its institutional survival depends on the advisory framing continuing to be seen as legitimate and useful by the states that fund it.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, trade_law_secretariat, agenda_setter,
    institutional, civilizational, analytical, global).

% Lobby their governments to maintain a contested trade measure even after an adverse panel opinion, since the advisory framing gives their government room to negotiate a slower phase-out or partial compensation rather than immediate removal. Bear the cost when their government does eventually trade away the protective measure as part of a broader settlement.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, domestic_industries_seeking_protection, payer,
    moderate, biographical, constrained, national).

% Formally have equal access to bring a case but lack the bilateral leverage to convert a favorable advisory opinion into an actual change in a large trading partner's behavior; their retaliation rights are often economically meaningless against much larger economies. Their structural position is not addressed by the advisory framing, which assumes rough parity in the ability to negotiate consequences.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, small_states_without_negotiating_leverage, excluded,
    powerless, biographical, trapped, national).

% Study compliance rates, retaliation patterns, and negotiated outcomes to assess whether the DSB functions as advisory coordination, binding adjudication, or something that drifts between the two depending on the parties involved.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, international_trade_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__advisory_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(wto_dsb_authority__advisory_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, expert, rules-referenced forum where trading partners can obtain a reasoned assessment of a contested measure, reducing the information and legitimacy costs of resolving trade disputes through pure power bargaining alone.
% TRANSFER_FUNCTION: Moves informational and reputational capital — a credible expert assessment — from the panel process to whichever party's negotiating position it supports; does not itself move enforceable obligations, since implementation remains a matter of subsequent bilateral bargaining.
% ABSENT_VOICES: Small states without meaningful retaliation capacity can access the forum formally but their post-ruling leverage is negligible; they would argue the advisory framing quietly re-imports the power asymmetries the system was meant to discipline, but their objection rarely surfaces in the process itself.
% DISAPPEARANCE_RATIONALE: Some parties would say the world barely rearranges — negotiations would proceed on raw bilateral leverage much as they do now, since compliance already depends on power dynamics rather than the ruling. Others would say the loss of even an advisory, non-binding expert forum removes a shared reference point that currently disciplines negotiation rhetoric and narrows the range of plausible bargaining positions.
% FOUNDING_PROBLEM: Pre-WTO trade disputes were resolved through raw bilateral leverage and ad hoc GATT panels with no consistent procedure, producing unpredictable, power-driven outcomes and frequent breakdowns into retaliatory spirals.
% FOUNDING_PROBLEM_CORROBORATION: Trade law scholars and smaller member states corroborate that a shared expert forum still serves a genuine function in narrowing factual and legal disagreement even where enforcement is weak; some large-power officials and domestic protected industries assert the DSB has drifted toward quasi-judicial overreach (a claim addressed in the sibling judicial_activism_reading), which is evidence the founding-problem-as-advisory-coordination framing is itself contested rather than settled outside the beneficiary set.
narrative_ontology:disappearance_verdict(wto_dsb_authority__advisory_coordination_reading, contested).
narrative_ontology:founding_problem_status(wto_dsb_authority__advisory_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__advisory_coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__advisory_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__advisory_coordination_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__advisory_coordination_reading_tests).
:- end_tests(wto_dsb_authority__advisory_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22 at interval end) because under this reading no party is compelled to transfer anything by the panel's opinion alone — the ruling is an input to a subsequent negotiation, and outcomes track bargaining power rather than the institution's coercive force. Suppression is low (0.15): there is no enforcement machinery suppressing alternatives, since a losing party's principal 'alternative' (declining to implement, offering compensation, accepting retaliation) is a standard, undisguised option within the framework, not something the system works to hide or foreclose. Theater ratio is moderate and rising slowly (0.18 to 0.30) — some of the panel process increasingly resembles judicial proceeding in form (extensive legal briefing, precedent citation) even though its output remains formally advisory, which is itself a mild drift signal worth tracking but not enough under this reading to indicate a shift toward the binding_referee reading.
 *
 * PERSPECTIVAL GAP:
 *   From the member-state agenda-setter seat, this looks like a functioning coordination mechanism: information is produced, negotiating positions are clarified, sovereignty is preserved. From the seat of a small state without negotiating leverage, the same non-binding structure can look like a forum that produces correct answers with no mechanism to make them matter — a divergence the engine should register as different computed seat outcomes from the same structural data, not as a contradiction to be resolved by picking one description as 'true.'
 *
 * DIRECTIONALITY LOGIC:
 *   Member states as a class are the structural beneficiaries of a shared expert forum — it lowers the cost of resolving disputes relative to naked bilateral coercion. Large trading powers benefit doubly: they get legitimacy when they win and retain effective discretion when they lose, since their capacity to absorb retaliation is high (arbitrage-grade exit). Export-dependent and smaller economies benefit from access to the forum but their post-ruling leverage is much lower, which is why small_states_without_negotiating_leverage is marked excluded rather than beneficiary — the forum's formal availability does not translate into the same practical benefit when bargaining power is asymmetric. Domestic industries lobbying for continued protection are payers in the eventual negotiated outcome, not in the ruling itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The advisory-coordination reading resists mandatrophy mislabeling in the direction of over-crediting the DSB with judicial force it does not (on this reading) actually possess — treating a coordination-facilitating advisory body as though it were a binding referee would misclassify negotiated settlements as compliance and would treat retaliation-driven outcomes as rule-of-law outcomes. Correctly identifying the advisory function prevents claiming the DSB solves the sovereignty-vs-legalization problem when, on this reading, it only informs the bargain that resolves it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_wto_dsb_authority,
    'Is the WTO DSB''s actual operative authority best characterized as advisory-coordination (this reading), binding-referee (surrendered sovereignty under treaty law), or judicial-activism (illegitimate interpretive overreach)? The same institutional kernel supports all three readings depending on which compliance data, which member states, and which historical period is emphasized.',
    'Empirical study of actual compliance rates across dispute types, state power asymmetries, and time periods; legal analysis of whether the DSU''s compensation/retaliation provisions were designed as genuine alternatives to compliance or merely as enforcement backstops; comparison of self-reported member-state understanding of obligation at accession versus current practice.',
    'If the binding_referee_reading is structurally more accurate, this story''s low extractiveness and suppression scores understate the system''s actual coercive character for compliant states, and the whole coordination framing becomes a legitimating gloss on what is functionally adjudication. If the judicial_activism_reading is more accurate, neither this reading nor the binding_referee_reading is stable, since both presuppose the panels are operating within mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_wto_dsb_authority, conceptual, 'Which of three structurally distinct readings of DSB authority best fits actual institutional operation.').

omega_variable(
    compliance_asymmetry_masking,
    'Does the advisory-coordination framing''s emphasis on ''retained policy discretion'' mask the fact that discretion is exercised very differently by powerful and weak states, such that the same formal structure produces near-binding effect for small states and genuinely advisory effect for large ones?',
    'Compare compliance timelines and settlement terms across disputes where the losing party was a major economy versus a small economy, controlling for issue area.',
    'If compliance asymmetry by power is severe, the advisory_coordination_reading may only be accurate as applied to large-power respondents, while functioning closer to the binding_referee_reading for smaller ones — suggesting the kernel itself may fracture along a power axis not captured by the three declared readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_asymmetry_masking, empirical, 'Whether the advisory characterization holds uniformly across power-asymmetric disputants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__advisory_coordination_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(wto__tr_t2006, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2006, 0.24).
narrative_ontology:measurement(wto__tr_t2012, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2012, 0.26).
narrative_ontology:measurement(wto__tr_t2018, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 1995, 0.12).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(wto__be_t2006, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2006, 0.18).
narrative_ontology:measurement(wto__be_t2012, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2012, 0.19).
narrative_ontology:measurement(wto__be_t2018, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2018, 0.2).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2024, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(wto_dsb_authority__advisory_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, judicial_activism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'WTO DSB authority,' per the ε-invariance principle: the concept covers structurally distinct claims about whether panel rulings bind, merely advise, or exceed mandate. advisory_coordination_reading (this file) treats rulings as negotiation inputs with low extraction and low suppression (rope). binding_referee_reading treats rulings as binding treaty obligations with surrendered sovereignty (likely higher extraction/suppression, tangled_rope or similar depending on enforcement asymmetry). judicial_activism_reading treats the same panels as illegitimately expanding their mandate (likely snare-flavored, given identified victims of interpretive overreach). Each carries its own ε and stakeholder structure; they are linked here as siblings sharing one contested kernel, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
