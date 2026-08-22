% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__contingent_choice_reading, []).

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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Bretton Woods Termination as Contingent Policy Choice
 *   domain: economic/political/international_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the contingent choice reading of the
 *   Bretton Woods transition kernel. The core claim: the August 1971
 *   suspension of dollar-gold convertibility was a deliberate policy decision
 *   by the Nixon administration, not a forced collapse. Viable alternatives
 *   existed: a managed par-value realignment (Smithsonian path), a two-tier
 *   gold market continuation, or an SDR-based reform. The constraint's
 *   extraction is low at the base (the coordination function of preventing
 *   payments chaos is real) but spikes at the decision node (1971) when the
 *   US unilaterally captured seigniorage and policy autonomy. The claimed
 *   type is rope — genuine coordination with minimal coercion — but the
 *   temporal measurements reveal a theater spike and suppression spike at the
 *   transition moment, marking the contingent extraction of privilege.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.12).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.08).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Bretton Woods Termination as Contingent Policy Choice").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "economic/political/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '8ee45a3e-b675-4489-b0c0-4e75a1714127').
narrative_ontology:cs_kernel_codification('8ee45a3e-b675-4489-b0c0-4e75a1714127', implicit).
narrative_ontology:cs_authority_grounding('8ee45a3e-b675-4489-b0c0-4e75a1714127', extraction).
narrative_ontology:cs_interpretation_layer_present('8ee45a3e-b675-4489-b0c0-4e75a1714127').
narrative_ontology:cs_reading_relation('8ee45a3e-b675-4489-b0c0-4e75a1714127', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ee45a3e-b675-4489-b0c0-4e75a1714127', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('8ee45a3e-b675-4489-b0c0-4e75a1714127', foundational, policy_choice_has_moral_weight).
narrative_ontology:cs_axiom_status(policy_choice_has_moral_weight, holdable).
narrative_ontology:cs_axiom_grounding('8ee45a3e-b675-4489-b0c0-4e75a1714127', policy_choice_has_moral_weight, deontological).
narrative_ontology:cs_axiom('8ee45a3e-b675-4489-b0c0-4e75a1714127', foundational, counterfactual_viability_establishes_agency).
narrative_ontology:cs_axiom_status(counterfactual_viability_establishes_agency, holdable).
narrative_ontology:cs_axiom_grounding('8ee45a3e-b675-4489-b0c0-4e75a1714127', counterfactual_viability_establishes_agency, empirically_contingent).
narrative_ontology:cs_reference_frame('8ee45a3e-b675-4489-b0c0-4e75a1714127', bretton_woods_par_value_order).
narrative_ontology:cs_drift_state('8ee45a3e-b675-4489-b0c0-4e75a1714127', nixon_shock_august_1971, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8ee45a3e-b675-4489-b0c0-4e75a1714127', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_treasury).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_federal_reserve).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, multinational_banks).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, emerging_eurodollar_market).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, fixed_exchange_regime_beneficiaries).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, gold_standard_advocates).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, small_open_economies_pegged_to_dollar).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Architected the August 1971 suspension of dollar-gold convertibility as executive policy choice. Gained unrestricted monetary sovereignty and seigniorage capture. Could have maintained convertibility at cost of reserves depletion and contractionary policy.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_treasury, agenda_setter,
    institutional, generational, arbitrage, global).

% Received mandate for independent domestic monetary policy without external anchor constraint. Avoided the impossible trinity bind. The contingency reading treats this as deliberate institutional redesign, not forced adaptation.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_federal_reserve, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__contingent_choice_reading, us_federal_reserve, agenda_setter).

% Expanded Eurodollar intermediation and currency trading profits after fixed parities dissolved. Their lobbying and market-making positioned them to capture the new floating-rate arbitrage. Exit was always available; they chose to deepen exposure.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, multinational_banks, beneficiary,
    organized, biographical, mobile, global).

% Offshore dollar deposits exploded when the Bretton Woods anchor failed. Market participants treat the transition as enabling infrastructure, not extraction. The contingency reading emphasizes their role as co-constructors of the post-1971 order.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, emerging_eurodollar_market, beneficiary,
    organized, biographical, mobile, global).

% Central banks and export sectors that planned around stable dollar parities. Absorbed transition costs: reserve losses, competitive devaluation spirals, imported inflation. Their exit was constrained by treaty obligations and trade dependence.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, fixed_exchange_regime_beneficiaries, payer,
    organized, biographical, constrained, global).

% Ideological commitment to monetary discipline through metallic anchor. The transition delegitimized their framework and excluded them from policy discourse. Identity-locked: professional and intellectual identity fused to the displaced paradigm.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, gold_standard_advocates, payer,
    moderate, generational, identity_locked, global).

% Had no voice in the Nixon Shock. Imported US inflation and volatility through the peg. Could not float without capital flight; could not maintain peg without reserves exhaustion. Trapped by structural dependence on the dollar system.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, small_open_economies_pegged_to_dollar, payer,
    powerless, biographical, trapped, national).

% Repurposed from par-value defender to Article IV consultation monitor. Gained institutional relevance from the transition's disorder. Analytical seat: sees the full structural break but bears neither gain nor loss.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, imf_surveillance_architecture, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a managed transition from a dollar-gold anchor to a dollar-fiat anchor, preventing a chaotic collapse of the international payments system. The coordination was the *managed* character of the break — swap lines, IMF reform, Smithsonian realignment — not the break itself.
% TRANSFER_FUNCTION: Transferred monetary policy autonomy and seigniorage from the collective Bretton Woods governance (IMF par-value system, gold pool) to the US Treasury and Federal Reserve as unilateral discretion. Transferred exchange rate risk from the US to all dollar-pegged economies and reserve holders.
% ABSENT_VOICES: Global South central banks and finance ministries excluded from the August 1971 Camp David decision. The G-10 deputies were informed, not consulted. Non-aligned movement states had no seat. Their objection would have been: the adjustment burden falls on those who did not create the imbalance.
% DISAPPEARANCE_RATIONALE: If the contingency reading vanished — i.e., if the transition were universally accepted as structurally inevitable — the legitimacy of discretionary US monetary policy would lose its founding narrative. The 'exorbitant privilege' critique would shift from contingent policy capture to structural necessity. Policy space for future regime changes (e.g., capital controls, SDR substitution) would contract.
% FOUNDING_PROBLEM: The Triffin dilemma: the Bretton Woods system required the US to run persistent deficits to supply global liquidity, but those same deficits undermined confidence in dollar-gold convertibility. The system faced a choice between global liquidity shortage and anchor collapse.
% FOUNDING_PROBLEM_CORROBORATION: Triffin himself (1960 testimony) and contemporary BIS reports corroborate the dilemma's reality. The contingency reading's specific claim — that the *Nixon administration's specific 1971 choices* were avoidable alternatives to managed reform — is corroborated by the Smithsonian Agreement negotiations (Dec 1971) which attempted to restore a modified par-value system, and by academic counterfactuals (Eichengreen, Temin) showing adjustable pegs with capital controls were viable.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__contingent_choice_reading_tests).
:- end_tests(transition_causality__contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.12 at interval end) because the post-1971 floating rate system *does* coordinate global trade and finance — it is not pure extraction. Suppression is low (0.08) because exit options exist: countries can float, peg to baskets, join currency unions. Theater ratio peaks at 0.35 in 1971 because the 'necessity' narrative was performative cover for a choice. The Smithsonian Agreement (Dec 1971) proves alternatives existed — the US *chose* not to pursue them after the initial suspension. Accessibility collapse (0.35) reflects that once the anchor broke, return was politically impossible, but alternatives *before* the break were accessible. Resistance (0.15) was muted because the transition was framed as technical necessity; the victims (small open economies) had no voice.
 *
 * PERSPECTIVAL GAP:
 *   The rope classification from the US institutional seat (coordination they built and benefit from) diverges sharply from the small open economy seat (extraction they cannot escape). The contingent choice reading makes this divergence *visible* by locating the extraction at a specific decision node with identifiable alternatives. The overdetermined collapse reading would smooth this into structural inevitability, erasing the seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   US Treasury and Fed sit at d ≈ 0.05 (full beneficiaries: they gained autonomy and seigniorage). Multinational banks and Eurodollar market at d ≈ 0.15 (beneficiaries with mobile exit — they captured new profits but could have stayed in fixed-rate business). Fixed-exchange beneficiaries at d ≈ 0.65 (payers with constrained exit — treaty-bound, trade-dependent). Gold standard advocates at d ≈ 0.85 (identity-locked: their paradigm was destroyed, exit meant professional erasure). Small open economies at d ≈ 0.95 (trapped: structural dependence, no voice). IMF as analytical observer at d ≈ 0.5. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The Bretton Woods mandate (stable parities for trade expansion) was live until ~1968. The contingency reading identifies a mandatrophy window: 1968–1971 where the mandate was dead (Triffin dilemma unresolved, gold pool failed) but the arrangement persisted theatrically (gold pool, two-tier market, Smithsonian attempt). The transition resolved the mandatrophy by *replacing* the mandate (discretionary policy autonomy) rather than eliminating the constraint. This is why theater spikes then falls — the new mandate is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nixon_decision_contingency,
    'Was the August 15, 1971 suspension decision truly contingent, or did the Triffin dilemma structurally force *some* break at *some* near-term moment?',
    'Counterfactual simulation of 1971–1973 under alternative policy paths: (a) Smithsonian-style realignment with capital controls, (b) two-tier gold market continuation, (c) SDR substitution accelerated. Compare reserve trajectories, inflation outcomes, trade stability.',
    'If all counterfactuals collapse into floating rates by 1974 regardless, the contingency claim weakens — the decision node was illusory, the hybrid reading gains. If stable alternatives persist, contingency is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nixon_decision_contingency, empirical, 'Whether the specific 1971 decision was a genuine choice among viable alternatives.').

omega_variable(
    beneficiary_intent_vs_windfall,
    'Did the US Treasury and Fed *intend* to capture seigniorage and autonomy, or did they inherit it as windfall from a forced break?',
    'Archival analysis of Nixon-Camp David deliberations (1971), Treasury memos on ''monetary sovereignty'', Volcker''s later testimony. Compare with simultaneous European central bank preferences (Giscard, Schmidt) for managed reform.',
    'If intent is documented, the beneficiary structure is *designed extraction* (snare-adjacent). If windfall, the extraction is emergent from coordination failure (rope with accidental capture). Changes the moral and institutional classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intent_vs_windfall, conceptual, 'Whether US institutional beneficiaries captured privilege by design or accident.').

omega_variable(
    eurodollar_causality,
    'Did the Eurodollar market *cause* the transition by making the gold pool indefensible, or did it *anticipate* and *position for* a transition it knew was coming?',
    'Bank of England and BIS archives on Eurodollar growth 1960–1971. London bank correspondence on dollar-gold arbitrage. Compare with US Treasury surveillance of offshore dollars.',
    'If Eurodollars caused the break, the multinational_banks beneficiary moves toward agenda_setter. If they anticipated, they are mobile beneficiaries. Affects the coordination/extraction boundary of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eurodollar_causality, empirical, 'Causal direction between offshore dollar markets and the Bretton Woods collapse.').

omega_variable(
    kernel_reading_boundary,
    'Does the contingent choice reading foreclose the overdetermined collapse reading, or do they coexist as competing explanations held by different epistemic communities?',
    'Test whether a single theoretical framework (e.g., structuralist political economy) can coherently hold both: that structural contradictions made collapse inevitable *and* that the specific 1971 path was a contingent choice among collapse modes. If yes, coexists_with. If the contradiction is logical, forecloses.',
    'Determines the reading_relations edge type in cs_structure. Affects how the engine models kernel-level contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between the contingency and overdetermination readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 1960, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(transition_causality_contingent_tr_t1960, transition_causality__contingent_choice_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(transition_causality_contingent_tr_t1965, transition_causality__contingent_choice_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(transition_causality_contingent_tr_t1968, transition_causality__contingent_choice_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(transition_causality_contingent_tr_t1971, transition_causality__contingent_choice_reading, theater_ratio, 1971, 0.35).
narrative_ontology:measurement(transition_causality_contingent_tr_t1973, transition_causality__contingent_choice_reading, theater_ratio, 1973, 0.28).
narrative_ontology:measurement(transition_causality_contingent_tr_t1976, transition_causality__contingent_choice_reading, theater_ratio, 1976, 0.22).

% Extraction over time
narrative_ontology:measurement(transition_causality_contingent_be_t1960, transition_causality__contingent_choice_reading, base_extractiveness, 1960, 0.02).
narrative_ontology:measurement(transition_causality_contingent_be_t1965, transition_causality__contingent_choice_reading, base_extractiveness, 1965, 0.04).
narrative_ontology:measurement(transition_causality_contingent_be_t1968, transition_causality__contingent_choice_reading, base_extractiveness, 1968, 0.08).
narrative_ontology:measurement(transition_causality_contingent_be_t1971, transition_causality__contingent_choice_reading, base_extractiveness, 1971, 0.18).
narrative_ontology:measurement(transition_causality_contingent_be_t1973, transition_causality__contingent_choice_reading, base_extractiveness, 1973, 0.15).
narrative_ontology:measurement(transition_causality_contingent_be_t1976, transition_causality__contingent_choice_reading, base_extractiveness, 1976, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(transition_causality_contingent_su_t1960, transition_causality__contingent_choice_reading, suppression_requirement, 1960, 0.03).
narrative_ontology:measurement(transition_causality_contingent_su_t1965, transition_causality__contingent_choice_reading, suppression_requirement, 1965, 0.05).
narrative_ontology:measurement(transition_causality_contingent_su_t1968, transition_causality__contingent_choice_reading, suppression_requirement, 1968, 0.12).
narrative_ontology:measurement(transition_causality_contingent_su_t1971, transition_causality__contingent_choice_reading, suppression_requirement, 1971, 0.25).
narrative_ontology:measurement(transition_causality_contingent_su_t1973, transition_causality__contingent_choice_reading, suppression_requirement, 1973, 0.15).
narrative_ontology:measurement(transition_causality_contingent_su_t1976, transition_causality__contingent_choice_reading, suppression_requirement, 1976, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(transition_causality__contingent_choice_reading, 0.18).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__hybrid_trigger_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, triffin_dilemma_constraint).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, exorbitant_privilege_constraint).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, petrodollar_recycling_constraint).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, plaza_accord_constraint).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, floating_exchange_rate_regime_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one member of the transition_causality kernel family. The contingent_choice_reading locates the extraction at a specific 1971 decision node with high counterfactual viability. The overdetermined_collapse_reading (sibling) distributes extraction across structural contradictions 1960–1971 with low counterfactual viability. The hybrid_trigger_reading (sibling) splits: structural accumulation (low extraction) + contingent trigger (high extraction at node). All three share the same referent (the Bretton Woods termination) but author different ε and different beneficiary structures. This decomposition follows the ε-invariance principle: one label ('the Nixon Shock'), three structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, institutional, 0.05).
constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, organized, 0.15).
constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, moderate, 0.85).
constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
