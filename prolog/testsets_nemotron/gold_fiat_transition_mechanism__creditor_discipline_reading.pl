% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__creditor_discipline_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Elimination of Creditor Gold Redemption Veto (Debtor-Flexibility Reading)
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story captures the creditor-discipline reading of the
 *   gold-to-fiat transition: the structural elimination of gold redemption as
 *   a creditor veto over debtor-nation fiscal and monetary policy. Under
 *   Bretton Woods, creditor nations (holding dollar reserves) could threaten
 *   gold conversion, forcing deficit countries into contractionary
 *   adjustment. The 1971 Nixon Shock removed this veto, but the resulting
 *   arrangement is not a pure coordination gain — it extracts from creditor
 *   nations (who lost redemption leverage and hold depreciating reserves) and
 *   from non-reserve holders (who face tighter external constraints without
 *   the reserve currency's exorbitant privilege), while benefiting debtor
 *   nations and especially the US as reserve issuer. The constraint persists
 *   through active enforcement: the dollar system's network effects, IMF
 *   conditionality, swap-line architecture, and the lack of a viable
 *   alternative reserve asset. This reading treats the transition as a
 *   geopolitical power shift from creditor to reserve-currency issuer, not
 *   merely a technical monetary reform.
 *
 * KEY AGENTS:
 *   - reserve_currency_issuer_us: Primary beneficiary (institutional/arbitrage) — issues the world's reserve currency, faces no external gold constraint
 *   - debtor_nations: Beneficiaries (organized/constrained) — gained fiscal/monetary flexibility, but remain subject to dollar-denominated discipline
 *   - creditor_nations: Victims (powerful/constrained) — lost gold redemption leverage, hold dollar reserves subject to US policy
 *   - non_reserve_holders: Victims (moderate/trapped) — face balance-of-payments discipline without reserve-currency privilege
 *   - imf_bis_network: Agenda setter (institutional/generational) — administers the post-gold discipline architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.72).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.58).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Elimination of Creditor Gold Redemption Veto (Debtor-Flexibility Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '0bcbaf4d-1d64-4dd0-b731-ad17683a8d91').
narrative_ontology:cs_kernel_codification('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91', fixed_text).
narrative_ontology:cs_authority_grounding('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91', lineage).
narrative_ontology:cs_interpretation_layer_present('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91').
narrative_ontology:cs_reading_relation('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91', foundational, creditor_veto_elimination_as_geopolitical_shift).
narrative_ontology:cs_axiom_status(creditor_veto_elimination_as_geopolitical_shift, holdable).
narrative_ontology:cs_axiom_grounding('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91', creditor_veto_elimination_as_geopolitical_shift, empirically_contingent).
narrative_ontology:cs_axiom('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91', foundational, reserve_issuer_captures_asymmetric_flexibility).
narrative_ontology:cs_axiom_status(reserve_issuer_captures_asymmetric_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91', reserve_issuer_captures_asymmetric_flexibility, empirically_contingent).
narrative_ontology:cs_reference_frame('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91', bretton_woods_creditor_veto_framework).
narrative_ontology:cs_drift_state('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91', contemporary_dollar_system, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0bcbaf4d-1d64-4dd0-b731-ad17683a8d91', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer_us).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the world's primary reserve currency; faces no external convertibility constraint; captures seigniorage and sets monetary policy for domestic objectives with global spillovers; administers swap lines and influences IMF governance; can always issue more dollars to meet obligations.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer_us, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer_us, agenda_setter).

% Gained fiscal/monetary flexibility after 1971 — no gold drain forces contraction; but remain subject to dollar-denominated balance-of-payments discipline, IMF conditionality, and sudden stops; exit requires building reserve buffers or alternative financing (regional arrangements, swap lines) which is slow and incomplete.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, payer).

% Lost gold redemption leverage post-1971; hold large dollar reserves that depreciate with US inflation and policy choices; cannot convert claims to gold; exit requires developing alternative reserve assets (euro, RMB, gold) or bilateral swap networks — structurally difficult due to dollar network effects and trade invoicing in dollars.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    powerful, biographical, constrained, global).

% Face full balance-of-payments discipline in dollars without reserve-currency privilege; sudden stops trigger IMF programs with conditionality; no seigniorage offset; limited voice in governance; exit is near-impossible individually — collective alternatives (regional monetary funds, SDR allocation) remain marginal.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_holders, payer,
    moderate, biographical, trapped, national).

% Administers the post-gold discipline architecture: IMF surveillance, lending programs, conditionality design; BIS coordinates central bank cooperation, swap lines, and financial stability standards; captures institutional rents and agenda-setting power; not directly extracting from the constraint but its mandate and resources depend on the system's persistence.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, imf_bis_network, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer_us).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides global liquidity and a common unit of account for international trade and finance; enables countercyclical policy by the reserve issuer that stabilizes the system; operates swap lines as emergency backstops.
% TRANSFER_FUNCTION: Moves real goods and services from creditor nations and non-reserve holders to the reserve issuer (via seigniorage and dollar depreciation) and to debtor nations (via fiscal flexibility financed by dollar issuance); moves adjustment burden from reserve issuer to all other participants.
% ABSENT_VOICES: Future generations who inherit the dollar-system's distributional consequences; populations in non-reserve-holder countries subject to IMF conditionality without democratic input; potential alternative reserve architects (eurozone, China) whose structural exit is blocked by network effects.
% DISAPPEARANCE_RATIONALE: If the dollar reserve system and its asymmetric discipline vanished overnight, global trade invoicing would fragment, US fiscal/monetary autonomy would contract sharply, creditor nations would regain leverage over debtors, non-reserve holders would face chaotic repricing, and the IMF/BIS architecture would lose its operational foundation — a fundamental rearrangement of the international monetary order.
% FOUNDING_PROBLEM: The gold-exchange standard's creditor veto (gold redemption threat) forced deficit countries into deflationary adjustment, transmitting recession globally and preventing countercyclical policy — the 'golden fetters' that worsened the Great Depression.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Eichengreen, Temin) and contemporary policymakers (Triffin, 1960) attest the creditor-veto deflation trap was the founding problem; the problem is dead because gold convertibility is gone and the reserve issuer faces no external constraint — but the arrangement persists and has accumulated extraction, corroborated by the Triffin dilemma literature and post-2008 reform debates (Zhou Xiaochuan 2009, Stiglitz commission) from outside the beneficiary set.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) reflects the asymmetric transfer: creditor nations and non-reserve holders bear the cost of dollar-system stability through reserve depreciation and balance-of-payments discipline, while the reserve issuer and debtor nations capture the flexibility gains. Suppression (0.58) is moderate — the constraint does not rely on overt coercion but on the structural impossibility of exiting the dollar system (no alternative reserve asset of sufficient depth). Theater ratio (0.32) captures the genuine coordination function (global liquidity provision, crisis lending) alongside the extractive overlay (seigniorage, asymmetric adjustment). Accessibility collapse (0.64) is elevated because alternatives (gold, SDRs, euro, RMB) have not materialized as full substitutes. Resistance (0.48) reflects periodic challenges (1970s SDR push, 2000s euro reserve aspirations, 2010s RMB internationalization) that have not displaced the core structure.
 *
 * PERSPECTIVAL GAP:
 *   From the US/reserve-issuer seat, the post-1971 system appears as genuine coordination (liquidity provision, lender of last resort, stable trade invoicing) — a rope-like structure. From creditor-nation seats, the same system appears as extraction without veto power — a snare-like structure. From non-reserve-holder seats, it appears as imposed discipline with no voice — a piton-like structure of inertial persistence. The engine computes these per-seat divergences from the structural data; the claimed_type (tangled_rope) reflects the system-level hybrid of coordination and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position in the dollar system: the US as reserve issuer sits at d ≈ 0.1 (full beneficiary — the constraint subsidizes its fiscal/monetary autonomy). Debtor nations sit at d ≈ 0.35 (net beneficiaries but still subject to dollar-denominated discipline). Creditor nations (e.g., Germany, Japan, China at various periods) sit at d ≈ 0.75 — they provide real goods for dollar claims they cannot convert to gold, and their exit is constrained by trade dependence and reserve management needs. Non-reserve holders (most developing economies) sit at d ≈ 0.9 — they bear the full discipline of dollar-funded balance-of-payments adjustment without the seigniorage offset. The IMF/BIS network sits at d ≈ 0.2 as agenda setter that administers the system and captures institutional rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creditor veto causing deflationary adjustment) was live in 1971. By the 1980s, the problem had mutated: the veto was gone but a new asymmetric adjustment burden fell on non-reserve holders. The arrangement persists not because the original problem remains, but because the reserve issuer and institutional administrators benefit from the extracted seigniorage and control. This is a classic mandatrophy: the coordination function (solving the creditor-veto deflation trap) has been overlaid with persistent extraction that the beneficiaries (US, debtor nations) have no incentive to dismantle, while the victims (creditors, non-reserve holders) lack the collective power to force revision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of the gold_fiat_transition_mechanism kernel, and if so, which structural elements distinguish it from sibling readings?',
    'Compare beneficiary/victim structure, epsilon referent, and coordination/extraction decomposition across the three declared readings; convergence on distinct structural profiles confirms kernel decomposition.',
    'Confirms this constraint is one instantiation of a contested kernel rather than a standalone constraint; requires sibling stories for complete family mapping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to kernel/reading frame and structural distinctness from siblings.').

omega_variable(
    creditor_discipline_vs_automatic_constraint,
    'Does the elimination of creditor veto power represent a qualitatively different constraint mechanism than the elimination of automatic gold-reserve limits on money creation?',
    'Trace whether post-1971 balance-of-payments adjustment operated through creditor discretion (this reading) or central bank discretion (automatic_constraint_reading) by examining IMF conditionality, swap lines, and reserve accumulation patterns.',
    'If mechanisms are distinct, the two readings instantiate different constraints with different epsilon profiles; if they collapse to the same discretionary authority, the kernel decomposition over-splits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_discipline_vs_automatic_constraint, empirical, 'Whether creditor-discipline and automatic-constraint are separable mechanisms or two framings of the same discretionary shift.').

omega_variable(
    reserve_issuer_asymmetry,
    'Is the constraint genuinely eliminated for the reserve issuer, or does it mutate into a different discipline mechanism (e.g., inflation targeting, swap-line conditionality)?',
    'Measure effective constraint on US fiscal/monetary policy post-1971: compare pre-1971 gold drain episodes to post-1971 inflation/twin-deficit episodes and their policy responses.',
    'If a mutated discipline mechanism exists, the ''eliminated'' claim overstates beneficiary freedom and understates residual extraction on the reserve issuer itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_issuer_asymmetry, empirical, 'Whether reserve-issuer freedom is absolute or constrained by successor mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1971, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gftm_cdr_tr_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1971, 0.15).
narrative_ontology:measurement(gftm_cdr_tr_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(gftm_cdr_tr_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(gftm_cdr_tr_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(gftm_cdr_tr_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2008, 0.32).
narrative_ontology:measurement(gftm_cdr_tr_t2020, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2020, 0.32).

% Extraction over time
narrative_ontology:measurement(gftm_cdr_be_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1971, 0.38).
narrative_ontology:measurement(gftm_cdr_be_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(gftm_cdr_be_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(gftm_cdr_be_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(gftm_cdr_be_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(gftm_cdr_be_t2020, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2020, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gftm_cdr_su_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1971, 0.42).
narrative_ontology:measurement(gftm_cdr_su_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement(gftm_cdr_su_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(gftm_cdr_su_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(gftm_cdr_su_t2008, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(gftm_cdr_su_t2020, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2020, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.15).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, dollar_reserve_system_persistence).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, imf_conditionality_architecture).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, swap_line_network_governance).

% DUAL FORMULATION NOTE:
% This constraint family (gold_fiat_transition_mechanism) decomposes the colloquial 'end of gold standard' into three structurally distinct claims with different epsilon profiles, beneficiary/victim structures, and coordination/extraction decompositions. The creditor_discipline_reading has the highest epsilon (0.72) because it centers the asymmetric transfer from creditors/non-reserve-holders to the reserve issuer. The automatic_constraint_reading has lower epsilon (coordination function of central bank discretion dominates). The composite_overdetermination_reading has distributed epsilon across multiple causal strands.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__creditor_discipline_reading, institutional, 0.1).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__creditor_discipline_reading, organized, 0.35).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__creditor_discipline_reading, powerful, 0.75).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__creditor_discipline_reading, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
