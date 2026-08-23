% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility as Conditional Obligation Subordinate to Domestic Stability
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   The Bretton Woods Article IV convertibility obligation ($35/oz gold) is
 *   read here as a conditional commitment: the U.S. pledges convertibility
 *   *provided* it does not conflict with domestic economic stability (full
 *   employment, growth). This reading, articulated by U.S. officials from
 *   Truman through Nixon and codified in the 1945 Bretton Woods Act's
 *   'consistent with domestic stability' language, makes convertibility
 *   subordinate to domestic policy. Structurally, this shifts the
 *   constraint's extraction: the U.S. gains monetary autonomy (exits victim
 *   set) while foreign dollar holders bear devaluation risk (enter victim
 *   set). The coordination function (stable reserve currency) is real but
 *   increasingly decoupled from the extraction function (exporting
 *   inflation). The constraint persists through active enforcement: the
 *   London Gold Pool (1961-68), swap networks, GAB, and political pressure on
 *   allies not to convert dollars.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.62).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.48).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility as Conditional Obligation Subordinate to Domestic Stability").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, 'fbbd0d35-7b4a-47ac-b38e-a424f05e685c').
narrative_ontology:cs_kernel_codification('fbbd0d35-7b4a-47ac-b38e-a424f05e685c', formalized).
narrative_ontology:cs_authority_grounding('fbbd0d35-7b4a-47ac-b38e-a424f05e685c', lineage).
narrative_ontology:cs_interpretation_layer_present('fbbd0d35-7b4a-47ac-b38e-a424f05e685c').
narrative_ontology:cs_reading_relation('fbbd0d35-7b4a-47ac-b38e-a424f05e685c', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbbd0d35-7b4a-47ac-b38e-a424f05e685c', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('fbbd0d35-7b4a-47ac-b38e-a424f05e685c', foundational, domestic_stability_supersedes_convertibility).
narrative_ontology:cs_axiom_status(domestic_stability_supersedes_convertibility, holdable).
narrative_ontology:cs_axiom_grounding('fbbd0d35-7b4a-47ac-b38e-a424f05e685c', domestic_stability_supersedes_convertibility, instrumental).
narrative_ontology:cs_axiom('fbbd0d35-7b4a-47ac-b38e-a424f05e685c', foundational, reserve_currency_issuer_autonomy_justified_by_systemic_liquidity_provision).
narrative_ontology:cs_axiom_status(reserve_currency_issuer_autonomy_justified_by_systemic_liquidity_provision, holdable).
narrative_ontology:cs_axiom_grounding('fbbd0d35-7b4a-47ac-b38e-a424f05e685c', reserve_currency_issuer_autonomy_justified_by_systemic_liquidity_provision, conventional).
narrative_ontology:cs_reference_frame('fbbd0d35-7b4a-47ac-b38e-a424f05e685c', breton_woods_conditional_parity).
narrative_ontology:cs_drift_state('fbbd0d35-7b4a-47ac-b38e-a424f05e685c', id_1971_pre_nixon_shock, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('fbbd0d35-7b4a-47ac-b38e-a424f05e685c', '2026-07-28T14:32:17Z').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_domestic_economy).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_sovereign_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, international_monetary_system_stability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, foreign_sovereign_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Bretton Woods convertibility framework through the Federal Reserve and Treasury. Retains de facto discretion to prioritize domestic employment and growth over strict gold parity. Can adjust discount rates, conduct open market operations, and negotiate international agreements (e.g., GAB, swap lines) that preserve operational flexibility. Benefits from seigniorage and the exorbitant privilege of issuing the global reserve currency.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities, beneficiary).

% The U.S. domestic economy — firms, workers, households — benefits from monetary policy oriented to full employment and stable growth rather than defending a fixed gold price. The conditional reading shields domestic policy from external discipline; the cost is potential future instability if confidence erodes. Exit is constrained: actors cannot individually leave the dollar system, but political pressure can shift policy.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_domestic_economy, beneficiary,
    organized, biographical, constrained, national).

% Hold dollar reserves as the anchor of their own monetary systems. Under the conditional reading, they bear devaluation risk when U.S. policy prioritizes domestic goals over convertibility discipline. Their exit options are limited: selling dollars at scale crashes the value of remaining holdings; shifting to alternatives (gold, SDRs, other currencies) is slow, politically fraught, and lacks depth. They are coordinated into the system but pay the adjustment cost.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer,
    institutional, generational, constrained, global).

% Sovereign wealth funds and treasuries of major surplus countries (e.g., Germany, Japan, later OPEC). Benefit from stable dollar assets for trade invoicing and reserve management, but pay through inflationary erosion and devaluation risk when U.S. policy diverges from convertibility. Have more leverage than smaller holders (can negotiate swap lines, threaten diversification) but remain locked into the dollar system by network effects.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_sovereign_holders, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, foreign_sovereign_holders, beneficiary).

% The systemic coherence of the Bretton Woods regime itself. The conditional reading introduces a structural tension: the reserve currency issuer's domestic priority becomes a source of systemic instability (Triffin dilemma). The system pays in recurring crises (1960, 1968, 1971) and eventual collapse. Not an actor — a structural proposition that bears the cost of the arrangement's internal contradiction.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_monetary_system_stability, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__policy_flexible_reading, international_monetary_system_stability).

% The IMF's Article IV consultation mandate to oversee the par value system. Under the conditional reading, surveillance is asymmetrical: it can pressure deficit countries but lacks effective leverage over the reserve currency issuer. Observes the constraint's operation and documents the widening gap between formal obligation and actual practice.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, imf_surveillance_function, observer,
    institutional, generational, analytical, global).

% Economists (Triffin, Mundell, Johnson, later Kenen) who analyze the constraint's structural logic. They do not collect rents or pay costs directly; they map the coordination-extraction boundary and identify the conditional reading's instability. Their exit is analytical: they can change frameworks without material cost.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, academic_monetary_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable nominal anchor for the post-war international monetary system: fixed dollar-gold parity ($35/oz) with dollar as the key reserve currency, enabling predictable exchange rates, trade invoicing, and reserve accumulation without each country holding gold directly.
% TRANSFER_FUNCTION: Transfers adjustment burden from the U.S. domestic economy (which retains policy autonomy) to foreign dollar holders (who absorb dollar inflation and devaluation risk). The U.S. supplies liquidity; foreigners supply the confidence that backs it. Seigniorage and exorbitant privilege flow to the U.S.; adjustment cost flows outward.
% ABSENT_VOICES: Smaller developing countries with limited reserves and no swap-line access — they bear disproportionate adjustment costs (import compression, debt crises) when U.S. policy shifts, but have no seat at the G10/Paris Club tables where the conditional reading is negotiated. Also absent: future generations who inherit the systemic fragility the conditional reading creates.
% DISAPPEARANCE_RATIONALE: If the conditional reading vanished and strict convertibility were enforced, U.S. monetary policy would lose its domestic-policy flexibility, likely causing recession/unemployment to defend gold parity. Foreign holders would gain certainty but the system would contract (Triffin: not enough gold to back global dollar demand). The world monetary order would reorganize around a tighter, likely deflationary regime — or fragment.
% FOUNDING_PROBLEM: Post-war reconstruction required a stable international monetary system that could finance global trade without the deflationary bias of the classical gold standard. The U.S. needed to supply dollars to a dollar-hungry world; foreigners needed assurance those dollars were 'as good as gold.' The conditional reading was the political compromise: formal convertibility with operational flexibility.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — reconstructing a stable post-war monetary order — was achieved by the late 1950s (European convertibility restored, trade expanding). The conditional reading persisted for 15+ years after the problem it was built for was solved. Corroboration: Triffin (1960) testified to Congress that the system had outlived its founding logic; the G10 deputies' own minutes (1964-67) record awareness that the conditional reading had become a mechanism for exporting U.S. inflation; the 1971 Nixon shock was the explicit abandonment of the conditional reading by its own creator.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness rises from 0.28 to 0.62 across the interval as U.S. balance-of-payments deficits widen, gold coverage falls (from ~100% to ~22%), and the gap between formal obligation and actual practice grows. Suppression is moderate (0.48) because the constraint relies more on confidence and coordination than overt coercion — but the London Gold Pool and political arm-twisting of allies (e.g., 1965 French conversion threat, 1968 two-tier market) are active enforcement. Theater ratio grows from 0.08 to 0.31: the 'gold standard' rituals (par value declarations, IMF consultations) become increasingly performative as the real policy operates in the gap. Accessibility collapse is low (0.38) — alternatives (SDRs, floating, European monetary union) are discussed throughout but blocked by U.S. veto power. Resistance is high (0.71) — from French gold conversions, German revaluation pressure, Triffin's critique, and eventual systemic breakdown.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. seat, this is a Rope: a genuine coordination mechanism the U.S. built and maintains, with costs justified by system-wide benefits. From foreign central banks' seat, it is a Snare: the coordination story is cover for exporting U.S. inflation; exit is suppressed by the dollar's network dominance. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) reflects the hybrid reality: real coordination function AND asymmetric extraction requiring active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. monetary authorities are the agenda-setter/beneficiary: they set the rules, collect seigniorage, and retain policy autonomy (d ~ 0.15). U.S. domestic economy is a beneficiary with constrained exit (d ~ 0.25). Foreign central banks are payers with constrained exit — they are coordinated into holding dollars but pay the inflation tax (d ~ 0.75). Foreign sovereign holders are dual-role: benefit from dollar system utility but pay devaluation risk (d ~ 0.60). The international monetary system itself is a non-agent payer — it bears the systemic instability cost. IMF and academics are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war monetary reconstruction) was solved by ~1958. The conditional reading persisted 13 more years, accumulating extraction (rising U.S. deficits, falling gold cover) while its coordination function degraded (confidence erosion, speculative attacks). The mandatrophy is resolved in the sense that the arrangement's original justification died but the constraint persisted — a classic zombie constraint. The 1971 closure was not a reform but a unilateral termination by the agenda-setter when the extraction became unsustainable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commitment_conditional_vs_binding,
    'Is the conditional reading a legitimate interpretation of Article IV Section 4 (par values maintained except for ''fundamental disequilibrium''), or a post-hoc justification for U.S. policy autonomy?',
    'Travaux préparatoires of the Bretton Woods Articles; U.S. legislative history (Bretton Woods Act 1945, ''consistent with domestic stability''); contemporary legal opinions (White, Keynes, IMF counsel).',
    'If the conditional reading is textually unsupported, the constraint is a Snare masquerading as a Rope — the coordination function is pretext. If supported, it is a genuine Tangled Rope: a coordination mechanism with a built-in escape clause that became extractive when the escape clause became the rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_conditional_vs_binding, conceptual, 'Whether the conditional obligation is a genuine textual interpretation or a constructed cover for extraction.').

omega_variable(
    triffin_dilemma_inevitability,
    'Was the Triffin dilemma (reserve currency issuer must run deficits to supply liquidity, undermining confidence) an inevitable structural feature of ANY gold-exchange standard, or a contingent outcome of the *conditional* reading''s specific extraction pattern?',
    'Counterfactual modeling: simulate a strict-convertibility Bretton Woods (U.S. defends parity at all costs) vs. the historical conditional path. Compare liquidity provision, adjustment symmetry, and crisis frequency.',
    'If inevitable, the kernel itself is a structural Mountain (unsustainable by design) and all readings are epiphenomenal. If contingent on the conditional reading, the policy_flexible_reading is the extraction mechanism and the Triffin reading is its diagnosis — the constraint family''s causal arrow runs from policy_flexible to triffin_structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(triffin_dilemma_inevitability, empirical, 'Whether the system''s instability is kernel-inherent or reading-contingent.').

omega_variable(
    european_complicity_vs_coercion,
    'Did European central banks (Bundesbank, Banque de France) actively enable the conditional reading (gaining seigniorage from dollar reserves, exporting their own inflation via dollar pegs) or were they coerced by U.S. hegemony?',
    'Central bank archives (Buba, BdF); G10/Paris Club minutes; bilateral correspondence (e.g., Blessing-Martin letters 1965-69); domestic political records in Germany/France.',
    'If complicity: the conditional reading is a multi-sided extraction cartel (Tangled Rope with distributed beneficiaries). If coercion: it is a U.S.-centric Snare with foreign payers. The stakeholder power assignments and directionality logic depend on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(european_complicity_vs_coercion, empirical, 'Whether foreign institutional actors were co-beneficiaries or coerced payers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgcfpr_tr_t1944, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1944, 0.08).
narrative_ontology:measurement(dgcfpr_tr_t1950, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(dgcfpr_tr_t1958, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1958, 0.18).
narrative_ontology:measurement(dgcfpr_tr_t1960, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(dgcfpr_tr_t1965, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1965, 0.27).
narrative_ontology:measurement(dgcfpr_tr_t1968, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1968, 0.3).
narrative_ontology:measurement(dgcfpr_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.31).

% Extraction over time
narrative_ontology:measurement(dgcfpr_be_t1944, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1944, 0.28).
narrative_ontology:measurement(dgcfpr_be_t1950, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1950, 0.32).
narrative_ontology:measurement(dgcfpr_be_t1958, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1958, 0.38).
narrative_ontology:measurement(dgcfpr_be_t1960, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(dgcfpr_be_t1965, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1965, 0.54).
narrative_ontology:measurement(dgcfpr_be_t1968, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1968, 0.58).
narrative_ontology:measurement(dgcfpr_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(dgcfpr_su_t1944, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1944, 0.35).
narrative_ontology:measurement(dgcfpr_su_t1950, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1950, 0.38).
narrative_ontology:measurement(dgcfpr_su_t1958, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1958, 0.4).
narrative_ontology:measurement(dgcfpr_su_t1960, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1960, 0.43).
narrative_ontology:measurement(dgcfpr_su_t1965, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1965, 0.46).
narrative_ontology:measurement(dgcfpr_su_t1968, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1968, 0.47).
narrative_ontology:measurement(dgcfpr_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__policy_flexible_reading, 0.22).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, breton_woods_adjustment_mechanism).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, london_gold_pool).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, special_drawing_rights_creation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, nixon_shock_1971).

% DUAL FORMULATION NOTE:
% Part of the dollar_gold_convertibility constraint family (3 readings). This reading (policy_flexible) was the operative U.S. position 1944-71. The strict_convertibility_reading is the legal-formalist position (held by European monetary authorities and IMF legal department). The triffin_structural_reading is the structural diagnosis (Triffin 1960, Kenen 1969, BIS annual reports). ε differs: policy_flexible ε rises 0.28→0.62 (extraction accumulates); strict_convertibility ε ≈ 0.15 (low extraction if enforced); triffin_structural ε ≈ 0.85 (kernel-level structural extraction). Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, institutional, 0.15).
constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, organized, 0.25).
constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
