% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility as Conditional, Policy-Subordinated Obligation
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This story instantiates the policy-flexible reading of the dollar-gold
 *   convertibility kernel: the Article IV obligation to redeem dollars for
 *   gold at $35/oz is treated by the U.S. as conditional on domestic economic
 *   stability, not as an unconditional legal commitment. Under this reading,
 *   the United States retains the effective right to subordinate
 *   convertibility to domestic policy goals (financing the Vietnam War and
 *   Great Society programs without raising taxes, avoiding contractionary
 *   monetary policy that would slow domestic growth), and periodically
 *   exercises that right through delay, diplomatic pressure on redemption
 *   requests, and ultimately outright suspension in August 1971 (the 'Nixon
 *   Shock'). The structural delta from the strict_convertibility_reading is
 *   exactly as expected: dollar-holding central banks and reserve holders
 *   bear the devaluation and adjustment risk (they enter the victim set),
 *   while the U.S. exits the victim set entirely and instead appears among
 *   the beneficiaries, regaining full monetary sovereignty. This is a
 *   distinct constraint from strict_convertibility_reading (which treats
 *   Article IV as binding and would show the U.S. as bound/constrained, not
 *   as beneficiary) and from triffin_structural_reading (which locates the
 *   extraction in the system's design rather than in U.S. policy choice). Do
 *   not average these three readings — each has a stable, distinct ε and
 *   beneficiary/victim structure.
 *
 * KEY AGENTS:
 *   - us_treasury: agenda_setter, administers the gold window and decides when the obligation is honored
 *   - us_domestic_policymakers: beneficiary, gains policy space from the flexible interpretation
 *   - foreign_central_banks_holding_dollars: payer, bears devaluation risk with no exit
 *   - gold_pool_participants: payer/co-agenda_setter, subsidized the peg they did not control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.58).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.42).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility as Conditional, Policy-Subordinated Obligation").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, '7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5').
narrative_ontology:cs_kernel_codification('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5', formalized).
narrative_ontology:cs_authority_grounding('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5', extraction).
narrative_ontology:cs_interpretation_layer_present('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5').
narrative_ontology:cs_reading_relation('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5', foundational, domestic_stability_supersedes_external_commitment).
narrative_ontology:cs_axiom_status(domestic_stability_supersedes_external_commitment, holdable).
narrative_ontology:cs_axiom_grounding('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5', domestic_stability_supersedes_external_commitment, conventional).
narrative_ontology:cs_axiom('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5', secondary, reserve_currency_issuer_retains_monetary_sovereignty).
narrative_ontology:cs_axiom_status(reserve_currency_issuer_retains_monetary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5', reserve_currency_issuer_retains_monetary_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5', bretton_woods_gold_dollar_standard).
narrative_ontology:cs_drift_state('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5', nixon_shock_1971, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('7523c2c2-e5f6-4d05-bf30-a2e4f90a49f5', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_treasury).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_domestic_policymakers).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_export_sector).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks_holding_dollars).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, european_dollar_reserve_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, gold_pool_participants).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, monetary_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the gold window and decides in practice when convertibility requests are honored promptly, delayed through diplomatic pressure, or ultimately suspended (as in August 1971). Treats the Article IV commitment as one input among several domestic priorities — employment, the Vietnam War budget, election cycles — and reserves the right to subordinate it to those priorities. Bears essentially no direct cost when it defers or exits gold obligations; the readjustment cost lands on those holding dollar claims.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_treasury, agenda_setter,
    institutional, generational, arbitrage, global).

% Runs fiscal and monetary policy (deficit spending, interest rate settings) without the discipline a hard convertibility rule would impose, because the convertibility obligation is understood internally as conditional on the balance of payments and domestic conditions allowing it. Gains policy space directly from the constraint's flexible interpretation.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_domestic_policymakers, beneficiary,
    institutional, biographical, mobile, national).

% Benefits when the dollar's peg is effectively loosened or suspended, since a devalued or floating dollar improves export competitiveness. Has no formal role in the convertibility decision but is a downstream beneficiary of the policy-flexible reading being acted upon.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_export_sector, beneficiary,
    organized, biographical, mobile, national).

% Accumulated dollar reserves under the Bretton Woods arrangement on the understanding that convertibility to gold was the system's anchor. Under the policy-flexible reading, discover the anchor is conditional and can be withdrawn unilaterally when it conflicts with U.S. domestic priorities. Cannot easily divest large dollar holdings without triggering the very devaluation they are trying to avoid, and cannot compel U.S. compliance — no external enforcement mechanism exists against the issuing sovereign.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks_holding_dollars, payer,
    institutional, generational, constrained, global).

% France and other European holders periodically test the convertibility commitment by requesting gold redemption at scale (de Gaulle's gold repatriation policy). Each test reveals the obligation's conditional character more starkly, and each round of dollar accumulation between tests increases exposure to an eventual suspension. Bear the devaluation loss when it comes.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, european_dollar_reserve_holders, payer,
    organized, biographical, constrained, continental).

% The London Gold Pool central banks jointly sold gold to defend the $35/oz price through the 1960s, subsidizing the U.S. commitment with their own reserves even as they had no control over U.S. fiscal and monetary choices driving the pressure on that price. Pool collapses in 1968, and full suspension follows in 1971 — the participants absorb the transition costs of a commitment they helped fund but did not set.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, gold_pool_participants, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, gold_pool_participants, agenda_setter).

% Administers the Article IV framework nominally but has no enforcement power over the reserve-currency issuer; can document breaches and convene negotiations (leading eventually to the Smithsonian Agreement and floating rates) but cannot compel adherence.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__policy_flexible_reading, us_treasury).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__policy_flexible_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a nominal anchor for post-war exchange rates, letting trading partners price currencies against a gold-backed dollar rather than negotiate bilateral pegs from scratch — genuine coordination value in the early Bretton Woods years.
% TRANSFER_FUNCTION: Moves monetary policy autonomy and adjustment costs from the United States to dollar-holding foreign central banks: the U.S. retains freedom to run domestic policy as it sees fit, and when that policy conflicts with the gold peg, the resulting devaluation risk and reserve losses land on those holding dollar claims rather than on U.S. domestic constituencies.
% ABSENT_VOICES: Ordinary savers and firms in dollar-reserve countries whose national reserves lose value are never party to the U.S. domestic policy debates that determine whether convertibility will be honored; they experience the consequences of decisions made entirely inside U.S. institutions.
% DISAPPEARANCE_RATIONALE: The August 1971 suspension is precisely this disappearance event: when the conditional character of the obligation was finally exercised in full, the entire post-war exchange rate architecture reorganized into floating rates within two years (Smithsonian Agreement failing by 1973), central banks restructured reserve management, and the IMF's Article IV was rewritten to reflect a non-gold-backed system.
% FOUNDING_PROBLEM: Post-war reconstruction required a stable, credible international payments system to avoid the competitive devaluations and trade collapse of the 1930s; convertibility to gold at a fixed dollar price was meant to anchor confidence in the system without requiring a literal gold standard for every currency.
% FOUNDING_PROBLEM_CORROBORATION: Foreign central bank officials (notably French monetary authorities in the 1960s) and later IMF historical reviews attest that by the mid-1960s the U.S. balance-of-payments deficit and gold outflows had made the peg's maintenance incompatible with U.S. domestic policy goals, and that the U.S. treated the obligation as suspendable well before it formally was — corroboration from outside the beneficiary set (European reserve holders, subsequent independent monetary historians), not merely from U.S. Treasury self-justification.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.25 (1958, early strain visible but manageable) to 0.58 by 1971 (full suspension) as the gap between U.S. domestic policy choices and the gold-backing commitment widens — this is the same underlying historical arc as the sibling readings but interpreted here as U.S. policy autonomy being exercised at the expense of dollar holders, rather than as either a binding-law violation or a structural design failure. Suppression is moderate (0.42): there is no formal coercive apparatus preventing central banks from requesting gold, but the U.S. used diplomatic leverage (pressuring allies not to convert, as with West Germany) to suppress redemption requests, which is real but falls short of the suppression seen in constraints with direct enforcement machinery. Theater ratio climbs to 0.4 by 1971, reflecting the growing gap between the Bretton Woods system's formal architecture (still nominally in force) and its actual operation (a de facto dollar standard propped up by moral suasion).
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. Treasury's seat, the arrangement looks like prudent policy flexibility — retaining the sovereign right to set domestic policy without being bound by a rigid external commitment. From the seat of foreign central banks holding accumulated dollar reserves, the same arrangement looks like a slow-motion transfer of adjustment costs: they accumulated dollars in good faith under a nominally fixed peg and bore the loss when the peg's conditional character was finally invoked. The engine should compute these as different types from the same structural data — this divergence is the point of the reading, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. Treasury and domestic policymakers sit near the full-beneficiary end: they set the terms, retain policy autonomy, and bear no direct cost from suspension. Foreign central banks and European reserve holders sit near the full-target end: they hold dollar claims they cannot easily liquidate without triggering the loss they fear, and have no enforcement mechanism against the issuing sovereign — constrained exit, high realized cost. Gold Pool participants are dual-positioned: they co-administered the defense mechanism (secondary agenda_setter role) while also absorbing its costs, which is why they carry both roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war exchange rate stability) was substantially solved by the mid-1960s in the narrow sense that trade and reconstruction had normalized; what persisted was a peg that had become policy-incompatible with U.S. domestic priorities. Classifying this as tangled_rope rather than snare preserves the genuine early coordination function (1944-late 1950s) while recognizing that by the 1960s the same structure was being actively used to extract policy space from dollar holders — it is not pure extraction from inception, which is why tangled_rope rather than snare is the structurally correct claim despite substantial victim-side extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditional_vs_binding_obligation,
    'Was the Article IV convertibility commitment ever, as a matter of international law and U.S. self-understanding at the time, genuinely unconditional — or was it understood by U.S. policymakers from the outset as implicitly conditional on domestic stability, with the 1971 suspension simply making explicit what had always been the operative interpretation?',
    'Archival review of U.S. Treasury and Federal Reserve internal deliberations from the Bretton Woods negotiations through the 1960s; comparison with contemporaneous statements by U.S. officials to allied central banks about the durability of the commitment.',
    'If the commitment was genuinely understood as unconditional at signing, this reading describes a later reinterpretation/breach rather than the system''s true original character, strengthening the strict_convertibility_reading''s claim that a binding obligation was violated. If conditionality was baked in from the start (as some IMF architects'' own writings suggest, given awareness of adjustment mechanisms), this reading''s structural delta is closer to descriptively accurate from day one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_vs_binding_obligation, empirical, 'Whether the conditional character was original design or later unilateral reinterpretation.').

omega_variable(
    sibling_reading_divergence_locus,
    'Where exactly does the disagreement between the three kernel readings live — is it a disagreement about historical fact (what the U.S. actually did and why), about legal interpretation (what Article IV actually required), or about systemic diagnosis (whether the Triffin dilemma made any convertibility commitment unsustainable regardless of U.S. policy choices)?',
    'This is inherently a framing question rather than one resolvable by additional data: the strict_convertibility_reading and this policy_flexible_reading agree on the historical facts (what happened) but disagree on the legal/moral characterization (whether it was a breach or an exercise of legitimate discretion); the triffin_structural_reading agrees with neither on locus, placing the extraction in the system''s design rather than in either party''s choices.',
    'Determines which reading a given institutional actor (IMF legal staff vs. U.S. Treasury vs. structural economists) will find persuasive, and therefore which constraint story should be treated as authoritative for a given analytical purpose. No single resolution collapses the three readings into one — they remain three distinct constraints by design (ε-invariance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_divergence_locus, conceptual, 'The three kernel readings diverge on legal/historical/systemic locus, not on contested facts alone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1958, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement(doll_tr_t1961, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1961, 0.24).
narrative_ontology:measurement(doll_tr_t1964, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1964, 0.28).
narrative_ontology:measurement(doll_tr_t1967, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1967, 0.34).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1968, 0.38).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.4).

% Extraction over time
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1958, 0.25).
narrative_ontology:measurement(doll_be_t1961, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1961, 0.34).
narrative_ontology:measurement(doll_be_t1964, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1964, 0.43).
narrative_ontology:measurement(doll_be_t1967, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1967, 0.5).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1968, 0.54).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1958, 0.2).
narrative_ontology:measurement(doll_su_t1961, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1961, 0.26).
narrative_ontology:measurement(doll_su_t1964, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1964, 0.32).
narrative_ontology:measurement(doll_su_t1967, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1967, 0.37).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1968, 0.4).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__policy_flexible_reading, 0.12).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the dollar_gold_convertibility kernel. strict_convertibility_reading treats Article IV as a binding legal obligation and places the U.S. among the constrained/bound parties; this policy_flexible_reading treats the same obligation as conditional on domestic stability and places the U.S. among the beneficiaries with dollar holders as victims — the beneficiary/victim sets are structurally inverted between the two readings, which is why they are separate stories rather than one story with a measurement parameter. triffin_structural_reading locates the extraction in the system's design (the reserve-currency country must run deficits to supply liquidity, guaranteeing eventual instability regardless of policy choices) rather than in either party's discretionary conduct, giving it yet a third distinct beneficiary/victim structure. All three share the same underlying historical events (1958-1971) but diverge in structural attribution of who is bound, who benefits, and why the strain emerged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
