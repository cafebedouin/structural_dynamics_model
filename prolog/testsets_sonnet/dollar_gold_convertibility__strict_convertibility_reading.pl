% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__strict_convertibility_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Article IV Convertibility as Binding Legal Obligation on U.S. Monetary Policy
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This story instantiates the strict_convertibility_reading of the
 *   dollar-gold convertibility kernel: Article IV of the IMF Articles of
 *   Agreement is read as a binding legal obligation, not a discretionary
 *   policy commitment, requiring the United States to redeem dollars
 *   presented by foreign monetary authorities at the fixed parity regardless
 *   of domestic economic conditions. Under this reading the U.S. Treasury and
 *   Federal Reserve occupy the victim/payer seat — a constrained issuer whose
 *   policy space is structurally narrowed by an enforceable external claim —
 *   while creditor central banks holding accumulated dollar reserves are
 *   beneficiaries with a legally cognizable right to extract gold. This is
 *   deliberately one reading among three sibling constraints sharing the same
 *   kernel: the policy_flexible_reading treats the same clause as conditional
 *   and subordinate to domestic stability (a materially different
 *   beneficiary/victim structure with the U.S. much closer to symmetric), and
 *   the triffin_structural_reading treats the whole arrangement as a doomed
 *   design flaw rather than either a binding obligation or a flexible one.
 *   Each reading is authored as its own ε-invariant constraint; this file
 *   does not average or hedge across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.71).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.62).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Convertibility as Binding Legal Obligation on U.S. Monetary Policy").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, 'eec97ae0-aaf1-4bb5-8516-c12399d56648').
narrative_ontology:cs_kernel_codification('eec97ae0-aaf1-4bb5-8516-c12399d56648', fixed_text).
narrative_ontology:cs_authority_grounding('eec97ae0-aaf1-4bb5-8516-c12399d56648', lineage).
narrative_ontology:cs_interpretation_layer_present('eec97ae0-aaf1-4bb5-8516-c12399d56648').
narrative_ontology:cs_reading_relation('eec97ae0-aaf1-4bb5-8516-c12399d56648', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_reading_relation('eec97ae0-aaf1-4bb5-8516-c12399d56648', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('eec97ae0-aaf1-4bb5-8516-c12399d56648', foundational, article_iv_text_binds_regardless_of_domestic_cost).
narrative_ontology:cs_axiom_status(article_iv_text_binds_regardless_of_domestic_cost, holdable).
narrative_ontology:cs_axiom_grounding('eec97ae0-aaf1-4bb5-8516-c12399d56648', article_iv_text_binds_regardless_of_domestic_cost, conventional).
narrative_ontology:cs_axiom('eec97ae0-aaf1-4bb5-8516-c12399d56648', secondary, treaty_commitment_outranks_domestic_stabilization_policy).
narrative_ontology:cs_axiom_status(treaty_commitment_outranks_domestic_stabilization_policy, overridden).
narrative_ontology:cs_axiom_grounding('eec97ae0-aaf1-4bb5-8516-c12399d56648', treaty_commitment_outranks_domestic_stabilization_policy, conventional).
narrative_ontology:cs_reference_frame('eec97ae0-aaf1-4bb5-8516-c12399d56648', bretton_woods_founding_text_as_hard_law).
narrative_ontology:cs_drift_state('eec97ae0-aaf1-4bb5-8516-c12399d56648', pre_nixon_shock_1971, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('eec97ae0-aaf1-4bb5-8516-c12399d56648', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, european_creditor_central_banks).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, gold_pool_surplus_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_treaty_architecture).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_treasury).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_federal_reserve).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_policy_space).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, pacta_sunt_servanda_in_monetary_treaties).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, fixed_parity_as_enforceable_commitment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound under Article IV of the IMF Articles of Agreement to redeem dollars presented by foreign monetary authorities at $35/ounce on demand. As gold reserves decline through the 1960s, the Treasury must subordinate domestic considerations to the redemption commitment, or be seen as violating a formal legal undertaking it signed at Bretton Woods. Exit would mean unilateral abrogation of a treaty obligation, with reputational and diplomatic costs. Under this reading it is the constrained party, not the discretionary steward of the system.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_treasury, payer,
    institutional, generational, trapped, global).

% Monetary policy decisions on interest rates and money supply are constrained by the need to defend the gold parity and prevent a run on Treasury reserves by foreign holders exercising their Article IV redemption right. Domestic employment and growth objectives are read, in this framing, as subordinate to the convertibility commitment — the Fed cannot expand freely without risking a breach of a binding external legal obligation.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_federal_reserve, payer,
    institutional, biographical, constrained, national).

% The set of fiscal and monetary policy options available to U.S. policymakers is narrowed by the binding redemption obligation: deficit spending, rate cuts, and expansionary policy are each weighed against their effect on gold outflows. This is a structural condition, not an actor, kept here to name what the obligation actually costs.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_policy_space, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_policy_space).

% Bundesbank, Banque de France, and other surplus-holding central banks accumulate dollar reserves through trade surpluses and hold an enforceable legal right under Article IV to convert those dollars into gold at the fixed parity. They can exercise this right unilaterally, extracting real value from the U.S. gold stock whenever they judge dollar exposure excessive or wish to pressure U.S. policy — most visibly France under de Gaulle in the mid-1960s.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, european_creditor_central_banks, beneficiary,
    institutional, generational, arbitrage, continental).

% A coalition of surplus nations participating in the London Gold Pool benefits from the credibility the binding-obligation reading lends to the parity, while retaining the individual option to convert reserves and thereby discipline U.S. policy. Their coordination is voluntary in form but backed by the treaty's enforceability.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, gold_pool_surplus_nations, beneficiary,
    organized, biographical, arbitrage, continental).

% The treaty framework itself is vindicated by this reading — every dollar redemption that occurs without renegotiation confirms that Article IV means what its text says. Listed for completeness as the doctrine benefiting from strict enforcement, not as a rent-collecting actor.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_treaty_architecture, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_treaty_architecture).

% Administers the Articles of Agreement and could, in principle, formally interpret, waive, or renegotiate Article IV obligations. Under the strict reading it treats the convertibility clause as a fixed legal text to be applied, not a policy lever to be adjusted, and resists framing U.S. difficulties as grounds for reinterpretation.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, imf_executive_board, agenda_setter,
    institutional, generational, analytical, global).

% Workers and borrowers affected by tight money defending the gold parity have no seat in the international legal forum where Article IV's binding character is asserted or contested; their domestic economic interests are not represented in the treaty-enforcement conversation at all.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_constituencies, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__strict_convertibility_reading, european_creditor_central_banks).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__strict_convertibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article IV's convertibility clause was designed to give the fixed-exchange-rate system a hard anchor: if every currency is pegged to a dollar that is itself convertible to gold at a fixed rate, exchange rate stability across the whole postwar trading system follows from one enforceable commitment.
% TRANSFER_FUNCTION: Under strict enforcement, the arrangement moves real reserve assets (gold) and policy discretion from the United States to creditor nations holding accumulated dollar balances, each time those nations exercise their Article IV redemption right rather than holding dollars as a store of value.
% ABSENT_VOICES: U.S. domestic constituencies bearing the cost of contractionary policy used to defend the parity have no standing in the international legal architecture where the obligation is asserted; the treaty conversation occurs between treasuries and central banks, not before domestic electorates.
% DISAPPEARANCE_RATIONALE: If the binding-obligation reading were abandoned overnight in favor of pure discretion, the entire fixed-parity system built on it would lose its anchor: creditor nations would have no enforceable claim, the Gold Pool coordination would dissolve, and the U.S. would gain immediate policy latitude at the cost of confidence in the dollar as a reserve asset — precisely what happened, in slower motion, culminating in the 1971 Nixon Shock.
% FOUNDING_PROBLEM: The interwar collapse of fixed exchange rates and competitive devaluation demonstrated that a monetary order without a hard, legally enforceable anchor invites beggar-thy-neighbor policy and currency chaos; Article IV was built to make the anchor a matter of law, not goodwill.
% FOUNDING_PROBLEM_CORROBORATION: U.S. Treasury officials in the late 1960s (Fowler, later Connally) and independent economic historians (Eichengreen, Bordo) attest from outside the creditor-nation beneficiary set that by the mid-1960s the founding problem of interwar instability had been supplanted by a new problem — U.S. gold reserves structurally insufficient to honor accumulated dollar claims — which the strict legal reading did not solve but merely delayed, ending in unilateral suspension in August 1971.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steadily across the interval (0.20 to 0.71) because the accumulating stock of foreign-held dollars under the fixed parity created a growing, legally enforceable claim on a fixed and depleting gold stock — the treaty's bite increased mechanically as the ratio of foreign dollar claims to U.S. gold reserves worsened, independent of any change in the treaty's text. Suppression tracks the same trajectory (0.30 to 0.62) because defending the parity required increasingly active measures — the Gold Pool, capital controls, Regulation Q adjustments, swap lines — each representing intensified enforcement machinery layered onto the original coordination function. Theater ratio remains comparatively low throughout (0.05 to 0.28) because the redemption mechanism was substantively real and exercised (not merely performed) until the 1971 suspension; the modest late-period rise reflects growing gaps between official reassurances of parity's permanence and private recognition that redemption was becoming untenable.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading's structural derivation, the U.S. Treasury and Federal Reserve are targets: they bear the cost of the binding obligation, have no meaningful exit short of unilateral treaty abrogation, and their institutional exit_options are coded trapped/constrained accordingly, pushing d toward the target end. European creditor central banks and Gold Pool nations are coded as beneficiaries with arbitrage-grade exit options — they can convert dollars to gold at will, extracting value on their own schedule, which pushes their d toward the beneficiary end. This is the inverse of the policy_flexible_reading, where the U.S. retains effective discretion and the beneficiary/victim structure is far more symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar currency chaos absent a legal anchor — was real in 1944 but had been effectively supplanted by a different problem (U.S. gold insufficiency relative to accumulated claims) by the mid-1960s. The strict reading's classification as tangled_rope rather than pure snare reflects that the underlying coordination function (a shared, credible reserve anchor for postwar trade) was genuine at founding; the tangled structure emerges because the same mechanism that solved 1944's problem became, by 1965-71, primarily an extraction channel from a structurally trapped issuer to mobile creditor holders, with the founding coordination story persisting as cover past its expiration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_bindingness_vs_political_discretion,
    'Was Article IV convertibility genuinely a binding legal obligation enforceable against U.S. domestic policy, or was it always understood by all parties as politically conditional and revisable — making the ''binding obligation'' framing itself a rhetorical construction that only later hardened into apparent law?',
    'Close reading of IMF Executive Board deliberations, U.S. Treasury internal memoranda from the Kennedy and Johnson administrations, and comparative analysis of how other Article IV signatories treated their own redemption obligations during balance-of-payments crises (e.g., UK sterling devaluations) to see whether unilateral suspension was treated as a live option or a genuine breach.',
    'If bindingness was always understood as conditional in practice, this reading''s classification collapses toward the policy_flexible_reading and the U.S. victim-seat framing weakens substantially; if bindingness was genuinely treated as hard law until 1971, this reading''s tangled_rope classification with the U.S. as structurally trapped payer is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_bindingness_vs_political_discretion, conceptual, 'Whether Article IV''s binding character was real law or retrospective legal framing of a political arrangement.').

omega_variable(
    committer_kernel_reading_divergence,
    'Given that policy_flexible_reading, strict_convertibility_reading, and triffin_structural_reading each instantiate structurally different beneficiary/victim sets and different extraction profiles from the same underlying Article IV text, which reading best describes the operative constraint actually governing U.S. policymakers'' decisions in real time, as opposed to the reading that best describes how the constraint was narrated after the fact?',
    'Cross-reference contemporaneous internal deliberation records (FOMC transcripts, Treasury cables, IMF staff reports) across all three readings'' predicted decision patterns to see which reading''s predicted policy behavior best matches the documented record at each decision point from 1960-1971.',
    'If the strict reading''s predicted rigidity does not match the documented flexibility U.S. policymakers actually exercised (e.g., repeated ad hoc departures, swap line improvisation), the operative constraint was closer to policy_flexible_reading and this file''s high extractiveness/high suppression profile overstates the U.S.''s structural entrapment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_reading_divergence, empirical, 'Which of the three sibling kernel readings best matches the documented decision record, versus the post-hoc narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(doll_tr_t1949, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1949, 0.07).
narrative_ontology:measurement(doll_tr_t1955, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1955, 0.1).
narrative_ontology:measurement(doll_tr_t1960, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1960, 0.14).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.28).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1944, 0.2).
narrative_ontology:measurement(doll_be_t1949, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1949, 0.28).
narrative_ontology:measurement(doll_be_t1955, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1955, 0.38).
narrative_ontology:measurement(doll_be_t1960, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1965, 0.63).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1968, 0.69).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1944, 0.3).
narrative_ontology:measurement(doll_su_t1949, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1949, 0.34).
narrative_ontology:measurement(doll_su_t1955, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1955, 0.4).
narrative_ontology:measurement(doll_su_t1960, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1960, 0.48).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1965, 0.56).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, london_gold_pool_coordination).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_par_value_system).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'Article IV convertibility' per the epsilon-invariance principle: strict_convertibility_reading (this file, tangled_rope, high extraction from a trapped U.S. issuer), policy_flexible_reading (lower extraction, more symmetric U.S. position, convertibility as conditional), and triffin_structural_reading (the sustainability question treated as architectural rather than either a strict-law or flexible-policy question). All three share the same kernel text but instantiate different epsilon values and different beneficiary/victim structures; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
