% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Inevitability Reading of the Monetary Anchor Principle
 *   domain: economic/international_finance/political_economy
 *
 * SUMMARY:
 *   This file authors ONE reading of the kernel monetary_anchor_principle:
 *   the claim that the end of Bretton Woods was not chosen but compelled —
 *   that any system tying world liquidity to a gold-convertible national
 *   currency contains a counting contradiction (reserve demand grows with
 *   world trade faster than the monetary gold stock at a fixed parity, so the
 *   issuer must run deficits that erode the convertibility promise) and must
 *   therefore terminate. Per the epsilon-invariance rule, the colloquial
 *   question 'why did Bretton Woods end?' decomposes into three structurally
 *   distinct constraints; this story instantiates only the inevitability
 *   reading, links its siblings through network edges, and hedges nothing
 *   across readings. The epsilon referent is the standing arrangement under
 *   contest — the Bretton Woods gold-exchange standard — assessed by this
 *   reading's own lights: an arrangement that genuinely supplied world
 *   liquidity while channeling seigniorage to the issuer and building a
 *   liability structure whose exhaustion was arithmetically dated. The
 *   claim/metrics gap is deliberate: the reading claims mountain (logical
 *   impossibility, zero degrees of freedom), while the authored metrics
 *   record that the constraint's operation transferred real resources to
 *   identifiable seats and that, after 1971, the constraint persists chiefly
 *   as performed doctrine — the engine measures that divergence. KEY AGENTS
 *   (by structural relationship): - united_states_monetary_authorities:
 *   agenda-setting beneficiary (institutional/arbitrage) — administers
 *   convertibility, collects seigniorage, holds unilateral exit -
 *   foreign_dollar_reserve_holders: primary target (powerful/trapped) —
 *   accumulate dollar claims whose collective redemption destroys their own
 *   value - nonissuer_pegged_economies: secondary target
 *   (moderate/constrained) — inherit US monetary conditions and adjustment
 *   burdens - bretton_woods_governance_institutions: institutional victim
 *   (institutional/trapped) — machinery whose purpose the bind exhausts -
 *   international_macroeconomics_profession: epistemic beneficiary
 *   (organized/identity_locked) — careers and curricula fused to the
 *   canonical diagnosis - gold_standard_restorationists: excluded voice
 *   (organized/identity_locked) — rival diagnosis kept outside the post-1971
 *   conversation - economic_historians: analytical observer — adjudicates
 *   structural versus volitional accounts
 *
 * KEY AGENTS:
 *   - united_states_monetary_authorities: agenda-setting beneficiary (institutional/arbitrage) — administers convertibility, collects seigniorage, holds unilateral exit
 *   - foreign_dollar_reserve_holders: primary target (powerful/trapped) — accumulate dollar claims whose collective redemption destroys their own value
 *   - nonissuer_pegged_economies: secondary target (moderate/constrained) — inherit US monetary conditions and adjustment burdens
 *   - bretton_woods_governance_institutions: institutional victim (institutional/trapped) — machinery whose purpose the bind exhausts
 *   - international_macroeconomics_profession: epistemic beneficiary (organized/identity_locked) — careers and curricula fused to the canonical diagnosis
 *   - gold_standard_restorationists: excluded voice (organized/identity_locked) — rival diagnosis kept outside the post-1971 conversation
 *   - economic_historians: analytical observer — adjudicates structural versus volitional accounts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.28).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.24).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.86).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Inevitability Reading of the Monetary Anchor Principle").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "economic/international_finance/political_economy").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '2a272929-6399-42ce-a16a-1429770a071d').
narrative_ontology:cs_kernel_codification('2a272929-6399-42ce-a16a-1429770a071d', formalized).
narrative_ontology:cs_authority_grounding('2a272929-6399-42ce-a16a-1429770a071d', expertise).
narrative_ontology:cs_interpretation_layer_present('2a272929-6399-42ce-a16a-1429770a071d').
narrative_ontology:cs_reading_relation('2a272929-6399-42ce-a16a-1429770a071d', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a272929-6399-42ce-a16a-1429770a071d', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('2a272929-6399-42ce-a16a-1429770a071d', foundational, structural_necessity_precludes_regime_choice).
narrative_ontology:cs_axiom_status(structural_necessity_precludes_regime_choice, holdable).
narrative_ontology:cs_axiom_grounding('2a272929-6399-42ce-a16a-1429770a071d', structural_necessity_precludes_regime_choice, empirically_contingent).
narrative_ontology:cs_axiom('2a272929-6399-42ce-a16a-1429770a071d', secondary, liquidity_provision_compels_deficit_issuance).
narrative_ontology:cs_axiom_status(liquidity_provision_compels_deficit_issuance, holdable).
narrative_ontology:cs_axiom_grounding('2a272929-6399-42ce-a16a-1429770a071d', liquidity_provision_compels_deficit_issuance, instrumental).
narrative_ontology:cs_reference_frame('2a272929-6399-42ce-a16a-1429770a071d', structural_necessity_canon).
narrative_ontology:cs_drift_state('2a272929-6399-42ce-a16a-1429770a071d', contemporary_pluralist_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a272929-6399-42ce-a16a-1429770a071d', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__triffin_inevitability_reading, united_states_monetary_authorities).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__triffin_inevitability_reading, international_macroeconomics_profession).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, nonissuer_pegged_economies).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_governance_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Federal Reserve and US Treasury jointly administer the anchor: they pledge dollar-gold convertibility at $35/oz, manage the gold-pool commitments, and decide when the defenses fail. Running the payment deficits that supply world reserves also finances domestic spending and foreign military commitments without immediate taxation — real goods flow in against paper liabilities. Exit is asymmetrically theirs: they can suspend convertibility by executive action, converting an obligation into a policy announcement. Their horizon is the electoral and administrative cycle.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, united_states_monetary_authorities, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__triffin_inevitability_reading, united_states_monetary_authorities, beneficiary).

% Economists build careers, curricula, and policy authority around the dilemma as a canonical result: it anchors graduate teaching, motivates research programs on international money, and supplies the standard argument that commodity anchors cannot survive open world growth. Leaving the framework means discarding the accumulated literature one's standing rests on; the profession's identity is fused with the diagnosis it canonized.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, international_macroeconomics_profession, beneficiary,
    organized, generational, identity_locked, global).

% Central banks of surplus countries — the Bundesbank, Bank of Japan, Banque de France — accumulate dollar claims as the byproduct of defending their own parities. Each dollar added is a claim on US gold they cannot present en masse: collective presentation collapses the dollar and vaporizes the value of their own holdings, so rational individual behavior (hold, protest, accumulate) deepens the exposure. Converting to gold is rationed by pool rules and diplomatic pressure; exit would destroy the value they are trying to protect.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, foreign_dollar_reserve_holders, payer,
    powerful, generational, trapped, global).

% Countries without reserve-currency status peg to the dollar and inherit its dilemmas: they must hold dollar reserves of declining real value, import US monetary conditions, and adjust domestic prices and wages whenever US deficits force the issue. Their policy autonomy is subordinate to an arrangement they help neither design nor dissolve; leaving means losing the anchor that disciplines their trade.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, nonissuer_pegged_economies, payer,
    moderate, generational, constrained, regional).

% The IMF, the BIS committees, and the gold-pool apparatus exist to operate convertibility: surveillance, swap lines, pool transactions. Their founding purpose is the arrangement the bind exhausts; they cannot repurpose themselves while the arrangement lives, and they lose it when it dies, and their staffs' professional lives are bound to the machinery.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_governance_institutions, payer,
    institutional, generational, trapped, global).

% Hard-money schools — Rueff's heirs, gold-standard advocacy networks — hold that the diagnosis is wrong or incomplete: the culprit was issuer profligacy, removable by discipline or a higher gold price, not arithmetic necessity. After 1971 they are outside the policy conversation; their objections appear in books and minority reports rather than negotiations.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, gold_standard_restorationists, excluded,
    organized, civilizational, identity_locked, global).

% Historians and historically minded economists reconstruct the decision record — the 1971 Camp David weekend, the 1968 pool collapse, the SDR negotiations — testing structural against volitional accounts. They collect no flows and bear no exposure; their product is the adjudication the other seats dispute.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__triffin_inevitability_reading, united_states_monetary_authorities).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__triffin_inevitability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interwar problem of chaotic, beggar-thy-neighbor exchange arrangements by supplying a common unit of account, fixed parities, and an elastic source of world reserves through the reserve-currency issuer's liabilities.
% TRANSFER_FUNCTION: Moves real goods, services, and assets from the rest of the world to the reserve-issuer economy in exchange for dollar liabilities held as reserves; at the termination, moves the resulting losses onto reserve holders through suspension of convertibility and subsequent devaluation.
% ABSENT_VOICES: Hard-money critics and gold-standard restorationists were heard before 1971 but excluded from the decision itself — the closure was an executive act announced on a Sunday night without legislative or creditor consultation; creditor-country publics learned the terms after the fact.
% DISAPPEARANCE_RATIONALE: If the bind did not hold — if convertibility could have been sustained by parameter adjustment — the 1971 rupture loses its shape: parities persist, reserve composition grows along a different path, and the floating-rate era's institutions (fund surveillance of floats, standing swap networks) do not arise as responses to a collapse that never came. Trade, banking, and policy-coordination arrangements reorganize around a continuing anchor.
% FOUNDING_PROBLEM: After the interwar breakdown — competitive devaluations, the 1930s gold blocs, chronic reserve shortage — rebuild a multilateral payments order with fixed but adjustable parities, anchored in gold-convertible national currencies, so that trade could expand without each country engineering its own advantage.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the Bretton Woods conference records and the Keynes and White plans state the founding problem independently of later doctrine; economic historians (Eichengreen, Bordo, James) reconstruct it from archives; the IMF's own institutional history attests it — though that last source sits partly inside the surviving institution and is weighted accordingly.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are authored independently. The reading's claim is mountain: within the framework's own parameters the arithmetic admits no steady state, so accessibility_collapse is high (0.86) — once the counting is grasped, within-frame alternatives collapse almost completely. Resistance is moderate (0.48), not the near-zero of a certified natural law, because the bind met real intellectual resistance (the Despres-Kindleberger-Salant intermediation rebuttal, Rueff's rival diagnosis blaming policy rather than arithmetic) and sustained political patching (gold pool, two-tier market, SDRs) that deferred rather than refuted it. Extractiveness (0.28) records what the arrangement's operation actually moved while the bind tightened — compelled deficit finance delivered real goods to the issuer against paper claims later written down — while the bind itself, as arithmetic, extracts nothing; the reading-indexed value sits deliberately below the snare range because the reading's own lights attribute the flow to systemic duty rather than predation, but above zero because the transfer and its incidence are facts the reading concedes. Suppression (0.24) is unscaled and structural-doctrinal: the 'it is just arithmetic' move closes debate without coercive machinery. Theater_ratio (0.60 at interval end) tracks the post-1971 transmutation — with the operative function dead, invocation of the dilemma became largely ritual (textbook canonization, rhetorical deployment in floating-rate and digital-currency debates), crossing the 0.5 Goodhart line; the series shows theater rising monotonically after the 1971 discontinuity while material extractiveness fell. All tracked series share one seven-point grid (1960, 1965, 1970, 1971, 1980, 2000, 2025); the 2025 points are marked projected. No suppression_requirement series is authored: the enforcement picture is static-by-nature (arithmetic needs no enforcement machinery), so the scalar captures it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the issuer seat the arrangement is a duty faithfully discharged until arithmetic intervened — low effective extraction, a subsidy-like position, and an exit (suspending convertibility) that no other seat possesses. From the reserve-holder seat the same years are a trap: each defensive dollar purchase deepened exposure, so the most powerful actors behaved as constrained targets. The profession's seat is neither — it collects epistemic rents, and its identity lock stabilizes the constraint's doctrinal form after the material form dies. The engine derives these divergences from role, exit, and lock; this story does not adjudicate which seat's experience is 'the' constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: united_states_monetary_authorities (material seigniorage while the bind operated) and international_macroeconomics_profession (doctrinal rents after) derive low d — subsidy-side positions; the issuer's arbitrage-grade exit caps any target-side exposure at zero. Victim declarations: foreign_dollar_reserve_holders (powerful but trapped) sit nearest the full-target end — trapping, not powerlessness, is what pins d high here, since exit would self-destruct the holdings they are protecting; nonissuer_pegged_economies (constrained) sit high but below them; bretton_woods_governance_institutions (trapped, institutional) bear the dissolution directly. No directionality overrides were needed: the derivation from declarations plus exit atoms reproduces the qualitative ordering, and the one candidate correction — discounting the profession's derived d because its rents are reputational rather than material — is handled by the seigniorage_beneficiary_attribution omega rather than a per-story override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is keeping three errors apart. Calling the whole constraint a snare would mistake the seigniorage flow for capture by design — but the reading's lights, and the coordination function the arrangement genuinely performed, forbid that; the flow rode a functioning liquidity machine. Certifying the mountain claim unexamined would launder a contested historiographic thesis into natural law while identifiable seats collect from the claim's operation — hence the FSM-triggering beneficiary declarations and the naturalness omega. And missing the post-1971 phase would hide the piton-shaped residue: the operative function died in 1971, yet the constraint persists with rising theater_ratio, maintained by pedagogical inertia and rhetorical deployment. The R5 interview records the founding problem as live (orderly multilateral payments remains a real problem), so no dead-mandate zombie flag fires, but the theater series documents that what persists is increasingly performance. Mandatrophy resolution therefore leaves the type question to the computed signatures while recording, as data, exactly where each mislabel would go wrong.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the triffin_inevitability_reading of kernel monetary_anchor_principle; its siblings relocate the causal locus — punctuated_swap_reading to a discrete choice event on 15 August 1971, overdetermined_composite_reading to a conjunction of pressures none singly sufficient. Which locus is structurally correct?',
    'Comparative adjudication on the decision record: archival reconstruction of the 1971 deliberations establishing whether any anchor-preserving option remained on the table, paired with econometric counterfactuals testing whether parameter adjustments (gold repricing, SDR substitution at scale) sustain convertibility.',
    'If the choice or composite locus wins, this reading''s mountain claim fails — a chosen termination implies policy responsibility, contestable rents, and a much higher epsilon; the constraint reclassifies away from the natural-law profile and toward the siblings'' structural profiles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Location of the causal locus across sibling readings of the monetary-anchor kernel.').

omega_variable(
    naturalness_vs_design_parameters,
    'Is the bind an irreducible logical limit of any commodity-anchored reserve system, or contingent on Bretton Woods design parameters — the fixed $35 parity, the dollar as sole marginal reserve asset, and the absence of a scaled supranational substitute?',
    'Formal sensitivity analysis: sweep the parameter space (parity flexibility, reserve-asset substitution elasticities, gold-stock growth) and test whether any region admits a stationary convertibility equilibrium.',
    'A natural-law verdict certifies the mountain profile; a constructed verdict identifies beneficiaries of the design choices themselves and routes the constraint through the false-summit chain toward hybrid coordination/extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_design_parameters, empirical, 'Natural law versus designed-parameter contingency of the reserve-currency bind.').

omega_variable(
    despres_kindleberger_intermediation_critique,
    'Was the reserve issuer''s deficit-running compelled liquidity provision (the bind''s load-bearing premise) or profitable voluntary intermediation — borrowing short through reserve liabilities and lending long — as Despres, Kindleberger, and Salant argued in 1966?',
    'Balance-sheet and flow-of-funds reconstruction of the US external position 1958-1971: separate seigniorage returns from intermediation spreads, and test whether the maturity transformation was stabilizing or destabilizing.',
    'If intermediation dominates, the ''must'' premise fails, the foundational axiom loses its empirical ground, and this reading collapses toward the composite sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(despres_kindleberger_intermediation_critique, empirical, 'Compelled duty versus profitable intermediation as the account of issuer deficits.').

omega_variable(
    seigniorage_beneficiary_attribution,
    'Do the seigniorage flows make the reserve issuer a beneficiary of the bind''s operation, or only of the Bretton Woods arrangement the bind destroys — and do the profession''s rents attach to the constraint or to the doctrine about it?',
    'Counterfactual welfare accounting: issuer and profession payoffs under the fixed arrangement with and without the bind''s arithmetic holding, separating gains sourced in the constraint from gains sourced in what it terminates.',
    'Determines whether the declared beneficiaries reflect this constraint''s operation or its victim''s; a negative verdict clears the false-summit trigger and restores the pure natural-law profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_beneficiary_attribution, conceptual, 'Attribution of declared benefits to the bind versus the arrangement it exhausts.').

omega_variable(
    doctrine_rent_contamination,
    'Do career, curricular, and policy-authority rents accruing to holders of the inevitability narrative sustain the claim by incumbency rather than evidence — and if so, how much of the post-1971 persistence is theatrical maintenance?',
    'Citation-network and curriculum longitudinal analysis: track whether the inevitability claim''s prevalence tracks new evidence or positional inheritance, and correlate with the theater_ratio series.',
    'A rent-driven verdict downgrades confidence in the mountain certification and shifts the constraint''s contemporary classification toward the degraded/inertial profile; an evidence-driven verdict supports the reading''s stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_rent_contamination, empirical, 'Whether doctrinal persistence tracks evidence or incumbency rents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1960, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1965, 0.16).
narrative_ontology:measurement(mone_tr_t1970, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.45).
narrative_ontology:measurement(mone_tr_t1980, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1980, 0.55).
narrative_ontology:measurement(mone_tr_t2000, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 2000, 0.58).
narrative_ontology:measurement(mone_tr_t2025, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(mone_be_t1960, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1960, 0.18).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1965, 0.26).
narrative_ontology:measurement(mone_be_t1970, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.42).
narrative_ontology:measurement(mone_be_t1980, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1980, 0.24).
narrative_ontology:measurement(mone_be_t2000, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(mone_be_t2025, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 2025, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monetary_anchor_principle__triffin_inevitability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'why did Bretton Woods end?' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that must not share one story: this file authors the inevitability reading (mountain-claimed, epsilon 0.28 over the BW arrangement as the reading sees it); monetary_anchor_principle__punctuated_swap_reading authors the discrete-choice reading; monetary_anchor_principle__overdetermined_composite_reading authors the conjunction reading. Causal-direction structure: the inevitability reading is upstream of the composite reading (the composite incorporates the Triffin component and must argue it down from sufficiency to contributory status — hence the influences edge), while the punctuated reading stands in rivalry with both without logical elimination in any single party's framework (hence coexists_with). Each family member carries its own epsilon, beneficiary/victim structure, and classification; the family edges enable contamination propagation when any member's purity assessment shifts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
