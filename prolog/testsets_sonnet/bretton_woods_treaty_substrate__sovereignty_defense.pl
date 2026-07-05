% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Gold-Dollar Peg as Sovereignty Defense Mechanism
 *   domain: international political economy / monetary history / institutional design
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-defense reading of the Bretton
 *   Woods kernel: the arrangement is read as a system of constraints on
 *   external monetary discipline that ostensibly preserves national monetary
 *   sovereignty. Under this reading, the promise of sovereignty is delivered
 *   asymmetrically — the reserve-currency issuer (the United States) obtains
 *   genuine policy autonomy and a structural rent (exorbitant privilege) from
 *   the gold-dollar anchor, while non-reserve states and especially
 *   peripheral developing economies experience the same anchor as an
 *   externally-imposed discipline mechanism that reduces, rather than
 *   defends, their monetary sovereignty. This is a distinct constraint from
 *   the embedded-liberalism reading (which frames Bretton Woods as protecting
 *   domestic policy space from capital mobility) and the
 *   neoliberal-convertibility reading (which frames it as constraining
 *   government intervention to enable free capital markets) — those are
 *   sibling stories with different beneficiary/victim structures and
 *   different epsilon values, linked via network.affects_constraints, not
 *   folded into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.71).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Gold-Dollar Peg as Sovereignty Defense Mechanism").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international political economy / monetary history / institutional design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '6a4a58bb-15c0-4c0a-8a11-94dca223b26f').
narrative_ontology:cs_kernel_codification('6a4a58bb-15c0-4c0a-8a11-94dca223b26f', formalized).
narrative_ontology:cs_authority_grounding('6a4a58bb-15c0-4c0a-8a11-94dca223b26f', extraction).
narrative_ontology:cs_interpretation_layer_present('6a4a58bb-15c0-4c0a-8a11-94dca223b26f').
narrative_ontology:cs_reading_relation('6a4a58bb-15c0-4c0a-8a11-94dca223b26f', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('6a4a58bb-15c0-4c0a-8a11-94dca223b26f', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_axiom('6a4a58bb-15c0-4c0a-8a11-94dca223b26f', foundational, reserve_issuer_autonomy_is_the_true_sovereignty_delivered).
narrative_ontology:cs_axiom_status(reserve_issuer_autonomy_is_the_true_sovereignty_delivered, holdable).
narrative_ontology:cs_axiom_grounding('6a4a58bb-15c0-4c0a-8a11-94dca223b26f', reserve_issuer_autonomy_is_the_true_sovereignty_delivered, empirically_contingent).
narrative_ontology:cs_axiom('6a4a58bb-15c0-4c0a-8a11-94dca223b26f', secondary, peripheral_state_sovereignty_claims_are_formally_real_but_operationally_hollow).
narrative_ontology:cs_axiom_status(peripheral_state_sovereignty_claims_are_formally_real_but_operationally_hollow, holdable).
narrative_ontology:cs_axiom_grounding('6a4a58bb-15c0-4c0a-8a11-94dca223b26f', peripheral_state_sovereignty_claims_are_formally_real_but_operationally_hollow, empirically_contingent).
narrative_ontology:cs_reference_frame('6a4a58bb-15c0-4c0a-8a11-94dca223b26f', gold_dollar_convertibility_at_thirtyfive_per_ounce).
narrative_ontology:cs_drift_state('6a4a58bb-15c0-4c0a-8a11-94dca223b26f', nixon_shock_1971, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('6a4a58bb-15c0-4c0a-8a11-94dca223b26f', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, us_dollar_seigniorage_economy).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_developing_economies).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, national_policy_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the reserve currency that anchors the entire system and sets the gold convertibility rate the whole arrangement depends on. Can run persistent balance-of-payments deficits financed by other states' demand for dollars, effectively borrowing in its own currency at below-market terms — the 'exorbitant privilege.' Enforces the system's rules on others while retaining unilateral discretion over its own domestic monetary policy, since the dollar is the thing everyone else must hold reserves in.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury, beneficiary).

% Must peg their currencies to the dollar and hold dollar reserves to participate in the system, importing U.S. monetary conditions regardless of their own domestic needs. Adjustment falls on them: when their balance of payments deteriorates they face devaluation, capital controls, or IMF-conditioned austerity, while the reserve issuer faces no equivalent external discipline. Exit means abandoning convertibility and the trade/credit access that comes with it.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    moderate, biographical, constrained, national).

% Lack the reserves, credit lines, or negotiating leverage of mid-tier states. Commodity export earnings and access to development finance are denominated in and disciplined by the dollar system. A U.S. monetary tightening transmits directly into their capital costs and terms of trade with no seat at the table where those decisions are made. Their formal sovereignty over domestic monetary policy is real on paper and largely fictional in practice.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_developing_economies, payer,
    powerless, biographical, trapped, national).

% U.S. financial institutions, importers, and the federal government benefit from cheap external financing, low borrowing costs, and the ability to run deficits that would trigger crises in any other state. This is a structural rent flowing from the system's architecture, not a service rendered.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, us_dollar_seigniorage_economy, beneficiary,
    institutional, generational, arbitrage, global).

% Administers the adjustment mechanism, conditionality, and technical rules of convertibility. Frames its role as neutral technical stewardship of a coordination system, but its enforcement machinery (drawing rights, conditionality) falls almost exclusively on deficit states other than the reserve issuer, whose deficits are treated as a structural feature rather than a violation requiring correction.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf_and_bretton_woods_secretariat, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, imf_and_bretton_woods_secretariat, observer).

% Bears the real consequences of externally-imposed monetary discipline — wage compression, currency devaluation, austerity conditionality — without any representation in the negotiations that set the system's rules at Bretton Woods or in subsequent IMF governance, which remained dominated by the reserve-currency states and their allies.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, domestic_labor_and_industry_in_peripheral_states, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__sovereignty_defense, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common reference point (gold-backed dollar) so that international trade and payments can be settled predictably without every pair of states negotiating bilateral exchange arrangements, and provides an emergency lending facility so a state's temporary balance-of-payments shock does not force disorderly devaluation.
% TRANSFER_FUNCTION: Moves adjustment costs from the reserve-currency issuer to reserve-holding and deficit states: the U.S. finances its deficits by having the world hold dollars and accept dollar-denominated rules, while non-reserve states absorb the discipline (reserve accumulation costs, austerity conditionality, devaluation risk) that in a symmetric system would fall on whichever party ran the deficit.
% ABSENT_VOICES: Peripheral and colonial/newly-independent states were largely absent or marginal at the 1944 negotiations, which were dominated by the U.S. and UK; their domestic labor and industry constituencies had no representation at all. They would have argued for a genuinely symmetric adjustment mechanism (as Keynes's bancor proposal attempted) rather than one anchored to a single national currency.
% DISAPPEARANCE_RATIONALE: The 1971 Nixon shock is the natural experiment: when the gold-dollar convertibility mechanism was suspended, the world did not return to bilateral barter — floating exchange rates, new IMF surveillance roles, and a reconfigured (still dollar-centric but formally unpegged) system emerged. Sovereignty-defense claims persisted in altered form (capital controls, currency pegs to the dollar without gold backing) showing the underlying tension the constraint managed did not disappear, only the specific mechanism did.
% FOUNDING_PROBLEM: The interwar experience of competitive devaluation, beggar-thy-neighbor tariff wars, and the collapse of the gold standard convinced negotiators that states needed a stable international monetary anchor that would prevent both currency chaos and the kind of externally-imposed deflationary discipline (as under the classical gold standard) that had fueled depression-era political instability, while preserving room for domestic policy autonomy.
% FOUNDING_PROBLEM_CORROBORATION: IMF historical staff and mainstream monetary historians (e.g., Barry Eichengreen) corroborate that the original 1944 problem — chaotic interwar exchange arrangements — was real and substantially solved by the early 1950s. Independent scholars from developing-country institutions and dependency-theory economists (outside the U.S./UK negotiating parties and outside the IMF's own historical narrative) attest that the sovereignty-preserving framing was asymmetric from inception: it protected the reserve issuer's policy autonomy far more than it protected peripheral states', and that asymmetry, not the original 1944 problem, is what persisted past 1971 in modified form.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.35 at founding to 0.68 by 1973 because the gap between the reserve issuer's actual policy discretion and the nominal symmetric-adjustment design widens over the interval — the U.S. runs increasing deficits with no corresponding adjustment pressure while other states accumulate reserve or face IMF conditionality. Suppression climbs similarly (0.40 to 0.71) as the machinery required to keep pegs credible (capital controls, IMF conditionality, gold pool interventions) intensifies. Theater ratio rises to 0.42 by the end because an increasing share of the system's public justification (sovereignty preservation, stability provision) diverges from its operative function (dollar-centric asymmetric adjustment) — the 'sovereignty defense' framing becomes performative cover for what is functioning as extraction as the gold-dollar link becomes visibly unsustainable in the late 1960s.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. Treasury's seat, the system looks like it is delivering exactly what it promises: monetary sovereignty preserved, external discipline avoided. From the seat of a peripheral state undergoing an IMF-conditioned devaluation, the same structure looks like the imposition of exactly the kind of externally-dictated monetary discipline the system was said to prevent. The engine should compute divergent per-seat classifications from this same structural data — that divergence is the analytical payload of choosing the sovereignty-defense reading rather than collapsing it into a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States Treasury and the broader dollar-seigniorage economy sit at the beneficiary end: the constraint subsidizes their policy autonomy and financing costs precisely because of their position issuing the reserve asset, not because of any symmetric coordination benefit available to all parties equally. Non-reserve-currency states and peripheral developing economies sit at the target end: they bear the adjustment burden, hold reserves at a real economic cost, and face conditionality that reserve issuers do not. Domestic labor and industry within peripheral states are further downstream targets with no direct voice at all. The IMF/Bretton Woods secretariat is structurally positioned as an enforcement agenda-setter whose neutral-technical self-presentation masks its asymmetric application.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interwar monetary chaos) was substantially resolved by the early 1950s, yet the arrangement's core asymmetric structure persisted and hardened through the 1960s — this is the classic mandatrophy signature: a coordination mandate whose original justifying function has been satisfied while the mechanism itself, having proven advantageous to its most powerful stakeholder, is defended using its original justification rather than a justification matching its current operation. Classifying this as tangled_rope rather than a pure mountain or pure rope prevents two errors: it would be wrong to treat the sovereignty-defense framing as either fully organic (mountain, no beneficiary) or fully benign coordination (rope, no victim) — the structure has a genuine coordination residue (a common reference point did reduce certain transaction costs) fused with an asymmetric extraction that required active enforcement (IMF conditionality, gold pool defense, capital controls) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_symmetry_or_asymmetry,
    'Was the Bretton Woods sovereignty-preservation function genuinely symmetric by design (with asymmetry emerging only as an unintended operational drift) or was asymmetric protection of the reserve issuer''s autonomy built into the negotiated architecture from 1944 onward?',
    'Archival analysis of the 1944 negotiating record, particularly the rejection of Keynes''s bancor proposal (which would have imposed symmetric adjustment obligations on surplus and deficit countries alike) in favor of the White plan''s dollar-centric structure, would establish whether asymmetry was a design choice or an emergent drift.',
    'If asymmetry was designed in, the sovereignty-defense reading more closely resembles a snare wearing coordination language from inception; if it emerged only through later operational drift (e.g., post-Marshall Plan dollar shortage reversing into dollar glut), the tangled_rope classification with worsening metrics over time is the more accurate structural account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_symmetry_or_asymmetry, empirical, 'Whether reserve-issuer asymmetry was designed into Bretton Woods or emerged operationally.').

omega_variable(
    gold_anchor_stabilizer_or_snare,
    'Under this sovereignty-defense reading, does the gold-dollar anchor function primarily as a stabilizing coordination device (reducing transaction costs and providing a credible nominal anchor) or primarily as a snare that extracts adjustment costs from non-reserve states while shielding the reserve issuer?',
    'Comparative analysis of adjustment burden distribution: measuring the frequency, severity, and conditionality attached to devaluations/reserve interventions undertaken by non-reserve states versus the absence of equivalent constraint on U.S. deficit financing across the 1944-1973 interval.',
    'If the anchor is better characterized as snare-dominant rather than genuinely mixed, this reading''s tangled_rope claim would be too generous and a reclassification toward snare (with the coordination function treated as pure cover) would better match the metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_anchor_stabilizer_or_snare, conceptual, 'Whether the gold-dollar anchor''s dominant function in this reading is coordination or extraction.').

omega_variable(
    reading_selection_evidentiary_basis,
    'The kernel bretton_woods_treaty_substrate admits at least three coherent readings (embedded liberalism, neoliberal convertibility, sovereignty defense) with materially different beneficiary/victim structures and epsilon estimates. What historiographical or structural evidence favors treating the sovereignty-defense framing as the operative one for the 1944-1971 period specifically, rather than the embedded-liberalism framing that dominates much of the mainstream IPE literature (Ruggie''s account)?',
    'This is inherently a framing question rather than a fact resolvable by additional data: it depends on which stakeholder''s stated justification (domestic policy autonomy vs. sovereignty vs. market-enabling convertibility) is treated as the operative one, and different historiographical schools weight the 1944 negotiating record differently.',
    'This ambiguity is precisely why the constraint is authored as one of three sibling readings rather than as a single averaged constraint; adopting a different reading would shift which agents are beneficiaries and victims and would very likely shift the computed type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'Under-determination among coexisting readings of the Bretton Woods kernel; documents why this story is one reading among several rather than an aggregate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.15).
narrative_ontology:measurement(bret_tr_t1949, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(bret_tr_t1955, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1955, 0.28).
narrative_ontology:measurement(bret_tr_t1961, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1961, 0.34).
narrative_ontology:measurement(bret_tr_t1967, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1967, 0.4).
narrative_ontology:measurement(bret_tr_t1973, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1973, 0.42).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement(bret_be_t1949, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1949, 0.42).
narrative_ontology:measurement(bret_be_t1955, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1955, 0.51).
narrative_ontology:measurement(bret_be_t1961, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1961, 0.58).
narrative_ontology:measurement(bret_be_t1967, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1967, 0.64).
narrative_ontology:measurement(bret_be_t1973, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1973, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.4).
narrative_ontology:measurement(bret_su_t1949, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1949, 0.48).
narrative_ontology:measurement(bret_su_t1955, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1955, 0.55).
narrative_ontology:measurement(bret_su_t1961, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1961, 0.62).
narrative_ontology:measurement(bret_su_t1967, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1967, 0.68).
narrative_ontology:measurement(bret_su_t1973, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1973, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__sovereignty_defense, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the bretton_woods_treaty_substrate kernel, each authored as its own constraint with its own epsilon, beneficiaries, victims, and claimed type per the epsilon-invariance principle. sovereignty_defense locates the U.S. reserve-issuer as principal beneficiary and non-reserve/peripheral states as victims, treating the gold anchor as asymmetric discipline rather than either pure domestic-policy-space protection (keynesian_embedded_liberalism) or pure capital-market-enabling constraint on government intervention (neoliberal_convertibility). All three are linked bidirectionally in intent; contamination or purity shifts in one reading's evidentiary basis should be checked against the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
