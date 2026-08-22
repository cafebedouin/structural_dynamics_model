% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Triffin Inevitability Reading — the Gold-Dollar Anchor as Structural Impossibility
 *   domain: monetary economics/political economy/international finance
 *
 * SUMMARY:
 *   The arrangement under contest is the Bretton Woods gold-dollar anchor
 *   (1944-1971): fixed par values, official dollar convertibility into gold
 *   at $35 an ounce, and the US dollar as the system's principal reserve
 *   asset. This story instantiates the triffin_inevitability_reading of the
 *   monetary_anchor_principle kernel: the claim that the anchor carried an
 *   insoluble contradiction — a reserve-currency issuer under gold
 *   convertibility must supply world liquidity by running deficits, dollar
 *   liabilities therefore outgrow the gold stock, and abandonment is
 *   structurally forced. Per the one-reading rule, the story authors only
 *   this reading: the claim is recorded as a mountain-type assertion
 *   (emerges_naturally true), while the metrics and structural data describe
 *   the anchor's actual operation — a constructed arrangement, administered
 *   by the US Treasury with IMF participation, defended by the London Gold
 *   Pool and escalating capital controls, yielding seigniorage and adjustment
 *   asymmetry to the issuer, and terminated by a discretionary unilateral act
 *   after a live reform menu (gold-price adjustment, SDR substitution, wider
 *   bands, negotiated revaluation) was bypassed. Beneficiaries are declared
 *   deliberately: the reading frames the collapse as a beneficiary-free
 *   system failure, but the structural record shows concentrated gains, so
 *   the false-summit evaluation path stays live, and omega
 *   natural_law_vs_constructed_constraint documents the required
 *   natural-law-versus-constructed ambiguity. Sibling readings are separate
 *   constraints; the kernel contest is routed to omegas, not folded into this
 *   story's epsilon. Note on epsilon: the manifest's very-low starting bin is
 *   refined upward to the moderate band — the inevitability framing
 *   naturalizes the collapse but does not erase the issuer privilege the
 *   dilemma itself names; this reading authors that privilege as structural
 *   rather than discretionary, which keeps epsilon below what a
 *   culpability-centered sibling would author over the same referent.
 *
 * KEY AGENTS:
 *   - us_monetary_authorities: Agenda-setting issuer (institutional/arbitrage) — administered the anchor, collected seigniorage, held the unilateral exit it ultimately exercised
 *   - foreign_dollar_holders: Primary targets (organized/trapped) — compelled reserve accumulation, a collective conversion dilemma, bore the terminal devaluation
 *   - export_led_peg_economies: Secondary beneficiaries with payer exposure (organized/constrained) — undervalued pegs and dollar liquidity powered export-led growth; reserves devalued at the end
 *   - deficit_adjustment_economies: Secondary targets (moderate/constrained) — bore deflationary adjustment under the system's asymmetric burden
 *   - multilateral_reform_advocates: Excluded voice (moderate/constrained) — the reserve-asset proposal and Committee of Twenty track, suspended rather than exhausted
 *   - bretton_woods_regime_itself: Non-agent framework (institutional/trapped) — the Articles-of-Agreement complex this reading names as the collapse's casualty; kept for narrative completeness, feeds no directionality
 *   - monetary_economists: Analytical observers (analytical/analytical) — adjudicate inevitability against choice from the archival record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.58).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.82).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Inevitability Reading — the Gold-Dollar Anchor as Structural Impossibility").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary economics/political economy/international finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__triffin_inevitability_reading).
domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '61d56594-b8d1-47c3-a08c-8826bafea65a').
narrative_ontology:cs_kernel_codification('61d56594-b8d1-47c3-a08c-8826bafea65a', fixed_text).
narrative_ontology:cs_authority_grounding('61d56594-b8d1-47c3-a08c-8826bafea65a', lineage).
narrative_ontology:cs_interpretation_layer_present('61d56594-b8d1-47c3-a08c-8826bafea65a').
narrative_ontology:cs_reading_relation('61d56594-b8d1-47c3-a08c-8826bafea65a', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('61d56594-b8d1-47c3-a08c-8826bafea65a', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('61d56594-b8d1-47c3-a08c-8826bafea65a', foundational, gold_liquidity_contradiction_inescapable).
narrative_ontology:cs_axiom_status(gold_liquidity_contradiction_inescapable, holdable).
narrative_ontology:cs_axiom_grounding('61d56594-b8d1-47c3-a08c-8826bafea65a', gold_liquidity_contradiction_inescapable, empirically_contingent).
narrative_ontology:cs_axiom('61d56594-b8d1-47c3-a08c-8826bafea65a', secondary, no_feasible_policy_path_preserved_convertibility).
narrative_ontology:cs_axiom_status(no_feasible_policy_path_preserved_convertibility, holdable).
narrative_ontology:cs_axiom_grounding('61d56594-b8d1-47c3-a08c-8826bafea65a', no_feasible_policy_path_preserved_convertibility, empirically_contingent).
narrative_ontology:cs_reference_frame('61d56594-b8d1-47c3-a08c-8826bafea65a', bretton_woods_par_value_framework).
narrative_ontology:cs_drift_state('61d56594-b8d1-47c3-a08c-8826bafea65a', post_closure_generalized_floating, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('61d56594-b8d1-47c3-a08c-8826bafea65a', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__triffin_inevitability_reading, us_monetary_authorities).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__triffin_inevitability_reading, export_led_peg_economies).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, foreign_dollar_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, deficit_adjustment_economies).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_regime_itself).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, export_led_peg_economies).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, triffin_dilemma_thesis).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, gold_exchange_standard_incoherence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administered the gold price and the dollar's convertibility pledge through the Treasury, the Federal Reserve, and the London Gold Pool, and decided when and how to defend or abandon them. Financed domestic spending — including the Vietnam War — partly by issuing dollar liabilities that foreign authorities held as reserves, collecting the resulting seigniorage. Held the system's decisive exit: the ability to close the gold window unilaterally, which is what happened in August 1971, settling foreign claims at a devalued dollar afterward and retaining reserve-currency privilege in fiat form.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, us_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__triffin_inevitability_reading, us_monetary_authorities, beneficiary).

% Central banks and official institutions outside the United States that accumulated dollar claims as reserves because the system made the dollar the only elastic reserve asset. Any large holder converting dollars to gold would have triggered the run every holder feared, so each held on while the collective claim outgrew the US gold stock. When the window closed, their accumulated claims were settled at a devalued dollar; they bore the system's terminal loss and had no seat at the August 1971 decision.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, foreign_dollar_holders, payer,
    organized, biographical, trapped, global).

% Export-oriented economies — West Germany, Japan, and smaller peggers — that fixed undervalued currencies to the dollar and grew through the anchored trading system while accumulating dollar reserves. They gained from stable parities and dollar liquidity, but imported US monetary expansion as domestic inflation and saw their dollar reserves devalued at the end; earlier revaluation was blocked by their own export industries.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, export_led_peg_economies, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__triffin_inevitability_reading, export_led_peg_economies, payer).

% Countries running external deficits under the par-value system — the United Kingdom recurrently, among others — that bore the system's adjustment burden: deflation, devaluation, and IMF conditionality, while the issuer escaped adjustment by virtue of issuing the reserve asset everyone else needed. Their exit ran through parity changes under IMF supervision, each one politically traumatic.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, deficit_adjustment_economies, payer,
    moderate, biographical, constrained, regional).

% The rules-and-institutions complex named in the 1944 Articles of Agreement — fixed par values, dollar-gold convertibility at $35 an ounce for official holders, IMF oversight — recorded here because this reading names the framework itself as the collapse's casualty. It was dismantled between August 1971 and early 1973: convertibility suspended, par values abandoned, generalized floating adopted. Kept for narrative completeness; it is a framework, not an actor.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_regime_itself, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_regime_itself).

% Economists and officials — Robert Triffin foremost, along with IMF staff and the Committee of Twenty reform process — who proposed substituting an international reserve asset for the dollar's role, adjusting the gold price, or renegotiating parities through a multilateral track. Their proposals were on the table throughout the 1960s and were suspended, not exhausted, when the unilateral closure bypassed them in August 1971.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, multilateral_reform_advocates, excluded,
    moderate, generational, constrained, global).

% The scholarly and historiographical seats — economic historians and monetary economists assessing the collapse decades later — who work from the archival record and adjudicate between inevitability, choice, and overdetermination accounts of the transition. They collect nothing from and bear nothing of the arrangement.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__triffin_inevitability_reading, us_monetary_authorities).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__triffin_inevitability_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The anchor solved the postwar liquidity problem: world trade needed an elastic reserve asset, but gold supply grew too slowly to provide one. A single convertible national currency bridged the gap — foreign authorities held dollars as reserves, fixed par values reduced trade and investment risk, and IMF credit cushioned temporary imbalances while reconstruction proceeded.
% TRANSFER_FUNCTION: Moved seigniorage and adjustment burden asymmetrically: the issuer financed domestic programs and war spending with dollar liabilities that foreign authorities were compelled to hold as reserves, and when convertibility ended, the accumulated claims were settled at a devalued dollar — a terminal transfer from foreign holders to the United States. Deflationary adjustment meanwhile fell on deficit countries while the issuer escaped it.
% ABSENT_VOICES: Foreign dollar holders had no seat at the August 1971 decision — the closure was announced at Camp David, not negotiated. The multilateral reform track (Triffin's reserve-asset proposal, the Committee of Twenty) was suspended rather than concluded; its advocates would have insisted on a negotiated transition with compensated adjustment instead of unilateral termination.
% DISAPPEARANCE_RATIONALE: It did disappear, and the world rearranged: par values gave way to generalized floating by 1973, inflation accelerated through the 1970s as the nominal anchor vanished, the eurodollar market expanded without a convertibility constraint, and the reserve system reorganized around pure fiat dollar holdings with the SDR as a token supplement — leaving the same liquidity-confidence tension operating in mutated form.
% FOUNDING_PROBLEM: The interwar monetary chaos — competitive devaluations, deflationary gold-standard discipline without adequate liquidity, beggar-thy-neighbor trade policy — and the specific design problem of reconciling gold-based confidence with an elastic supply of world reserves as trade grew faster than gold mining.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's reality is corroborated from outside the benefiting parties: IMF archives and the Committee of Twenty deliberations document the liquidity shortfall as a live official concern through the 1960s; Triffin's 1960 congressional testimony predates and predicts the mechanism; and the economic-historical literature (Bordo, Eichengreen and collaborators) attests both that the liquidity problem was real and that whether it made collapse inevitable remains disputed among historians. No source outside the dispute attests the problem class is dead — the modern safe-asset literature treats it as live in fiat form.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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
 *   Extractiveness (0.58, reading-indexed) traces the privilege the dilemma itself names: minimal while the United States ran surpluses and Europe lacked dollars (1944-1957), rising as deficit finance of the Vietnam War and domestic programs accumulated dollar liabilities against a fixed gold stock, peaking when the closure settled foreign claims at a devalued rate. Suppression (0.82) is a raw structural property, unscaled by power or scope: the anchor's persistence depended on the London Gold Pool's market operations, the Interest Equalization Tax, voluntary then mandatory capital controls, and the 1971 package of closure, import surcharge, and wage-price freeze — coercion aimed at holders and allies rather than resting on participant preference. Theater (0.62) rises with the 1968 two-tier gold market, where the official $35 price survived only as an intra-central-bank fiction while the free market priced gold far higher. Accessibility collapse is low (0.30): the reform menu stayed live throughout — gold-price adjustment debated from 1960, SDRs operational by 1970, wider bands and floating openly advocated — so the option space never collapsed, and the unilateral act was chosen from a crowded menu, which is exactly what the inevitability claim must explain away. Resistance is substantial (0.60): the 1965 French convertibility demand, France's 1967 withdrawal from the Gold Pool, recurrent gold runs (1960, 1961, 1967, 1968, 1971), and sustained market speculation. All three series share one time grid (1944-1971); every point is observed. The claimed type (mountain) is the reading's assertion under test; the metrics describe actual operation; the divergence is the datum the corpus exists to take, not an error to reconcile. Receipt surface: the gains demonstrably accrued to the issuer seat (seigniorage during operation, devalued settlement at termination), so gain_flow names that seat; removal of the constraint was cheap for the seat that could remove it — the closure was unilateral and its costs were externalized onto holders and allies — hence fixing_cost: cheap, authored independently of gain_flow.
 *
 * PERSPECTIVAL GAP:
 *   From the issuer seat the anchor was a public utility run at a loss — the United States supplied the world's reserves, defended the gold price with its own metal, and absorbed the drain; from this seat the deficits were the world's privilege. From the holder seats the same structure was compelled lending: reserves accumulated because any unilateral conversion would have triggered the run every holder feared, and the terminal settlement devalued exactly the claims the system had obliged them to hold. The holders' coalition was real but individually defection-dominant — France's aggressive conversions from 1965 are the defection that broke it. The inevitability reading is the issuer-seat narrative in naturalized form: no one chose this, so no one owes anything. The engine computes the per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   us_monetary_authorities derives near-beneficiary directionality: declared beneficiary, institutional power, and arbitrage-grade exit — it could and did redefine the regime unilaterally, retaining reserve-currency privilege in fiat form after the anchor died. foreign_dollar_holders derive near-target directionality: declared victims, organized but trapped, since the conversion dilemma made each holder's exit a threat to all. export_led_peg_economies sit mid-scale: declared beneficiaries with genuine payer exposure (imported inflation, devalued reserves) and constrained exit through their own export interests. deficit_adjustment_economies derive near-target: declared victims whose exit ran through politically traumatic parity changes under IMF supervision. The non-agent framework entry feeds no directionality by design, and the excluded and observer seats carry no beneficiary or victim declarations. No directionality overrides were needed: the declarations plus the exit atoms produce the correct values directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling gold-based confidence with elastic world liquidity — is contested rather than dead: the gold-convertibility version died in 1971, but the liquidity-confidence tension persists in mutated fiat form, and founding_problem_status is authored accordingly. The mountain claim, accepted uncritically, would exempt the arrangement from extraction analysis altogether — a natural process has no victims and no beneficiaries — and the reading's beneficiary-free framing performs exactly that exemption. Declaring the beneficiaries keeps the false-summit evaluation live, while the declared coordination function (elastic liquidity provision, parity stability, IMF cushioning) keeps the genuine coordination visible, so the computed classification can land on the hybrid type that honors both the coordination and the asymmetric extraction instead of collapsing to pure coordination or pure extraction. The classification thereby blocks two opposite mislabels: reading the anchor as pure coordination (the issuer's self-description) and reading it as pure extraction (which would erase the real liquidity problem the system solved for two decades).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_constraint,
    'Is the gold-dollar anchor''s collapse a genuine structural impossibility that no institutional design could have avoided, or a constructed constraint whose abandonment was a policy sequence that concentrated gains on identifiable agents?',
    'Counterfactual institutional analysis of the reform menu actually on the table — gold-price adjustment, SDR substitution accounts, wider bands, negotiated revaluation — assessed against the archival record of why each was rejected (Treasury and Federal Reserve papers, Committee of Twenty deliberations, IMF archives).',
    'If a feasible reform path existed and was bypassed for distributional reasons, the mountain claim is a false summit and the computed classification moves toward the hybrid coordination/extraction type; if no path was feasible, the reading''s structural-necessity claim stands and the reading-indexed extractiveness collapses toward zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_constraint, conceptual, 'Whether inevitability reflects structural impossibility or a naturalized policy choice.').

omega_variable(
    kernel_contest_inevitability_vs_choice,
    'Which sibling reading of the monetary_anchor_principle kernel does the causal record best support: Triffin-only inevitability (this reading), the punctuated discretionary swap, or the overdetermined composite?',
    'Archival decision analysis of the August 1971 closure (recorded deliberations, Treasury memoranda) plus counterfactual modeling of the 1970 reform track that was suspended rather than exhausted.',
    'Determines corpus weighting across the kernel family: a punctuated-swap verdict demotes this reading''s structural-necessity claim to cover for a discretionary act, while a composite verdict preserves structural pressure but redistributes causal weight away from the Triffin mechanism alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_inevitability_vs_choice, empirical, 'Sibling-reading contest over the collapse''s causal structure.').

omega_variable(
    triffin_invariance_across_regimes,
    'Does the liquidity-confidence contradiction inhere in any reserve-currency arrangement (a genuine regularity of monetary structure) or only in gold-convertible configurations (a property of one constructed design)?',
    'Comparative analysis of reserve regimes: the modern fiat-dollar system''s mutated version of the same dilemma (safe-asset supply versus issuer balance-sheet capacity), the euro''s incomplete reserve status, and sterling-era dynamics before 1931.',
    'If the tension is regime-general, this reading''s structural claim strengthens; if it is configuration-specific, the claim reduces to a critique of one design choice and the inevitability framing weakens substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(triffin_invariance_across_regimes, empirical, 'Regime-generality of the Triffin contradiction.').

omega_variable(
    seigniorage_net_privilege_magnitude,
    'How large was the issuer''s net privilege (seigniorage plus adjustment asymmetry) relative to the coordination value the anchor delivered over its operating life?',
    'Exorbitant-privilege quantification (reserve-currency return differentials, valuation effects) applied to the 1944-1971 window, using IMF balance-of-payments archives and the reserve-adequacy literature.',
    'A large net privilege supports extraction-dominated classification; a small net privilege supports the coordination-dominated reading and softens the divergence between this story''s mountain claim and its computed type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_net_privilege_magnitude, empirical, 'Magnitude of issuer privilege versus coordination benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(triffin_inevitability_tr_t1944, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement_basis(triffin_inevitability_tr_t1944, observed).
narrative_ontology:measurement(triffin_inevitability_tr_t1950, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement_basis(triffin_inevitability_tr_t1950, observed).
narrative_ontology:measurement(triffin_inevitability_tr_t1957, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1957, 0.16).
narrative_ontology:measurement_basis(triffin_inevitability_tr_t1957, observed).
narrative_ontology:measurement(triffin_inevitability_tr_t1960, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement_basis(triffin_inevitability_tr_t1960, observed).
narrative_ontology:measurement(triffin_inevitability_tr_t1965, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1965, 0.33).
narrative_ontology:measurement_basis(triffin_inevitability_tr_t1965, observed).
narrative_ontology:measurement(triffin_inevitability_tr_t1968, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1968, 0.55).
narrative_ontology:measurement_basis(triffin_inevitability_tr_t1968, observed).
narrative_ontology:measurement(triffin_inevitability_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.62).
narrative_ontology:measurement_basis(triffin_inevitability_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(triffin_inevitability_be_t1944, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1944, 0.26).
narrative_ontology:measurement_basis(triffin_inevitability_be_t1944, observed).
narrative_ontology:measurement(triffin_inevitability_be_t1950, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement_basis(triffin_inevitability_be_t1950, observed).
narrative_ontology:measurement(triffin_inevitability_be_t1957, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1957, 0.33).
narrative_ontology:measurement_basis(triffin_inevitability_be_t1957, observed).
narrative_ontology:measurement(triffin_inevitability_be_t1960, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1960, 0.37).
narrative_ontology:measurement_basis(triffin_inevitability_be_t1960, observed).
narrative_ontology:measurement(triffin_inevitability_be_t1965, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement_basis(triffin_inevitability_be_t1965, observed).
narrative_ontology:measurement(triffin_inevitability_be_t1968, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1968, 0.52).
narrative_ontology:measurement_basis(triffin_inevitability_be_t1968, observed).
narrative_ontology:measurement(triffin_inevitability_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.58).
narrative_ontology:measurement_basis(triffin_inevitability_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(triffin_inevitability_su_t1944, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1944, 0.15).
narrative_ontology:measurement_basis(triffin_inevitability_su_t1944, observed).
narrative_ontology:measurement(triffin_inevitability_su_t1950, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement_basis(triffin_inevitability_su_t1950, observed).
narrative_ontology:measurement(triffin_inevitability_su_t1957, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1957, 0.28).
narrative_ontology:measurement_basis(triffin_inevitability_su_t1957, observed).
narrative_ontology:measurement(triffin_inevitability_su_t1960, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement_basis(triffin_inevitability_su_t1960, observed).
narrative_ontology:measurement(triffin_inevitability_su_t1965, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1965, 0.52).
narrative_ontology:measurement_basis(triffin_inevitability_su_t1965, observed).
narrative_ontology:measurement(triffin_inevitability_su_t1968, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement_basis(triffin_inevitability_su_t1968, observed).
narrative_ontology:measurement(triffin_inevitability_su_t1971, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1971, 0.82).
narrative_ontology:measurement_basis(triffin_inevitability_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the end of Bretton Woods' covers three structurally distinct claims, decomposed per the epsilon-invariance principle into a constraint family. This story (triffin_inevitability_reading) authors the single-mechanism inevitability claim as a mountain-type assertion with moderate reading-indexed epsilon; monetary_anchor_principle__punctuated_swap_reading authors the discrete-choice claim; monetary_anchor_principle__overdetermined_composite_reading authors the multi-cause composite. Each carries its own epsilon, beneficiaries, and type. The family links let contamination analysis track how evidence against one reading propagates: this reading is the purest single-mechanism account, the composite subsumes its mechanism among others, and the punctuated-swap reading contradicts its agency premise outright.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
