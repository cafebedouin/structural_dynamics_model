% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Triffin-Dilemma Bind on the Bretton Woods Par-Value Regime (Overdetermined-Collapse Reading)
 *   domain: economic/political_economy/international_finance
 *
 * SUMMARY:
 *   Between 1958 and 1973 the Bretton Woods par-value regime operated under a
 *   contradiction its own designers had been warned of: with the dollar the
 *   sole reserve asset convertible into gold at $35 per ounce, world
 *   liquidity could grow only through US external deficits, and those same
 *   deficits eroded the confidence that made the dollar worth holding
 *   (Triffin, 1960). Once the postwar dollar gap flipped to a dollar glut,
 *   multiple reinforcing pathways converged on the same terminus — the
 *   liquidity imperative, Vietnam-era deficit finance, Gold Pool exhaustion,
 *   creditor resistance, and accelerating speculative runs — and the regime
 *   ended in the August 1971 convertibility suspension and the March 1973
 *   generalized float. This file instantiates ONE reading of the kernel
 *   transition_causality: the overdetermined_collapse_reading, which holds
 *   the transition structurally inevitable and treats the Triffin bind as a
 *   mountain-grade limit of single-reserve-currency design. The sibling
 *   readings (contingent_choice_reading, hybrid_trigger_reading) are separate
 *   constraints in separate files. Claim and metrics are authored
 *   independently: the claimed type asserts mountain status for the
 *   structural bind; the metrics describe how the arrangement-under-bind
 *   actually operated — increasingly extractive, coerced, and theatrical as
 *   the contradiction deepened. The divergence between claim and computed
 *   classification is the datum this story exists to take. Epsilon's referent
 *   is the standing arrangement under contest — the par-value regime as it
 *   operated — assessed by this reading's lights, never by any endorsed
 *   alternative (this reading endorses none).
 *
 * KEY AGENTS:
 *   - us_monetary_authorities: Agenda-setter and primary beneficiary (institutional/arbitrage) — administered the gold anchor, collected seigniorage, exited by unilateral rule change in 1971
 *   - foreign_dollar_reserve_holders: Primary target (institutional/trapped) — held eroding convertibility guarantees, locked by the self-defeating nature of collective redemption
 *   - surplus_creditor_nations: Target (institutional/constrained) — financed US deficits involuntarily, imported inflation, held revaluation as their only lever
 *   - deficit_country_economies: Target (institutional/constrained) — bore the regime's deflationary adjustment discipline and conditionality
 *   - gold_producing_states: Secondary beneficiary (organized/constrained) — sold into the officially defended $35 floor
 *   - imf_par_value_administration: Co-administrator (institutional/identity_locked) — policed par values, institutionally fused with the arrangement it serviced
 *   - private_currency_speculators: Excluded actor (organized/mobile) — no governance seat, decisive liquidation power
 *   - monetary_history_analysts: Analytical observer (analytical/analytical) — diagnosed the contradiction from outside the operating institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.72).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.7).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Triffin-Dilemma Bind on the Bretton Woods Par-Value Regime (Overdetermined-Collapse Reading)").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "economic/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__overdetermined_collapse_reading).
domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, 'b9860aad-e5bd-435d-8144-d5c63b3bd17d').
narrative_ontology:cs_kernel_codification('b9860aad-e5bd-435d-8144-d5c63b3bd17d', formalized).
narrative_ontology:cs_authority_grounding('b9860aad-e5bd-435d-8144-d5c63b3bd17d', lineage).
narrative_ontology:cs_interpretation_layer_present('b9860aad-e5bd-435d-8144-d5c63b3bd17d').
narrative_ontology:cs_reading_relation('b9860aad-e5bd-435d-8144-d5c63b3bd17d', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('b9860aad-e5bd-435d-8144-d5c63b3bd17d', transition_causality__hybrid_trigger_reading, forecloses).
narrative_ontology:cs_axiom('b9860aad-e5bd-435d-8144-d5c63b3bd17d', foundational, structural_contradictions_jointly_sufficient_for_collapse).
narrative_ontology:cs_axiom_status(structural_contradictions_jointly_sufficient_for_collapse, holdable).
narrative_ontology:cs_axiom_grounding('b9860aad-e5bd-435d-8144-d5c63b3bd17d', structural_contradictions_jointly_sufficient_for_collapse, empirically_contingent).
narrative_ontology:cs_axiom('b9860aad-e5bd-435d-8144-d5c63b3bd17d', secondary, design_internal_reform_cannot_escape_triffin_bind).
narrative_ontology:cs_axiom_status(design_internal_reform_cannot_escape_triffin_bind, holdable).
narrative_ontology:cs_axiom_grounding('b9860aad-e5bd-435d-8144-d5c63b3bd17d', design_internal_reform_cannot_escape_triffin_bind, empirically_contingent).
narrative_ontology:cs_reference_frame('b9860aad-e5bd-435d-8144-d5c63b3bd17d', articles_of_agreement_fixed_parity_order).
narrative_ontology:cs_drift_state('b9860aad-e5bd-435d-8144-d5c63b3bd17d', generalized_float_1973, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('b9860aad-e5bd-435d-8144-d5c63b3bd17d', '2026-08-10T12:00:00Z').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, us_monetary_authorities).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, gold_producing_states).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, surplus_creditor_nations).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, deficit_country_economies).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, triffin_dilemma_thesis).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, gold_exchange_standard_internal_contradiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and defended the dollar's $35-per-ounce gold parity, ran the Treasury's gold window, financed widening external deficits by issuing dollar liabilities that foreign central banks were expected to hold, and deployed capital controls, swap networks, and diplomatic pressure to deter conversion. Collected seigniorage throughout: deficits could be financed in its own currency. Its exit was unilateral rule change — exercised in August 1971 by suspending convertibility.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, us_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, us_monetary_authorities, beneficiary).

% Central banks worldwide held the bulk of their reserves in dollars guaranteed convertible at $35 per ounce. Any large holder converting first would depress the value of every other holder's reserves, so collective redemption was self-defeating; individually each stayed and absorbed erosion. Alliance dependence on the United States added political cost to exit on top of the financial trap.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, foreign_dollar_reserve_holders, payer,
    institutional, biographical, trapped, global).

% West Germany, Japan, and later Switzerland ran persistent surpluses under parities set below market-clearing levels. Accumulating dollars was the price of export-led growth; refusing meant appreciating and injuring exporters. Their dollar holdings lost real value as convertibility eroded, they imported US inflation, and they had no formal mechanism to force adjustment onto the issuer.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, surplus_creditor_nations, payer,
    institutional, generational, constrained, continental).

% The United Kingdom and other chronic deficit countries bore the system's deflationary discipline: stop-go domestic cycles, IMF conditionality, and devaluation under duress (sterling, November 1967). The Articles placed adjustment obligations on deficit countries while the reserve issuer faced none of them.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, deficit_country_economies, payer,
    institutional, biographical, constrained, regional).

% South Africa and the Soviet Union sold newly mined gold into a market whose official $35 floor the Gold Pool defended. The anchor sustained demand and a price floor for their principal export, giving them a material stake in the regime's continuation and in the suppression of free-market price discovery.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, gold_producing_states, beneficiary,
    organized, generational, constrained, global).

% Administered par values, pooled adjustment financing, and policed compliance with the Articles of Agreement. Its mandate, staffing, and professional self-conception were constituted by the par-value system it serviced; the institution's identity fused with the arrangement, making advocacy of fundamental redesign internally unthinkable even as its own reporting documented dollar-glut strains.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, imf_par_value_administration, agenda_setter,
    institutional, generational, identity_locked, global).

% Held no seat in the regime's governance yet repeatedly moved the constraint: the London gold rushes of 1960 and 1968, forward-market runs on sterling and the dollar. Their trades transmitted the underlying contradiction faster than officials acknowledged. They had no voice in the design, only in the liquidation.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, private_currency_speculators, excluded,
    organized, immediate, mobile, global).

% Triffin, Rueff, and later economic historians analyzed the reserve-supply-versus-confidence contradiction from outside the operating institutions, publishing diagnoses — including Triffin's 1959-60 congressional testimony — that the operating authorities heard, recorded, and declined to act on at the level of design.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, monetary_history_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__overdetermined_collapse_reading, us_monetary_authorities).
narrative_ontology:fixing_cost_class(transition_causality__overdetermined_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplied a common unit of account, a settlement asset, and stable nominal parities that eliminated the interwar pattern of competitive devaluation; pooled adjustment financing through the IMF; provided the liquidity infrastructure for the postwar trade expansion.
% TRANSFER_FUNCTION: Moved seigniorage and real purchasing power from foreign economies to the reserve issuer (dollars issued against goods and held as reserves at eroding convertibility); moved gold from European central banks to Fort Knox during the confidence phases; moved adjustment burden onto deficit economies through deflationary discipline and onto reserve holders through the terminal devaluation losses.
% ABSENT_VOICES: Surplus creditors had no formal mechanism to compel adjustment onto the reserve issuer — the Articles stated symmetric obligations but provided no enforcement path against the issuer itself; private dollar holders and speculators had no seat despite decisively moving the constraint; economies outside the Atlantic core held IMF quotas too small to matter. Unanimity in defense of the regime arose partly because the seats that would have forced the question earliest were never in the room.
% DISAPPEARANCE_RATIONALE: Had the par-value regime and its bind dissolved overnight in the mid-1960s, exchange rates would have repriced immediately, reserve composition would have diversified, the 1971-73 losses imposed on holders would never have accrued in that form, and trade finance would have reorganized around whatever settlement asset replaced the dollar-gold anchor. The world demonstrably rearranged when dissolution actually arrived: generalized floating, the inflationary decade, and petrodollar recycling followed within months.
% FOUNDING_PROBLEM: The interwar monetary order: competitive devaluations, beggar-thy-neighbor trade policy, and gold-standard rigidity that deepened the Depression. Bretton Woods was designed to combine exchange-rate stability with an adjustment mechanism flexible enough to avoid deflationary deadlock, and to rebuild multilateral trade on convertible currencies.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Triffin's 1959-60 congressional testimony and Rueff's contemporaneous critiques diagnosed the design's internal contradiction before the collapse; IMF Annual Reports and Group of Ten ministerial documents of 1965-68 recorded the dollar-glut condition the founding design produced; subsequent economic history (Bordo, Eichengreen, James) attests that the original problem — interwar chaos — was substantially solved by the 1958 restoration of convertibility, while the arrangement persisted under a new and fatal one. The beneficiary set itself attests only a continuing need for liquidity, not the founding problem's liveness.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.72 reflects end-state operation: seigniorage transferred from foreign holders, adjustment obligations asymmetrically placed on deficit economies, and the 1971-73 devaluation losses imposed on reserve holders who had been promised $35 convertibility. Suppression 0.70 is the raw structural coercion the regime required — the Interest Equalization Tax, Federal Reserve swap networks, Gold Pool interventions, voluntary restraint programs, and finally the wage-price freeze and import surcharge — authored unscaled as a structural property; only extraction is scaled downstream by directionality and scope. Theater_ratio 0.38 tracks the decay of real function into performance: genuine trade-expansion and reconstruction finance early, two-tier gold fiction and cosmetic cover-ratio accounting late. Accessibility_collapse 0.80 encodes this reading's core assertion: within the single-reserve-currency design, every internal reform (SDRs, wider bands, managed devaluation) re-encountered the same bind, so alternatives collapsed to the single exit of abandoning the design itself. Resistance 0.62 records real, powerful opposition — de Gaulle's public convertibility campaign, German revaluations against US wishes, serial speculative attacks — fragmented by the collective-action trap that made each holder's exit self-punishing. The measurement series share one seven-point grid, every tracked metric authored at every point. The trajectory is a ratchet rather than smooth drift: each crisis (1960, 1965, 1967-68, 1971) drew a patch (swap lines, pool, two-tier market, freeze), a partial relaxation, then deeper accumulation — an intermittent-reinforcement dynamic in which the patching itself sustained participation while the underlying position worsened.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the bind appeared as a manageable sequence of liquidity-management problems, each patchable — hence arbitrage exit and generational patience. From the trapped reserve-holder seat the identical structure operated as slow confiscation: every year of participation eroded holdings that exit would crystallize, so staying was rational and ruinous at once. Creditor seats experienced a third version: involuntary financing of another state's deficits at eroding terms, with revaluation as the only lever and domestic export interests punishing its use. A creditor coalition capable of forcing adjustment existed structurally — France and Germany together held enough gold claims to matter — but was neutralized by divergent member interests (export competitiveness for Germany, strategic autonomy for France), so coalition power remained latent rather than absent. The observer seat, positioned outside, saw the design fatality the operators treated as weather.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. us_monetary_authorities sits nearest the beneficiary pole: it collected the seigniorage, wrote the rules, and held unilateral rule-change exit (arbitrage), realized in August 1971. foreign_dollar_reserve_holders sit nearest the target pole: declared victims, trapped by the self-defeating nature of collective redemption and by alliance dependence on the issuer. surplus_creditor_nations and deficit_country_economies are declared victims with constrained rather than trapped exit — revaluation and devaluation existed but at severe domestic political cost — placing them high-d but slightly damped relative to the trapped holders. gold_producing_states derive low d as beneficiaries of the defended floor. imf_par_value_administration derives mid-low d: it administered rather than collected, but its identity-lock removed internal pressure for redesign. private_currency_speculators are excluded rather than coordinated — exclusion from governance while retaining liquidation power registers as mid-range d with mobile exit. Global spatial scope amplifies effective extraction modestly through verification difficulty per the engine's scope handling; suppression receives no such scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings. Reading the regime as pure snare erases the genuine quarter-century coordination achievement — stable parities underwriting the trade expansion and reconstruction the founding problem demanded. Reading it as pure rope erases the asymmetric extraction the bind structured onto holders and deficit economies. The mountain claim locates the irreducible element in the design logic rather than in any actor's will, which is why the R5 genealogy matters: the founding problem (interwar chaos) was substantially dead by 1958 — corroborated from outside the beneficiary set — and the arrangement persisted fifteen more years under a condition (dollar glut) its design could not digest. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no zombie flag fires, because unlike a piton the arrangement did not outlive its constraint theatrically — the constraint killed the arrangement. Mandatrophy resolved in the opposite direction from institutional inertia: obsolescence by detonation, not by drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fsm_naturalness_vs_constructed_fragility,
    'Is the Triffin contradiction a genuine structural law of any single-national-currency reserve standard (a mountain), or a constructed fragility of the 1944 design choices that identifiable agents benefited from leaving unfixed?',
    'Comparative institutional analysis of design variants — Keynes''s clearing union, multiple national reserve currencies, commodity-basket anchors — tested for whether each escapes the liquidity-versus-confidence contradiction; plus archival study of why known remedies (early gold devaluation, SDR substitution accounts) were declined by the actors with authority to apply them.',
    'If constructed and escapable, the mountain claim fails false-summit certification and the constraint reclassifies toward tangled_rope (genuine coordination plus asymmetric extraction under active enforcement); the overdetermined reading loses its foundation and the sibling readings gain ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_naturalness_vs_constructed_fragility, conceptual, 'Natural-law versus constructed status of the reserve-currency contradiction.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the overdetermined_collapse_reading of kernel transition_causality; would the contingent_choice_reading or hybrid_trigger_reading restructure the same history into a different constraint with different epsilon, beneficiary structure, and type?',
    'Authoring the sibling files and comparing computed classifications across the constraint family; the disagreement is located in the modal status of the collapse (necessary, avoidable, or trigger-actualized), not in any measured quantity.',
    'A sibling instantiation shifts claimed_type (rope or scaffold for the contingent policy framing; tangled_rope for the hybrid) and redistributes directionality toward policy choosers; cross-family comparison is the corpus''s measurement of the dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel.').

omega_variable(
    counterfactual_viability_near_zero,
    'Is counterfactual viability of the fixed-rate regime past 1973 genuinely near-zero, as this reading asserts, or merely low?',
    'Historical simulation and the archival counterfactual literature scoring the feasible-policy sets actually available to the US authorities 1965-1971 (earlier gold devaluation, tighter capital controls, earlier Gold Pool abandonment) for probability of convertibility preservation.',
    'Demonstrated non-zero viability downgrades the mountain claim toward hybrid-trigger territory and widens uncertainty on accessibility_collapse; confirmed zero viability hardens mountain certification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_near_zero, empirical, 'Magnitude of the counterfactual viability claim underlying the inevitability assertion.').

omega_variable(
    issuer_victim_status_ambiguity,
    'Does the victim structure include the reserve issuer itself — bound by the same dilemma it administered — or only the foreign actors constrained by the regime?',
    'Distributional accounting of net costs 1958-1973 across seats: seigniorage gains weighed against gold drain, loss of monetary autonomy, and the inherited inflation of the 1970s for the issuer; devaluation losses and imported inflation for foreign holders.',
    'If the issuer is also a victim, the structure reads as a shared trap supporting mountain certification; if victims are exclusively foreign, the asymmetry supports tangled_rope reclassification with the issuer as captor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(issuer_victim_status_ambiguity, empirical, 'Whether the constraint''s victim set includes its own administrator.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1958, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1958, transition_causality__overdetermined_collapse_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement_basis(tran_tr_t1958, observed).
narrative_ontology:measurement(tran_tr_t1960, transition_causality__overdetermined_collapse_reading, theater_ratio, 1960, 0.14).
narrative_ontology:measurement_basis(tran_tr_t1960, observed).
narrative_ontology:measurement(tran_tr_t1963, transition_causality__overdetermined_collapse_reading, theater_ratio, 1963, 0.18).
narrative_ontology:measurement_basis(tran_tr_t1963, observed).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__overdetermined_collapse_reading, theater_ratio, 1965, 0.24).
narrative_ontology:measurement_basis(tran_tr_t1965, observed).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__overdetermined_collapse_reading, theater_ratio, 1968, 0.33).
narrative_ontology:measurement_basis(tran_tr_t1968, observed).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__overdetermined_collapse_reading, theater_ratio, 1971, 0.36).
narrative_ontology:measurement_basis(tran_tr_t1971, observed).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__overdetermined_collapse_reading, theater_ratio, 1973, 0.38).
narrative_ontology:measurement_basis(tran_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t1958, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1958, 0.42).
narrative_ontology:measurement_basis(tran_be_t1958, observed).
narrative_ontology:measurement(tran_be_t1960, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1960, 0.46).
narrative_ontology:measurement_basis(tran_be_t1960, observed).
narrative_ontology:measurement(tran_be_t1963, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1963, 0.51).
narrative_ontology:measurement_basis(tran_be_t1963, observed).
narrative_ontology:measurement(tran_be_t1965, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1965, 0.57).
narrative_ontology:measurement_basis(tran_be_t1965, observed).
narrative_ontology:measurement(tran_be_t1968, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1968, 0.64).
narrative_ontology:measurement_basis(tran_be_t1968, observed).
narrative_ontology:measurement(tran_be_t1971, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1971, 0.7).
narrative_ontology:measurement_basis(tran_be_t1971, observed).
narrative_ontology:measurement(tran_be_t1973, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1973, 0.72).
narrative_ontology:measurement_basis(tran_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1958, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1958, 0.28).
narrative_ontology:measurement_basis(tran_su_t1958, observed).
narrative_ontology:measurement(tran_su_t1960, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1960, 0.34).
narrative_ontology:measurement_basis(tran_su_t1960, observed).
narrative_ontology:measurement(tran_su_t1963, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1963, 0.44).
narrative_ontology:measurement_basis(tran_su_t1963, observed).
narrative_ontology:measurement(tran_su_t1965, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1965, 0.52).
narrative_ontology:measurement_basis(tran_su_t1965, observed).
narrative_ontology:measurement(tran_su_t1968, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1968, 0.61).
narrative_ontology:measurement_basis(tran_su_t1968, observed).
narrative_ontology:measurement(tran_su_t1971, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1971, 0.69).
narrative_ontology:measurement_basis(tran_su_t1971, observed).
narrative_ontology:measurement(tran_su_t1973, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1973, 0.7).
narrative_ontology:measurement_basis(tran_su_t1973, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, resource_allocation).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the end of Bretton Woods' decomposes into three structurally distinct causal claims forming the transition_causality family. This file instantiates the overdetermined_collapse_reading (structural inevitability; Triffin bind as mountain; counterfactual viability near-zero). The sibling files instantiate contingent_choice_reading (avoidable policy decision) and hybrid_trigger_reading (contradictions plus contingent triggers). Each reading is a separate constraint with its own epsilon over the same referent arrangement — the par-value regime as it operated 1958-1973 — because epsilon is reading-indexed: the shared referent legitimately carries different authored values per reading. Downstream structure: the hybrid reading cites this reading's structural-contradiction component as its accumulating substrate, while the contingent reading positions itself as its refutation; the family link set records both dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
