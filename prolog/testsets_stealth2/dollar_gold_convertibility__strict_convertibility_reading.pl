% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [CLOSED_BREACH_1971]
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Article IV Gold Convertibility Obligation — Strict Reading
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   Under the Bretton Woods Articles of Agreement (effective December 1945),
 *   the United States pledged to redeem dollar holdings presented by foreign
 *   official institutions in gold at $35 per ounce — the linchpin of the
 *   par-value system, since every other member pegged to a dollar made
 *   credible by that pledge. This story instantiates the STRICT READING of
 *   that commitment: Article IV convertibility as a binding legal obligation
 *   that subordinates U.S. monetary policy to the external parity. On this
 *   reading the U.S. monetary authorities are the constrained issuer — the
 *   seat that bears the arrangement's costs — while creditor nations holding
 *   enforceable conversion rights (led by France after 1965) and the broader
 *   set of official dollar holders are the seats the arrangement serves. The
 *   interval runs from the Articles' entry into force (t0 = 1945) to the
 *   Nixon administration's unilateral suspension of convertibility (t26 =
 *   August 1971), with intermediate points mapped as t4≈1949 (peak U.S. gold
 *   stock), t12≈1957 (eve of restored European convertibility), t16≈1961
 *   (Gold Pool era), t20≈1965 (French conversion challenge), t23≈1968 (Pool
 *   collapse, two-tier market). The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as tangled_rope on structural grounds (genuine
 *   coordination function plus asymmetric burden plus active enforcement),
 *   while the authored metrics describe its actual operation independently —
 *   heavily extractive at end-state, increasingly suppressive, moderately
 *   theatrical. The engine computes per-seat classifications from the
 *   structural data; the authored claim does not adjudicate them.
 *
 * KEY AGENTS:
 *   - - us_monetary_authorities: Primary target (institutional/constrained) — the constrained issuer; bears the arrangement's policy-space costs
 *   - - creditor_nation_gold_claimants: Primary beneficiary (powerful/arbitrage) — holds enforceable conversion rights, exercises them strategically
 *   - - foreign_official_dollar_holders: Secondary beneficiary (institutional/mobile) — holds dollar reserves made acceptable by the redemption backstop
 *   - - imf_par_value_administration: Agenda-setter (institutional/constrained) — administers and interprets the par-value framework the obligation sits inside
 *   - - private_gold_markets: Excluded actor (organized/mobile) — barred from the official redemption window by design
 *   - - monetary_history_analysts: Analytical observer — reconstructs legal force and adjudicates between readings from the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.8).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.82).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Gold Convertibility Obligation — Strict Reading").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '51933250-2e85-4a1d-8148-f7de031a6aec').
narrative_ontology:cs_kernel_codification('51933250-2e85-4a1d-8148-f7de031a6aec', formalized).
narrative_ontology:cs_authority_grounding('51933250-2e85-4a1d-8148-f7de031a6aec', lineage).
narrative_ontology:cs_interpretation_layer_present('51933250-2e85-4a1d-8148-f7de031a6aec').
narrative_ontology:cs_reading_relation('51933250-2e85-4a1d-8148-f7de031a6aec', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_reading_relation('51933250-2e85-4a1d-8148-f7de031a6aec', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('51933250-2e85-4a1d-8148-f7de031a6aec', foundational, external_parity_precedence_over_domestic_discretion).
narrative_ontology:cs_axiom_status(external_parity_precedence_over_domestic_discretion, holdable).
narrative_ontology:cs_axiom_grounding('51933250-2e85-4a1d-8148-f7de031a6aec', external_parity_precedence_over_domestic_discretion, conventional).
narrative_ontology:cs_axiom('51933250-2e85-4a1d-8148-f7de031a6aec', secondary, convertibility_is_unconditional_treaty_duty).
narrative_ontology:cs_axiom_status(convertibility_is_unconditional_treaty_duty, holdable).
narrative_ontology:cs_axiom_grounding('51933250-2e85-4a1d-8148-f7de031a6aec', convertibility_is_unconditional_treaty_duty, conventional).
narrative_ontology:cs_reference_frame('51933250-2e85-4a1d-8148-f7de031a6aec', binding_par_value_obligation).
narrative_ontology:cs_drift_state('51933250-2e85-4a1d-8148-f7de031a6aec', nixon_gold_window_closure, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('51933250-2e85-4a1d-8148-f7de031a6aec', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, foreign_official_dollar_holders).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nation_gold_claimants).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_authorities).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, hume_price_specie_flow_mechanism).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, article_iv_treaty_bindingness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the world's primary reserve currency under a treaty pledge to redeem official dollar holdings in gold at $35 per ounce. Every extension of domestic credit or fiscal deficit beyond gold-stock growth hands foreign officials redeemable claims against the U.S. gold stock; defending the parity repeatedly forces contractionary policy, capital controls, and allied-pressure campaigns that the domestic agenda did not choose. Leaving the arrangement means unilateral suspension — reputational rupture and the probable unwind of the fixed-rate system — an option held in reserve for twenty-six years and exercised only when the gold stock could no longer plausibly cover the claims.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_authorities, payer,
    institutional, generational, constrained, global).

% Sovereign creditors — France foremost after 1965 — hold enforceable treaty rights to present dollars for gold at the fixed official price. They choose when to exercise: passively accumulating claims as leverage over U.S. policy, or converting aggressively when confidence wanes. After the 1968 two-tier split, exercising the right means acquiring gold at $35 against a higher free-market price — a riskless gain funded by the issuer's metal. Their exit from the arrangement is trivial: present dollars, take gold, and the obligation works exactly as written in their favor.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nation_gold_claimants, beneficiary,
    powerful, generational, arbitrage, global).

% Central banks and finance ministries — Germany, Japan, the United Kingdom, others — hold dollar reserves made acceptable by the redemption backstop: the dollar functions as a yield-bearing gold substitute. Most restrain their conversion rights to avoid collapsing the system that stores their wealth; individually able to redeem at will, collectively unable to without destroying the value of their own holdings. They benefit from the guarantee while their restraint finances the issuer's deficits.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, foreign_official_dollar_holders, beneficiary,
    institutional, biographical, mobile, global).

% Administers the par-value framework the redemption pledge sits inside: registers par values, conducts surveillance consultations, interprets Article IV provisions, and manages the fundamental-disequilibrium escape clause. It inherited the architecture from the 1944 conference dominated by the U.S. and UK, and it cannot compel the system's largest shareholder; its instruments are publicity, consultation, and suasion. Its own operations depend on the framework continuing, so it manages drift rather than adjudicating breaches.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, imf_par_value_administration, agenda_setter,
    institutional, generational, constrained, global).

% Bullion dealers, speculators, and private holders face the official $35 price only indirectly through the London and Zurich markets; the redemption window is reserved for official institutions and closed to them by rule. After 1968 the two-tier split freezes their exclusion into architecture: the free price floats above an official price they may not access. Admitted to the window, they would close the gap instantly; the bar that keeps them out is part of what holds the official price up.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, private_gold_markets, excluded,
    organized, immediate, mobile, global).

% Economic historians and international-monetary scholars reconstruct the pledge's legal force from treaty text and Fund practice, track gold flows against policy episodes, and argue over which reading of Article IV the historical record supports. They bear none of the arrangement's costs and collect none of its gains; their seat is the archive.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, monetary_history_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__strict_convertibility_reading, creditor_nation_gold_claimants).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__strict_convertibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors the par-value system: by pledging redemption of official dollar holdings in gold at a fixed price, the U.S. makes the dollar universally acceptable as a reserve asset, allowing every other member to peg to the dollar without holding gold — solving the postwar scarcity of monetary reserves and suppressing the interwar pattern of competitive devaluation.
% TRANSFER_FUNCTION: Moves enforceable gold claims from the U.S. gold stock to whichever foreign official presents dollars; moves monetary-policy autonomy from U.S. authorities to the external parity commitment; and, running the other way, transfers reserve-currency seigniorage and financial centrality to the U.S. as compensation the arrangement itself never priced.
% ABSENT_VOICES: Private gold holders and bullion markets are barred from the official window by rule and would object to the two-tier fiction if consulted. Debtor and developing members held minimal voice in quota-weighted Fund governance while absorbing the system's spillovers. U.S. domestic constituencies bearing the contractionary side of dollar defense had no seat in the international bargain at all.
% DISAPPEARANCE_RATIONALE: Overnight removal strips the par-value system of its anchor: creditor nations reprice their dollar holdings immediately, the fixed-rate architecture loses its credibility foundation, and the trading world reorganizes around whatever successor arrangement the major economies improvise — which is, historically, exactly what followed August 1971: the Smithsonian attempt, generalized floating by March 1973, and the end of gold-parity money.
% FOUNDING_PROBLEM: Postwar monetary disorder: interwar competitive devaluations had destroyed trade and cooperation; postwar Europe faced a chronic dollar shortage and needed reconstruction finance; the world required a stable unit of account backed by credible reserves, which in 1944 only the U.S. gold stock could provide.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: U.S. Treasury and Federal Reserve records document the pivot from managing dollar scarcity to defending against dollar glut; independent monetary economics attests the founding scarcity ended around 1958-60 (the shift from the 'dollar shortage' to the 'dollar glut' literature, and Triffin's 1960 Congressional testimony on the system's changed problem); IMF Executive Board discussions of the late 1960s acknowledge the adjustment asymmetry the founding design never contemplated. No party disputes that the founding conditions passed; France's gold campaign attacked the arrangement's continuation, not the reality of its original purpose.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (end-state 0.80) is high because the strict reading prices the obligation at what it actually cost the issuer by 1971: domestic credit expansion drew redeemable claims against a finite gold stock, and defending the parity repeatedly forced policy the domestic agenda had not chosen. Suppression (0.82) reflects the end-state absence of alternatives: comply through contraction and controls, negotiate a devaluation the system's politics made nearly impossible, or breach with systemic consequences — the U.S. exhausted the first two before choosing the third. Theater (0.45) rises across the interval as functional defense gave way to partial and performative measures: the Interest Equalization Tax, voluntary foreign credit restraint programs, rhetorical 'defense of the dollar,' and finally the two-tier market, which institutionalized a pretense that the official price still governed. The suppression_requirement series is authored deliberately: this story traces enforcement-capacity change, from dormancy during the dollar-shortage years (no one wanted to redeem), through the Gold Pool, swap networks, and GAB machinery of the early 1960s, to crisis-level enforcement attempts that failed in 1971. All three series share one time grid; every metric is authored at every examined point. The claim (tangled_rope) is stated from the structure — coordination function, asymmetric burden, active enforcement — and the metrics are stated from the record; where they diverge from any predicted engine output, that divergence is the datum.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the U.S. payer seat, the arrangement is a binding subordination of domestic policy to an external parity — high effective burden, worsening as gold drained. From the creditor-claimant seat, the same structure is an enforceable property right: a standing option to swap paper for metal at a fixed price, which after 1968 became literal arbitrage once the free-market price separated from the official one. From the broad reserve-holder seat, it is a valuable guarantee best left unexercised. From the Fund's administrative seat, it is a surveillance workload without capture. The engine derives these divergent classifications from the role, power, and exit data; nothing in the authored claim forces them to agree.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain runs on the declared structure. us_monetary_authorities sits in the victims array with constrained exit — near the full-target end of directionality, and its burden amplifies as scope-wide verification of U.S. deficits tightened. creditor_nation_gold_claimants sit in beneficiaries with arbitrage-grade exit (post-1968, exercising the claim at $35 against a higher market price is the cleanest arbitrage in the story) — nearest the full-beneficiary end. foreign_official_dollar_holders are beneficiaries with mobile exit, though the commentary notes the collective-action trap: individually able to redeem, collectively unable to without destroying the value of their own holdings. imf_par_value_administration appears in neither array — it administers without collecting — so it takes its power-atom fallback rather than a structural d. No directionality overrides are authored: the three institutional-power seats differ in exactly the dimensions the derivation reads (array membership and exit options), so overrides keyed by power atom would flatten distinctions the structural data already carries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — postwar monetary disorder, the dollar shortage, and preventing a repeat of interwar competitive devaluation — was substantially dead by 1958, when European convertibility was restored and the dollar shortage flipped to a dollar glut. The obligation persisted thirteen more years, now serving dollar-hegemony finance and creditor reserve storage rather than its founding purpose. The tangled_rope classification is what keeps both faces visible: a snare label would erase twenty-five years of genuinely delivered exchange-rate stability and the real coordination the anchor provided; a rope label would erase the asymmetric burden, the enforceable-claim structure, and the enforcement machinery the asymmetry required. A piton reading fails on the record — the constraint was never mostly performance; gold physically left Fort Knox at accelerating rates, and the 1971 breach marks precisely the point where the burden outran the coordination value and the payer chose default over continued compliance. The R5 interview records the founding problem as dead with the world still rearranging on removal — the mismatch flag this pairing raises is the correct diagnostic for late-Bretton-Woods.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_kernel,
    'This story instantiates the strict_convertibility_reading of the dollar_gold_convertibility kernel; how would the classification change under the sibling readings?',
    'Cross-read the sibling stories: the policy_flexible_reading authors the obligation as conditional on domestic stability (lower epsilon, the U.S. partially exits the victim set, likely rope-flavored); the triffin_structural_reading reframes the harm as an unsustainable design flaw borne system-wide rather than by any single seat.',
    'Classification is indexical to the reading: the strict reading yields a tangled_rope profile with the U.S. as constrained payer; the flexible reading likely yields rope with roughly symmetric seats; the Triffin reading relocates the burden from the U.S. seat to the system''s architecture. No single classification is reading-independent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_of_kernel, conceptual, 'Constraint identity depends on which reading of the Article IV kernel is instantiated.').

omega_variable(
    article_iv_justiciability,
    'Was the convertibility obligation ever binding in the justiciable legal sense, or only politically binding through the gold-drain consequences of non-compliance?',
    'Legal-historical analysis: the Fund possessed no coercive sanction over the system''s reserve center, no tribunal ever adjudicated Article IV compliance, and bindingness operated through creditors'' conversion rights and market discipline rather than adjudication. Archival study of Fund practice and U.S. Treasury legal opinions would settle the instrument''s formal force.',
    'If the obligation was purely political, the strict reading overstates legal force: epsilon falls and the arrangement reads closer to a self-enforcing equilibrium among interested states than an enforced legal duty; if genuinely treaty-binding, the tangled_rope classification stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_iv_justiciability, empirical, 'Legal versus political bindingness of the Article IV obligation.').

omega_variable(
    seigniorage_offset_ambiguity,
    'Does the seigniorage and financial-centrality income the U.S. collected concurrently offset the policy-space burden, making the U.S. a net gainer despite its victim-seat declaration?',
    'Quantify exorbitant-privilege returns (deficit finance below market cost, reserve-currency rents, transaction-demand for dollars) against measured adjustment costs (contractionary episodes, gold-stock losses, capital-control regimes) across the interval.',
    'If net-positive for the U.S., the issuer seat''s directionality drops sharply and the arrangement reads as a bargain the U.S. renewed annually rather than an imposed obligation; the strict reading''s high-extraction claim would then rest on the 1965-1971 window alone rather than the full interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_offset_ambiguity, empirical, 'Net burden on the U.S. seat after seigniorage offset.').

omega_variable(
    compliance_motivation_ambiguity,
    'Did the U.S. honor convertibility because the obligation bound it (vindicating the strict reading) or because convertibility served hegemonic interests it would have chosen anyway?',
    'Archival and counterfactual analysis comparing U.S. behavior before the pledge (interwar), during slack (1945-1957, when compliance cost approximately zero), and under strain (1958-1971, when compliance cost soared yet unilateral breach waited until August 1971).',
    'If compliance tracks interest rather than obligation, the arrangement is better modeled as self-imposed coordination and the strict reading''s extraction claim narrows to the final decade; if compliance persisted against evident interest, the binding-obligation reading is vindicated and the authored metrics stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_motivation_ambiguity, conceptual, 'Whether observed compliance evidences bindingness or convergent interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgconv_strict_tr_t0, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(dgconv_strict_tr_t0, observed).
narrative_ontology:measurement(dgconv_strict_tr_t4, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement_basis(dgconv_strict_tr_t4, observed).
narrative_ontology:measurement(dgconv_strict_tr_t8, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(dgconv_strict_tr_t8, observed).
narrative_ontology:measurement(dgconv_strict_tr_t12, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement_basis(dgconv_strict_tr_t12, observed).
narrative_ontology:measurement(dgconv_strict_tr_t16, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement_basis(dgconv_strict_tr_t16, observed).
narrative_ontology:measurement(dgconv_strict_tr_t20, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(dgconv_strict_tr_t20, observed).
narrative_ontology:measurement(dgconv_strict_tr_t23, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 23, 0.42).
narrative_ontology:measurement_basis(dgconv_strict_tr_t23, observed).
narrative_ontology:measurement(dgconv_strict_tr_t26, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 26, 0.45).
narrative_ontology:measurement_basis(dgconv_strict_tr_t26, observed).

% Extraction over time
narrative_ontology:measurement(dgconv_strict_be_t0, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(dgconv_strict_be_t0, observed).
narrative_ontology:measurement(dgconv_strict_be_t4, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement_basis(dgconv_strict_be_t4, observed).
narrative_ontology:measurement(dgconv_strict_be_t8, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement_basis(dgconv_strict_be_t8, observed).
narrative_ontology:measurement(dgconv_strict_be_t12, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement_basis(dgconv_strict_be_t12, observed).
narrative_ontology:measurement(dgconv_strict_be_t16, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement_basis(dgconv_strict_be_t16, observed).
narrative_ontology:measurement(dgconv_strict_be_t20, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(dgconv_strict_be_t20, observed).
narrative_ontology:measurement(dgconv_strict_be_t23, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 23, 0.73).
narrative_ontology:measurement_basis(dgconv_strict_be_t23, observed).
narrative_ontology:measurement(dgconv_strict_be_t26, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 26, 0.8).
narrative_ontology:measurement_basis(dgconv_strict_be_t26, observed).

% Suppression requirement over time
narrative_ontology:measurement(dgconv_strict_su_t0, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(dgconv_strict_su_t0, observed).
narrative_ontology:measurement(dgconv_strict_su_t4, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 4, 0.12).
narrative_ontology:measurement_basis(dgconv_strict_su_t4, observed).
narrative_ontology:measurement(dgconv_strict_su_t8, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 8, 0.18).
narrative_ontology:measurement_basis(dgconv_strict_su_t8, observed).
narrative_ontology:measurement(dgconv_strict_su_t12, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement_basis(dgconv_strict_su_t12, observed).
narrative_ontology:measurement(dgconv_strict_su_t16, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement_basis(dgconv_strict_su_t16, observed).
narrative_ontology:measurement(dgconv_strict_su_t20, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(dgconv_strict_su_t20, observed).
narrative_ontology:measurement(dgconv_strict_su_t23, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 23, 0.72).
narrative_ontology:measurement_basis(dgconv_strict_su_t23, observed).
narrative_ontology:measurement(dgconv_strict_su_t26, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 26, 0.82).
narrative_ontology:measurement_basis(dgconv_strict_su_t26, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bretton Woods convertibility' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle into a three-story constraint family sharing the kernel dollar_gold_convertibility. This story (strict_convertibility_reading) authors the obligation as binding law: epsilon is high, the U.S. is the victim seat, creditor claimants are beneficiaries. The policy_flexible_reading authors the same text as a conditional commitment subordinate to domestic stability: lower epsilon, softened U.S. victim position. The triffin_structural_reading authors convertibility as an inherently unsustainable design flaw: the harm relocates from any single seat to the system's architecture, and the relevant failure mode is cumulative reserve growth rather than policy subordination. Each story carries its own epsilon, beneficiary/victim structure, and classification; this file links both siblings via affects_constraints, upstream in the sense that the strict reading's bindingness premise is what gives the Triffin confidence problem its force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
