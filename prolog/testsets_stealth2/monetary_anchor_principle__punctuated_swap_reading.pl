% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Bretton Woods Gold-Exchange Anchor — Punctuated Swap Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   Between 1958 and 1971 the international monetary system ran on a formally
 *   chosen anchor: currencies pegged to the dollar, the dollar convertible
 *   into gold at $35 per ounce, the whole administered through IMF
 *   surveillance and an active central-bank practice layer (the Gold Pool,
 *   swap networks, sterilization operations). This story instantiates the
 *   punctuated_swap_reading of the monetary_anchor_principle kernel: the
 *   regime was a discrete institutional construction solving a real
 *   collective-action problem after the interwar chaos, it remained
 *   functional until August 15, 1971, and it ended because the United States
 *   chose to end it — a single televised suspension of convertibility, not
 *   the inevitable release of accumulated structural pressure. The ε referent
 *   is the standing gold-exchange anchor itself, assessed by this reading's
 *   own lights: real coordination delivered, moderate and rising privilege
 *   extraction by the reserve issuer, and a terminal devaluation of foreign
 *   claims. The claim/metric gap is deliberate: this reading claims rope
 *   (chosen coordination, cheaply removable — the removal cost one
 *   announcement), while the authored metrics describe rising extraction,
 *   active enforcement, and real resistance; the engine computes per-seat
 *   types from the structural data, and any divergence from the rope claim is
 *   the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - us_fiscal_authority: agenda-setter and primary beneficiary (institutional/arbitrage) — issues the reserve asset, holds the convertibility commitment, and alone possessed the unilateral exit it exercised on August 15, 1971
 *   - bretton_woods_member_states: coordination beneficiaries turned terminal payers (organized/constrained) — gained two decades of parity stability, then absorbed the devaluation of their reserve claims
 *   - foreign_dollar_holders: primary target (organized/constrained) — official institutions and private parties holding dollar claims presented as gold-redeemable, locked in by the dollar-trap logic until the run of 1971
 *   - imf_oversight_body: excluded institutional guardian (institutional/trapped) — the treaty machinery for parity consultation, bypassed by the unilateral suspension
 *   - monetary_historians: analytical observer (analytical/analytical) — holds the rival readings of why the regime ended; collects nothing from the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.55).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.55).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Bretton Woods Gold-Exchange Anchor — Punctuated Swap Reading").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, 'c8f33fe9-1a10-46a2-88fd-2cc43a819927').
narrative_ontology:cs_kernel_codification('c8f33fe9-1a10-46a2-88fd-2cc43a819927', formalized).
narrative_ontology:cs_authority_grounding('c8f33fe9-1a10-46a2-88fd-2cc43a819927', practice).
narrative_ontology:cs_interpretation_layer_present('c8f33fe9-1a10-46a2-88fd-2cc43a819927').
narrative_ontology:cs_reading_relation('c8f33fe9-1a10-46a2-88fd-2cc43a819927', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8f33fe9-1a10-46a2-88fd-2cc43a819927', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('c8f33fe9-1a10-46a2-88fd-2cc43a819927', foundational, regime_termination_was_discretionary).
narrative_ontology:cs_axiom_status(regime_termination_was_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('c8f33fe9-1a10-46a2-88fd-2cc43a819927', regime_termination_was_discretionary, empirically_contingent).
narrative_ontology:cs_axiom('c8f33fe9-1a10-46a2-88fd-2cc43a819927', foundational, anchor_functional_until_swap).
narrative_ontology:cs_axiom_status(anchor_functional_until_swap, holdable).
narrative_ontology:cs_axiom_grounding('c8f33fe9-1a10-46a2-88fd-2cc43a819927', anchor_functional_until_swap, empirically_contingent).
narrative_ontology:cs_reference_frame('c8f33fe9-1a10-46a2-88fd-2cc43a819927', gold_exchange_anchor_coordination).
narrative_ontology:cs_drift_state('c8f33fe9-1a10-46a2-88fd-2cc43a819927', august_1971_suspension_eve, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c8f33fe9-1a10-46a2-88fd-2cc43a819927', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_member_states).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_member_states).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, par_value_coordination_doctrine).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, dollar_as_gold_substitute_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the dollar, sets and administers the gold-convertibility commitment at $35 per ounce, and finances domestic spending partly in its own liability. Foreign governments and holders accept dollars on the understanding they can be exchanged for gold; through the 1960s the authority finances deficits well beyond its gold stock while pressing allies to keep holding dollars. On August 15, 1971 it suspends convertibility by televised announcement, adds an import surcharge, and freezes wages and prices — the exit it alone could take, taken without consultation. The gains from the suspension accrue here: deficits continue without gold discipline, and outstanding claims are settled at devalued rates.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority, beneficiary).

% Sovereign members peg their currencies to the dollar and hold reserve assets in it. They obtain stable exchange rates, trade liquidity, and a common adjustment framework; their central banks simultaneously accumulate the dollar claims whose redemption is promised. When the window closes, those claims lose gold value — the same governments sit on both sides of the arrangement, gaining from parity stability across two decades and losing part of their reserve value in the terminal devaluations of 1971 and 1973. Leaving the peg means revaluing against partners and repricing trade; most revalue only after the anchor is gone.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_member_states, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_member_states, payer).

% Official institutions and private parties outside the United States hold dollar claims presented as redeemable in gold. Through the 1960s they accumulate claims far beyond the gold stock backing them; converting en masse would collapse the system and their own reserves with it, so exit is available only in ways that destroy the thing being exited. In 1971 official holders run the conversion window hard enough that the United States closes it; their claims are then settled at successively devalued rates. Private holders lost access to official-parity conversion in 1968 and carry the devaluation directly.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer,
    organized, generational, constrained, global).

% The treaty body created to administer the par-value system: members are obliged to consult it before changing parities, and it supplies credit and surveillance to keep the system running. The August 1971 suspension is announced without consulting it; its machinery for defending parities is sidelined by the very member whose commitment backs the system. It continues to exist and later brokers the Smithsonian realignment, but the unilateral act establishes that its oversight does not bind the reserve issuer.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, imf_oversight_body, excluded,
    institutional, generational, trapped, global).

% Scholars of the episode who hold rival accounts of why the regime ended: one camp reads August 1971 as a discrete choice that could have gone otherwise; others read it as the release of pressures that made some termination inevitable. They work from archives, reserve data, and decision records; their seat is analytical and they collect nothing from the arrangement.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__punctuated_swap_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the postwar payments problem: how to run expanding international trade on stable exchange rates without a world currency or recurring gold shipments. The dollar, convertible at $35 per ounce, served as the common reserve asset and nominal anchor; members pegged to it, the issuer converted, and IMF credit bridged temporary imbalances.
% TRANSFER_FUNCTION: Moves real resources and seigniorage from foreign holders to the reserve issuer: exporters and governments accept dollar claims in exchange for goods, the issuer finances domestic spending in its own liability without immediate gold settlement, and at termination outstanding claims are marked down by devaluation rather than redeemed at parity — a transfer from foreign reserves to the issuer's fiscal position.
% ABSENT_VOICES: Every holder of redeemable claims and every allied government: the suspension was announced by television on Sunday, August 15, 1971, without consulting foreign finance ministries, the IMF's governing bodies, or any holder whose reserves the act devalued. They would have argued for negotiated devaluation or shared adjustment; the absence of their voice is the unilateral character of the act itself.
% DISAPPEARANCE_RATIONALE: The rearrangement is the historical record: generalized floating by March 1973, a decade of inflationary finance across the system, the search for replacement reserve assets, and eventually oil-surplus recycling through the same dollar. Trade did not collapse — managed floats and IMF surveillance partially replaced the anchor — but reserve holding, exchange-rate practice, and the terms on which the issuer finances itself all reorganized around the swap.
% FOUNDING_PROBLEM: The interwar monetary breakdown: competitive devaluations, beggar-thy-neighbor trade policy, and the gold standard's deflationary rigidity during the Depression. Bretton Woods was designed to combine gold discipline with elastic liquidity — stable parities without a world scarcity of reserves.
% FOUNDING_PROBLEM_CORROBORATION: European central bank archives (Bundesbank, Bank of England) and IMF institutional records — sources outside the beneficiary seat — corroborate that liquidity provision and parity stability remained the regime's operating functions into 1971, and the U.S. Treasury's own decade-long defense of convertibility corroborates that the founding problem was treated as live by the benefiting party until the swap itself. No source outside the scholarly dispute adjudicates whether the problem was still live in August 1971; that open adjudication is the contested status.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 (moderate) and rises monotonically across the interval: the reserve issuer's privilege — financing deficits in its own liability without gold settlement — grew with Vietnam-era deficits and terminated in the 1971–73 devaluations of outstanding claims. Suppression 0.55 is authored as a raw structural property (the engine, not the story, scales extraction by directionality and scope): the regime required active enforcement throughout — the Gold Pool's gold defense, offset agreements tying troop deployments to dollar holdings, pressure on allies not to convert — and holders' exit was constrained by the dollar trap, in which converting en masse destroys the reserves being converted. Theater 0.30: the anchor's functions were real (liquidity, parity stability, trade settlement), with a theatrical component growing after 1968, when the two-tier gold market preserved the pretense of official parity while private parity was gone. Accessibility collapse 0.25 is the reading's core empirical commitment: alternatives did not collapse — states revalued, floated, or could have continued under modified rules; the regime was ended by choice among live options, which is what distinguishes this reading from its inevitability siblings. Resistance 0.45: French convertibility demands from 1965, sterling's 1967 devaluation spilling onto the dollar, and the 1971 official run — real pressure, but on this reading not regime-fatal until the choice. All three metric series run on one shared time grid (1958, 1961, 1964, 1967, 1970, 1971) so every tracked metric is authored at every examined point; final values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the us_fiscal_authority seat the arrangement is a commitment it made while advantageous and revoked when the cost flipped — the same event that reads as sovereign choice from inside the Treasury reads as repudiation of treaty-held claims from the holder seat, whose exit was structurally constrained in a way the issuer's never was. bretton_woods_member_states straddle the divide: net gainers from two decades of parity stability and simultaneous losers in the terminal devaluation; their dual beneficiary/payer declarations derive a mid-range directionality. The IMF seat experienced the same suspension as the sidelining of its consultative mandate. The engine computes these per-seat classifications from power, exit, and role data; the authored rope claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   us_fiscal_authority sits near the beneficiary end: it declares the rules, collects the seigniorage and the fiscal autonomy, and held the only unconstrained exit — demonstrated by the swap itself. foreign_dollar_holders sit near the target end: they bore the transfer (real goods for claims later marked down) and their exit was constrained by the dollar trap. bretton_woods_member_states carry dual declarations (beneficiary of coordination, payer of the terminal devaluation), deriving a mid-range d. The imf_oversight_body is an excluded seat: it neither collected nor paid, and its absence from the August 15 decision is recorded in absent_voices rather than in the directionality structure. Scope is global throughout — the dollar system spanned every trading economy — which the engine folds into effective extraction: verifying a convertibility promise across the whole system is hard, and the dollar overhang grew unverified for years before the run exposed it.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against one error and the metrics guard against its mirror. Claiming rope prevents reading the anchor's termination as proof that the regime was always extraction in coordination's clothing: the coordination was real — postwar trade expanded under stable parities, and the founding problem (interwar chaos) was genuine, with corroboration from outside the beneficiary seat. The rising extraction series prevents the mirror error of romanticizing the anchor as pure coordination: privilege accumulated for a decade and terminated in a devaluation of claims held under treaty promise. The punctuated framing also blocks the mandatrophy mislabel of natural death: founding_problem_status is contested, not dead — on this reading the arrangement was killed while still functional, which is the opposite of an expired mandate maintained by inertia. The receipt surface records the structural signature that makes this reading coherent: gains flow to a single named seat (us_fiscal_authority) and fixing was cheap for that seat — a single announcement removed the arrangement, which is what a chosen institution looks like and what an inevitability-driven collapse does not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_choice_vs_inevitability,
    'This story is one reading of the monetary_anchor_principle kernel — the punctuated_swap_reading, which holds the August 1971 termination was a discrete institutional choice. Do the sibling readings (overdetermined_composite_reading, triffin_inevitability_reading) better capture the termination''s modal status — was some end inevitable by 1971 regardless of choice?',
    'Archival decision records from the Camp David weekend and Treasury staff papers establishing which options were actually on the table; counterfactual analysis of whether orderly devaluation or continued convertibility with capital controls was feasible given 1971 reserve positions.',
    'If some termination was inevitable, this reading''s rope-with-cheap-removal classification misleads and the siblings'' structural-inevitability ε applies; if the choice was live, the discrete-swap framing stands and the regime''s death was discretionary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_choice_vs_inevitability, empirical, 'Whether the 1971 termination was chosen or structurally inevitable — the kernel''s central contest.').

omega_variable(
    expropriation_vs_priced_default,
    'Was the 1971–73 devaluation of dollar claims an uncompensated expropriation of holders who were promised gold redemption, or an orderly default on a risk holders had already priced (the two-tier gold market spread from 1968 suggests devaluation risk was visible)?',
    'Compare the real redemption value of official dollar claims before and after suspension against the treaty promise; examine whether official holders'' behavior (running the window) reflects priced or unpriced risk.',
    'If expropriation, foreign_dollar_holders'' position as victims strengthens and ε rises above the authored 0.55; if priced-in default risk, the terminal transfer is ordinary sovereign default and ε falls toward the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expropriation_vs_priced_default, empirical, 'Whether the terminal devaluation counts as expropriation or as priced default risk.').

omega_variable(
    reversibility_in_principle,
    'The reading holds the institutional choice was reversible in principle — was a restored anchor actually feasible after August 1971, given that the Smithsonian realignment and the February 1973 devaluation both failed within months?',
    'Analyze why the Smithsonian attempt failed: speculative flow magnitudes versus available commitment technology; whether any parity the United States could defend was consistent with the dollar overhang.',
    'If restoration was infeasible, the reversible-in-principle premise weakens and the reading converges toward the inevitability siblings; if feasible but politically refused, the discretionary-choice framing is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_in_principle, empirical, 'Whether the post-suspension regime swap was reversible in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1958, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(punctuated_swap_tr_t1958, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement_basis(punctuated_swap_tr_t1958, observed).
narrative_ontology:measurement(punctuated_swap_tr_t1961, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1961, 0.12).
narrative_ontology:measurement_basis(punctuated_swap_tr_t1961, observed).
narrative_ontology:measurement(punctuated_swap_tr_t1964, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1964, 0.15).
narrative_ontology:measurement_basis(punctuated_swap_tr_t1964, observed).
narrative_ontology:measurement(punctuated_swap_tr_t1967, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement_basis(punctuated_swap_tr_t1967, observed).
narrative_ontology:measurement(punctuated_swap_tr_t1970, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1970, 0.26).
narrative_ontology:measurement_basis(punctuated_swap_tr_t1970, observed).
narrative_ontology:measurement(punctuated_swap_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.3).
narrative_ontology:measurement_basis(punctuated_swap_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(punctuated_swap_be_t1958, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1958, 0.28).
narrative_ontology:measurement_basis(punctuated_swap_be_t1958, observed).
narrative_ontology:measurement(punctuated_swap_be_t1961, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1961, 0.31).
narrative_ontology:measurement_basis(punctuated_swap_be_t1961, observed).
narrative_ontology:measurement(punctuated_swap_be_t1964, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1964, 0.36).
narrative_ontology:measurement_basis(punctuated_swap_be_t1964, observed).
narrative_ontology:measurement(punctuated_swap_be_t1967, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1967, 0.42).
narrative_ontology:measurement_basis(punctuated_swap_be_t1967, observed).
narrative_ontology:measurement(punctuated_swap_be_t1970, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement_basis(punctuated_swap_be_t1970, observed).
narrative_ontology:measurement(punctuated_swap_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.55).
narrative_ontology:measurement_basis(punctuated_swap_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(punctuated_swap_su_t1958, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1958, 0.22).
narrative_ontology:measurement_basis(punctuated_swap_su_t1958, observed).
narrative_ontology:measurement(punctuated_swap_su_t1961, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1961, 0.3).
narrative_ontology:measurement_basis(punctuated_swap_su_t1961, observed).
narrative_ontology:measurement(punctuated_swap_su_t1964, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1964, 0.35).
narrative_ontology:measurement_basis(punctuated_swap_su_t1964, observed).
narrative_ontology:measurement(punctuated_swap_su_t1967, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1967, 0.42).
narrative_ontology:measurement_basis(punctuated_swap_su_t1967, observed).
narrative_ontology:measurement(punctuated_swap_su_t1970, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement_basis(punctuated_swap_su_t1970, observed).
narrative_ontology:measurement(punctuated_swap_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.55).
narrative_ontology:measurement_basis(punctuated_swap_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, fiat_dollar_reserve_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Nixon Shock' / 'the end of Bretton Woods' conflates three structurally distinct claims about the same transition. This story authors ε only for the standing gold-exchange anchor as the punctuated_swap_reading assesses it: a chosen coordination regime terminated by discretionary act (ε 0.55, moderate, reversible in principle). The sibling readings instantiate different constraints: the overdetermined_composite_reading assesses a regime already structurally doomed by composite pressures (different ε, different failure mode), and the triffin_inevitability_reading assesses a reserve-issuer dilemma that forces abandonment (different victim structure — global liquidity seekers rather than treaty claimants). The dispute between the readings is counterfactual and empirical, not logical: a single framework can acknowledge accumulated pressure while denying inevitability, so the readings coexist as live positions rather than foreclosing one another. The termination's downstream product — the pure fiat dollar reserve regime — is a separate constraint this one causally feeds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
