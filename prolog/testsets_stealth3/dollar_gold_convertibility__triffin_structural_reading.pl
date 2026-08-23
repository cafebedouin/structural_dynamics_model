% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [SUPERSEDED - COLLAPSED AUGUST 1971]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility (Triffin Structural Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This story instantiates the Triffin structural reading of the dollar-gold
 *   convertibility kernel: the Article IV par-value commitment, operative for
 *   external accounts from 1958, contained a design-level contradiction -
 *   supplying the world's reserve liquidity required the issuing country to
 *   run deficits that progressively undermined the very convertibility being
 *   promised, so the arrangement charged BOTH principals (the issuing
 *   authorities and the creditor central banks) for its entire operating life
 *   and could end only in unilateral termination. The referent of epsilon is
 *   the standing 1958-71 convertibility arrangement as this reading assesses
 *   it - not the floating regime this reading endorsed, and not the treaty's
 *   aspirational text. Claim/metric independence is preserved: the claimed
 *   type is tangled_rope because the structure coupled a genuine coordination
 *   service (a single settlement asset, stable announced parities) to
 *   asymmetric extraction borne by both principals under active enforcement
 *   (London Gold Pool, swap network, capital controls, allied diplomatic
 *   pressure); the metrics describe that coupled operation rising under load
 *   until August 1971. Stated assumptions: the interval runs from restoration
 *   of European external convertibility (December 1958) to generalized
 *   floating (March 1973); base_properties.scalars represent the
 *   enforcement-intensive plateau (suppression 0.72) and the end-state
 *   respectively (extractiveness 0.78, theater 0.30); gain_flow names the
 *   U.S. authorities' seat because the seigniorage receipt demonstrably
 *   landed there, while receipt is kept distinct from net benefit - the same
 *   seat sits in the victim set under this reading's accounting. Per the
 *   epsilon-invariance principle this is one of three files decomposing the
 *   colloquial label 'dollar-gold convertibility'; the siblings
 *   (strict_convertibility_reading, policy_flexible_reading) carry different
 *   epsilon values and are linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - united_states_monetary_authorities: Primary target and administrator ([institutional]/[identity_locked]) - runs the defense machinery while absorbing its costs
 *   - creditor_nation_central_banks: Co-target ([institutional]/[trapped]) - accumulate irredeemable-in-practice claims; early coordination dividend, late trap
 *   - france_de_gaulle_government: Dissident creditor ([institutional]/[constrained]) - exercises partial exit by visible gold conversion
 *   - dollar_settlement_users: Coordination constituency ([organized]/[constrained]) - receives settlement services and stable parities
 *   - private_gold_speculators: Arbitrage seat ([organized]/[arbitrage]) - harvests the official/free-market gold spread
 *   - imf_secretariat: Administrative collector ([institutional]/[mobile]) - scales with the arrangement it polices
 *   - floating_rate_regime_constituents: Residual beneficiary ([organized]/[mobile]) - gains materialize at the moment of collapse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.78).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.72).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility (Triffin Structural Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, '7c8bb4bf-caa1-4aa6-85dd-de995be5e915').
narrative_ontology:cs_kernel_codification('7c8bb4bf-caa1-4aa6-85dd-de995be5e915', formalized).
narrative_ontology:cs_authority_grounding('7c8bb4bf-caa1-4aa6-85dd-de995be5e915', extraction).
narrative_ontology:cs_interpretation_layer_present('7c8bb4bf-caa1-4aa6-85dd-de995be5e915').
narrative_ontology:cs_reading_relation('7c8bb4bf-caa1-4aa6-85dd-de995be5e915', dollar_gold_convertibility__strict_convertibility_reading, influences).
narrative_ontology:cs_reading_relation('7c8bb4bf-caa1-4aa6-85dd-de995be5e915', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_axiom('7c8bb4bf-caa1-4aa6-85dd-de995be5e915', foundational, reserve_currency_hard_redemption_structurally_unworkable).
narrative_ontology:cs_axiom_status(reserve_currency_hard_redemption_structurally_unworkable, holdable).
narrative_ontology:cs_axiom_grounding('7c8bb4bf-caa1-4aa6-85dd-de995be5e915', reserve_currency_hard_redemption_structurally_unworkable, empirically_contingent).
narrative_ontology:cs_axiom('7c8bb4bf-caa1-4aa6-85dd-de995be5e915', secondary, systemic_revision_required_over_patchwork_preservation).
narrative_ontology:cs_axiom_status(systemic_revision_required_over_patchwork_preservation, holdable).
narrative_ontology:cs_axiom_grounding('7c8bb4bf-caa1-4aa6-85dd-de995be5e915', systemic_revision_required_over_patchwork_preservation, instrumental).
narrative_ontology:cs_reference_frame('7c8bb4bf-caa1-4aa6-85dd-de995be5e915', designed_postwar_liquidity_mechanism).
narrative_ontology:cs_drift_state('7c8bb4bf-caa1-4aa6-85dd-de995be5e915', post_august_1971_collapse, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('7c8bb4bf-caa1-4aa6-85dd-de995be5e915', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, floating_rate_regime_constituents).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, dollar_settlement_users).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authorities).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nation_central_banks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, creditor_nation_central_banks).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, private_gold_speculators).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, imf_secretariat).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, france_de_gaulle_government).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, triffin_dilemma_thesis).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, friedman_floating_rate_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the arrangement: fixes the dollar's gold price at $35 an ounce, commits to redeem foreign official dollar holdings, and deploys swap lines, the London Gold Pool, capital controls, and diplomatic pressure to hold the parity. Collects the privilege of settling external deficits in its own currency, financing overseas military and aid commitments without prior taxation. Pays in kind: gold reserves fall from roughly $24 billion to under $11 billion across the interval, domestic interest-rate and budget choices bend to parity defense, and every year of continued issuance enlarges the volume of claims it may one day be unable to redeem. Leaving the arrangement means renouncing the leadership role the arrangement expresses; staying means accumulating obligations that grow faster than the metal backing them.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authorities, payer,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authorities, agenda_setter).

% Accumulate dollar claims as the counterpart of export surpluses with the United States. Each holds the option to demand gold at the fixed price, yet mass redemption would exhaust U.S. gold and end the system that supplies their reserve assets and stable trade parities; individually rational conversion is collectively ruinous, so holdings pile up as unredeemable-in-practice paper. They absorb imported U.S. inflation, submit to repeated parity revaluations negotiated under allied pressure (mark 1961, 1969, 1971), and contribute to the arrangement's defense through offset agreements and reserve cooperation. Early in the interval they also collected real dividends - liquid reserves, stable rates for their exporters. Exit by redemption is available only at the cost of destroying the arrangement itself.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nation_central_banks, payer,
    institutional, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, creditor_nation_central_banks, beneficiary).

% A creditor government that declines the quiet-accumulation role. It converts dollar reserves into gold in visible volumes from 1965 onward, argues publicly that the arrangement privileges the issuer, and demands return to a gold-disciplined order. It bears imported inflation and alliance friction, and it spends political capital contesting the rules rather than accepting revaluation quietly. Its conversions are tolerated only up to the volume that would break the system outright - beyond that point the governors lean on it diplomatically.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, france_de_gaulle_government, payer,
    institutional, biographical, constrained, national).

% Exporters, importers, banks, and governments that invoice and settle cross-border trade in dollars at stable announced parities. They receive deep, liquid settlement rails and predictable conversion rates for the duration of the arrangement; they bear its costs only diffusely, through prices and taxes. Their alternative - invoicing in other currencies - stays thin throughout, since rival settlement currencies are themselves defined against the dollar.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, dollar_settlement_users, beneficiary,
    organized, biographical, constrained, global).

% Banks and traders in London, Zurich, and Paris who buy and sell gold around the official price. Every defense of the $35 price against rising free-market demand widens the spread they capture; the 1968 two-tier arrangement hands them an officially sanctioned private market priced far above the official one. Their position requires no permission from the arrangement's governors, and their exits - physical metal, francs, marks - stay open throughout.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, private_gold_speculators, beneficiary,
    organized, immediate, arbitrage, continental).

% Administers surveillance of par values, brokers standby arrangements and parity changes, and staffs a permanent bureaucracy funded by member quotas. Its relevance, staffing, and authority scale with the arrangement it polices; it collects administrative continuity whichever way parity disputes resolve. It never compels the United States - the largest quota holder and the arrangement's anchor - to any course of action.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, imf_secretariat, beneficiary,
    institutional, generational, mobile, global).

% Foreign exchange dealers, multinational treasurers, and central banks whose operating space expands the moment convertibility ends. During the arrangement's life they hold no seat in its governance; their gains arrive at the moment of collapse, when exchange risk becomes a tradable product, hedging becomes a business line, and national monetary policy detaches from the parity commitment. They inherit the arrangement's client base: the same cross-border commerce that settled in convertible dollars continues, repriced daily.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, floating_rate_regime_constituents, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authorities).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__triffin_structural_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a common international reserve asset and fixed announced parities: cross-border trade and lending settle through one convertible unit, eliminating the bilateral-clearing and competitive-devaluation chaos of the 1930s, and the reserve-currency country recycles its liabilities as working balances for everyone else's trade.
% TRANSFER_FUNCTION: Moves real goods, services, and policy concessions from creditor economies to the reserve-currency country in exchange for dollar claims (1958 to the mid-1960s); moves gold from U.S. vaults to converting creditors as confidence erodes (mid-1960s to 1971); moves domestic policy autonomy from every participant into the defense of the parity; finally, at termination, converts accumulated dollar claims into losses for their holders.
% ABSENT_VOICES: Floating-rate economists argued from outside the par-value consensus throughout and were heard in print but not in governance; private dollar holders and non-G10 trading partners had no seat where liquidity rules were set; developing economies - whose later G77/24 caucus would demand a voice - were absent entirely from both the 1944 design conference and the 1960s defense councils.
% DISAPPEARANCE_RATIONALE: Within twenty-six months of termination the par-value system is gone, exchange rates float generally, the IMF rewrites its articles (Jamaica, 1976) to legalize what happened, the two-tier gold market becomes a free market, and a foreign-exchange industry measured in trillions grows where a fixed price used to stand. Nothing of the 1958-71 configuration survives except the dollar's reserve role - now unanchored.
% FOUNDING_PROBLEM: Postwar reconstruction faced an acute dollar shortage and the living memory of interwar competitive devaluations and discriminatory currency blocs; the arrangement was built to deliver adequate international liquidity plus stable parities until national economies recovered enough for multilateral trade to run on its own.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Robert Triffin's 1959-60 Joint Economic Committee testimony framed the liquidity-versus-confidence contradiction as an academic critic with no governance seat; French treasury memoranda pressed the same diagnosis from a creditor seat; economic historians (Eichengreen, Bordo) confirm the founding problem was real and document its recurrence - the same liquidity-provider dilemma returns in the 1980s debt crisis, the 1990s emerging-market dollar shortages, and the 2008 Federal Reserve swap-line episode. No party claims the founding problem was fictitious; the dispute is over whether it ever admitted a workable solution of this shape.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78: the arrangement charged both principals continuously - the issuing side paid in metal and policy autonomy, the creditor side in frozen claims and imported inflation - while delivering a real settlement service, placing it high but short of confiscatory levels. Suppression 0.72 is authored as a raw structural property and is deliberately NOT scaled here by power or scope; the engine applies its own scaling to effective extraction only. The suppression_requirement series is authored because this story specifically traces enforcement-capacity change: machinery built from the 1961 Gold Pool through the 1963 Interest Equalization Tax and the 1968 two-tier gold market, peaking at the August 1971 controls package, then dismantled as floating began. Theater_ratio peaks at 0.42 in 1971: after March 1968 the official $35 price coexisted with a free-market price near $43, so the final defense consisted substantially of maintaining a price nobody traded at. Accessibility_collapse 0.45: alternatives (floating rates, SDRs, crawling bands) stayed visible and publishable throughout - the floating-rate advocacy never stopped - but politically unreachable until collapse forced them. Resistance 0.6: open French conversion campaigns, academic attack, recurrent speculative runs. All measurements share one seven-point grid (1958, 1961, 1964, 1967, 1969, 1971, 1973) with every tracked metric authored at every point; the trajectories are monotone ratchets rather than cycles - no intermittent-reinforcement dynamic is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structural facts. From the issuing authorities' chair the arrangement is the burden of a leadership role they cannot renounce without dissolving the alliance economy it finances; from a creditor central bank's chair it is a trap in which every rational act (redeeming, hoarding, revaluing) damages the actor; from the speculator's desk it is a widening spread; from the exporter's office it is dependable infrastructure. The creditor seats diverge among themselves at equal nominal power: France converted visibly and contested the rules, Germany absorbed revaluations quietly - same power atom, different exit exercise, which is why exit_options rather than power carries the differentiation. The engine computes per-seat classifications from these structural data; the story-level claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations push four seats toward the subsidized pole: floating_rate_regime_constituents (gains arrive at collapse), dollar_settlement_users (service recipients), imf_secretariat (administrative rents scale with the arrangement), private_gold_speculators (spread harvesters). Victim declarations push the two principals toward the target pole: united_states_monetary_authorities and creditor_nation_central_banks. Two complications are handled by declaration rather than override. First, the U.S. seat receives the seigniorage flow (gain_flow names it) yet is declared a victim under this reading - the receipt is recorded, the net position is left to the net_us_position omega. Second, creditor banks carry a secondary beneficiary role for the early liquidity and parity dividend, tempering but not reversing their derived directionality. No directionality_overrides entries are authored: overrides key on power_atom, and this story seats four agents at the institutional power level with opposed relationships - a single override would sweep them together and corrupt a derivation the structural data already produces correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work this story performs is refusing three easier labels. Not scaffold: nothing about the arrangement carried a designed exit - no sunset clause existed, and the systemic revision this reading demands arrived by rupture, not by scheduled transition; has_sunset_clause stays false, keeping the scaffold gate closed. Not mountain: the obligation was signed, amended, and broken - emerges_naturally stays false; the genuinely lawlike element (the arithmetic by which liquidity provision erodes redemption cover) is quarantined in the design_flaw_vs_policy_failure omega rather than asserted as naturality. Not piton: the enforcement apparatus was load-bearing to the end - theater peaked only at the terminal improvisation, and the function did not atrophy before the structure broke; a piton reading would mistake the 1968-71 price fiction for the whole arrangement. Tangled rope fits because coordination and extraction ran through the same pipes for both principals until rupture. On obsolescence: the founding problem (who supplies international liquidity, and at whose risk) is live - it recurs in the 1980s debt crisis, the 1990s dollar shortages, and the 2008 swap lines - while the arrangement itself is gone, so the mismatch consumer reads live-status against a world_rearranges verdict with no zombie flag: the arrangement died with its mandate rather than outliving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This file instantiates one reading (triffin_structural_reading) of the kernel dollar_gold_convertibility; how would classification shift under the sibling readings strict_convertibility_reading and policy_flexible_reading?',
    'Compare compiled classifications of the sibling stories: the strict reading models the obligation as enforceable law constraining U.S. monetary policy (the U.S. seat becomes the bound obligor rather than a trapped party); the flexible reading models it as a defeasible conditional duty (deviations lawful, suppression drops toward ordinary compliance friction). Convergence or divergence across the three files maps the kernel''s contest surface.',
    'Under the strict reading the U.S. seat loses victim standing and extraction compresses toward legal-compliance cost; under the flexible reading suppression falls and the arrangement reads as ordinary coordination with occasional lawful derogation. Only this reading places both the U.S. and the creditor nations in the victim set with high extraction until collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the convertibility kernel governs the classification.').

omega_variable(
    design_flaw_vs_policy_failure,
    'Was the collapse a mathematical necessity built into the arrangement''s design, or a contingent outcome of particular policy choices (Vietnam deficit finance, deferred adjustment, refusal to devalue earlier)?',
    'Counterfactual economic history: model whether a disciplined-surplus United States, an earlier agreed dollar devaluation, or timely SDR substitution could have sustained official convertibility indefinitely; test the model against analogous reserve arrangements.',
    'If necessity, the dilemma behaves like natural law riding inside a constructed obligation and the arrangement was doomed at signature; if contingency, the extraction was policy-made and remediable. The resolution separates a quasi-mountain core from a purely constructed tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_flaw_vs_policy_failure, conceptual, 'Whether unsustainability inheres in the design or in the policies run on top of it.').

omega_variable(
    net_us_position,
    'Across the full interval, did the United States net-collect more than it net-paid (seigniorage and deficit finance versus gold losses and forgone policy autonomy), or did payments exceed collections?',
    'Full-interval accounting: value the seigniorage flow (real resources absorbed against dollar liabilities), the decline of the gold stock, and the interest-rate and fiscal distortions attributable to parity defense; compare cumulative totals.',
    'If net collection dominates, the U.S. seat''s directionality sits nearer the beneficiary pole than the victim declaration implies and the arrangement shades toward extraction from creditors with a privileged capturer; if net payment dominates, the mutual-trap reading stands and both principal seats remain targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_us_position, empirical, 'Whether the issuing government was net gainer or net loser across the arrangement''s life.').

omega_variable(
    suppression_trap_vs_coercion,
    'How much of the measured suppression was active coercive machinery (capital controls, the Interest Equalization Tax, diplomatic pressure on allies) versus self-enforcing trap logic in which no holder could redeem without destroying the asset''s value?',
    'Compare holder behavior across windows of differing formal-control intensity (the relatively open window before 1963 versus the controlled 1963-69 span) and test whether redemption abstention tracks legal barriers or collective-action fear.',
    'If trap logic dominates, suppression is an equilibrium property that survives removal of any given control, and enforcement-decay readings understate the binding force; if coercion dominates, the 1971-73 dismantling of controls genuinely released the seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_trap_vs_coercion, conceptual, 'Structural versus enforced character of the arrangement''s hold on its participants.').

omega_variable(
    transition_welfare,
    'Did the collapse-and-float transition leave the parties whose gains define the beneficiary seat better off in aggregate than continued repair of convertibility would have?',
    'Welfare comparison of floating-era outcomes (trade growth under floating, hedging costs, crisis frequency) against counterfactual repaired-parity scenarios developed in the economic-history literature.',
    'If floating dominated, the beneficiary seat''s gains are genuine compensation and the collapse reads as a costly-but-correct transition; if floating underperformed the counterfactual, the gains are relocated rents and the beneficiary declaration marks winners of a destruction rather than heirs of a success.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_welfare, preference, 'Whether the successor regime''s position represents improvement or merely relocated advantage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1958, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement_basis(doll_tr_t1958, observed).
narrative_ontology:measurement(doll_tr_t1961, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1961, 0.14).
narrative_ontology:measurement_basis(doll_tr_t1961, observed).
narrative_ontology:measurement(doll_tr_t1964, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1964, 0.2).
narrative_ontology:measurement_basis(doll_tr_t1964, observed).
narrative_ontology:measurement(doll_tr_t1967, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement_basis(doll_tr_t1967, observed).
narrative_ontology:measurement(doll_tr_t1969, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1969, 0.35).
narrative_ontology:measurement_basis(doll_tr_t1969, observed).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.42).
narrative_ontology:measurement_basis(doll_tr_t1971, observed).
narrative_ontology:measurement(doll_tr_t1973, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1973, 0.3).
narrative_ontology:measurement_basis(doll_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1958, 0.52).
narrative_ontology:measurement_basis(doll_be_t1958, observed).
narrative_ontology:measurement(doll_be_t1961, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1961, 0.58).
narrative_ontology:measurement_basis(doll_be_t1961, observed).
narrative_ontology:measurement(doll_be_t1964, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1964, 0.65).
narrative_ontology:measurement_basis(doll_be_t1964, observed).
narrative_ontology:measurement(doll_be_t1967, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1967, 0.72).
narrative_ontology:measurement_basis(doll_be_t1967, observed).
narrative_ontology:measurement(doll_be_t1969, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1969, 0.79).
narrative_ontology:measurement_basis(doll_be_t1969, observed).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.85).
narrative_ontology:measurement_basis(doll_be_t1971, observed).
narrative_ontology:measurement(doll_be_t1973, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1973, 0.78).
narrative_ontology:measurement_basis(doll_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1958, 0.35).
narrative_ontology:measurement_basis(doll_su_t1958, observed).
narrative_ontology:measurement(doll_su_t1961, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1961, 0.48).
narrative_ontology:measurement_basis(doll_su_t1961, observed).
narrative_ontology:measurement(doll_su_t1964, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1964, 0.58).
narrative_ontology:measurement_basis(doll_su_t1964, observed).
narrative_ontology:measurement(doll_su_t1967, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement_basis(doll_su_t1967, observed).
narrative_ontology:measurement(doll_su_t1969, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1969, 0.76).
narrative_ontology:measurement_basis(doll_su_t1969, observed).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.9).
narrative_ontology:measurement_basis(doll_su_t1971, observed).
narrative_ontology:measurement(doll_su_t1973, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1973, 0.55).
narrative_ontology:measurement_basis(doll_su_t1973, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, policy_flexible_reading).

% DUAL FORMULATION NOTE:
% 'Dollar-gold convertibility' colloquially conflates three structurally distinct claims with different epsilon values: a binding legal obligation (strict_convertibility_reading), a defeasible policy commitment (policy_flexible_reading), and an unsustainable design (this file). Each is authored as its own story with its own beneficiaries, victims, and metrics; the family is linked through affects_constraints so contamination analysis can trace how the strict reading's erosion fed this reading's ascent. Upstream/downstream: the strict reading was the regime's self-description; this reading is the diagnosis that consumed it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
