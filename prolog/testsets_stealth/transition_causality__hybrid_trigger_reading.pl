% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__hybrid_trigger_reading, []).

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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Dollar-Gold Convertibility Regime (Hybrid Trigger Reading)
 *   domain: economic/political/monetary
 *
 * SUMMARY:
 *   The Bretton Woods dollar-gold regime required the United States to
 *   convert official dollar holdings into gold at thirty-five dollars per
 *   ounce while supplying the world's reserve currency. Supplying liquidity
 *   demanded US deficits; deficits eroded the metallic backing behind every
 *   outstanding claim. This is the Triffin arithmetic: a slow-burning
 *   structural tension that widened for thirteen years without ending the
 *   regime, until the Vietnam War fiscal expansion blew the deficit open and
 *   the French conversion campaign turned the arithmetic into a visible
 *   drain. The London Gold Pool collapsed in March 1968, a two-tier gold
 *   market admitted the official price no longer governed anything, and in
 *   August 1971 the United States closed the window. This story instantiates
 *   the hybrid trigger reading of the transition-causality kernel: the
 *   contradictions were real and cumulative, but their actualization required
 *   contingent shocks, and the collapse date was therefore not fixed. KEY
 *   AGENTS (by structural relationship): - us_treasury: Agenda-setting seat
 *   (institutional/arbitrage) — administers the gold window, collects the
 *   seigniorage margin, can rewrite the rules unilaterally -
 *   federal_reserve_system: Primary beneficiary (institutional/arbitrage) —
 *   exports its monetary policy to the pegged world, bears little of the cost
 *   - european_japanese_central_banks: Primary target
 *   (institutional/constrained) — accumulates depreciating claims, imports
 *   inflation, every exit carries a bill - imf_program_deficit_countries:
 *   Secondary target (moderate/trapped) — faces deflationary discipline the
 *   issuer escapes - banque_de_france_gaullist_campaign: Organized challenger
 *   inside the payer seat (powerful/constrained) — converts reserves to gold,
 *   forces the arithmetic into the open - private_forex_speculators:
 *   Dual-positioned flow actor (moderate/mobile) — profits from rigidity,
 *   accelerates crises - academic_monetary_economists: Analytical observer
 *   (analytical/analytical) — diagnosed the dilemma in advance, framed the
 *   reform space
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.68).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.55).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Dollar-Gold Convertibility Regime (Hybrid Trigger Reading)").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "economic/political/monetary").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '395c44ec-063d-4c6f-9cdb-470e159a3c1a').
narrative_ontology:cs_kernel_codification('395c44ec-063d-4c6f-9cdb-470e159a3c1a', formalized).
narrative_ontology:cs_authority_grounding('395c44ec-063d-4c6f-9cdb-470e159a3c1a', lineage).
narrative_ontology:cs_interpretation_layer_present('395c44ec-063d-4c6f-9cdb-470e159a3c1a').
narrative_ontology:cs_reading_relation('395c44ec-063d-4c6f-9cdb-470e159a3c1a', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('395c44ec-063d-4c6f-9cdb-470e159a3c1a', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('395c44ec-063d-4c6f-9cdb-470e159a3c1a', foundational, structural_accumulation_necessary_not_sufficient).
narrative_ontology:cs_axiom_status(structural_accumulation_necessary_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('395c44ec-063d-4c6f-9cdb-470e159a3c1a', structural_accumulation_necessary_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('395c44ec-063d-4c6f-9cdb-470e159a3c1a', foundational, trigger_events_required_for_transition_timing).
narrative_ontology:cs_axiom_status(trigger_events_required_for_transition_timing, holdable).
narrative_ontology:cs_axiom_grounding('395c44ec-063d-4c6f-9cdb-470e159a3c1a', trigger_events_required_for_transition_timing, empirically_contingent).
narrative_ontology:cs_reference_frame('395c44ec-063d-4c6f-9cdb-470e159a3c1a', par_value_convertibility_design).
narrative_ontology:cs_drift_state('395c44ec-063d-4c6f-9cdb-470e159a3c1a', late_1960s_two_tier_gold_market, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('395c44ec-063d-4c6f-9cdb-470e159a3c1a', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_treasury).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, federal_reserve_system).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, european_japanese_central_banks).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, imf_program_deficit_countries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, private_forex_speculators).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, private_forex_speculators).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, banque_de_france_gaullist_campaign).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, triffin_dilemma_thesis).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, rueff_deficit_without_tears_critique).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the gold window at thirty-five dollars per ounce, issues the debt that foreign authorities hold as reserves, and decides when to defend convertibility and when to stop. It collects the margin between issuing paper claims and redeeming metal, which finances spending abroad would otherwise have to tax domestically. Its exit is unilateral: in August 1971 it simply closed the window.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_treasury, agenda_setter,
    institutional, biographical, arbitrage, global).

% Sets United States monetary policy, which every pegged economy imports when it defends its parity. It supplies the dollars that become other countries' reserves, operates swap lines and gold-pool interventions alongside partner central banks, and bears little of the inflation its policy exports. Changing course costs it nothing comparable to what accommodation costs its partners.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, federal_reserve_system, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, federal_reserve_system, agenda_setter).

% Buy dollars to hold their announced parities, accumulating claims whose metallic backing shrinks every year relative to the outstanding total. Sterilizing dollar inflows imports American inflation into their economies. Demanding gold at scale, as France did, drains the system they depend on for export markets; revaluing instead, as Germany did, accepts a competitiveness loss. Every exit route carries a bill.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, european_japanese_central_banks, payer,
    institutional, generational, constrained, continental).

% When their currencies come under attack they borrow from the Fund against conditions of deflation and austerity, while the reserve issuer faces no equivalent discipline for its deficits. Their policy space is settled in Washington meetings they attend but do not chair, and leaving the arrangement means losing the credit line that keeps them solvent.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, imf_program_deficit_countries, payer,
    moderate, immediate, trapped, global).

% Exploit the one-way bet of parities that adjust rarely and almost never downward for strong currencies through the 1960s, then front-run expected devaluations and gold conversions once confidence cracks. Their flows are fast and reversible: they profit from the regime's rigidity in calm years and accelerate its crises in tense ones, moving before officials can respond.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, private_forex_speculators, beneficiary,
    moderate, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, private_forex_speculators, payer).

% Converts dollar reserves into gold at scale from 1965 onward and argues publicly that the system lets America run deficits without tears. It withdraws support from the London Gold Pool and forces the arithmetic of liabilities-versus-metal into the open. It remains inside the arrangement it is attacking, because full withdrawal would break the trade order its own exporters sell into.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, banque_de_france_gaullist_campaign, payer,
    powerful, biographical, constrained, national).

% Diagnose the dilemma in advance, as Triffin did in his 1960 congressional testimony, design reforms ranging from SDRs to crawling pegs to floating rates, and publish the critiques that officials quote and resist. They hold no operational power, but their framing determines what governments treat as thinkable.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, academic_monetary_economists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__hybrid_trigger_reading, us_treasury).
narrative_ontology:fixing_cost_class(transition_causality__hybrid_trigger_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gave the postwar world a stable nominal anchor and elastic international liquidity: fixed parities around a dollar convertible into gold for official holders, letting war-damaged economies rebuild and world trade expand without competitive devaluations or deflationary reserve hoarding.
% TRANSFER_FUNCTION: Moves purchasing power and inflation from every holder of dollar reserves to the United States fiscal authority, which finances deficits with paper the rest of the system must absorb; in the regime's early years it also moved American capital and aid outward toward reconstruction.
% ABSENT_VOICES: Households in the surplus economies absorbing imported inflation had no seat at the table; developing countries facing Fund conditionality lacked agenda influence in the G10 inner circle; gold-producing countries were affected by pool operations but absent from design decisions.
% DISAPPEARANCE_RATIONALE: Its overnight disappearance is what actually happened in 1971-1973, and the world rearranged completely: generalized floating replaced parities, the petrodollar recycling system emerged, and the modern fiat-dollar order descends directly from the suspension.
% FOUNDING_PROBLEM: Interwar monetary chaos: competitive devaluations, beggar-thy-neighbor trade policy, and the gold standard's deflationary discipline had collapsed world trade in the 1930s; the 1944 design sought liquidity without gold scarcity and exchange-rate stability without rigid bullion discipline.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the benefiting parties: Triffin's 1960 congressional testimony warned of the liabilities-gold arithmetic years before the crisis; Rueff's and de Gaulle's public critiques attacked the arrangement from inside the system; IMF reserve-adequacy reports and Friedman's academic case for floating rates documented the shifted debate. The United States Treasury attests the founding problem stayed live; the challengers attest it was solved by the mid-1960s and what persisted was the issuer's advantage.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__hybrid_trigger_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__hybrid_trigger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.30 to 0.68 across the interval because the transfer mechanism scaled with the liabilities-gold gap: in 1958 the United States still ran external surpluses and the margin was thin; by 1971 foreign dollar claims exceeded ten times the remaining gold stock, and the difference was a claim on future American output that holders could not collect. The 1964-1966 inflection marks Vietnam escalation; the 1968 step marks the Gold Pool collapse. Suppression (0.55) is the enforcement machinery: the Interest Equalization Tax, voluntary credit restraint programs, convertibility restrictions, and sustained diplomatic pressure on allies to hold dollars rather than demand metal. The suppression_requirement series is authored because this story specifically traces enforcement build-up — the machinery matured and hardened as confidence fell, which is exactly the ratchet trajectory the scalar alone cannot show. Theater (0.46) climbs sharply after 1968: once the two-tier market existed, maintaining the official price was openly ceremonial, yet the fiction was kept until the end. Accessibility_collapse (0.45) reflects partially available alternatives — SDRs were created in 1969, Germany revalued twice, floating was a live proposal — but each was blocked by collective-action problems or an American veto. Resistance (0.58) is unusually high for a monetary regime: a sovereign member mounted an open conversion campaign, the strongest member economy revalued unilaterally, and the academic critique predates the crisis by a decade. The claimed type is tangled_rope on structural grounds independent of these scores: the regime solved a real collective-action problem for two decades (liquidity without gold scarcity, stability without deflation) while the same structure transferred seigniorage and inflation asymmetrically to the issuer, and it required active enforcement throughout. Receipt surface: the gains demonstrably accrued to the US Treasury, which spent the seigniorage; fixing was cheap in the relevant sense — the agenda setter terminated the arrangement with a single televised announcement in August 1971, so the barrier to removal was preference, not cost.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently because the same rules bind them unequally. From the Treasury and Federal Reserve seats the regime is cooperative management they happen to host: they see swap lines honored, consultations held, and a system they can leave at will. From the European and Japanese central bank seats the identical structure is an asymmetric burden with costly exits — convert, and you break your own export markets; accommodate, and you import another country's inflation. From the deficit-country seats it is a discipline asymmetry made procedural. France and Germany occupy the same institutional tier yet diverge completely — conversion campaign versus accommodative revaluation — because doctrine (the Rueff critique) and export interests weighted the same facts oppositely. The engine computes this divergence from the structural data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place the two issuer seats near the beneficiary end: they collect the margin and hold arbitrage-grade exit (they wrote the rules and can rewrite them). The victim declarations place the central banks and program countries near the target end: constrained and trapped respectively, they bear the transfer with no clean way out. Speculators sit mid-range with mobile exit, profiting in calm years and paying in crashes. The analytical observer takes no directional position. No directionality overrides are needed: the derivation from beneficiary/victim declarations plus exit options reproduces the actual structural relationships, because the regime's asymmetry maps cleanly onto who issued and who accumulated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar chaos and reconstruction-era liquidity scarcity — was substantially solved by the mid-1960s: reconstruction was complete, trade had boomed for a decade, and the devaluation wars the designers feared had not returned. What persisted was the issuer's advantage and the absence of a successor architecture. The classification prevents mislabeling in both directions: calling the regime pure coordination erases the accumulating transfer that France named and the arithmetic confirmed; calling it pure extraction erases the genuine liquidity service that made the system indispensable for twenty years and that every participant initially defended. The R5 interview records the residue: founding_problem_status is contested (the specific founding conditions died; the underlying liquidity problem migrated into the successor system), while disappearance_verdict is world_rearranges — the arrangement's end reorganized the entire monetary order, which is the signature of a structure the world had organized itself around, whatever its mandate's condition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (hybrid_trigger_reading) of the kernel transition_causality; what structurally changes under the sibling readings contingent_choice_reading and overdetermined_collapse_reading?',
    'Comparative classification across the three sibling stories: contingent_choice_reading attributes the arrangement''s persistence and end to decisions rather than compulsion (lower structural binding, higher attribution of the transfer to chosen policy); overdetermined_collapse_reading removes trigger contingency (collapse date robust to any particular shock). The disagreement is located in the counterfactual weight assigned to the 1965-1971 decision points versus the accumulated Triffin arithmetic.',
    'If contingent_choice dominates, the arrangement reads as a maintained instrument whose burdens were chosen, not compelled; if overdetermined dominates, the trigger events were replaceable and the end-date was structural. This reading''s medium counterfactual viability sits between the two.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame routing: one of three readings of the transition-causality kernel; siblings instantiate different constraints.').

omega_variable(
    counterfactual_no_vietnam_path,
    'Absent the Vietnam War fiscal shock, would dollar-gold convertibility have survived beyond 1971 under gradual SDR substitution and slower reserve accumulation?',
    'Archival reconstruction of Treasury and Federal Reserve deliberations 1964-1966 combined with counterfactual reserve-adequacy modeling along a non-escalation fiscal path.',
    'A surviving convertibility path confirms trigger necessity and strengthens this reading; rapid crystallization of alternative triggers (sterling crises, French politics, speculative waves) in the counterfactual shifts weight toward the overdetermined sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_no_vietnam_path, empirical, 'Whether the Vietnam fiscal shock was a necessary trigger or one instance of an inevitable class.').

omega_variable(
    surplus_economy_net_position,
    'Did the European and Japanese economies bear net costs under the regime, or did undervalued pegs subsidize their export-led growth enough to offset reserve losses and imported inflation?',
    'General-equilibrium estimation comparing the undervaluation subsidy to exporters against the inflation tax borne by reserve accumulators and the endgame devaluation losses on dollar holdings.',
    'If surplus economies were net beneficiaries until the endgame, the victim set narrows to final-stage losers and the transfer concentrates differently, changing the computed asymmetry for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surplus_economy_net_position, empirical, 'Net incidence of the regime on the surplus economies that nominally opposed it.').

omega_variable(
    intended_transitoriness,
    'Was the arrangement designed as permanent, or as transitional pending a supranational reserve asset in the lineage of Keynes''s clearing union?',
    'Drafting history of the Articles of Agreement and systematic comparison of the White and Keynes plans against the implemented regime.',
    'If transitional intent is established, the regime reads as a support structure whose sunset mechanism was never written, recasting its persistence as unfinished transition rather than entrenchment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intended_transitoriness, conceptual, 'Design intent: permanent architecture or unwritten-sunset transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1958, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1958, transition_causality__hybrid_trigger_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement_basis(tran_tr_t1958, observed).
narrative_ontology:measurement(tran_tr_t1961, transition_causality__hybrid_trigger_reading, theater_ratio, 1961, 0.15).
narrative_ontology:measurement_basis(tran_tr_t1961, observed).
narrative_ontology:measurement(tran_tr_t1964, transition_causality__hybrid_trigger_reading, theater_ratio, 1964, 0.2).
narrative_ontology:measurement_basis(tran_tr_t1964, observed).
narrative_ontology:measurement(tran_tr_t1966, transition_causality__hybrid_trigger_reading, theater_ratio, 1966, 0.26).
narrative_ontology:measurement_basis(tran_tr_t1966, observed).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__hybrid_trigger_reading, theater_ratio, 1968, 0.38).
narrative_ontology:measurement_basis(tran_tr_t1968, observed).
narrative_ontology:measurement(tran_tr_t1970, transition_causality__hybrid_trigger_reading, theater_ratio, 1970, 0.44).
narrative_ontology:measurement_basis(tran_tr_t1970, observed).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.46).
narrative_ontology:measurement_basis(tran_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t1958, transition_causality__hybrid_trigger_reading, base_extractiveness, 1958, 0.3).
narrative_ontology:measurement_basis(tran_be_t1958, observed).
narrative_ontology:measurement(tran_be_t1961, transition_causality__hybrid_trigger_reading, base_extractiveness, 1961, 0.34).
narrative_ontology:measurement_basis(tran_be_t1961, observed).
narrative_ontology:measurement(tran_be_t1964, transition_causality__hybrid_trigger_reading, base_extractiveness, 1964, 0.42).
narrative_ontology:measurement_basis(tran_be_t1964, observed).
narrative_ontology:measurement(tran_be_t1966, transition_causality__hybrid_trigger_reading, base_extractiveness, 1966, 0.5).
narrative_ontology:measurement_basis(tran_be_t1966, observed).
narrative_ontology:measurement(tran_be_t1968, transition_causality__hybrid_trigger_reading, base_extractiveness, 1968, 0.58).
narrative_ontology:measurement_basis(tran_be_t1968, observed).
narrative_ontology:measurement(tran_be_t1970, transition_causality__hybrid_trigger_reading, base_extractiveness, 1970, 0.64).
narrative_ontology:measurement_basis(tran_be_t1970, observed).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement_basis(tran_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1958, transition_causality__hybrid_trigger_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement_basis(tran_su_t1958, observed).
narrative_ontology:measurement(tran_su_t1961, transition_causality__hybrid_trigger_reading, suppression_requirement, 1961, 0.28).
narrative_ontology:measurement_basis(tran_su_t1961, observed).
narrative_ontology:measurement(tran_su_t1964, transition_causality__hybrid_trigger_reading, suppression_requirement, 1964, 0.33).
narrative_ontology:measurement_basis(tran_su_t1964, observed).
narrative_ontology:measurement(tran_su_t1966, transition_causality__hybrid_trigger_reading, suppression_requirement, 1966, 0.38).
narrative_ontology:measurement_basis(tran_su_t1966, observed).
narrative_ontology:measurement(tran_su_t1968, transition_causality__hybrid_trigger_reading, suppression_requirement, 1968, 0.47).
narrative_ontology:measurement_basis(tran_su_t1968, observed).
narrative_ontology:measurement(tran_su_t1970, transition_causality__hybrid_trigger_reading, suppression_requirement, 1970, 0.52).
narrative_ontology:measurement_basis(tran_su_t1970, observed).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.55).
narrative_ontology:measurement_basis(tran_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, resource_allocation).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__overdetermined_collapse_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'why did Bretton Woods end' decomposes into three epsilon-invariant readings of the kernel transition_causality. This story instantiates the hybrid trigger reading (accumulated structural tension plus necessary contingent triggers, medium counterfactual viability); the contingent-choice sibling instantiates the decision-centered reading; the overdetermined sibling instantiates the inevitability reading. All three share the same referent arrangement (the dollar-gold regime) and differ in the counterfactual weighting of the 1965-1971 record; each is authored as a separate story with its own epsilon, beneficiaries, and classification, linked here per the decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
