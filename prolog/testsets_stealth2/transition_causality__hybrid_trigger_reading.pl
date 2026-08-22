% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Bretton Woods Gold-Exchange Standard — Hybrid Trigger Reading
 *   domain: monetary economics/political economy/international finance
 *
 * SUMMARY:
 *   The Bretton Woods gold-exchange standard (operational 1947-1971) fixed
 *   member-currency parities to the dollar and the dollar to gold at
 *   thirty-five dollars per ounce, with the United States pledging
 *   convertibility for official holders. This story instantiates ONE reading
 *   of why that arrangement ended — the hybrid trigger reading: structural
 *   contradictions of the Triffin type (world liquidity supplied through the
 *   reserve issuer's deficits, progressively undermining the gold anchor
 *   those deficits drew down) accumulated steadily across the interval, but
 *   the collapse required contingent trigger events — the Vietnam-era fiscal
 *   expansion that accelerated dollar issuance abroad, and the French-led
 *   official gold conversion campaigns — to actualize. On this reading the
 *   outcome was neither freely avoidable by better policy nor fatally
 *   inevitable from structure alone: medium counterfactual viability attaches
 *   to different trigger timing. The epsilon referent is the standing
 *   arrangement as it actually operated, assessed by this reading's own
 *   lights: a functioning coordination order carrying a growing asymmetric
 *   transfer — not the counterfactual regimes any reading would endorse.
 *   CONSTRAINT FAMILY: the colloquial question 'why did Bretton Woods end?'
 *   decomposes into three structurally distinct causal claims (this hybrid
 *   reading; a contingent-choice reading; an overdetermined-collapse
 *   reading), authored as separate constraint stories linked via
 *   network.affects_constraints; this file authors only the hybrid reading
 *   and does not average across siblings. KEY AGENTS (by structural
 *   relationship): - us_federal_authorities: Primary beneficiary and agenda
 *   setter (institutional/arbitrage) — issues the reserve asset, pledges
 *   convertibility, collects seigniorage, enforces via the gold pool and
 *   alliance pressure - export_led_surplus_economies: Secondary beneficiary
 *   with payer exposure (organized/constrained) — undervalued parities
 *   subsidize exports; forced dollar accumulation carries erosion risk -
 *   international_trading_finance_sector: Incidental beneficiary
 *   (powerful/mobile) — profits from stable parities and expanding dollar
 *   liquidity; routes around controls -
 *   deficit_countries_under_conditionality: Primary target among states
 *   (moderate/trapped) — bears deflationary adjustment and credit conditions
 *   - adjustment_country_populations: Deepest target (powerless/trapped) —
 *   absorbs unemployment and wage restraint under programs they did not set -
 *   dissident_gold_creditor_nations: Target exercising partial sanctioned
 *   exit (organized/constrained) — converts reserves to gold, publicly
 *   indicting the privilege - imf_secretariat: Administrative enforcer with
 *   asymmetric reach (institutional/analytical) -
 *   independent_monetary_analysts: Analytical observers
 *   (analytical/analytical) — Triffin, Rueff, and successors supply the
 *   diagnostic vocabulary
 *
 * KEY AGENTS:
 *   - us_federal_authorities: primary beneficiary and agenda setter (institutional/arbitrage) — reserve issuer with unilateral settlement option
 *   - export_led_surplus_economies: secondary beneficiary with payer exposure (organized/constrained) — subsidized growth against eroding claims
 *   - international_trading_finance_sector: incidental beneficiary (powerful/mobile) — profits from stability, arbitrages the controls
 *   - deficit_countries_under_conditionality: primary state-level target (moderate/trapped) — bears assigned adjustment
 *   - adjustment_country_populations: deepest target (powerless/trapped) — bears the domestic incidence of adjustment
 *   - dissident_gold_creditor_nations: target with partial sanctioned exit (organized/constrained) — conversion right exercised as indictment
 *   - imf_secretariat: administrative enforcer, asymmetric jurisdiction (institutional/analytical)
 *   - independent_monetary_analysts: analytical observers outside the operating committees
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.68).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.68).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Gold-Exchange Standard — Hybrid Trigger Reading").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary economics/political economy/international finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, 'd2cbfbea-a3d8-4c03-b614-2b4816c87b3d').
narrative_ontology:cs_kernel_codification('d2cbfbea-a3d8-4c03-b614-2b4816c87b3d', distributed).
narrative_ontology:cs_authority_grounding('d2cbfbea-a3d8-4c03-b614-2b4816c87b3d', distributed).
narrative_ontology:cs_reading_relation('d2cbfbea-a3d8-4c03-b614-2b4816c87b3d', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2cbfbea-a3d8-4c03-b614-2b4816c87b3d', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_axiom('d2cbfbea-a3d8-4c03-b614-2b4816c87b3d', foundational, structural_accumulation_necessary_not_sufficient).
narrative_ontology:cs_axiom_status(structural_accumulation_necessary_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('d2cbfbea-a3d8-4c03-b614-2b4816c87b3d', structural_accumulation_necessary_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('d2cbfbea-a3d8-4c03-b614-2b4816c87b3d', foundational, contingent_triggers_causally_required).
narrative_ontology:cs_axiom_status(contingent_triggers_causally_required, holdable).
narrative_ontology:cs_axiom_grounding('d2cbfbea-a3d8-4c03-b614-2b4816c87b3d', contingent_triggers_causally_required, empirically_contingent).
narrative_ontology:cs_reference_frame('d2cbfbea-a3d8-4c03-b614-2b4816c87b3d', straining_but_viable_par_system).
narrative_ontology:cs_drift_state('d2cbfbea-a3d8-4c03-b614-2b4816c87b3d', post_collapse_retrospective, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d2cbfbea-a3d8-4c03-b614-2b4816c87b3d', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_federal_authorities).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, export_led_surplus_economies).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, international_trading_finance_sector).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, deficit_countries_under_conditionality).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, adjustment_country_populations).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, dissident_gold_creditor_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, export_led_surplus_economies).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, triffin_dilemma_diagnosis).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, rueff_exorbitant_privilege_critique).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the dollar and pledges to convert official dollar holdings into gold at thirty-five dollars per ounce. Finances overseas military commitments and domestic programs partly through dollar liabilities that foreign governments and central banks must hold as reserves. Operates the London Gold Pool with partner central banks to hold the market price near the official one, extends swap lines, and presses allies — through offset agreements, troop-level hints, and moral suasion — to accept dollar claims rather than request gold. Settles its own obligations in its own liability, an option no other member has.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_federal_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% West Germany, Japan, and later peers hold parities set well below post-reconstruction market-clearing levels, which subsidizes export-led growth through the 1950s and 1960s. The counterpart of export success is accumulating dollar reserves whose gold backing shrinks year by year. Washington presses them to hold dollars rather than convert — offset agreements with Bonn, quiet quotas elsewhere — and periodic revaluation demands arrive alongside threats of trade or troop measures. Revaluing unilaterally would tax their own exporters; converting at scale would break the arrangement they grow under.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, export_led_surplus_economies, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, export_led_surplus_economies, payer).

% Multinational firms and international banks earn from predictable parities, expanding trade volumes, and deepening dollar credit markets. When national controls bind, they route around them — the Eurodollar market in London grows precisely in the gaps between national regulations — keeping their options open whichever way official rules move.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, international_trading_finance_sector, beneficiary,
    powerful, biographical, mobile, global).

% The United Kingdom and smaller deficit economies must defend their parities with deflation, wage freezes, and standby credits carrying fund conditions. Devaluing invites speculative attack and signals weakness; borrowing brings policy strings. The rules assign them the adjustment work while the issuer of the reserve asset adjusts not at all.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, deficit_countries_under_conditionality, payer,
    moderate, biographical, trapped, global).

% Workers and households in the adjusting economies absorb the unemployment, frozen wages, and cut public spending that parity defense requires. They sat at no table where the parity was chosen, and their practical exits — emigration, informal work — are narrow and personal.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, adjustment_country_populations, payer,
    powerless, immediate, trapped, national).

% France, episodically joined by others, converts dollar reserves into gold and says publicly what conversion implies: the reserve issuer enjoys a privilege no other country has, and paper claims against a shrinking gold stock are a poor store of value. Conversion is formally every holder's right, yet exercising it draws diplomatic penalty and informal rationing, so the exit is real but costly and politically loaded.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, dissident_gold_creditor_nations, payer,
    organized, biographical, constrained, continental).

% Administers the par-value system: approves par changes, extends conditional credit to deficit members, surveys exchange practices, and reports on members' conduct. Its leverage runs one way — decisive over deficit countries needing credit, nearly nil over the reserve issuer, whose cooperation it can only request.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, imf_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% Academic and official-sector economists outside the operating committees — Triffin diagnosing the liquidity dilemma in 1960, Rueff indicting the deficit-without-payment privilege, later historians reconstructing the record — publish analyses that participants read and dispute. They bear no costs and collect no rents from the arrangement; their seat is observational.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, independent_monetary_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__hybrid_trigger_reading, us_federal_authorities).
narrative_ontology:fixing_cost_class(transition_causality__hybrid_trigger_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the interwar coordination failure: competitive devaluations, currency blocs, and liquidity scarcity that choked multilateral trade. Fixed but adjustable parities gave traders predictable prices; the dollar's reserve role supplied elastic liquidity that gold alone could not.
% TRANSFER_FUNCTION: Moves real goods, services, and assets to the United States in exchange for dollar claims — seigniorage financing US military and domestic spending — and moves adjustment burdens onto deficit countries through credit conditions and onto surplus countries through forced reserve accumulation; moves liquidity outward from the US via payment deficits.
% ABSENT_VOICES: Deficit-country labor forces bore adjustment without representation at the design or operating tables; surplus-country publics absorbing imported inflation were unrepresented; most colonial and newly independent economies had marginal voice at the 1944 conference and none in G10 gold-pool decisions. Present, they would object to the adjustment asymmetry and to financing another power's war.
% DISAPPEARANCE_RATIONALE: After August 1971 the world rearranged: par values dissolved into generalized floating by 1973, the gold anchor and its discipline vanished, reserve composition diversified, inflation accelerated through the 1970s, and subsequent regional arrangements — the Snake, the EMS, the euro — attempted to rebuild pieces of what was lost. Every successor regime is an arrangement responding to this one's absence.
% FOUNDING_PROBLEM: Interwar monetary disorder: competitive devaluation, beggar-thy-neighbor trade policy, gold-standard deflationary discipline without liquidity elasticity, and destabilizing capital flight. The founders sought stable exchange rates combined with adjustment flexibility and adequate international liquidity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: economic-historical scholarship (Eichengreen, Bordo, James) documents the interwar problems the founders cited; independent fund evaluations and BIS historical papers trace the same problems recurring under successor regimes (the 1992-93 EMS crises, eurozone adjustment asymmetry after 2010); central-bank archives preserve contemporaneous non-US policymakers attesting the liquidity and instability problems. US authorities' own accounts count as corroboration only where they overlap with these external sources.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because the arrangement demonstrably solved a real coordination problem — interwar-style disorder stayed solved for a generation — while operating a standing asymmetric transfer (seigniorage and external deficit finance to the issuing government; adjustment burdens assigned away from the issuer) held together by active enforcement (gold pool, swap lines, conditionality, alliance pressure). The metrics describe how it actually ran. Extractiveness ends at 0.68: by 1971 foreign official dollar claims stood far above the gold stock backing them, a gap that widened almost every year after convertibility was restored (1958-61); the scalar reflects the end-of-interval state, with the series showing accumulation from 0.28 in 1947, when scarce dollars and Marshall Plan flows ran in the opposite direction. Suppression (0.68) is structural throughout — treaty machinery, capital controls, gold-pool intervention, credit conditions, alliance pressure — with no meaningful internalized component, since this is an interstate arrangement whose compliance burdens are carried by institutions, not fused identities. The suppression_requirement series is authored because enforcement capacity is exactly what this story traces: it decays from 0.48 to 0.36 as European convertibility returns and controls ease (1947-1959), then ratchets to 0.68 as the widening imbalance demands harder instruments (Interest Equalization Tax 1963, voluntary credit restraint, gold-pool intensification, the August 1971 package). Theater_ratio rises from 0.12 to 0.55 as maintenance turns performative: official assurances that the dollar is as good as gold continue while the two-tier gold market of March 1968 institutionalizes the pretense that the official price is a price. Accessibility_collapse is 0.5: alternatives were visible (revaluation, crawling adjustment, SDRs from 1969, floating) but politically blocked and untested, so understanding the arrangement handed no one a usable exit. Resistance 0.6 reflects the open French challenge, successive sterling and dollar runs, and speculative flows that grew with the overhang. All three series share one time grid (1947, 1953, 1959, 1965, 1968, 1971); the base_properties scalars equal the 1971 endpoints. Receipt: the gains demonstrably accrue to us_federal_authorities — seigniorage and the capacity to finance the war externally — so gain_flow names that seat rather than diffuse. Fixing was prohibitive for every seat that could attempt it: for the issuer, closing the window meant surrendering the privilege and admitting the pledge's insolvency mid-war; collectively, renegotiating parities required consent across G10 members with opposed interests, and the one founding design that would have symmetrized adjustment (Keynes's clearing union) had been rejected at the start. The deficit countries never formed the coalition that could have forced symmetric adjustment — their numbers favored it, their reserve dependence prevented it.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical nominal membership. The issuer's seat experiences the arrangement as infrastructure it built and manages — coordination first, privilege incidental. The deficit-conditionality seat meets the same rules as externally administered austerity. Surplus exporters experience subsidized growth with a slowly souring asset on the balance sheet. Among same-level G10 creditors, France and Germany held comparable power and identical formal rights yet diverged completely: Paris converted and indicted, Bonn signed offset agreements and held — differentiated by political stance toward the issuer, not by power or information. The IMF's administrative seat sees enforceable rules that stop at the issuer's border. No identity-fusion binds any seat here; exit differences are material (reserve dependence, market access, military alignment), which is why the engine should find wide per-seat divergence computed from one structural dataset.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. us_federal_authorities: agenda-setting beneficiary with arbitrage-grade exit (settles its obligations in its own liability) — nearest the beneficiary pole. international_trading_finance_sector: beneficiary with mobile exit — likewise near the beneficiary pole, damped further by mobility. export_led_surplus_economies: declared beneficiary with a secondary payer exposure — derived d sits low but not at zero, since forced reserve accumulation imports the issuer's inflation. deficit_countries_under_conditionality and adjustment_country_populations: trapped payers — near the full-target pole, the populations furthest of all. dissident_gold_creditor_nations: payers whose sanctioned exit (conversion) is real but rationed — high d, with the structural peculiarity that exercising their exit is what destroys the arrangement. Global spatial scope raises verification difficulty and modestly amplifies effective extraction on the target side; suppression enters unscaled, as a raw structural property. No directionality overrides are needed: the beneficiary/victim declarations plus exit atoms already place every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   This arrangement ended by rupture, not atrophy, so the degraded-inertia question barely arises — though the late theater_ratio (0.55) records how close performative maintenance came to being the whole remainder by 1968-71. The classification guards against two mislabels. Reading the collapse as a transitional support's planned sunset fails: no sunset clause existed, the founders designed a permanent order, and the founding problems were live when it broke. Reading it as natural law fails equally: enforcement machinery, named beneficiaries, and discretionary triggers all appear in the structural data. The hybrid reading's own contribution is the distinction between a mandate outliving its function (absent here — the functions were performed until the end) and a mechanism destroying itself through its operating rule (present — the liquidity-provision rule mechanically undermined the anchor). mandatrophy_resolved stays false: the mandate did not outlive the function; the function killed the mechanism. Coherence check for the genealogy consumer: founding_problem_status is live and disappearance_verdict is world_rearranges, so no dead-mandate mismatch flag arises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the hybrid_trigger_reading of the transition_causality kernel; how would instantiating the sibling readings change the structural data?',
    'Generate the sibling stories (transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading) and compare epsilon, beneficiary/victim weighting, and counterfactual-viability parameters across the family.',
    'The contingent_choice_reading would shift causal weight from accumulated structure to decision points, raising attributed agency and lowering structural necessity; the overdetermined_collapse_reading would raise structural necessity to sufficiency and drive counterfactual viability toward zero, moving the family''s shared referent toward impersonal-law framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of the transition_causality kernel; disagreement located in necessity versus sufficiency of contingent triggers.').

omega_variable(
    counterfactual_without_triggers,
    'Absent the Vietnam-era fiscal expansion and the French-led official gold conversion campaigns, would the gold-exchange standard have collapsed within the interval, collapsed later under a different trigger, or held under disciplined adjustment?',
    'Archival counterfactual reconstruction of US balance-of-payments trajectories excluding war-related outflows; comparison with contemporaneous internal Treasury and Federal Reserve scenario memoranda.',
    'Collapse without the identified triggers supports sliding toward the overdetermined sibling; indefinite sustainability under discipline supports sliding toward the contingent_choice sibling; delayed collapse under a different trigger confirms this reading''s medium counterfactual viability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_without_triggers, empirical, 'Whether the named triggers were necessary for collapse, or merely determined its timing.').

omega_variable(
    france_endogenous_or_independent,
    'Were the French gold runs an independent contingent trigger, or an endogenous symptom of structural deterioration that any rational reserve holder would eventually have executed?',
    'Decision-record analysis of Gaullist monetary policy formation (Rueff''s influence, NATO and sterling-related political motives) contrasted with the revealed behavior of other holders who faced the same reserve risk but did not convert at scale.',
    'If the French challenge was endogenous to the overhang, the trigger layer dissolves into structure and this reading converges on the overdetermined sibling; if it was politically independent, the hybrid structure stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(france_endogenous_or_independent, empirical, 'Independence of the principal trigger event from the structural trend it actualized.').

omega_variable(
    seigniorage_transfer_magnitude,
    'How large was the real resource transfer to the United States from foreign reserve accumulation, net of the coordination value and services delivered to the holders?',
    'Balance-of-payments and reserve-asset composition accounting for 1958-1971; valuation of seigniorage against interest earned and transaction services received by official holders.',
    'A large net transfer raises effective extraction on the holder seats and pushes computed per-seat classifications toward capture-flavored readings; a negligible net transfer supports coordination-dominant classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_transfer_magnitude, empirical, 'Net magnitude of the asymmetric transfer riding on the coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1947, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1947, transition_causality__hybrid_trigger_reading, theater_ratio, 1947, 0.12).
narrative_ontology:measurement_basis(tran_tr_t1947, observed).
narrative_ontology:measurement(tran_tr_t1953, transition_causality__hybrid_trigger_reading, theater_ratio, 1953, 0.14).
narrative_ontology:measurement_basis(tran_tr_t1953, observed).
narrative_ontology:measurement(tran_tr_t1959, transition_causality__hybrid_trigger_reading, theater_ratio, 1959, 0.2).
narrative_ontology:measurement_basis(tran_tr_t1959, observed).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__hybrid_trigger_reading, theater_ratio, 1965, 0.32).
narrative_ontology:measurement_basis(tran_tr_t1965, observed).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__hybrid_trigger_reading, theater_ratio, 1968, 0.45).
narrative_ontology:measurement_basis(tran_tr_t1968, observed).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.55).
narrative_ontology:measurement_basis(tran_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t1947, transition_causality__hybrid_trigger_reading, base_extractiveness, 1947, 0.28).
narrative_ontology:measurement_basis(tran_be_t1947, observed).
narrative_ontology:measurement(tran_be_t1953, transition_causality__hybrid_trigger_reading, base_extractiveness, 1953, 0.34).
narrative_ontology:measurement_basis(tran_be_t1953, observed).
narrative_ontology:measurement(tran_be_t1959, transition_causality__hybrid_trigger_reading, base_extractiveness, 1959, 0.44).
narrative_ontology:measurement_basis(tran_be_t1959, observed).
narrative_ontology:measurement(tran_be_t1965, transition_causality__hybrid_trigger_reading, base_extractiveness, 1965, 0.56).
narrative_ontology:measurement_basis(tran_be_t1965, observed).
narrative_ontology:measurement(tran_be_t1968, transition_causality__hybrid_trigger_reading, base_extractiveness, 1968, 0.63).
narrative_ontology:measurement_basis(tran_be_t1968, observed).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement_basis(tran_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1947, transition_causality__hybrid_trigger_reading, suppression_requirement, 1947, 0.48).
narrative_ontology:measurement_basis(tran_su_t1947, observed).
narrative_ontology:measurement(tran_su_t1953, transition_causality__hybrid_trigger_reading, suppression_requirement, 1953, 0.38).
narrative_ontology:measurement_basis(tran_su_t1953, observed).
narrative_ontology:measurement(tran_su_t1959, transition_causality__hybrid_trigger_reading, suppression_requirement, 1959, 0.36).
narrative_ontology:measurement_basis(tran_su_t1959, observed).
narrative_ontology:measurement(tran_su_t1965, transition_causality__hybrid_trigger_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement_basis(tran_su_t1965, observed).
narrative_ontology:measurement(tran_su_t1968, transition_causality__hybrid_trigger_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement_basis(tran_su_t1968, observed).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.68).
narrative_ontology:measurement_basis(tran_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, resource_allocation).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__overdetermined_collapse_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'why Bretton Woods ended' conflates three structurally distinct causal claims with different epsilon values and different counterfactual structures. This story (hybrid_trigger_reading) authors the middle position: accumulated contradiction (Triffin dynamics) as necessary, contingent triggers (Vietnam fiscal shock, French-led gold conversion) as also necessary, medium counterfactual viability. The overdetermined_collapse_reading claims structural sufficiency; the contingent_choice_reading attributes the outcome to decision points. Family members link via affects_constraints; each file keeps a single stable epsilon over the shared referent — the operating gold-exchange standard itself, not any reading's endorsed alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
