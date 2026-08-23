% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__punitive_liability_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles Reparations — Punitive Liability Reading (Article 231 Full-Charge Arrangement)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   A defeated Germany is charged with the total costs of the war on the
 *   finding that it bears unique responsibility for causing it: Article 231
 *   supplies the liability premise, the London Schedule of 1921 fixes a
 *   nominal 132 billion gold marks, and a Paris commission of creditor
 *   delegates fixes schedules, values deliveries, and sanctions default. The
 *   arrangement operates through a single collection channel backed by
 *   continuing blockade until ratification, sanctions ultimatums, the 1921
 *   seizure of Rhineland bridgeheads, the 1923 occupation of the Ruhr behind
 *   a customs frontier, and, after 1924, mortgages on German railways,
 *   customs, and the central bank. Wealth moves from German taxpayers,
 *   workers, and savers to Allied treasuries and connected industries — in
 *   currency, coal, timber, rolling stock, and ships — while German fiscal
 *   sovereignty is subordinated to externally appointed control. This file
 *   instantiates one reading of the reparations-clauses kernel; the
 *   reading-level positioning is recorded in kernel_context and the omegas,
 *   not averaged into the structure. Base-property metrics characterize the
 *   arrangement at its mature operation (the 1921-1927 punitive-liability
 *   regime proper); the measurement series tracks the full 1919-1932
 *   lifecycle including terminal decay. KEY AGENTS (by structural
 *   relationship): - allied_creditor_states: Primary beneficiary
 *   (institutional/constrained) — receives allocated shares; bears
 *   enforcement costs - french_reconstruction_interests: Secondary
 *   beneficiary (powerful/arbitrage) — receives in-kind deliveries and
 *   controlled-price purchases - reparations_commission: Agenda-setter
 *   (institutional/constrained) — fixes schedules, valuations, and sanctions
 *   - weimar_government: Dual-positioned payer-administrator
 *   (institutional/constrained) - german_taxpayers,
 *   german_middle_class_savers: Targets of the fiscal transfer
 *   (moderate/trapped) - german_industrial_workers,
 *   ruhr_occupation_civilians: Directly coerced targets (powerless/trapped) -
 *   united_states_government: Peripheral arbiter on the debt circuit
 *   (institutional/arbitrage) — observer seat - german_war_guilt_revisers,
 *   british_keynesian_dissenters: Excluded voices (organized; identity_locked
 *   / mobile)
 *
 * KEY AGENTS:
 *   - - allied_creditor_states: Primary beneficiary (institutional/constrained) — receives allocated shares of every transfer; bears occupation and blockade costs from the same budgets
 *   - - french_reconstruction_interests: Secondary beneficiary (powerful/arbitrage) — takes German coal, timber, and livestock as deliveries-in-kind and buys at controlled prices
 *   - - reparations_commission: Agenda-setter (institutional/constrained) — determines schedules, valuations, and sanctions; exists only as long as creditor governments sustain it
 *   - - weimar_government: Dual-positioned payer and domestic administrator (institutional/constrained) — negotiates, legislates, and delivers while absorbing the political cost
 *   - - german_taxpayers: Target of the recurring fiscal transfer (moderate/trapped)
 *   - - german_industrial_workers: Target of the employment and wage channel (powerless/trapped)
 *   - - ruhr_occupation_civilians: Target under direct military coercion (powerless/trapped)
 *   - - german_middle_class_savers: Target of the inflation channel (moderate/trapped)
 *   - - united_states_government: Peripheral arbiter and debt-circuit claimant (institutional/arbitrage) — observer seat
 *   - - german_war_guilt_revisers: Excluded voice (organized/identity_locked) — contests the guilt finding with no seat in the determining bodies
 *   - - british_keynesian_dissenters: Excluded voice (organized/mobile) — capacity critique outvoted inside the victor coalition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.84).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.78).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Reparations — Punitive Liability Reading (Article 231 Full-Charge Arrangement)").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, '0dd4b9f4-65df-4766-80f3-4b643d17d635').
narrative_ontology:cs_kernel_codification('0dd4b9f4-65df-4766-80f3-4b643d17d635', formalized).
narrative_ontology:cs_authority_grounding('0dd4b9f4-65df-4766-80f3-4b643d17d635', extraction).
narrative_ontology:cs_interpretation_layer_present('0dd4b9f4-65df-4766-80f3-4b643d17d635').
narrative_ontology:cs_reading_relation('0dd4b9f4-65df-4766-80f3-4b643d17d635', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_reading_relation('0dd4b9f4-65df-4766-80f3-4b643d17d635', versailles_reparations_clauses__repudiation_reading, forecloses).
narrative_ontology:cs_axiom('0dd4b9f4-65df-4766-80f3-4b643d17d635', foundational, germany_unique_total_war_responsibility).
narrative_ontology:cs_axiom_status(germany_unique_total_war_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('0dd4b9f4-65df-4766-80f3-4b643d17d635', germany_unique_total_war_responsibility, empirically_contingent).
narrative_ontology:cs_axiom('0dd4b9f4-65df-4766-80f3-4b643d17d635', foundational, liability_unbounded_by_debtor_capacity).
narrative_ontology:cs_axiom_status(liability_unbounded_by_debtor_capacity, holdable).
narrative_ontology:cs_axiom_grounding('0dd4b9f4-65df-4766-80f3-4b643d17d635', liability_unbounded_by_debtor_capacity, deontological).
narrative_ontology:cs_axiom('0dd4b9f4-65df-4766-80f3-4b643d17d635', secondary, article_231_binding_guilt_instrument).
narrative_ontology:cs_axiom_status(article_231_binding_guilt_instrument, holdable).
narrative_ontology:cs_axiom_grounding('0dd4b9f4-65df-4766-80f3-4b643d17d635', article_231_binding_guilt_instrument, conventional).
narrative_ontology:cs_reference_frame('0dd4b9f4-65df-4766-80f3-4b643d17d635', full_german_war_liability_order).
narrative_ontology:cs_drift_state('0dd4b9f4-65df-4766-80f3-4b643d17d635', lausanne_termination_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0dd4b9f4-65df-4766-80f3-4b643d17d635', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, french_reconstruction_interests).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_industrial_workers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, ruhr_occupation_civilians).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_middle_class_savers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, weimar_government).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, war_guilt_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, treaty_obligation_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% An inter-Allied body seated in Paris that determines payment schedules, values deliveries-in-kind, adjudicates creditor claims against German assets, and may sanction default. Delegates of the creditor governments staff it, and its determinations bind German fiscal policy between conferences. It raises no funds of its own; it directs where German revenue and deliveries flow.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, reparations_commission, agenda_setter,
    institutional, generational, constrained, continental).

% The signatory governments entitled to shares of German payments — France and Belgium foremost, Britain and Italy secondary. Receipts fund reconstruction budgets and service, indirectly, the war debts owed to the United States. Occupation armies, blockade patrols, and occupied-area administration draw on the same treasuries the receipts replenish. Their exit is collective — suspension, rescheduling, cancellation — and each step carries years of domestic political cost before it is taken.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, beneficiary,
    institutional, generational, constrained, global).

% French coal, steel, and chemical firms and the devastated-region reconstruction offices that receive German coal, coke, timber, and livestock as deliveries, and later buy from German plants at controlled prices under occupation-era accords. They take inputs below prevailing prices while rival French producers lobby for tariffs against the same deliveries; whichever way the arrangement bends, they are positioned to gain.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, french_reconstruction_interests, beneficiary,
    powerful, biographical, arbitrage, regional).

% The German Reich government negotiates schedules, enacts the taxes and levies that fund deliveries, finances domestic resistance payments by issuing currency, and administers collection internally through its own ministries. Every concession costs it coalition support in the Reichstag; every refusal invites ultimatum or occupation. Its maneuvering room runs between creditor demands at one edge and parliamentary collapse at the other.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, weimar_government, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, weimar_government, agenda_setter).

% Households and firms paying the recurring taxes, levies, and forced contributions that fund deliveries and occupation-related expenditure. Their burden is set in Berlin under totals fixed in Paris and London; they vote for the governments that pass the implementing laws but cannot reach the bodies that fix the amounts.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_taxpayers, payer,
    moderate, biographical, trapped, national).

% Workers in the Ruhr and Rhineland whose employment stops when collieries idle under occupation or passive resistance, whose real wages dissolve when the currency is issued to fund strike pay, and whose employers' deliveries-in-kind substitute for export earnings. Rehiring depends on deliveries resuming on terms set elsewhere.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_industrial_workers, payer,
    powerless, immediate, trapped, regional).

% Residents of the occupied territories living behind a customs frontier, subject to requisitioning, expulsion of resisters, and recurring violence between occupation troops and civilian crowds. Both their own government, which pays them not to cooperate, and the occupiers, who punish non-cooperation, direct their daily conduct from outside the region.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, ruhr_occupation_civilians, payer,
    powerless, immediate, trapped, regional).

% Bondholders, pensioners, and depositors whose wealth is held in fixed mark claims. Currency issuance undertaken to meet delivery deadlines converts lifetime savings into nearly nothing; their claims rank senior to nothing and are protected by nothing in the payment machinery.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_middle_class_savers, payer,
    moderate, biographical, trapped, national).

% A non-signatory government that declines treaty responsibilities while privately conditioning goodwill on repayment of Allied war debts, which the creditors intend to service from German receipts. It chairs the expert panels that redesign the machinery in 1924 and 1929 and grants the 1931 suspension. It collects nothing under the settlement itself; its exposure runs entirely through the inter-governmental debt circuit.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, united_states_government, observer,
    institutional, generational, arbitrage, global).

% Foreign Office officials, academics, and publicists running the campaign against the war-guilt finding — publishing document collections, briefing sympathetic journalists abroad, petitioning for an impartial tribunal. They hold no seat in the bodies that fix schedules or adjudicate claims, and their access runs through governments that treat the guilt question as settled. The campaign to overturn the finding constitutes their careers and institutions.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_war_guilt_revisers, excluded,
    organized, generational, identity_locked, continental).

% Economists, Labour politicians, and liberal editors inside the victorious coalition who argue the totals exceed anything transferable and that the guilt clause poisons the settlement. They publish, stand for office, and advise opposition factions; within governing coalitions they are outvoted, and their memoranda circulate without altering the schedules.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, british_keynesian_dissenters, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__punitive_liability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates the bilateral damage claims of several creditor governments into one schedule, one valuation office, and one allocation key, so that creditors collect through a single channel rather than competing seizures, and Germany faces one counterparty rather than many.
% TRANSFER_FUNCTION: Moves gold, currency, coal, timber, rolling stock, and ships from German taxpayers, workers, and savers to Allied treasuries and connected industries; moves control levers over German fiscal policy — mortgages on railways, customs, and the central bank — from Berlin to creditor-appointed administrators.
% ABSENT_VOICES: German war-guilt revisers hold no seat in the commission or the conferences that fix the totals, though their documentary campaign runs continuously; Keynesian and Labour dissenters inside the victor coalition are outvoted and their capacity critiques never reach the schedules; German populations are never consulted on any capacity determination; and the colonial subjects of the creditor empires bear imperial costs without representation anywhere in the machinery. The unanimity of the determining bodies presupposes these absences.
% DISAPPEARANCE_RATIONALE: Creditor reconstruction budgets, the inter-Allied debt circuit routed through Washington, German fiscal and monetary policy, and central European monetary stability all attach to the payment machinery. When collection halts in 1931-32, French budget planning, Belgian reconstruction accounts, and the Young-loan refinancing chain all break or renegotiate within months — the surrounding arrangements demonstrably depend on the structure's existence and terms.
% FOUNDING_PROBLEM: How to charge a defeated Germany with the total costs of a war fought on Allied soil: satisfy domestic demands that Germany pay for the devastation, fund the rebuilding of occupied France and Belgium, settle the web of inter-Allied war debts, and keep German recovery bounded by French security requirements.
% FOUNDING_PROBLEM_CORROBORATION: Corroborating sources outside the beneficiary set: J.M. Keynes's 1919 assessment that the sums exceeded anything transferable; the 1924 Dawes expert committee's conclusion that payments must ride on a verified budget-surplus and transfer mechanism rather than a fixed total; and the creditor side's own conduct in 1931-32 — the Hoover Moratorium and the Lausanne final act — conceding that the machinery could no longer collect. No body outside the creditor coalition attests that the original unlimited-charge formulation remained viable after 1921, and the German parties dispute the legitimacy of the founding problem itself rather than its solution.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.84 as the characteristic operating value of the mature punitive-liability regime: the charge structure was quasi-unlimited, the nominal total (132 billion gold marks) far exceeded any assessed damage calculus or collectible core, and the 1923 peak reached 0.90 under occupation and hyperinflation. Suppression is authored at 0.78 as a raw structural property, unscaled by power or scope: the arrangement's persistence demonstrably depended on blockade, sanctions threats, military occupation, and financial receivership rather than on participant assent. Theater at 0.33 reflects a real early collection function wrapped in substantial cover performance — the optical headline figure, the guilt liturgy, the failed separatist gambits — with the theater series rising monotonically to 0.71 as the function died before the machinery did (Goodhart drift). Accessibility collapse at 0.64: alternatives narrowed sharply under coercion but were never fully closed, and Germany exercised several of them — default, passive resistance, the Rapallo opening eastward, currency liquidation. Resistance at 0.73 reflects the Ruhr passive resistance, strike funding, political upheaval, and continuous revisionist diplomacy. The claimed_type (snare) is stated independently of the metrics: the genuine coordination layer — consolidating dozens of bilateral claims into one channel — is real but thin and instrumental, subordinate to an extraction whose persistence required coercion and exit suppression; the engine may compute tangled_rope given the declared resource_allocation coordination function, and that divergence is data, not error. All three temporal series run on one shared eight-point grid (1919, 1921, 1923, 1924, 1927, 1929, 1931, 1932) with every metric authored at every point. The suppression_requirement series is authored deliberately because enforcement-capacity change is the traced dynamic of this interval: buildup through 1923, institutionalization under Dawes, decay through Young, moratorium, and extinction. Base-property scalars characterize the mature phase rather than the terminal residue because the story's subject is the punitive arrangement itself, not the husk that survived it.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the commission and creditor seats the arrangement presents as lawful execution of a binding instrument — schedules fixed, valuations adjudicated, sanctions proportionate to default; their derived directionality sits near the subsidized end and effective extraction collapses toward the coordination floor. From the German payer seats the same structure presents as open-ended confiscation under duress: trapped exits amplify their effective extraction toward the full-target pole, and the four victim classes sit there with different channels (fiscal, wage, inflation, physical coercion). The Weimar government straddles: it administers collection domestically while bearing its costs politically, computing mid-to-high. The United States seat sees a debt-arbitration problem rather than a justice problem. The excluded seats see a legitimacy fraud — the guilt finding unexamined by any impartial tribunal — and their exclusion is what the unanimity of the determining bodies presupposes. The engine computes this divergence per seat; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   The two declared beneficiary groups derive directionality near the subsidized pole: allied_creditor_states collect the allocated shares (France foremost, roughly half of all receipts) and french_reconstruction_interests take deliveries-in-kind below market price with arbitrage-grade upside either way the arrangement bends. The four declared victim classes derive directionality near the full-target pole, amplified by trapped exits: taxpayers and savers cannot leave the currency, workers cannot leave the labor market, and the occupied population cannot leave the region. The Weimar government's payer declaration places it target-side despite its administrative role. The United States seat derives as an analytical/peripheral position — it collects nothing under the settlement itself, its exposure running through the inter-governmental debt circuit. No directionality overrides are authored: every seat's position follows from its declared role, power atom, and exit options, and the derivation chain handles the dual-positioned administrator correctly through its payer declaration; a blanket power-atom override would wrongly homogenize the three institutionally-powered seats whose structural positions differ.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding charge — make Germany bear the total costs of the war — was progressively hollowed while its moral frame stayed officially intact: the 1924 mechanism indexed payments to capacity, the 1929 schedule trimmed and lengthened the debt, the 1931 moratorium suspended collection, and the 1932 conference extinguished it, all without any formal repudiation of the guilt premise. Classifying this as a snare prevents two symmetrical misreadings. Against the creditor framing, it blocks the arrangement from passing as pure coordination (lawful collection of a lawful debt) by forcing the questions the cover story exists to deflect: who pays, whether exits are suppressed, whether the total tracks any damage calculus. Against a fatalist framing, it blocks treatment of the burden as an immutable fact of defeat — the arrangement was built, enforced, renegotiated, and dismantled by identifiable agents with identifiable gains. The theater series is the mandatrophy instrument here: its climb from 0.18 to 0.71 marks the mandate dying ahead of the machinery, the classic signature of a constraint kept alive ceremonially after its function ceased — visible in the moratorium diplomacy and the Lausanne proceedings, which buried an obligation whose collection had already stopped.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'Which reading of the versailles_reparations_clauses kernel does this constraint instantiate, and what would the sibling readings change structurally?',
    'Comparison against the two sibling stories: limited_responsibility_reading retains the same seats but bounds claims by German transfer capacity (victim burden shrinks toward the collectible surplus, epsilon falls toward coordination cost); repudiation_reading treats the imposition as void under duress (victims released, beneficiary claims extinguished, no lawful extraction surface remains).',
    'If the limited reading governs, this story''s high epsilon and trapped-target structure relax toward a capacity-indexed schedule. If the repudiation reading governs, the beneficiary seats lose standing entirely and the arrangement dissolves rather than reclassifying. The disagreement is located in Article 231''s semantic status (binding guilt-grounding instrument versus legal formality) and in whether the licensed claims are valid and unbounded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer-frame positioning: this file is one of three readings of the reparations-clauses kernel.').

omega_variable(
    war_guilt_evidential_basis,
    'Does the historical record sustain unique German responsibility for causing and widening the war — the empirical premise on which the punitive reading''s moral authority rests?',
    'Publication and archival testing of foreign-ministry records: the Kautsky collection and German White Books from 1919 forward, culminating in full archive access and professional historiography at mid-century.',
    'Sustained sole-guilt evidence anchors the reading''s foundational axiom; shared-responsibility findings strip its empirical grounding, drive axiom_overriding drift, and push the punitive frame toward computed foreclosure within its own tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(war_guilt_evidential_basis, empirical, 'Whether the unique war-guilt attribution survives evidential scrutiny.').

omega_variable(
    capacity_versus_will_default,
    'Were the German defaults of 1921-1923 driven by genuine incapacity to transfer, or by unwillingness that a differently designed schedule could have overcome?',
    'Counterfiscal reconstruction comparing German budgetary and balance-of-payments capacity in 1920-1923 against the Dawes-scale transfers actually achieved after 1924 under supervision and external lending.',
    'If capacity existed, the measured burden reflects deliberate punishment design; if it did not, the claim was engineered beyond feasibility, which strengthens the cover-story reading of the guilt framing and raises effective suppression above the authored scalar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_versus_will_default, empirical, 'Separating inability from unwillingness in the default cascade.').

omega_variable(
    dawes_circuit_net_extraction,
    'How much net wealth crossed from Germany to the creditors after 1924, once American lending recycled through German payments and Allied debt service back toward New York?',
    'Net-flow accounting separating gross annuities from concurrent borrowing and inter-Allied debt remittances over the 1924-1931 window.',
    'A near-zero net transfer relocates the arrangement''s late-period bite from fiscal extraction to sovereignty subordination, flattening the extractiveness series after 1924 and shifting analytic weight onto the control-lever dimension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dawes_circuit_net_extraction, empirical, 'Gross annuities versus net circular flows in the post-1924 settlement.').

omega_variable(
    occupation_function_or_performance,
    'Was the Ruhr occupation''s operation predominantly enforcement-functional (securing deliveries) or performative-punitive (separatist leverage that failed)?',
    'Delivery ledgers weighed against occupation costs, plus the fate of the Rhenish separatist projects the occupation sponsored.',
    'A predominantly performative record raises the theater series'' early values and pushes the late-interval profile toward inertial maintenance; a functional record supports the harder enforcement reading throughout the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupation_function_or_performance, empirical, 'Functional versus theatrical character of the enforcement peak.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(versailles_punitive_tr_t1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1919, 0.18).
narrative_ontology:measurement_basis(versailles_punitive_tr_t1919, observed).
narrative_ontology:measurement(versailles_punitive_tr_t1921, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1921, 0.22).
narrative_ontology:measurement_basis(versailles_punitive_tr_t1921, observed).
narrative_ontology:measurement(versailles_punitive_tr_t1923, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1923, 0.26).
narrative_ontology:measurement_basis(versailles_punitive_tr_t1923, observed).
narrative_ontology:measurement(versailles_punitive_tr_t1924, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1924, 0.34).
narrative_ontology:measurement_basis(versailles_punitive_tr_t1924, observed).
narrative_ontology:measurement(versailles_punitive_tr_t1927, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1927, 0.38).
narrative_ontology:measurement_basis(versailles_punitive_tr_t1927, observed).
narrative_ontology:measurement(versailles_punitive_tr_t1929, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1929, 0.44).
narrative_ontology:measurement_basis(versailles_punitive_tr_t1929, observed).
narrative_ontology:measurement(versailles_punitive_tr_t1931, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1931, 0.58).
narrative_ontology:measurement_basis(versailles_punitive_tr_t1931, observed).
narrative_ontology:measurement(versailles_punitive_tr_t1932, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1932, 0.71).
narrative_ontology:measurement_basis(versailles_punitive_tr_t1932, observed).

% Extraction over time
narrative_ontology:measurement(versailles_punitive_be_t1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1919, 0.74).
narrative_ontology:measurement_basis(versailles_punitive_be_t1919, observed).
narrative_ontology:measurement(versailles_punitive_be_t1921, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1921, 0.8).
narrative_ontology:measurement_basis(versailles_punitive_be_t1921, observed).
narrative_ontology:measurement(versailles_punitive_be_t1923, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1923, 0.9).
narrative_ontology:measurement_basis(versailles_punitive_be_t1923, observed).
narrative_ontology:measurement(versailles_punitive_be_t1924, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1924, 0.82).
narrative_ontology:measurement_basis(versailles_punitive_be_t1924, observed).
narrative_ontology:measurement(versailles_punitive_be_t1927, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1927, 0.84).
narrative_ontology:measurement_basis(versailles_punitive_be_t1927, observed).
narrative_ontology:measurement(versailles_punitive_be_t1929, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1929, 0.76).
narrative_ontology:measurement_basis(versailles_punitive_be_t1929, observed).
narrative_ontology:measurement(versailles_punitive_be_t1931, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1931, 0.42).
narrative_ontology:measurement_basis(versailles_punitive_be_t1931, observed).
narrative_ontology:measurement(versailles_punitive_be_t1932, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1932, 0.12).
narrative_ontology:measurement_basis(versailles_punitive_be_t1932, observed).

% Suppression requirement over time
narrative_ontology:measurement(versailles_punitive_su_t1919, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1919, 0.7).
narrative_ontology:measurement_basis(versailles_punitive_su_t1919, observed).
narrative_ontology:measurement(versailles_punitive_su_t1921, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1921, 0.74).
narrative_ontology:measurement_basis(versailles_punitive_su_t1921, observed).
narrative_ontology:measurement(versailles_punitive_su_t1923, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1923, 0.92).
narrative_ontology:measurement_basis(versailles_punitive_su_t1923, observed).
narrative_ontology:measurement(versailles_punitive_su_t1924, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1924, 0.66).
narrative_ontology:measurement_basis(versailles_punitive_su_t1924, observed).
narrative_ontology:measurement(versailles_punitive_su_t1927, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1927, 0.6).
narrative_ontology:measurement_basis(versailles_punitive_su_t1927, observed).
narrative_ontology:measurement(versailles_punitive_su_t1929, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1929, 0.48).
narrative_ontology:measurement_basis(versailles_punitive_su_t1929, observed).
narrative_ontology:measurement(versailles_punitive_su_t1931, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1931, 0.3).
narrative_ontology:measurement_basis(versailles_punitive_su_t1931, observed).
narrative_ontology:measurement(versailles_punitive_su_t1932, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1932, 0.1).
narrative_ontology:measurement_basis(versailles_punitive_su_t1932, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, repudiation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Versailles reparations': the kernel splits into three epsilon-distinct readings, each a separate story with its own beneficiary structure and classification. This file authors the punitive_liability_reading (guilt-grounded quasi-unlimited charges; high extraction; trapped German targets; creditor beneficiaries). limited_responsibility_reading retains the same seats but bounds claims by transfer capacity, lowering epsilon toward coordination cost; repudiation_reading voids the obligation entirely, leaving no lawful extraction surface. The punitive reading is upstream in legitimacy: its moral-certainty frame is the structure within which the limited reading negotiates and against which the repudiation reading reacts. Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
