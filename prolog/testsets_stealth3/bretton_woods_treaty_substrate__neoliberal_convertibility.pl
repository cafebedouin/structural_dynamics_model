% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__neoliberal_convertibility, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods Substrate — Neoliberal Convertibility Reading (Government Intervention Constrained)
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the bretton_woods_treaty_substrate
 *   kernel: the neoliberal_convertibility reading, under which the treaty
 *   substrate (the Articles of Agreement and the institutional apparatus
 *   built on them — the Fund, the dollar anchor, the multilateral payments
 *   framework) constrains GOVERNMENT INTERVENTION in order to enable free
 *   capital markets. On this reading capital controls are violations rather
 *   than tools, national policy autonomy sits in the victim set, and
 *   international finance is the beneficiary. The sibling readings —
 *   keynesian_embedded_liberalism (capital constrained to protect domestic
 *   policy space) and sovereignty_defense (external discipline constrained to
 *   preserve monetary sovereignty) — are separate constraints in separate
 *   files with their own epsilon, victim sets, and classifications; nothing
 *   about them is averaged into this story. KEY AGENTS (by structural
 *   relationship): - imf_management_and_board: agenda-setter
 *   (institutional/constrained) — administers, surveils, conditions; -
 *   us_treasury_and_federal_reserve: agenda-setter and beneficiary
 *   (powerful/arbitrage) — issues the reserve asset, exempt from the
 *   discipline it anchors; - transnational_financial_investors: primary
 *   beneficiary (powerful/arbitrage) — collects convertibility guarantees,
 *   rescue flows, and exit speed; - multinational_export_corporations:
 *   secondary beneficiary (powerful/mobile) — collects exchange-rate
 *   predictability; - advanced_economy_deficit_governments: payer
 *   (moderate/identity_locked) — deflates and submits to preserve
 *   credibility; - developing_economy_debtors: payer (powerless/trapped) —
 *   borrows in dollars, accepts written-in policy; -
 *   debtor_country_populations: payer (powerless/trapped) — absorbs austerity
 *   and devaluation costs; - developmental_state_advocates: excluded
 *   (moderate/constrained) — objects from outside the quota-weighted room; -
 *   international_monetary_economists: analytical observer — sees the full
 *   structure. The claim/metric gap is deliberate: the reading's own seat
 *   presents the constraint as coordination (rope), while the authored
 *   structural data — named victims, named beneficiaries, active enforcement
 *   — supports tangled_rope. The engine measures that divergence; this file
 *   does not reconcile it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.64).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.57).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.64).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.57).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Substrate — Neoliberal Convertibility Reading (Government Intervention Constrained)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '1da8a8a8-d856-4d7f-a658-f14ad32d7b43').
narrative_ontology:cs_kernel_codification('1da8a8a8-d856-4d7f-a658-f14ad32d7b43', formalized).
narrative_ontology:cs_authority_grounding('1da8a8a8-d856-4d7f-a658-f14ad32d7b43', extraction).
narrative_ontology:cs_interpretation_layer_present('1da8a8a8-d856-4d7f-a658-f14ad32d7b43').
narrative_ontology:cs_reading_relation('1da8a8a8-d856-4d7f-a658-f14ad32d7b43', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, influences).
narrative_ontology:cs_reading_relation('1da8a8a8-d856-4d7f-a658-f14ad32d7b43', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('1da8a8a8-d856-4d7f-a658-f14ad32d7b43', foundational, capital_mobility_maximizes_welfare).
narrative_ontology:cs_axiom_status(capital_mobility_maximizes_welfare, holdable).
narrative_ontology:cs_axiom_grounding('1da8a8a8-d856-4d7f-a658-f14ad32d7b43', capital_mobility_maximizes_welfare, empirically_contingent).
narrative_ontology:cs_axiom('1da8a8a8-d856-4d7f-a658-f14ad32d7b43', secondary, government_intervention_distorts_allocation).
narrative_ontology:cs_axiom_status(government_intervention_distorts_allocation, holdable).
narrative_ontology:cs_axiom_grounding('1da8a8a8-d856-4d7f-a658-f14ad32d7b43', government_intervention_distorts_allocation, empirically_contingent).
narrative_ontology:cs_reference_frame('1da8a8a8-d856-4d7f-a658-f14ad32d7b43', liberal_convertibility_framework).
narrative_ontology:cs_drift_state('1da8a8a8-d856-4d7f-a658-f14ad32d7b43', post_global_financial_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1da8a8a8-d856-4d7f-a658-f14ad32d7b43', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, transnational_financial_investors).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_export_corporations).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, us_treasury_and_federal_reserve).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, advanced_economy_deficit_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economy_debtors).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, debtor_country_populations).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_mobility_maximizes_welfare).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, government_intervention_distorts_allocation).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, washington_consensus_prescriptions).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, market_confidence_credibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Articles of Agreement, runs surveillance over members' exchange-rate and capital-account practices, and attaches conditions to standby and extended lending facilities. Its quota-weighted governance gives creditor governments blocking weight over revision. Its operational relevance, staffing, and budget depend on a continuing flow of members seeking resources on its terms; abandoning that role would mean ceding the payments-framework function to ad hoc creditor coalitions.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_management_and_board, agenda_setter,
    institutional, generational, constrained, global).

% Issues the system's reserve asset and anchors the payments order; its monetary decisions set the conditions to which the rest of the system adjusts. It settles its external position in its own currency, so the external discipline applied to deficit countries never binds it directly. It collects seigniorage, enjoys first claim on global savings, and holds an effective veto over Articles revision, entrenching the current reading of the treaty.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, us_treasury_and_federal_reserve, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, us_treasury_and_federal_reserve, beneficiary).

% Global banks, asset managers, and portfolio holders moving capital across borders the regime keeps open. They obtain guaranteed convertibility and repatriation, deep liquid markets, and rescue flows routed through official lending that repays private creditors. When a host government threatens controls or default, they can reprice, withhold, or exit faster than any regulator can respond.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, transnational_financial_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Firms producing and selling across multiple currency zones that rely on stable conversion for invoicing, hedging, and profit repatriation. They gain predictability from the framework and can relocate production when a jurisdiction's policies threaten returns, which converts their siting decisions into standing pressure on host-country policy.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_export_corporations, beneficiary,
    powerful, biographical, mobile, global).

% Governments running external deficits under surveillance: they defend exchange-rate parity, deflate on schedule, and submit to policy review to preserve their standing as credible members of the international financial community. They retain the legal capacity for controls or devaluation, but their finance ministries' professional identity is fused with market confidence, so departing from the consensus registers internally as national disgrace rather than available policy choice.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, advanced_economy_deficit_governments, payer,
    moderate, biographical, identity_locked, national).

% Countries dependent on external financing who borrow in currencies they cannot issue. Market access and official lending are conditioned on opening the capital account and tightening budgets; refusal brings capital flight and cutoff. Serial programs leave policy commitments written into loan letters, and each renegotiation begins from a weaker reserve position than the last.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economy_debtors, payer,
    powerless, biographical, trapped, national).

% Households in adjusting economies who absorb the domestic side of external discipline: austerity budgets, subsidy removal, unemployment from deflationary programs, and inflation following devaluations. They vote in the affected countries but hold no voice in the institutions that design the programs applied to them.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, debtor_country_populations, payer,
    powerless, biographical, trapped, national).

% Post-Keynesian and structuralist economists, UNCTAD analysts, and capital-management proponents who argue that policy space and capital regulation are complements to development rather than obstacles to it. They publish, advise, and testify but hold no quota votes and no seat where the reading is adjudicated; their proposals reach the agenda mainly as crisis-time exceptions granted by the institutions they criticize.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developmental_state_advocates, excluded,
    moderate, generational, constrained, regional).

% Academic researchers mapping the policy trilemma, crisis transmission, and program outcomes. They can see the whole structure — who adjusts, who exits, who is paid — and their findings feed back into institutional doctrine slowly and selectively.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, transnational_financial_investors).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a multilateral payments and settlement framework: current-account convertibility, exchange-rate stability with orderly adjustment, and an official lender for balance-of-payments emergencies — replacing the interwar pattern of competitive devaluation, discriminatory clearing blocs, and trade collapse.
% TRANSFER_FUNCTION: Moves policy discretion from national governments to financial markets and creditor institutions; moves adjustment costs onto deficit economies and their populations; moves interest and profit streams from debtor economies to transnational creditors; moves seigniorage to the reserve issuer.
% ABSENT_VOICES: Developmental-state and capital-management traditions — post-Keynesian economists, structuralist development economists, UNCTAD analysts, and debtor-country coalitions — would object that sacrificing policy space to capital mobility reproduces the very instability the founders built the system to escape. They sit outside quota-weighted governance and outside the creditor-dominated agenda; their proposals surface chiefly as crisis exceptions granted by the institutions they criticize.
% DISAPPEARANCE_RATIONALE: Cross-border lending and portfolio investment would reprice sharply or retreat behind bilateral state-to-state deals; trade finance would tighten; exchange rates would fragment into blocs or float chaotically; governments would recover capital-control and monetary instruments immediately; the Fund's lending role and the dollar's institutional anchoring would lapse into whatever arrangements creditor coalitions improvised.
% FOUNDING_PROBLEM: Interwar monetary chaos: competitive devaluations, discriminatory trade and clearing blocs, destabilizing capital flights, and the transmission of the Great Depression across borders. The 1944 design sought to reconcile open multilateral trade with domestic stabilization.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the interwar period — outside the benefiting parties — attest the founding problem and confirm the 1944 design targeted it. Debtor-government testimony, UNCTAD reporting, and the Fund's own Independent Evaluation Office corroborate that adjustment burdens remain live. No source outside the beneficiary set attests this reading's claim that the founding problem is now government intervention itself; that attestation comes from the creditor institutions and financial industry the arrangement benefits.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.64 at interval end) because the arrangement decouples who adjusts from who errs: deficit countries and their populations bear deflation, austerity, and crisis costs, while rescue lending routes through to private creditors and the reserve issuer settles in its own money. Suppression (0.68) is raw structural force — conditionality, market-access threats, capital-flight risk — and is deliberately authored unscaled; only extractiveness is scaled by directionality and scope in the engine's computation. Theater rises across the interval (0.15 to 0.57) because the discipline story (rules bind everyone, markets allocate) increasingly diverges from operation: rules bind deficit countries, rescues socialize creditor losses, and the issuer is exempt. Accessibility collapse is moderate (0.50): exit remains possible — Malaysia 1998, Iceland 2008, China's managed account — but each demonstration is punished by repricing, so alternatives survive only at premium cost. Resistance (0.60) is real and recurring: control episodes, defaults, the global-justice movement, and the Fund's own eventual endorsement of capital-flow management. The temporal series share one nine-point grid. Three phases: (i) 1944-1971 compromise phase — the reading is latent, extraction moderate, enforcement light; (ii) 1971-1997 ratchet — dollar standard, petrodollar recycling, debt-crisis conditionality, Washington Consensus codification, peaking at 1997; (iii) 2003-2024 oscillation — post-Asian-crisis tolerance for capital management, then eurozone-troika re-hardening. The cycle is driven by crisis feedback: each crisis discredits the reading temporarily, then institutional re-assertion restores it; the oscillation functions as intermittent reinforcement rather than noise. Base_properties scalars reflect the interval-end state (post-re-hardening plateau). Coalition note: the powerless victims (developing debtors, debtor populations) have repeatedly attempted coalition — G77 bloc voting, debt-jubilee campaigns, BRICS financial arrangements — but creditor leverage over market access and the collective-action cost of coordinated default have kept coalition power from converting into exit.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter and beneficiary seats the arrangement is a functioning payments order they administer and profit from — coordination with overhead. From the trapped payer seats the same structure is enforced adjustment with the exit doors wired shut. Among payers at the same nominal class (national governments), constraint-specific factors differentiate exit: advanced-economy finance ministries are identity_locked (professional identity fused with market credibility makes departure unthinkable even where legally possible), while developing debtors are materially trapped (dollar-denominated debt, funding cutoff). The cross-reading gap is larger still: the neoliberal seat computes something close to rope; the sibling stories author the inverted computation. The engine derives all of this from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the low-d pole: transnational_financial_investors (declared beneficiary, arbitrage exit — the regime subsidizes precisely their mobility), multinational_export_corporations (beneficiary, mobile), and us_treasury_and_federal_reserve (agenda-setter with declared beneficiary secondary role and arbitrage exit — it issues the constraint's anchor and is exempt from its discipline). Victims map to the high-d pole: developing_economy_debtors and debtor_country_populations (trapped, powerless — nearest the full-target end), and advanced_economy_deficit_governments, whose identity_locked exit places them near the target end despite moderate power. One override is declared: institutional -> 0.30. The derivation chain has no beneficiary/victim entry for imf_management_and_board and would fall back to the canonical institutional default; that fallback misplaces an administrator whose stake is institutional persistence rather than rent capture — the Fund enforces the extraction but captures little of it, sitting between symmetric and beneficiary. The us_treasury_and_federal_reserve seat carries the powerful atom rather than institutional specifically so this override does not collide with it; the dial is chosen for fitness, since the reserve issuer's leverage exceeds that of ordinary institutional actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar monetary chaos — was real, is attested by economic historians outside the benefiting parties, and was genuinely addressed by the 1944 design. The mandate has since drifted: this reading now justifies constraining the very domestic interventions the founders protected, and the enforcement machinery built for payments cooperation is deployed for capital-account liberalization. The R5 mismatch consumer reads founding_problem_status (contested) x disappearance_verdict (world_rearranges): no dead-mandate zombie flag fires, correctly — the arrangement still organizes the world and its problem-status is disputed rather than dead. The drift is nonetheless documented twice: in the rising theater_ratio series and in the treaty_text_vs_operating_regime omega. Classification hygiene runs both directions here: because the payments-coordination function is real, the corpus resists labeling this a pure snare; because the victim set is named, enforced, and persistent, it equally resists the reading's own rope claim. Tangled_rope is the honest structural landing: genuine coordination and asymmetric extraction through the same machinery, held in place by active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the bretton_woods_treaty_substrate kernel is the operative constraint — this one (government intervention constrained, capital freed), keynesian_embedded_liberalism (capital constrained, policy space protected), or sovereignty_defense (external discipline constrained, sovereignty preserved)?',
    'Drafting-history analysis of the Articles of Agreement, enforcement records across 1944-2024, and identification of which provisions creditor institutions actually invoked during crises.',
    'Under keynesian_embedded_liberalism the victim and beneficiary sets invert and the classification moves toward rope or scaffold; under sovereignty_defense the target set shifts to external disciplinarians. Effective extraction and per-seat classifications change accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'This story instantiates one reading of a contested treaty kernel; sibling readings assign the constraint''s burden to different sides of the ledger.').

omega_variable(
    treaty_text_vs_operating_regime,
    'Does the constraint authored here belong to the 1944 treaty text itself (which legally permitted widespread capital controls for decades) or to the post-1971 dollar-standard operating regime that inherited the treaty''s institutions?',
    'Compare the obligations the Articles actually imposed (current-account convertibility, Article VIII acceptance) against the capital-account liberalization agenda enforced after 1971; date the binding constraint''s origin.',
    'If the operative constraint dates from the successor regime, pre-1971 measurements describe a different constraint and the founding-problem genealogy shifts; the extractiveness trajectory would effectively restart at 1971.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_text_vs_operating_regime, empirical, 'Whether the neoliberal reading describes the treaty or its successor regime wearing the treaty''s name.').

omega_variable(
    reserve_issuer_exemption_status,
    'Is the reserve issuer''s exemption from the payments discipline imposed on other members a structural asymmetry concentrating extraction, or compensation for the liquidity and crisis-lending services the issuer provides?',
    'Estimate seigniorage and financing advantages against the cost of supplying reserve assets and emergency liquidity; compare adjustment frequency borne by the issuer versus deficit countries.',
    'If uncompensated asymmetry, the us_treasury_and_federal_reserve seat''s directionality sits nearer the beneficiary pole than its administrative role suggests and effective extraction concentrates further; if compensated, part of the measured asymmetry is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_issuer_exemption_status, empirical, 'Whether exorbitant privilege is rent or payment for system services.').

omega_variable(
    crisis_cost_attribution,
    'Are the recurrent emerging-market crises under liberalized capital accounts failures intrinsic to the constraint (evidence of extraction) or failures of incomplete implementation that this reading attributes to half-open accounts?',
    'Compare crisis incidence and output losses across fully open, partially open, and managed capital accounts controlling for fundamentals; examine program conditionality outcomes.',
    'Intrinsic failure raises epsilon and pushes the computed type toward snare; implementation-shortfall attribution supports the reading''s coordination defense and lowers effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crisis_cost_attribution, conceptual, 'Attribution dispute over crisis evidence under this reading.').

omega_variable(
    trilemma_necessity_vs_rent,
    'How much of the loss of national policy autonomy is a logical consequence of choosing the open-capital corner of the impossible trinity, and how much is discretionary extraction layered above that choice?',
    'Structural comparison of countries occupying different trilemma corners at comparable income levels; decompose autonomy loss into corner-choice cost and conditionality-imposed cost.',
    'The corner-choice component behaves like a fixed cost of the selected arrangement, not removable by enforcement reform; the discretionary component is removable and counts as extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trilemma_necessity_vs_rent, conceptual, 'Separating the trilemma''s logical cost from discretionary extraction.').

omega_variable(
    technocratic_identity_internalization,
    'Is the constraint''s hold on advanced-economy policy elites material (market access, funding costs) or internalized (professional identity fused with credibility and market confidence)?',
    'Examine post-crisis policy trajectories where material pressure eased: if governments restored controls or restructured once costs permitted, the lock was material; where they did not despite capacity, the lock is internalized.',
    'An internalized lock keeps effective suppression high even when structural barriers loosen; the advanced-economy payer seat''s persistence profile shifts accordingly and exit costs stay elevated after material conditions change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technocratic_identity_internalization, conceptual, 'Structural versus internalized suppression mechanism for elite compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1944, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1944, 0.15).
narrative_ontology:measurement_basis(bret_tr_t1944, observed).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1958, 0.18).
narrative_ontology:measurement_basis(bret_tr_t1958, observed).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.3).
narrative_ontology:measurement_basis(bret_tr_t1971, observed).
narrative_ontology:measurement(bret_tr_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1980, 0.4).
narrative_ontology:measurement_basis(bret_tr_t1980, observed).
narrative_ontology:measurement(bret_tr_t1990, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1990, 0.5).
narrative_ontology:measurement_basis(bret_tr_t1990, observed).
narrative_ontology:measurement(bret_tr_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1997, 0.58).
narrative_ontology:measurement_basis(bret_tr_t1997, observed).
narrative_ontology:measurement(bret_tr_t2003, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2003, 0.52).
narrative_ontology:measurement_basis(bret_tr_t2003, observed).
narrative_ontology:measurement(bret_tr_t2012, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2012, 0.6).
narrative_ontology:measurement_basis(bret_tr_t2012, observed).
narrative_ontology:measurement(bret_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.57).
narrative_ontology:measurement_basis(bret_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1944, 0.32).
narrative_ontology:measurement_basis(bret_be_t1944, observed).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1958, 0.38).
narrative_ontology:measurement_basis(bret_be_t1958, observed).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.45).
narrative_ontology:measurement_basis(bret_be_t1971, observed).
narrative_ontology:measurement(bret_be_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement_basis(bret_be_t1980, observed).
narrative_ontology:measurement(bret_be_t1990, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement_basis(bret_be_t1990, observed).
narrative_ontology:measurement(bret_be_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1997, 0.72).
narrative_ontology:measurement_basis(bret_be_t1997, observed).
narrative_ontology:measurement(bret_be_t2003, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2003, 0.62).
narrative_ontology:measurement_basis(bret_be_t2003, observed).
narrative_ontology:measurement(bret_be_t2012, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2012, 0.66).
narrative_ontology:measurement_basis(bret_be_t2012, observed).
narrative_ontology:measurement(bret_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.64).
narrative_ontology:measurement_basis(bret_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1944, 0.25).
narrative_ontology:measurement_basis(bret_su_t1944, observed).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1958, 0.35).
narrative_ontology:measurement_basis(bret_su_t1958, observed).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.42).
narrative_ontology:measurement_basis(bret_su_t1971, observed).
narrative_ontology:measurement(bret_su_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement_basis(bret_su_t1980, observed).
narrative_ontology:measurement(bret_su_t1990, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement_basis(bret_su_t1990, observed).
narrative_ontology:measurement(bret_su_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1997, 0.78).
narrative_ontology:measurement_basis(bret_su_t1997, observed).
narrative_ontology:measurement(bret_su_t2003, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2003, 0.6).
narrative_ontology:measurement_basis(bret_su_t2003, observed).
narrative_ontology:measurement(bret_su_t2012, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2012, 0.72).
narrative_ontology:measurement_basis(bret_su_t2012, observed).
narrative_ontology:measurement(bret_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.68).
narrative_ontology:measurement_basis(bret_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, resource_allocation).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, sovereignty_defense).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Bretton Woods system' conflates three structurally distinct constraints — one per reading of the bretton_woods_treaty_substrate kernel. This file decomposes the neoliberal reading: constraints on government intervention enabling free capital markets, with national policy autonomy in the victim set and international finance as beneficiary. The keynesian_embedded_liberalism and sovereignty_defense stories carry inverted victim/beneficiary structures and different epsilon values over the same historical substrate. The treaty text is the upstream member (highest-confidence historical record); each downstream reading cites it as evidence for its own constraint, which is why the family edges run from this story to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
