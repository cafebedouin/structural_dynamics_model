% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market-as-Natural-Default: Two-Stage Amnesia-then-Capture Reading
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This story instantiates the hybrid_amnesia_reading of the
 *   market_as_natural_default kernel: a two-stage process in which genuine
 *   institutional forgetting (roughly 1930s-1970s, as Depression-era
 *   cooperative and mutualist experiments faded from policy memory alongside
 *   postwar market-plus-welfare-state growth) created a naturalized default
 *   with no identifiable extractor, and a second stage (1980s-present) in
 *   which pre-existing beneficiaries of market-centric arrangements inherited
 *   that amnesia and actively weaponized it — funding academic
 *   infrastructure, staffing policy networks, and imposing asymmetric
 *   evidentiary burdens on non-market alternatives. The sibling readings
 *   treat this as a single-stage story: lapsed_alternative_reading claims the
 *   whole phenomenon is passive forgetting with no active closure at any
 *   point; beneficiary_maintained_reading claims the naturalization was
 *   actively defended from the start with no innocent forgetting period. This
 *   story's distinguishing claim is the TWO-STAGE structure itself and the
 *   specific transition it dates to the 1980s.
 *
 * KEY AGENTS:
 *   - financialized_capital_holders: primary beneficiary of stage-two doctrinal hardening (institutional/arbitrage)
 *   - market_economics_professoriate: agenda-setter administering the naturalized curriculum, identity-fused with the doctrine (institutional/mobile)
 *   - deregulatory_policy_networks: agenda-setter imposing asymmetric evidentiary burden in policy (organized/mobile)
 *   - displaced_cooperative_sector_workers: bear the cost of stage-one forgetting, powerless and trapped
 *   - public_provisioning_advocates: excluded voice, forced to clear an evidentiary bar the default never faces
 *   - post_crisis_precarious_households: bear stage-two extraction directly, powerless and trapped
 *   - economic_historians: analytical observers documenting the pre-naturalization institutional record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.5).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market-as-Natural-Default: Two-Stage Amnesia-then-Capture Reading").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '8ccd044f-a277-41df-b0c8-6eb497014efb').
narrative_ontology:cs_kernel_codification('8ccd044f-a277-41df-b0c8-6eb497014efb', distributed).
narrative_ontology:cs_authority_grounding('8ccd044f-a277-41df-b0c8-6eb497014efb', extraction).
narrative_ontology:cs_interpretation_layer_present('8ccd044f-a277-41df-b0c8-6eb497014efb').
narrative_ontology:cs_reading_relation('8ccd044f-a277-41df-b0c8-6eb497014efb', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('8ccd044f-a277-41df-b0c8-6eb497014efb', market_as_natural_default__beneficiary_maintained_reading, influences).
narrative_ontology:cs_axiom('8ccd044f-a277-41df-b0c8-6eb497014efb', foundational, amnesia_precedes_and_enables_capture).
narrative_ontology:cs_axiom_status(amnesia_precedes_and_enables_capture, holdable).
narrative_ontology:cs_axiom_grounding('8ccd044f-a277-41df-b0c8-6eb497014efb', amnesia_precedes_and_enables_capture, empirically_contingent).
narrative_ontology:cs_axiom('8ccd044f-a277-41df-b0c8-6eb497014efb', secondary, extraction_intensity_tracks_doctrinal_consolidation_not_origin).
narrative_ontology:cs_axiom_status(extraction_intensity_tracks_doctrinal_consolidation_not_origin, holdable).
narrative_ontology:cs_axiom_grounding('8ccd044f-a277-41df-b0c8-6eb497014efb', extraction_intensity_tracks_doctrinal_consolidation_not_origin, empirically_contingent).
narrative_ontology:cs_reference_frame('8ccd044f-a277-41df-b0c8-6eb497014efb', pre_naturalization_institutional_pluralism).
narrative_ontology:cs_drift_state('8ccd044f-a277-41df-b0c8-6eb497014efb', post_1980_doctrinal_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8ccd044f-a277-41df-b0c8-6eb497014efb', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, financialized_capital_holders).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, market_economics_professoriate).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, deregulatory_policy_networks).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, displaced_cooperative_sector_workers).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, public_provisioning_advocates).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, post_crisis_precarious_households).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, market_allocation_is_efficient_default).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, there_is_no_alternative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own the capital stock whose returns are maximized when market allocation is treated as the unmarked default and alternatives (cooperative ownership, public provisioning, guild-based coordination) are treated as historical curiosities. Did not engineer the original 1930s-1970s forgetting but from the 1980s onward funded think tanks, endowed chairs, and media outlets that converted the inherited amnesia into an actively defended doctrine. Face essentially no exit cost from the arrangement; can relocate capital across jurisdictions freely.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, financialized_capital_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Trains the next generation of policymakers and journalists using textbooks and models in which market allocation is the analytical starting point and non-market coordination appears only as 'market failure' correction. Administers the curriculum and citation networks that reproduce the framing. Their careers, tenure, and journal placement depend on maintaining the framing's centrality — a genuine identity-fusion between professional standing and the doctrine's naturalness claim.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, market_economics_professoriate, agenda_setter,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, market_economics_professoriate, beneficiary).

% Legislative staff, regulatory appointees, and advisory bodies who administer policy on the premise that market default arrangements require no special justification while any alternative requires extraordinary evidentiary burden. Actively cite the naturalized framing to block cooperative, mutualist, or public-option policy proposals. Rotate between government and industry roles, giving them mobile exit but strong incentive to maintain the doctrine that legitimates the rotation.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, deregulatory_policy_networks, agenda_setter,
    organized, generational, mobile, national).

% Work in sectors where mutual aid societies, worker cooperatives, and municipal provisioning were historically viable competitors to market firms before the mid-century collapse of institutional memory. Bear the cost of policy defaults that treat market-firm employment as the only legible option; lack the organizational memory or legal templates to reconstitute alternatives. Cannot exit the framing because it structures the available job categories and legal forms itself.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, displaced_cooperative_sector_workers, payer,
    powerless, biographical, trapped, national).

% Policy advocates and municipal officials who propose public or commons-based provisioning for housing, utilities, or care work. Are required to clear an evidentiary bar ('prove market failure first') that the market default itself never has to clear, because the default is treated as needing no justification. Their proposals are heard but structurally disadvantaged before the argument begins.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, public_provisioning_advocates, excluded,
    moderate, biographical, constrained, national).

% Households absorbing the volatility of market-default social provisioning (housing, healthcare, retirement) after the 2008 and subsequent crises, at a moment when the doctrine's active defense (post-1980s) coincided with the dismantling of remaining non-market buffers. Cannot exit market-mediated provisioning because alternative institutions were defunded or delegitimized during the same period the doctrine hardened.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, post_crisis_precarious_households, payer,
    powerless, immediate, trapped, national).

% Document the actual institutional record — cooperative banking, guild coordination, municipal ownership, mutual insurance — that predates and coexisted with market-dominant arrangements. Their scholarship is available but occupies a minority position within the disciplines that set policy-relevant curricula, giving them analytical visibility without agenda-setting power.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, financialized_capital_holders).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces the cognitive and negotiating cost of economic coordination by supplying a single default institutional form (the market transaction) that does not require case-by-case justification, allowing large-scale economic activity to proceed without re-litigating governance structure at every transaction.
% TRANSFER_FUNCTION: In its first stage (1930s-1970s), moves nothing extractively — it is a genuine collective forgetting of the menu of alternatives, a coordination cost reduction with no identified extractor. In its second stage (1980s-present), moves policy deference, evidentiary burden, and default-option status from public/cooperative institutions to market-based ones, and via that channel moves income and asset returns toward financialized capital holders and the professional class that certifies the doctrine.
% ABSENT_VOICES: Displaced cooperative-sector workers and historical memory of pre-1930s mutualist institutions are structurally absent from the policy conversation — not excluded by active suppression in stage one, but unavailable because the institutional templates and organizational memory atrophied. Public provisioning advocates are present but forced to argue uphill against a default that bears no equivalent burden of proof.
% DISAPPEARANCE_RATIONALE: If the naturalization collapsed overnight, the professoriate and policy networks that have built careers, tenure, and legitimating frameworks on the default would face a sudden loss of authority, and evidentiary-burden asymmetries in policymaking would flip — the world clearly rearranges for these organized actors. But whether ordinary economic activity itself would reorganize is contested: some argue market coordination would persist on its practical merits even without the naturalized framing (closer to world_unchanged for the coordination function itself), while others argue that removing the unmarked-default status would immediately reopen space for cooperative and public alternatives currently foreclosed by evidentiary asymmetry alone.
% FOUNDING_PROBLEM: In the stage-one period, no one built anything: the founding 'problem' was the practical collapse of Depression-era experiments with alternative coordination (guild socialism, extensive cooperative sectors, some municipal ownership) combined with postwar economic growth under market-plus-welfare-state arrangements, which together allowed institutional memory of the wider menu to lapse through ordinary forgetting rather than design. The stage-two 'founding problem' claimed by beneficiaries — stagflation-era failure of managed economies — was a genuine crisis, but the response (naturalizing markets as needing no justification, rather than treating market and non-market coordination symmetrically) exceeds what the crisis required.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the beneficiary set (e.g., scholarship documenting the pre-1930s cooperative and mutualist sector, and comparative institutional economics literature on the stagflation response) corroborate that the stage-one forgetting was real but that the scope of the stage-two doctrinal hardening exceeded the stagflation crisis that beneficiaries cite as its justification. No corroboration exists from outside the market-economics professoriate and deregulatory policy networks for the claim that the current evidentiary asymmetry between market and non-market defaults is itself necessary rather than constructed.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, contested).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.45, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.20 near the start of the interval (1930) reflecting the genuinely low-extraction character of stage-one forgetting — a coordination-cost reduction with no clear extractor — rising to 0.45 by 2025 as stage-two active defense concentrates rents with financialized capital and legitimating professions. Suppression rises in parallel (0.10 to 0.50) because stage one required essentially no active suppression (alternatives simply were not remembered, not blocked) while stage two required actively raising the evidentiary bar against cooperative and public alternatives that were being proposed. Theater ratio rises from 0.05 to 0.40 because stage-two defense increasingly relies on performative appeals to market naturalness (textbook framing, media commentary treating the default as needing no defense) rather than substantive comparative institutional analysis. All three metrics share one time grid across 1930-2025 as required.
 *
 * PERSPECTIVAL GAP:
 *   From the market_economics_professoriate and deregulatory_policy_networks seats, the arrangement looks like the application of settled, well-tested economic principles requiring no special defense — that is the naturalization succeeding. From displaced_cooperative_sector_workers and public_provisioning_advocates, the same arrangement looks like an artificially unmarked default that forecloses viable alternatives without ever having to justify itself. The engine computes these divergent seat-level readings from the structural power/exit data; the claimed_type (tangled_rope) reflects the authoring judgment that BOTH a genuine coordination function (stage one) and asymmetric extraction requiring active enforcement (stage two) are present in the same constraint across its lifecycle.
 *
 * DIRECTIONALITY LOGIC:
 *   Financialized capital holders and the professoriate are declared beneficiaries with arbitrage/mobile exit — low directionality, benefiting from the doctrine's persistence. Displaced cooperative-sector workers and post-crisis precarious households are declared victims with trapped exit — high directionality, bearing the doctrine's costs with no meaningful exit because the doctrine itself structures the available institutional forms. Public provisioning advocates are excluded rather than victimized outright — they participate in the conversation but under a structurally unequal evidentiary burden, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The two-stage structure specifically prevents mislabeling this as pure extraction from the start (which would erase the genuine, non-extractive coordination-cost-reduction function of stage-one forgetting) or as pure innocent forgetting throughout (which would erase the active, enforced capture of stage two). Treating it as tangled_rope with a rising extraction trajectory lets both truths stand: coordination function was real and remains partly real (the market default does reduce negotiation costs), but the current evidentiary asymmetry and doctrinal defense apparatus are actively maintained extraction riding on that residual coordination function, not merely inherited absence of memory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stage_transition_dating_precision,
    'Can the transition from genuine forgetting to active defensive rationalization be dated more precisely than ''the 1980s,'' and does the precision matter for classification?',
    'Archival analysis of think tank founding dates, economics curriculum revisions, and citation-network formation to identify whether the shift was gradual (supporting a smooth ε trajectory) or triggered by an identifiable event (supporting a step-function reading closer to beneficiary_maintained_reading''s profile).',
    'A sharp, event-triggered transition would push this reading''s profile closer to beneficiary_maintained_reading for the post-transition period; a gradual, diffuse transition supports the smooth trajectory currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stage_transition_dating_precision, empirical, 'Precision of the stage-one-to-stage-two transition dating.').

omega_variable(
    innocence_of_stage_one_forgetting,
    'Was the 1930s-1970s forgetting genuinely innocent (no beneficiary steering it), or did early beneficiaries of market-centric postwar growth passively but non-innocently allow institutional memory to lapse by underfunding cooperative-sector historiography?',
    'Comparative funding-history analysis: did postwar market-favoring institutions (business schools, central banks, trade associations) actively defund or neglect cooperative-sector institutional memory during the alleged ''innocent'' period, versus simply not engaging with it?',
    'If stage-one neglect was itself beneficiary-influenced (even passively, through funding allocation), the clean two-stage story collapses toward beneficiary_maintained_reading — there would be no genuinely innocent period, only variable intensity of the same underlying capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innocence_of_stage_one_forgetting, conceptual, 'Whether the claimed innocent stage-one forgetting was truly free of beneficiary influence.').

omega_variable(
    coordination_function_residual_validity,
    'How much of the market default''s current low-negotiation-cost coordination function is genuine versus how much is itself a product of the doctrinal apparatus suppressing comparative institutional evaluation?',
    'Comparative case studies of jurisdictions with symmetric evidentiary treatment of market and non-market defaults, measuring actual coordination costs where the naturalization is weaker.',
    'If coordination costs remain low even absent the naturalized framing, the residual coordination-function claim in the tangled_rope classification weakens, pushing the reading toward snare; if coordination costs rise substantially without the framing, it supports genuine ongoing hybrid coordination/extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_residual_validity, empirical, 'Whether the market default''s coordination benefit is genuine or itself a product of suppressed comparison.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 1930, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1930, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement_basis(mark_tr_t1930, observed).
narrative_ontology:measurement(mark_tr_t1955, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1955, 0.08).
narrative_ontology:measurement_basis(mark_tr_t1955, observed).
narrative_ontology:measurement(mark_tr_t1975, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement_basis(mark_tr_t1975, observed).
narrative_ontology:measurement(mark_tr_t1985, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement_basis(mark_tr_t1985, observed).
narrative_ontology:measurement(mark_tr_t1995, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1995, 0.31).
narrative_ontology:measurement_basis(mark_tr_t1995, observed).
narrative_ontology:measurement(mark_tr_t2008, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement_basis(mark_tr_t2008, observed).
narrative_ontology:measurement(mark_tr_t2016, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2016, 0.38).
narrative_ontology:measurement_basis(mark_tr_t2016, observed).
narrative_ontology:measurement(mark_tr_t2025, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(mark_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t1930, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1930, 0.18).
narrative_ontology:measurement_basis(mark_be_t1930, observed).
narrative_ontology:measurement(mark_be_t1955, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1955, 0.2).
narrative_ontology:measurement_basis(mark_be_t1955, observed).
narrative_ontology:measurement(mark_be_t1975, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1975, 0.22).
narrative_ontology:measurement_basis(mark_be_t1975, observed).
narrative_ontology:measurement(mark_be_t1985, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1985, 0.29).
narrative_ontology:measurement_basis(mark_be_t1985, observed).
narrative_ontology:measurement(mark_be_t1995, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1995, 0.34).
narrative_ontology:measurement_basis(mark_be_t1995, observed).
narrative_ontology:measurement(mark_be_t2008, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2008, 0.39).
narrative_ontology:measurement_basis(mark_be_t2008, observed).
narrative_ontology:measurement(mark_be_t2016, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2016, 0.42).
narrative_ontology:measurement_basis(mark_be_t2016, observed).
narrative_ontology:measurement(mark_be_t2025, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2025, 0.45).
narrative_ontology:measurement_basis(mark_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1930, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1930, 0.1).
narrative_ontology:measurement_basis(mark_su_t1930, observed).
narrative_ontology:measurement(mark_su_t1955, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1955, 0.12).
narrative_ontology:measurement_basis(mark_su_t1955, observed).
narrative_ontology:measurement(mark_su_t1975, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1975, 0.15).
narrative_ontology:measurement_basis(mark_su_t1975, observed).
narrative_ontology:measurement(mark_su_t1985, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1985, 0.3).
narrative_ontology:measurement_basis(mark_su_t1985, observed).
narrative_ontology:measurement(mark_su_t1995, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement_basis(mark_su_t1995, observed).
narrative_ontology:measurement(mark_su_t2008, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2008, 0.43).
narrative_ontology:measurement_basis(mark_su_t2008, observed).
narrative_ontology:measurement(mark_su_t2016, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2016, 0.47).
narrative_ontology:measurement_basis(mark_su_t2016, observed).
narrative_ontology:measurement(mark_su_t2025, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(mark_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__hybrid_amnesia_reading, 0.12).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the market_as_natural_default kernel. lapsed_alternative_reading models the same historical span as pure passive forgetting with no active closure (flat, low ε throughout). beneficiary_maintained_reading models it as active defense from the outset with no innocent period (high, roughly flat ε from an early date). This hybrid_amnesia_reading is distinguished by its two-stage structure and rising ε trajectory (0.20 to 0.45), dating a specific transition from passive lapse to active capture around 1980. All three stories share the same underlying historical material but instantiate structurally distinct claims about WHEN and WHETHER active agency was present, and therefore carry different ε values, different beneficiary/victim structures at different points in time, and different classifications — per the ε-invariance principle, they are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
