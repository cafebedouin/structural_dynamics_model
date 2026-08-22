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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods Convertibility Discipline (Neoliberal Reading)
 *   domain: international political economy / monetary history / institutional design
 *
 * SUMMARY:
 *   This story instantiates the NEOLIBERAL CONVERTIBILITY reading of the
 *   Bretton Woods kernel: the treaty substrate is read as constraining
 *   GOVERNMENT INTERVENTION in order to enable free capital movement, with
 *   international finance and creditor-state export sectors as the structural
 *   beneficiaries and national policy autonomy (developing-state treasuries,
 *   full-employment constituencies, central banks) as the structural victims.
 *   This is a distinct constraint from the sibling readings —
 *   keynesian_embedded_liberalism (which reads the SAME treaty text as
 *   constraining international capital to protect domestic policy space,
 *   inverting the beneficiary/victim structure) and sovereignty_defense
 *   (which reads it as constraining external monetary discipline to preserve
 *   national sovereignty). All three share the founding text but diverge on
 *   which party the treaty substrate actually disciplines; per the
 *   ε-invariance principle each reading is authored as its own constraint
 *   with its own ε, not averaged.
 *
 * KEY AGENTS:
 *   - international_finance_capital: primary beneficiary (organized/arbitrage) — collects from unconstrained capital mobility
 *   - creditor_nation_export_sectors: agenda-setting beneficiary (institutional/arbitrage) — drafted and enforces the convertibility rules
 *   - reserve_currency_issuer: structural beneficiary (institutional/arbitrage) — exports its monetary policy through the convertibility anchor
 *   - developing_state_treasuries: primary target (powerless/trapped) — loses capital-control and exchange-management tools
 *   - domestic_full_employment_constituencies: diffuse target (powerless/trapped) — bears unemployment when external discipline overrides full-employment policy
 *   - national_central_banks: constrained administrator (moderate/constrained) — retains formal but eroded authority
 *   - keynesian_policy_architects: excluded voice (institutional/analytical) — the founding intent this reading reads past
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.62).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Convertibility Discipline (Neoliberal Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international political economy / monetary history / institutional design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, 'd926781f-3bbd-476d-8947-7c106d3a8e3f').
narrative_ontology:cs_kernel_codification('d926781f-3bbd-476d-8947-7c106d3a8e3f', fixed_text).
narrative_ontology:cs_authority_grounding('d926781f-3bbd-476d-8947-7c106d3a8e3f', extraction).
narrative_ontology:cs_interpretation_layer_present('d926781f-3bbd-476d-8947-7c106d3a8e3f').
narrative_ontology:cs_reading_relation('d926781f-3bbd-476d-8947-7c106d3a8e3f', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('d926781f-3bbd-476d-8947-7c106d3a8e3f', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('d926781f-3bbd-476d-8947-7c106d3a8e3f', foundational, capital_mobility_is_the_protected_value).
narrative_ontology:cs_axiom_status(capital_mobility_is_the_protected_value, holdable).
narrative_ontology:cs_axiom_grounding('d926781f-3bbd-476d-8947-7c106d3a8e3f', capital_mobility_is_the_protected_value, instrumental).
narrative_ontology:cs_axiom('d926781f-3bbd-476d-8947-7c106d3a8e3f', foundational, capital_controls_are_treaty_violations_not_tools).
narrative_ontology:cs_axiom_status(capital_controls_are_treaty_violations_not_tools, holdable).
narrative_ontology:cs_axiom_grounding('d926781f-3bbd-476d-8947-7c106d3a8e3f', capital_controls_are_treaty_violations_not_tools, conventional).
narrative_ontology:cs_reference_frame('d926781f-3bbd-476d-8947-7c106d3a8e3f', bretton_woods_1944_articles_of_agreement).
narrative_ontology:cs_drift_state('d926781f-3bbd-476d-8947-7c106d3a8e3f', post_1997_asian_financial_crisis_liberalization_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d926781f-3bbd-476d-8947-7c106d3a8e3f', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, creditor_nation_export_sectors).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, reserve_currency_issuer).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_state_treasuries).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_full_employment_constituencies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, national_central_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds mobile capital that moves across borders seeking yield and stability; the convertibility regime and the IMF's Article VIII disciplines exist, on this reading, to guarantee that governments cannot arbitrarily block repatriation or devalue against creditors. Benefits directly from every ratchet that narrows the space for capital controls, since each closure widens the set of jurisdictions where capital can move freely and be repriced without political interference.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital, beneficiary,
    organized, generational, arbitrage, global).

% Drafted and continue to police the convertibility rules through the IMF Articles of Agreement, staff conditionality, and Article IV surveillance. Frames capital account liberalization and current account convertibility as the treaty's core achievement and the discipline on 'exchange restrictions' as the mechanism that keeps trade and payments predictable for exporters and lenders headquartered in the founding creditor states.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, creditor_nation_export_sectors, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, creditor_nation_export_sectors, agenda_setter).

% Occupies the dollar-gold anchor position (and its post-1971 successor as fiat reserve issuer). Gains seigniorage and near-unconstrained external financing because the convertibility architecture routes global settlement through its currency; the constraint on OTHER governments' intervention is precisely what makes this seat's own monetary policy exportable without matching external discipline.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, reserve_currency_issuer, beneficiary,
    institutional, civilizational, arbitrage, global).

% Must maintain current account convertibility and increasingly capital account openness to remain in good standing for IMF access, credit ratings, and reserve currency-denominated trade. Loses the capital controls, selective credit allocation, and exchange-rate management tools that industrializing creditor states historically used themselves, on pain of capital flight, downgrade, or conditionality-linked lending cutoffs. Exit means self-exclusion from the international payments system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_state_treasuries, payer,
    powerless, biographical, trapped, national).

% Workers and domestic-oriented firms whose governments face a narrowed policy menu: expansionary fiscal or monetary policy risks a balance-of-payments crisis or capital flight once external convertibility is prioritized. Bear unemployment and wage suppression when governments choose external monetary discipline over full employment to defend the currency peg or maintain investor confidence. Have no direct standing in IMF governance.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_full_employment_constituencies, payer,
    powerless, biographical, trapped, national).

% Administer domestic monetary policy but are structurally disciplined by the convertibility commitment: reserve adequacy, exchange-rate defense, and IMF Article VIII compliance narrow the toolkit available for domestic stabilization. Some retain formal authority to impose controls but face escalating reputational and market costs for doing so as the convertibility norm hardens.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, national_central_banks, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, national_central_banks, agenda_setter).

% The White-Keynes negotiators who explicitly built capital controls into the original Articles of Agreement as a permanent tool for policy space are, on this reading, treated as a historical artifact whose provision (Article VI) has been progressively read down by IMF practice and OECD/EU capital liberalization codes. Their sovereignty- and employment-protecting intent is not part of the operative convertibility regime and has no institutional voice in Article IV surveillance today.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_policy_architects, excluded,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a predictable, rules-based system of currency convertibility and capital mobility so that cross-border trade, lending, and investment can be priced and settled without unilateral government interference — solving the genuine problem of interwar competitive devaluation and payments chaos by fixing a discipline that all signatories submit to.
% TRANSFER_FUNCTION: Moves policy discretion away from national governments (especially weaker, capital-importing states) and toward capital holders and creditor-state institutions: the convertibility commitment transfers exchange-rate and capital-account control from sovereign treasuries to the expectations and exit threats of mobile capital, with IMF conditionality enforcing compliance on debtor states.
% ABSENT_VOICES: The original architects of the Article VI capital-controls carve-out, and labor/full-employment constituencies in both creditor and debtor states, are structurally absent from IMF Article IV surveillance and Executive Board weighting; their objection — that convertibility discipline was meant to be bounded, not maximal — is documented in the founding negotiating record but has no seat in current enforcement.
% DISAPPEARANCE_RATIONALE: If the convertibility discipline vanished overnight, states would reintroduce capital controls and managed exchange rates immediately (as many did unilaterally after 1971 and during the Asian financial crisis), capital would reprice sovereign risk without the anchor of convertibility norms, and the IMF's core surveillance and conditionality function would lose its principal lever — global finance would reorganize around bilateral and regional arrangements rather than a universal convertibility norm.
% FOUNDING_PROBLEM: Interwar competitive devaluation, exchange controls, and beggar-thy-neighbor trade policy had produced currency chaos and collapsing trade volumes; a rules-based international monetary order was built to prevent recurrence.
% FOUNDING_PROBLEM_CORROBORATION: IMF Article IV staff and creditor-state finance ministries attest the convertibility discipline remains necessary to prevent competitive devaluation and capital flight. Independent economic historians (e.g., work tracing the drafting history of Article VI and the Keynes-White negotiations) and UNCTAD-affiliated development economists attest, from outside the beneficiary set, that the original architecture explicitly preserved capital-control space and that the maximal-convertibility reading emerged later through IMF practice and OECD/EU liberalization codes, not from the founding text itself.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.68 — substantial but not maximal — because the coordination function (predictable convertibility preventing competitive devaluation) is genuinely real even under this reading; what makes it extractive on THIS reading is the asymmetric direction of the discipline: it falls overwhelmingly on capital-importing and policy-constrained states while capital and creditor-state institutions retain effectively unconstrained mobility and enforcement power. Suppression (0.62) tracks the IMF conditionality apparatus, credit-rating discipline, and capital-flight threat that has hardened over the interval — the suppression_requirement series rises from 0.20 at founding (Bretton Woods itself still permitted capital controls under Article VI) to 0.62 by 2024 (post-liberalization IMF Article VIII/capital account openness norms, OECD Codes of Liberalisation, EU treaty-level free movement of capital). Theater ratio is moderate (0.30) reflecting that surveillance and Article IV consultation retain real analytical function alongside an increasing performative compliance-signaling component for markets and rating agencies.
 *
 * PERSPECTIVAL GAP:
 *   From the international-finance-capital and creditor-institution seats, the convertibility discipline is genuine coordination they built and maintain to prevent a real historical failure mode (competitive devaluation). From the developing-state-treasury and full-employment-constituency seats, the identical rule structure operates as an enforced narrowing of the policy toolkit that industrialized creditor states themselves used during their own development — the engine's per-seat computation is expected to diverge sharply here, which is the intended diagnostic, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   International finance capital and the reserve currency issuer sit near the full-beneficiary end: mobile, arbitrage-capable, and structurally advantaged by every closure of capital-control space. Creditor-nation export sectors occupy the agenda-setter/beneficiary seat, having authored the enforcement apparatus (IMF Articles, conditionality) that operationalizes the discipline. Developing-state treasuries and full-employment constituencies sit near the full-target end: trapped exit options (self-exclusion from the payments system is not a real choice), powerless in IMF governance weighting, and bearing the transfer directly through lost policy tools and unemployment. National central banks are intermediate — moderate power, constrained rather than trapped exit, since some retain latitude to defy convertibility norms at real but non-catastrophic cost (as several EM central banks have done post-2008).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interwar currency chaos) is genuinely dead as a live crisis in its original form, yet the convertibility discipline has not sunset — it has instead been generalized and hardened (Article VI's controls carve-out progressively read down) well beyond what the founding negotiators intended. This reading's tangled_rope classification prevents mislabeling the arrangement as pure extraction (it retains real coordination value against currency-war risk) while also refusing to certify it as innocent coordination (the enforcement apparatus now serves creditor-state and capital interests asymmetrically). The founding_problem_status is marked contested precisely because IMF/creditor seats and independent historians corroborate different genealogies of the same clause.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convertibility_reading_indeterminacy,
    'Does the Bretton Woods treaty substrate''s own text and negotiating history support the neoliberal-convertibility reading (capital mobility as the protected value) as the CORRECT reading, or is this reading a later institutional drift away from the founding Keynes-White compromise which explicitly preserved capital-control authority under Article VI?',
    'Comparative textual and archival analysis of the 1944 Articles of Agreement (especially Article VI''s capital-controls provision) against the subsequent IMF Article VIII amendment history, OECD Codes of Liberalisation, and EU treaty-level capital-movement provisions, tracing which institutional actors drove the reinterpretation and when the convertibility-maximalist norm became dominant IMF practice.',
    'If the neoliberal reading is shown to be a later institutional capture of an originally more balanced text, this constraint''s claimed_type and beneficiary structure would need to be understood as describing a DRIFTED arrangement rather than the founding compromise itself — strengthening the tangled_rope classification by making the extraction component more clearly a later addition rather than an original design feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convertibility_reading_indeterminacy, conceptual, 'Whether the neoliberal-convertibility reading reflects the founding compromise or a later institutional drift from it.').

omega_variable(
    reading_indexed_beneficiary_inversion,
    'Given that the sibling reading (keynesian_embedded_liberalism) authors the EXACT INVERSE beneficiary/victim structure from the same treaty text, is there a fact of the matter about which reading is structurally correct, or are both readings simultaneously operative for different signatory states depending on their capital-account posture and IMF relationship?',
    'Cross-national comparison of capital-account regime outcomes: states that maintained Article VI-style capital controls (e.g., India, China through much of the interval) versus states that fully liberalized (most OECD members post-1980s) — if outcomes diverge sharply and predictably by regime choice rather than converging under IMF pressure, both readings may be simultaneously true for different subsets of the international system rather than one being the ''real'' kernel.',
    'If both readings are simultaneously operative depending on state capacity to resist IMF/creditor pressure, this suggests the kernel itself is genuinely indeterminate rather than resolved toward either pole, and the three-way kernel decomposition (this story plus its two siblings) may need a fourth reading capturing the differentiated-outcome case rather than treating all signatories as facing one uniform constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_indexed_beneficiary_inversion, conceptual, 'Whether the neoliberal and Keynesian readings are mutually exclusive claims about one kernel or coexisting descriptions of different subsets of signatory states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1944, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.15).
narrative_ontology:measurement(bret_tr_t1982, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(bret_tr_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1997, 0.26).
narrative_ontology:measurement(bret_tr_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2008, 0.28).
narrative_ontology:measurement(bret_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1944, 0.25).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.35).
narrative_ontology:measurement(bret_be_t1982, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1982, 0.48).
narrative_ontology:measurement(bret_be_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1997, 0.62).
narrative_ontology:measurement(bret_be_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2008, 0.66).
narrative_ontology:measurement(bret_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.3).
narrative_ontology:measurement(bret_su_t1982, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(bret_su_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1997, 0.58).
narrative_ontology:measurement(bret_su_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(bret_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_conditionality_lending).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_account_liberalization_norm).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the bretton_woods_treaty_substrate kernel, each authored as a separate constraint per the ε-invariance principle. neoliberal_convertibility (this story) reads the treaty as disciplining government intervention for the benefit of mobile capital (ε=0.68, tangled_rope). keynesian_embedded_liberalism reads the SAME text as disciplining international capital to protect domestic policy space, inverting the beneficiary/victim structure entirely. sovereignty_defense reads it as disciplining external monetary imposition to preserve national sovereignty, centering a third distinct value. All three link to each other via affects_constraints and share the founding treaty text as their common textual kernel while diverging sharply on ε, claimed_type, and stakeholder structure — they are not the same constraint measured three ways but three distinct constraints sharing an origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
