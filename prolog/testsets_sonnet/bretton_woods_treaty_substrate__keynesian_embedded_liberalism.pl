% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__keynesian_embedded_liberalism, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods Capital Controls as Domestic Policy Space Protection (Keynesian/Embedded Liberalism Reading)
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the embedded-liberalism reading of the Bretton
 *   Woods treaty substrate: the same Article VI provisions permitting
 *   national capital controls are read here as a legitimate constraint on
 *   international finance, protecting domestic policy autonomy (full
 *   employment, welfare-state redistribution) from speculative capital
 *   discipline. Under this reading, national governments and their domestic
 *   constituencies are beneficiaries; international finance and speculative
 *   capital are the victim set whose mobility is the thing being constrained.
 *   This is a distinct constraint from the sibling readings — the
 *   neoliberal_convertibility reading treats government intervention as the
 *   constrained object and free capital markets as the beneficiary set; the
 *   sovereignty_defense reading treats external monetary discipline itself as
 *   the constrained object. All three readings share the same treaty text and
 *   dates but differ in which party sits in the beneficiary/victim slot and
 *   in what the coordination function is held to solve. Per the ε-invariance
 *   principle, these are three separate constraint files linked by network
 *   edges, not one story with an observable parameter.
 *
 * KEY AGENTS:
 *   - national_governments: primary beneficiary and agenda_setter (institutional/arbitrage) — administers and benefits from capital-control authority
 *   - postwar_welfare_state_architects: institutional beneficiary, co-agenda-setter — designed the Article VI legitimation of controls
 *   - domestic_labor_constituencies: diffuse beneficiary (organized/constrained) — gains policy shelter without direct lever over the treaty
 *   - international_finance: primary target/victim (powerful/trapped) — mobility explicitly constrained by design
 *   - cross_border_speculative_capital: primary target/victim (powerful/trapped) — the constraint's founding object of restraint
 *   - creditor_nation_export_sectors: secondary payer (powerful/constrained) — opportunity cost of restricted capital deployment abroad
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.32).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.58).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.32).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital Controls as Domestic Policy Space Protection (Keynesian/Embedded Liberalism Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '5c35c165-9b6a-4596-b150-35e603dcec65').
narrative_ontology:cs_kernel_codification('5c35c165-9b6a-4596-b150-35e603dcec65', formalized).
narrative_ontology:cs_authority_grounding('5c35c165-9b6a-4596-b150-35e603dcec65', lineage).
narrative_ontology:cs_interpretation_layer_present('5c35c165-9b6a-4596-b150-35e603dcec65').
narrative_ontology:cs_reading_relation('5c35c165-9b6a-4596-b150-35e603dcec65', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_reading_relation('5c35c165-9b6a-4596-b150-35e603dcec65', bretton_woods_treaty_substrate__sovereignty_defense, influences).
narrative_ontology:cs_axiom('5c35c165-9b6a-4596-b150-35e603dcec65', foundational, domestic_policy_autonomy_takes_priority_over_capital_mobility).
narrative_ontology:cs_axiom_status(domestic_policy_autonomy_takes_priority_over_capital_mobility, holdable).
narrative_ontology:cs_axiom_grounding('5c35c165-9b6a-4596-b150-35e603dcec65', domestic_policy_autonomy_takes_priority_over_capital_mobility, instrumental).
narrative_ontology:cs_axiom('5c35c165-9b6a-4596-b150-35e603dcec65', secondary, speculative_capital_lacks_standing_against_sovereign_macroeconomic_choice).
narrative_ontology:cs_axiom_status(speculative_capital_lacks_standing_against_sovereign_macroeconomic_choice, holdable).
narrative_ontology:cs_axiom_grounding('5c35c165-9b6a-4596-b150-35e603dcec65', speculative_capital_lacks_standing_against_sovereign_macroeconomic_choice, conventional).
narrative_ontology:cs_reference_frame('5c35c165-9b6a-4596-b150-35e603dcec65', keynesian_embedded_liberalism_founding_settlement).
narrative_ontology:cs_drift_state('5c35c165-9b6a-4596-b150-35e603dcec65', post_1971_nixon_shock_float_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5c35c165-9b6a-4596-b150-35e603dcec65', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_constituencies).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postwar_welfare_state_architects).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, cross_border_speculative_capital).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, creditor_nation_export_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and enforce Article VI-sanctioned capital controls at the border, insulating domestic interest-rate policy and fiscal choices from speculative capital flight. They administer the licensing and exchange-control apparatus and can adjust it as domestic political needs demand. Their exit from the arrangement is effectively arbitrage-grade: they wrote the rules and can waive or tighten them unilaterally within the treaty's tolerance.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, beneficiary).

% Benefit from a policy environment where full-employment commitments and wage bargaining are not immediately punished by capital flight; the controls buy governments room to run counter-cyclical policy without a currency crisis every time unemployment falls. They have no direct lever over the treaty architecture but benefit from the shelter it provides domestic macroeconomic choices.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_constituencies, beneficiary,
    organized, biographical, constrained, national).

% Economists and treasury officials (in the Keynes tradition) who designed the IMF Articles specifically to legitimate capital controls as compatible with, not violations of, orderly international monetary relations. They collect no direct rent but their institutional and intellectual project — embedded liberalism — is vindicated each time a control is deployed without triggering IMF sanction.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postwar_welfare_state_architects, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postwar_welfare_state_architects, agenda_setter).

% Private banks and cross-border lenders whose arbitrage and speculative positioning are the explicit target of the controls — the treaty's founding purpose was to deny them the leverage they held in the interwar period. They are legally barred from certain transactions and have no standing within the IMF's Article VI framework to contest the controls; their exit is blocked by design, which is the point of the constraint from this reading's seat.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance, payer,
    powerful, biographical, trapped, global).

% Short-horizon capital seeking interest-rate or currency arbitrage across borders. The exchange-control apparatus exists precisely to slow or deny these flows so that a government's domestic rate-setting is not immediately arbitraged away. From this reading, their loss of mobility is the coordination good, not a side effect.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, cross_border_speculative_capital, payer,
    powerful, immediate, trapped, global).

% Export-oriented industries in surplus countries (principally the U.S. in the early Bretton Woods years) whose capital could otherwise flow freely to seek the highest return; the controls constrain where their capital can go and slow the reinvestment of trade surpluses abroad. They bear an opportunity cost so that deficit countries retain policy autonomy.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, creditor_nation_export_sectors, payer,
    powerful, generational, constrained, continental).

% Administers the formal treaty language distinguishing current-account convertibility (required) from capital-account convertibility (explicitly not required, and controls explicitly sanctioned under Article VI). Adjudicates disputes about whether a member's controls are treaty-compliant.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_article_vi_secretariat, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides national governments a legitimate, treaty-sanctioned tool to prevent short-term speculative capital flows from overriding domestic full-employment and welfare-state policy choices — solving the interwar problem where mobile capital punished any government that pursued counter-cyclical or redistributive policy.
% TRANSFER_FUNCTION: Moves policy autonomy and macroeconomic stability from international financial actors (who lose the ability to arbitrage domestic policy divergence) to national governments and their domestic constituencies (who gain room to run independent fiscal and monetary policy).
% ABSENT_VOICES: International banks and speculative capital had no seat in the Bretton Woods negotiations — the U.S. and U.K. delegations (with White and Keynes) designed the capital-control provisions specifically over financial industry objection; the financial sector's exclusion from the drafting room is the mechanism, not an oversight.
% DISAPPEARANCE_RATIONALE: If Article VI's sanction for capital controls vanished, deficit-country governments pursuing full-employment or redistributive policy would face immediate speculative attack on their currencies, as occurred repeatedly in the interwar gold-standard years; domestic policy autonomy would be re-subordinated to capital mobility, which is exactly the outcome this reading holds the constraint was built to prevent.
% FOUNDING_PROBLEM: Interwar experience (1920s–30s) showed that unrestricted capital mobility let financial markets punish any government's independent macroeconomic policy via currency attack and capital flight, producing competitive devaluations, beggar-thy-neighbor policy, and the political conditions that fed the Depression and its aftermath.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the architects' own tradition (e.g., Eichengreen's account of interwar gold-standard fragility) corroborate that unconstrained capital mobility was structurally implicated in the interwar collapse. Financial-sector economists and neoliberal-reading advocates dispute that the problem remains live post-1970s float, arguing capital mobility now disciplines governments productively rather than destructively — that dispute is exactly the sibling-reading contest this story routes to omega variables rather than resolving here.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).
:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.32 at interval end) because this reading holds the constraint's transfer function to be legitimate redistribution of policy leverage, not rent extraction — the 'extraction' here is the loss of speculative optionality by financial actors who, in the founding-problem framing, should not have held that leverage over sovereign policy in the first place. Suppression is authored higher (0.58) because the exchange-control apparatus is a real, actively enforced restriction on capital movement — the coordination good depends on that restriction holding, and holding it required border-control infrastructure, licensing regimes, and IMF Article VI adjudication. Theater ratio is low (0.20) and rises only mildly across the interval, reflecting that the controls were functionally load-bearing for most of the period (not yet performative) but drift upward as capital markets began finding workarounds (Euromarkets) that let the letter of the rule persist while its grip loosened.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute this reading's agenda_setter/beneficiary seats (national governments, welfare-state architects) as experiencing something closer to a rope or tangled_rope in the coordination-favorable direction, while the payer seats (international finance, speculative capital) experience the identical structural facts as a snare-like restriction on their mobility. This divergence is the intended output, not an error — it is exactly the seat-divergence the framework is built to surface, and it is symmetric with (but inverted relative to) the neoliberal_convertibility sibling reading's seat computation.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and welfare-state architects derive low d (near beneficiary) because they are the declared beneficiaries with arbitrage-grade exit — they wrote and administer the rule. International finance and speculative capital derive high d (near full target) because they are declared victims with trapped exit — Article VI explicitly denies them standing to contest controls. Creditor-nation export sectors sit in between: powerful but only constrained (not trapped), since their capital can still deploy domestically or through permitted channels, just not freely abroad.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored 'contested' rather than 'dead' precisely to prevent this reading from mislabeling the arrangement as either a permanently necessary Mountain (which the sibling sovereignty_defense reading might drift toward) or a fully obsolete piton (which the neoliberal_convertibility reading, having declared the problem dead by the 1970s float, would support). Embedded liberalism's own institutional heirs (this reading's beneficiary set) will tend to assert the problem is still live; outside corroboration (Eichengreen-style historical analysis) supports the interwar diagnosis without settling whether post-Bretton-Woods capital mobility reproduces the same danger — that residual dispute is exactly what routes to the omega variables and to the sibling-reading network rather than being resolved unilaterally by this file.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_control_legitimacy_ambiguity,
    'Are the Article VI capital-control provisions a genuine, treaty-legitimated coordination mechanism protecting domestic policy space, or a constraint imposed on international finance that this reading recasts as legitimate to serve national-government interests?',
    'Comparative analysis of policy outcomes in control-using vs. control-abstaining Bretton Woods members during balance-of-payments crises (e.g. UK 1947, France 1968) — if controls demonstrably preserved policy autonomy without disproportionate cost to the restrained capital, the coordination reading gains support; if the costs to constrained capital were systematically borne without commensurate domestic benefit, the extraction reading gains support.',
    'Resolution toward ''genuine coordination'' supports a rope/tangled_rope classification for the beneficiary seats; resolution toward ''imposed constraint dressed as coordination'' would push the classification toward snare from the international-finance seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_control_legitimacy_ambiguity, conceptual, 'Whether capital controls are genuine coordination or extraction relabeled as coordination, from this reading''s own vantage.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the keynesian_embedded_liberalism reading the historically dominant reading of Bretton Woods, or is it one of three co-equal contested readings (alongside neoliberal_convertibility and sovereignty_defense) with no single authoritative framing?',
    'Historiographical survey of how the IMF Articles of Agreement have been invoked in practice across member disputes — whether Article VI has functioned primarily to shield governments (this reading), to discipline them toward eventual convertibility (neoliberal_convertibility), or to preserve sovereign monetary independence as such (sovereignty_defense).',
    'If one reading is shown to be the operative institutional practice while the others are minority scholarly framings, the network weighting between sibling constraints should shift; if all three are genuinely co-equal contested readings, no single constraint should be treated as canonical and all three should carry equal analytical weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading is dominant, minority, or co-equal among the kernel''s three declared readings.').

omega_variable(
    eurodollar_erosion_naturalness,
    'Did the rise of offshore Eurodollar markets in the 1960s represent a natural, inevitable erosion of the capital-control regime (a mountain-like drift), or a constructed regulatory-arbitrage response actively cultivated by financial institutions seeking to escape the constraint?',
    'Institutional history of Eurodollar market formation — whether it emerged from unplanned regulatory gaps or from deliberate institutional design by banks and complicit regulators seeking to circumvent Bretton Woods controls.',
    'If natural/inevitable, the rising theater_ratio in this story''s late measurements reflects an exogenous erosion the coordination mechanism could not have prevented; if constructed/deliberate, the erosion is itself evidence of the constraint''s victims (international finance) successfully organizing exit, which would argue for a lower accessibility_collapse value than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eurodollar_erosion_naturalness, empirical, 'Whether Eurodollar-market erosion of capital controls was natural drift or deliberate circumvention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.08).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(bret_tr_t1955, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1955, 0.13).
narrative_ontology:measurement(bret_tr_t1961, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1961, 0.16).
narrative_ontology:measurement(bret_tr_t1967, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1967, 0.19).
narrative_ontology:measurement(bret_tr_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1973, 0.2).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.22).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1950, 0.26).
narrative_ontology:measurement(bret_be_t1955, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1955, 0.28).
narrative_ontology:measurement(bret_be_t1961, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1961, 0.3).
narrative_ontology:measurement(bret_be_t1967, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1967, 0.32).
narrative_ontology:measurement(bret_be_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1973, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.45).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(bret_su_t1955, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1955, 0.52).
narrative_ontology:measurement(bret_su_t1961, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1961, 0.55).
narrative_ontology:measurement(bret_su_t1967, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1967, 0.57).
narrative_ontology:measurement(bret_su_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1973, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__sovereignty_defense).

% DUAL FORMULATION NOTE:
% This file is one of three sibling constraints reading the same Bretton Woods treaty substrate (1944-1973). bretton_woods_treaty_substrate__neoliberal_convertibility inverts the beneficiary/victim assignment (free capital markets as beneficiary, government intervention as constrained object). bretton_woods_treaty_substrate__sovereignty_defense locates the constrained object as external monetary discipline itself, with national monetary sovereignty as the protected good. Per the ε-invariance principle these are three distinct constraints sharing one kernel, not one constraint with an observable parameter — each carries its own ε, its own stakeholder set, and its own classification, linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
