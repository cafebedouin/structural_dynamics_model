% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_neoliberal_convertibility, []).

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
 *   human_readable: Bretton Woods: Neoliberal Convertibility Reading
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   The Bretton Woods system (1944) established fixed exchange rates and
 *   dollar convertibility as the centerpiece of post-war monetary order. The
 *   neoliberal reading interprets this system as a device that structurally
 *   constrains national governments from pursuing autonomous macroeconomic
 *   policy: capital convertibility forces domestic policy to accommodate
 *   international finance, and the resulting capital-account openness makes
 *   capital strikes the binding constraint on government action. In this
 *   reading, Bretton Woods converts 'policy space' into 'investor
 *   protection.' The measurement series trace how enforcement
 *   machinery—particularly IMF conditionality and surveillance—shifted from
 *   multilateral oversight of ALL imbalances to asymmetric pressure on
 *   governments to liberalize capital flows (1947–1970 symmetric period to
 *   1970–1992 asymmetric period). The theater_ratio rises sharply through
 *   1974 then plateaus, indicating that after the system's formal collapse
 *   the apparatus persisted through institutional channels while the original
 *   coordination rationale decayed.
 *
 * KEY AGENTS:
 *   - National governments (especially developing-economy sovereigns): structurally forced to hold reserves in foreign currency, accept capital mobility constraints on autonomy, and face capital-strike discipline if policy deviates from international-finance-acceptable bounds.
 *   - International Monetary Fund: administrator of the constraint, initially mandated to enforce symmetric adjustment but instrumentally captured to enforce asymmetric capital-account liberalization on debtor nations.
 *   - Capital-exporting economies and multinational finance: primary beneficiaries of convertibility rules, which guarantee them market access and capital mobility while constraining their trading partners.
 *   - Labor constituencies and domestic-policy advocates: bearers of the suppressed policy space; constrained from labor-friendly redistribution, countercyclical spending, or autonomous exchange-rate management.
 *   - International capital markets (banks, portfolio investors, foreign-direct-investment firms): direct beneficiaries of the convertibility rule, which makes their investments liquid and removable.
 *   - Analytical observer (this reading's seat): sees the apparatus as a mechanism for embedding capital-export interests into binding international law, disguised as neutral monetary cooperation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.71).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods: Neoliberal Convertibility Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '6bfc1a6d-4527-429e-9cab-d5ad13b64526').
narrative_ontology:cs_kernel_codification('6bfc1a6d-4527-429e-9cab-d5ad13b64526', fixed_text).
narrative_ontology:cs_authority_grounding('6bfc1a6d-4527-429e-9cab-d5ad13b64526', extraction).
narrative_ontology:cs_interpretation_layer_present('6bfc1a6d-4527-429e-9cab-d5ad13b64526').
narrative_ontology:cs_reading_relation('6bfc1a6d-4527-429e-9cab-d5ad13b64526', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, forecloses).
narrative_ontology:cs_reading_relation('6bfc1a6d-4527-429e-9cab-d5ad13b64526', bretton_woods_treaty_substrate__sovereignty_defense, influences).
narrative_ontology:cs_axiom('6bfc1a6d-4527-429e-9cab-d5ad13b64526', foundational, capital_account_openness_enables_efficiency).
narrative_ontology:cs_axiom_status(capital_account_openness_enables_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('6bfc1a6d-4527-429e-9cab-d5ad13b64526', capital_account_openness_enables_efficiency, empirically_contingent).
narrative_ontology:cs_axiom('6bfc1a6d-4527-429e-9cab-d5ad13b64526', foundational, policy_autonomy_subordinate_to_currency_stability).
narrative_ontology:cs_axiom_status(policy_autonomy_subordinate_to_currency_stability, holdable).
narrative_ontology:cs_axiom_grounding('6bfc1a6d-4527-429e-9cab-d5ad13b64526', policy_autonomy_subordinate_to_currency_stability, instrumental).
narrative_ontology:cs_reference_frame('6bfc1a6d-4527-429e-9cab-d5ad13b64526', capital_liberalizing_multilateral_order).
narrative_ontology:cs_drift_state('6bfc1a6d-4527-429e-9cab-d5ad13b64526', post_bretton_woods_collapse_1992, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6bfc1a6d-4527-429e-9cab-d5ad13b64526', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_exporting_economies).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_finance).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, labor_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Required to maintain fixed exchange rates and dollar convertibility, which forces them to hold foreign reserves and subordinate domestic policy to external balance constraints. Capital mobility limits their ability to pursue independent monetary policy, conduct capital controls, or implement redistributive fiscal policy without triggering capital strikes or IMF intervention. Exit options: capital controls (regime violation, brings Fund surveillance and conditionality), exchange-rate adjustment (regime abandonment, economic isolation), or acceptance of policy constraints.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments, payer,
    institutional, generational, constrained, national).

% Structurally dependent on imported capital to finance development. Bretton Woods architecture makes them price-takers in international capital markets with no countervailing power in Fund governance. Subject to asymmetric conditionality: their capital-account policy is continuously scrutinized and liberalization is compelled, while capital-exporting states face no equivalent pressure. Trapped by external debt servicing obligations, IMF program conditionality, and the threat of capital strike.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economies, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economies, excluded).

% Experience suppressed policy space through government's inability to implement full employment, wage-supporting fiscal policy, or labor-friendly exchange-rate adjustment without triggering capital flight. The constraint forecloses political demands for countercyclical employment and wages because governments face binding capital-mobility constraints. Exit: labor organizing/unionization faces capital mobility threat ('we'll move production'), limiting their leverage.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, labor_constituencies, payer,
    organized, biographical, constrained, national).

% Administer the apparatus: the IMF enforces exchange-rate discipline, surveillance, and conditionality. Initially mandated to enforce symmetric adjustment on surplus and deficit countries; institutional practice shifted over time to asymmetric enforcement of capital-account liberalization on debtor nations. Formally independent of any single state but structurally shaped by capital-exporting state preferences (voting power, program design, surveillance priorities).
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Guaranteed policy predictability, exchange-rate stability, and capital mobility under the fixed-rate regime. Their residents and firms can invest abroad with confidence that returns will be repatriable in stable currency terms. The constraint secures their capital markets' dominance by locking other governments into externally-oriented macroeconomic discipline. They do not directly administer the apparatus but their interests dominate Fund governance structure and priority-setting.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_exporting_economies, beneficiary,
    powerful, generational, arbitrage, global).

% Direct beneficiary of convertibility: guaranteed capital mobility, predictable exchange rates, and access to capital-importing markets. The constraint funnels developing-economy policy toward capital-account openness, creating opportunities for foreign direct investment, portfolio flows, and cross-border lending. Extracts rents through this guaranteed market access.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_finance, beneficiary,
    institutional, biographical, arbitrage, global).

% Sovereigns or movements attempting autonomous monetary policy, exchange-rate management, or capital controls face capital strikes, Fund intervention, or regime instability under Bretton Woods. Their policy experiments are suppressed structurally: the system makes them visible as 'violations' and punishes them through capital flight. Trapped because alternative systems require either broad coalition defection or international institution redesign.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, alternative_monetary_experimenters, excluded,
    moderate, generational, trapped, global).

% Observes the apparatus from outside, tracing how the original multilateral coordination mandate (symmetric adjustment) was instrumentally captured by capital-exporting interests and repurposed toward capital-account liberalization enforcement. Measures the extractiveness and theatrical maintenance of the system post-1974 when the founding problem had substantially resolved.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the reading's charitable accounting: provides predictable exchange rates, unified currency anchor (dollar), and multilateral oversight of balance-of-payments adjustment, which solves the coordination problem of post-war monetary chaos. In the reading's critical accounting: functions primarily to lock in capital-exporting state privilege and prevent autonomous monetary experimentation by developing economies.
% TRANSFER_FUNCTION: Transfers policy autonomy from national governments (especially developing-economy sovereigns) to international finance and capital-exporting states. Mechanisms: (1) fixed rates force reserve-holding in foreign assets (capital drain); (2) convertibility enables capital exit on demand (disciplinary threat); (3) IMF conditionality mandates capital-account liberalization and fiscal/monetary retrenchment. The transfer is not money-to-money but autonomy-to-constraint-acceptance.
% ABSENT_VOICES: Nations that attempted autonomous monetary policy or capital controls are excluded from governance: they faced Fund intervention, capital strikes, or regime destabilization. Communist and non-aligned states were structurally outside the system. Labor movements opposing capital mobility were never seated in Fund governance. Ecological critics of growth-oriented development mandates were absent. The absent voices are those who would argue for policy space autonomy, capital controls as development tools, and symmetric rather than asymmetric adjustment burden.
% DISAPPEARANCE_RATIONALE: If Bretton Woods convertibility rules disappeared, developing economies would pursue autonomous monetary policy, capital controls would re-emerge as legitimate tools, and macroeconomic adjustment burdens would rebalance. Redistribution becomes politically feasible again; exchange-rate management becomes available policy instrument; countercyclical spending becomes possible without capital-strike discipline. The international financial architecture reorganizes around either genuine multilateralism or regional/South-South arrangements that do not subordinate periphery autonomy to center capital flows.
% FOUNDING_PROBLEM: Post-war monetary chaos: exchange rates unstable, trade fragmented, currency convertibility collapsed, nations hoarding reserves, capital flows disrupted. The system was built to restore predictability, enable trade, and provide multilateral oversight of adjustment.
% FOUNDING_PROBLEM_CORROBORATION: By 1960, exchange rates had stabilized, capital flows resumed, and trade integration deepened—the founding problem was substantially resolved. Historical economists (Eichengreen, Helleiner, Ruggie) and development scholars (Rodrik, Stiglitz) outside the benefiting parties attest the problem was solved; yet the apparatus intensified in extractive function post-1970 through IMF conditionality and capital-account-liberalization enforcement, showing mandatrophy: the coordination function died but the constraint persisted and was repurposed by institutional capture.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured at 0.68 at interval end because the reading asserts that Bretton Woods funnels policy autonomy toward capital-exporting interests via convertibility rules. The early period (0.38 at t=0, 1947) reflects the original system's genuine attempt at multilateral discipline—the charter included symmetric pressure on surplus states and emergency capital controls. Extractiveness rises sharply through 1974 (peak 0.72) as conditionality shifts asymmetrically toward debtor-country adjustment and capital-account liberalization, then stabilizes post-1974 even after formal collapse because the institutional apparatus persists through IMF/World Bank mandate. Suppression is high (0.71 at interval end) because the constraint's persistence depends on preventing alternative policy experiments: countries that tried autonomous monetary policy faced capital strikes and Fund intervention. Theater rises from near-zero (0.08 at t=0—genuine coordination attempt) to peak 0.48 by 1974 (apparatus defending capital liberalization now masked as 'structural adjustment' and 'stabilization' policy) then plateaus at 0.42 as the fiction stabilizes. The measurement grid is aligned: all three metrics share the same six time points (0, 9, 18, 27, 36, 45) representing 9-year intervals from 1947–1992, capturing the transition from Keynesian embedding to neoliberal enforcement.
 *
 * PERSPECTIVAL GAP:
 *   National governments experience Bretton Woods as a binding constraint on policy autonomy: capital controls become 'violations,' redistribution becomes 'market distortion,' countercyclical spending becomes 'fiscal indiscipline.' They are seated at high directionality (d→1.0) because the constraint structures them as targets. International finance experiences the system as beneficial coordination: transparent rules, capital mobility, policy predictability. They are seated at low directionality (d→0.0). The IMF occupies an ambiguous middle: it is both the apparatus administrator (agenda-setter role) and pressured by capital-exporting state governments (constrained by beneficiary preferences). The engine will compute different per-seat types: governments likely compute toward snare; finance toward rope; IMF toward tangled_rope if it genuinely attempts coordination, or toward piton if its coordinative function has atrophied and it now sustains extractive rules through institutional inertia alone.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary set (international_financial_institutions, capital_exporting_economies, multinational_finance) occupies seats with high exit_options (arbitrage, mobile capital) and institutional power. They collect the constraint's benefits (market access, capital mobility, policy predictability) without running it—their role is beneficiary, not agenda_setter, because the apparatus is administered by multilateral institutions formally independent of any single state. The victim set (national_governments, developing_economies, labor_constituencies) occupies seats with trapped or constrained exit and lower institutional power. They bear suppressed-policy costs without direct access to the constraint-setting machinery. Directionality derives from this asymmetry: beneficiaries sit near d=0.0 (full beneficiary, costs to them damped); victims sit near d=0.8–1.0 (full target, extraction amplified). The developing_economies seat receives the highest directionality because exit is most trapped (capital strike threat, Fund conditionality, external debt servicing obligations) and extraction is highest (policy constraints binding hardest where institutional capacity is weakest).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war monetary chaos, exchange instability, trade fragmentation) was substantially solved by ~1960: exchange rates stabilized, capital flows resumed, trade integration deepened. Yet the constraint persists and actually INTENSIFIES post-1970 in its extractive function. Theater_ratio rising from 0.08 to 0.48 documents this mandatrophy: the coordination rationale decayed but the apparatus continued, now justifying itself through new doctrines (structural adjustment, neoliberal reform) rather than the original mandate. This is the piton signature: the coordination function died but inertia and beneficiary capture maintain the structure. However, suppression remains high (0.71) because active enforcement continues—capital liberalization must be compelled; nations resist it—so the constraint is not purely theatrical. Classification diverges: from the neoliberal reading's analytical seat, this appears as tangled_rope in the measurement era (real coordination elements remain, extraction is substantial and actively defended) but with rising piton risk (if the coordination function continues to erode while enforcement persists through pure institutional inertia, the classification would shift toward piton in a future measurement interval). The measurement series capture this in-flight transition: theater rising sharply through 1974, plateauing thereafter as the new doctrine stabilizes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does Bretton Woods constrain governments TO protect capital flows (neoliberal reading), or constrain capital flows TO protect government autonomy (Keynesian reading)?',
    'Historical-institutional analysis: which constraint''s beneficiaries shaped the treaty institutions'' enforcement machinery over time? Which reading''s policy prescriptions governed Fund conditionality, surveillance, and technical assistance?',
    'If neoliberal reading: national autonomy is the victim; extraction is the cost of convertibility. If Keynesian reading: capital controls are tools, not violations; extraction lies elsewhere (in IMF conditionality divorced from original intent). The constraint itself is the same institutional apparatus; the ε-referent differs by reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the treaty''s fundamental purpose is capital liberalization or capital discipline.').

omega_variable(
    enforcement_mechanism_shift,
    'Did Bretton Woods enforcement shift from multilateral discipline of ALL capital flows (original charter intent: asymmetric pressure on surplus states) to unilateral enforcement of capital-account openness?',
    'Archive analysis of Fund programs 1947–1970 vs. 1970–2000: which state adjustments were demanded? Were capital exporters ever required to restrict outflows? When did conditionality flip to demand recipient capital-account liberalization?',
    'Shift would establish the reading as post-hoc institutional capture: the apparatus persists but its direction reversed. Extraction would be structural (asymmetric) rather than coordination-cost. Measured extractiveness could be substantially underestimated if enforcement reversal post-dates measurement interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_shift, empirical, 'Whether enforcement machinery was repurposed toward capital liberalization.').

omega_variable(
    domestic_policy_space_absorption,
    'To what extent does capital-account openness FORCE particular macroeconomic policies (tight money, fiscal retrenchment, labor flexibility) on governments, versus merely enabling capital exit?',
    'Policy-space analysis: compare macroeconomic autonomy of closed-capital-account governments vs. open regimes at similar development levels, controlling for IMF program participation. Measure the fraction of domestic policy constraints arising from convertibility threat vs. from explicit conditionality.',
    'If convertibility forces policy passivity (capital strike threat), the extraction is more severe than the formal rate (capital-control relaxation) suggests. The suppression metric would understate the constraint''s coercive reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_policy_space_absorption, empirical, 'Degree to which capital openness constrains domestic policy choice.').

omega_variable(
    beneficiary_identification_instability,
    'Are the beneficiaries of Bretton Woods the stated multilateral institutions (IMF/World Bank), or the capital-exporting state governments and financial firms that use the institutions'' authority?',
    'Institutional accountability trace: who sets Fund policy priorities? Whose interests dominate surveillance and program design? Are the institutions agents or instruments?',
    'If institutions are agents, the constraint is tangled_rope with coordination (multilateral oversight) and extraction (capital liberalization enforcement). If institutions are captured, the constraint is snare with false coordination narrative masking capital-state rent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_instability, conceptual, 'Whether Bretton Woods benefits multilateral coordination or capital-exporting interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bw_neolib_theater_1947, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0, 0.08).
narrative_ontology:measurement(bw_neolib_theater_1956, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 9, 0.18).
narrative_ontology:measurement(bw_neolib_theater_1965, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 18, 0.32).
narrative_ontology:measurement(bw_neolib_theater_1974, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 27, 0.48).
narrative_ontology:measurement(bw_neolib_theater_1983, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 36, 0.45).
narrative_ontology:measurement(bw_neolib_theater_1992, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 45, 0.42).

% Extraction over time
narrative_ontology:measurement(bw_neolib_extractiveness_1947, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bw_neolib_extractiveness_1956, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(bw_neolib_extractiveness_1965, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 18, 0.64).
narrative_ontology:measurement(bw_neolib_extractiveness_1974, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 27, 0.72).
narrative_ontology:measurement(bw_neolib_extractiveness_1983, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 36, 0.68).
narrative_ontology:measurement(bw_neolib_extractiveness_1992, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 45, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bw_neolib_suppression_1947, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(bw_neolib_suppression_1956, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 9, 0.58).
narrative_ontology:measurement(bw_neolib_suppression_1965, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(bw_neolib_suppression_1974, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 27, 0.75).
narrative_ontology:measurement(bw_neolib_suppression_1983, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 36, 0.72).
narrative_ontology:measurement(bw_neolib_suppression_1992, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 45, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.22).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_structural_adjustment_conditionality).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_account_liberalization_mandate).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, dollar_standard_monetary_hegemony).

% DUAL FORMULATION NOTE:
% The kernel bretton_woods_treaty_substrate decomposes into three constraint stories, each instantiating a different reading of the treaty's purpose and effects. This story (neoliberal_convertibility) reads the apparatus as constraining national policy autonomy to enable capital markets. The sibling reading bretton_woods_treaty_substrate__keynesian_embedded_liberalism reads the same apparatus as constraining capital flows to enable national policy autonomy. The sibling reading bretton_woods_treaty_substrate__sovereignty_defense reads the apparatus as constraining external discipline to preserve monetary sovereignty. The three stories share the kernel (fixed rates, dollar convertibility, multilateral institutions) but instantiate it with opposite directionality and beneficiary/victim structure. ε-invariance is preserved: each reading's ε is measured relative to its own framing of the constraint's function (the standing arrangement under contest, per that reading's lights). Ε does not average or hedge across readings; each is a clean, ε-invariant story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
