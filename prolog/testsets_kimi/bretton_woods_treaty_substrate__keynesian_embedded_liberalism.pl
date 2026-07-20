% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Bretton Woods Capital Control Architecture (Keynesian Embedded Liberalism Reading)
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   The Bretton Woods Agreements (1944) established a monetary order in which
 *   international capital mobility was deliberately constrained through
 *   regulated exchange rates and capital controls. This story instantiates
 *   the Keynesian 'embedded liberalism' reading: the treaty was designed to
 *   protect domestic policy space for full-employment and welfare-state
 *   programs by subordinating international finance to national macroeconomic
 *   priorities. The constraint extracts from mobile capital and international
 *   financial markets to subsidize governmental policy autonomy. It is
 *   claimed as tangled_rope because the arrangement coordinates a genuine
 *   collective-action problem (preventing competitive devaluations and
 *   currency wars) while asymmetrically burdening international finance.
 *
 * KEY AGENTS:
 *   - National governments (agenda_setter/beneficiary): Negotiated and administered the treaty; collect policy autonomy.
 *   - International finance (payer): Banks and currency traders whose cross-border mobility is restricted.
 *   - Domestic policy constituencies (beneficiary): Labor and industry benefiting from stable exchange rates and autonomous fiscal policy.
 *   - IMF bureaucracy (agenda_setter): Administrative enforcer of the capital-control and exchange-rate norms.
 *   - Developing nations (excluded): Marginalized participants whose development interests were under-weighted.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.62).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.6).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.62).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital Control Architecture (Keynesian Embedded Liberalism Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'd4eaf573-434d-4467-a93f-bda1e244f4e8').
narrative_ontology:cs_kernel_codification('d4eaf573-434d-4467-a93f-bda1e244f4e8', formalized).
narrative_ontology:cs_authority_grounding('d4eaf573-434d-4467-a93f-bda1e244f4e8', lineage).
narrative_ontology:cs_interpretation_layer_present('d4eaf573-434d-4467-a93f-bda1e244f4e8').
narrative_ontology:cs_reading_relation('d4eaf573-434d-4467-a93f-bda1e244f4e8', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('d4eaf573-434d-4467-a93f-bda1e244f4e8', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('d4eaf573-434d-4467-a93f-bda1e244f4e8', foundational, capital_controls_legitimate_policy_tool).
narrative_ontology:cs_axiom_status(capital_controls_legitimate_policy_tool, holdable).
narrative_ontology:cs_axiom_grounding('d4eaf573-434d-4467-a93f-bda1e244f4e8', capital_controls_legitimate_policy_tool, conventional).
narrative_ontology:cs_axiom('d4eaf573-434d-4467-a93f-bda1e244f4e8', foundational, domestic_policy_space_priority_over_capital_mobility).
narrative_ontology:cs_axiom_status(domestic_policy_space_priority_over_capital_mobility, holdable).
narrative_ontology:cs_axiom_grounding('d4eaf573-434d-4467-a93f-bda1e244f4e8', domestic_policy_space_priority_over_capital_mobility, deontological).
narrative_ontology:cs_reference_frame('d4eaf573-434d-4467-a93f-bda1e244f4e8', keynesian_embedded_liberalism_compromise).
narrative_ontology:cs_drift_state('d4eaf573-434d-4467-a93f-bda1e244f4e8', post_nixon_shock_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('d4eaf573-434d-4467-a93f-bda1e244f4e8', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_policy_constituencies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, embedded_liberalism_compromise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the Bretton Woods Articles of Agreement and administer the treaty through treasury and central-bank institutions; they deploy capital controls and adjustable pegs to shield domestic fiscal and monetary policy from disruptive capital flows, collecting policy autonomy as the arrangement's primary return.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, beneficiary).

% Commercial banks, bond markets, and currency traders whose cross-border arbitrage and speculative mobility are restricted by capital-control regulations and fixed-exchange-rate obligations; they bear the cost of reduced liquidity and profit opportunity.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance, payer,
    powerful, biographical, constrained, global).

% Domestic industries, labor unions, and social-welfare constituencies that benefit from national governments' capacity to maintain full-employment programs and counter-cyclical spending without immediate punishment by capital flight.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_policy_constituencies, beneficiary,
    organized, biographical, constrained, national).

% Administers treaty surveillance, approves exchange-rate changes, and monitors capital-control compliance; its authority derives from the Articles of Agreement and it acts as the operational enforcement layer for the regime.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_bureaucracy, agenda_setter,
    institutional, generational, constrained, global).

% Formal participants in the Bretton Woods conference but with limited agenda-setting influence; their structural interests in commodity-price stabilization and development finance were marginalized in the final architecture.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, developing_nations, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevent the interwar pattern of competitive devaluations, currency wars, and procyclical austerity by stabilizing exchange rates and legitimizing capital controls, thereby creating a macroeconomic environment where national governments can pursue full-employment and welfare-state policies without immediate retaliation by mobile capital.
% TRANSFER_FUNCTION: Transfers cross-border financial mobility from international investors and currency speculators to national governments in the form of policy autonomy and exchange-rate stability; the cost of capital mobility is borne by finance, while the benefit of domestic policy space accrues to governments and their domestic constituencies.
% ABSENT_VOICES: Transnational banking consortia and offshore financial centers were structurally excluded from the drafting rooms at Bretton Woods; developing nations were present but held limited bargaining power, leaving their development-finance and commodity-stabilization concerns under-weighted in the final rules.
% DISAPPEARANCE_RATIONALE: If the capital-control constraint disappeared overnight, fixed exchange rates would collapse under speculative pressure, national governments would lose the policy space to run independent fiscal and monetary regimes, and the post-war embedded-liberalism compromise would unravel into the pre-war pattern of austerity-driven currency competition.
% FOUNDING_PROBLEM: The interwar gold-standard collapse and the subsequent competitive devaluations of the 1930s demonstrated that unregulated international capital mobility destroys domestic macroeconomic policy space, producing deflation, unemployment, and political extremism.
% FOUNDING_PROBLEM_CORROBORATION: Keynesian and historiographic scholars attest the problem was live in 1944 and that the treaty addressed it. Neoliberal economists and some financial historians argue the problem was overstated or that the cure became worse than the disease; their testimony from outside the primary beneficiary set supports the contested status.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.45-0.68) is moderate-to-high because capital controls directly restrict the profit opportunities and liquidity of international finance. Suppression (0.50-0.78) is moderate, rising over time as Eurodollar evasion forced harder enforcement. Theater ratio (0.10-0.50) climbs sharply in the final decade as the gap between legal controls and actual capital mobility widened, turning much enforcement into performance. Accessibility collapse (0.50) reflects that while alternatives (bilateralism, autarky, floating) existed, they were costly and unattractive. Resistance (0.55) reflects persistent lobbying by financial interests and the eventual political pressure that collapsed the system in 1971.
 *
 * PERSPECTIVAL GAP:
 *   From the national-government seat, the constraint is a necessary coordination device that solved the interwar currency-war problem and enabled the post-war welfare state. From the international-finance seat, the same structure is an enforced extraction of mobility rents to subsidize politically favored domestic programs. The engine computes this divergence from the structural data: the same treaty reads as coordination to the beneficiary and as extraction to the payer.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments sit near the beneficiary end (d low): they created the constraint, control its levers, and receive policy autonomy. Domestic constituencies are indirect beneficiaries (d low). International finance sits near the target end (d high): it bears the direct cost of restricted mobility and has limited exit within the treaty space. The IMF bureaucracy sits near symmetric (d ~0.5): it administers the constraint without being its primary beneficiary or victim.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) or pure coordination (rope). The genuine coordination functionâpreventing the deflationary, protectionist spiral of the 1930sâis historically real and valued by multiple parties. However, the asymmetric transfer from finance to governments means the coordination is not neutral; someone pays for the stability. Mandatrophy is resolved by documenting that the founding problem (interwar monetary chaos) was live in 1944 but contested by 1971, matching the rising theater ratio and eventual systemic collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Bretton Woods treaty substrate legitimately read as a Keynesian coordination mechanism protecting domestic policy space, or as a hegemonic framework whose true operation privileged Anglo-American monetary interests?',
    'Archival analysis of negotiation records (particularly US/UK Treasury minutes) and comparative policy-outcome studies across participant states to determine whose policy space was actually protected.',
    'If the hegemonic interpretation is validated, the beneficiary set narrows and the constraint''s coordination function becomes cover for asymmetric extraction, shifting classification toward snare; if the Keynesian reading holds, tangled_rope is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the Keynesian reading is a genuine structural description or a legitimating narrative.').

omega_variable(
    capital_control_efficacy,
    'To what extent did Bretton Woods-era capital controls actually constrain international capital mobility, as opposed to being circumvented through Eurodollar markets and regulatory arbitrage?',
    'Quantitative financial-history reconstruction of unrecorded capital flows, interest-rate parity deviations, and offshore-market growth during 1950-1971.',
    'If controls were systematically evaded, the authored base_extractiveness and suppression metrics overstate the constraint''s effective bite; if effective, the metrics are validated and the victim seat''s burden is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_control_efficacy, empirical, 'Empirical gap between legal capital controls and actual capital mobility.').

omega_variable(
    sibling_axiom_boundary,
    'Does the Keynesian embedded-liberalism reading logically foreclose the neoliberal-convertibility reading within a single historiographic framework, or can they coexist as alternative descriptions?',
    'Logical analysis of whether the claim ''capital controls are legitimate policy tools'' can coexist with the claim ''Bretton Woods was designed to maximize capital-market freedom'' within one coherent interpretation of the treaty text.',
    'If they foreclose each other, the kernel is structurally bifurcated and classification must be reading-dependent; if they coexist, the kernel is ambiguous but not contradictory, suggesting a distributed authority grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_axiom_boundary, conceptual, 'Structural relationship between competing readings of the same treaty kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bret_tr_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 5, 0.12).
narrative_ontology:measurement(bret_tr_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 10, 0.15).
narrative_ontology:measurement(bret_tr_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 15, 0.25).
narrative_ontology:measurement(bret_tr_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 20, 0.38).
narrative_ontology:measurement(bret_tr_t27, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 27, 0.5).

% Extraction over time
narrative_ontology:measurement(bret_be_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bret_be_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(bret_be_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(bret_be_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(bret_be_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(bret_be_t27, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 27, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bret_su_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(bret_su_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(bret_su_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(bret_su_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(bret_su_t27, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 27, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, sovereignty_defense).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested bretton_woods_treaty_substrate kernel. The sibling readings (neoliberal_convertibility, sovereignty_defense) instantiate structurally distinct constraints from the same treaty text. They are linked as a constraint family due to shared kernel provenance, not causal upstream/downstream influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
