% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Treaty Substrate â Sovereignty Defense Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   The Bretton Woods Articles of Agreement established a
 *   fixed-but-adjustable exchange rate system anchored to the U.S. dollar and
 *   gold. Under the sovereignty_defense reading, this architecture is
 *   understood as creating constraints on external monetary discipline â
 *   capital controls, IMF conditionality, and parity maintenance â intended
 *   to preserve national monetary sovereignty against the chaos of the 1930s.
 *   Structurally, however, the arrangement subordinates non-reserve-currency
 *   states to dollar-center discipline while exempting the U.S. from
 *   symmetric adjustment, converting the gold anchor into an extraction
 *   mechanism. The U.S. Treasury and Federal Reserve administer the standard;
 *   non-reserve governments and gold producers bear the costs; the IMF
 *   enforces rules it cannot apply to the center. Post-colonial states are
 *   excluded from the rule-making conversation entirely.
 *
 * KEY AGENTS:
 *   - U.S. Treasury and Federal Reserve: Primary agenda-setter and beneficiary (institutional/arbitrage) â controls the reserve currency and collects seigniorage.
 *   - Non-reserve-currency governments: Primary payer (organized/constrained) â bears adjustment costs and IMF conditionality.
 *   - Gold producer states: Secondary payer (moderate/constrained) â forced to accept below-market gold prices.
 *   - IMF Executive Board: Secondary agenda-setter (institutional/constrained) â enforces rules asymmetrically.
 *   - Post-colonial governments: Excluded voice (powerless/trapped) â inherited rules they did not write.
 *   - Sovereignty-defense analytical observers: See the constraint as protecting sovereignty in rhetoric while undermining it in structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.74).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.75).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.74).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Treaty Substrate â Sovereignty Defense Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '6997b02e-a809-406b-805c-471939330269').
narrative_ontology:cs_kernel_codification('6997b02e-a809-406b-805c-471939330269', formalized).
narrative_ontology:cs_authority_grounding('6997b02e-a809-406b-805c-471939330269', lineage).
narrative_ontology:cs_interpretation_layer_present('6997b02e-a809-406b-805c-471939330269').
narrative_ontology:cs_reading_relation('6997b02e-a809-406b-805c-471939330269', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('6997b02e-a809-406b-805c-471939330269', bretton_woods_treaty_substrate__neoliberal_convertibility, influences).
narrative_ontology:cs_axiom('6997b02e-a809-406b-805c-471939330269', foundational, monetary_sovereignty_requires_external_discipline).
narrative_ontology:cs_axiom_status(monetary_sovereignty_requires_external_discipline, holdable).
narrative_ontology:cs_axiom_grounding('6997b02e-a809-406b-805c-471939330269', monetary_sovereignty_requires_external_discipline, instrumental).
narrative_ontology:cs_axiom('6997b02e-a809-406b-805c-471939330269', foundational, asymmetric_adjustment_is_structural_not_accidental).
narrative_ontology:cs_axiom_status(asymmetric_adjustment_is_structural_not_accidental, holdable).
narrative_ontology:cs_axiom_grounding('6997b02e-a809-406b-805c-471939330269', asymmetric_adjustment_is_structural_not_accidental, empirically_contingent).
narrative_ontology:cs_reference_frame('6997b02e-a809-406b-805c-471939330269', treaty_embedded_sovereignty).
narrative_ontology:cs_drift_state('6997b02e-a809-406b-805c-471939330269', dollar_glut_and_gold_crisis_1960s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6997b02e-a809-406b-805c-471939330269', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, us_treasury_federal_reserve).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, us_commercial_banks).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, gold_producer_states).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, dollar_hegemony_doctrine).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, fixed_exchange_rate_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dollar-gold exchange standard, sets par values, and controls the world's primary reserve currency. Benefits from seigniorage and the exorbitant privilege of financing deficits in its own currency without symmetric adjustment pressure.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, us_treasury_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, us_treasury_federal_reserve, beneficiary).

% Profit from dollar-denominated intermediation, Eurodollar market expansion, and privileged access to the reserve currency center. Their global operations are subsidized by the systemic demand for dollars as reserves.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, us_commercial_banks, beneficiary,
    powerful, biographical, arbitrage, global).

% Must maintain external monetary discipline, accept IMF conditionality, and bear austerity costs to defend fixed parities. Their nominal sovereignty is preserved in rhetoric while actual monetary autonomy is constrained by dollar shortage and reserve requirements.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_governments, payer,
    organized, biographical, constrained, national).

% Forced to accept a fixed $35-per-ounce gold price that often fell below market-clearing levels. Their sovereign resource wealth is subordinated to the reserve currency mechanism, effectively taxing their primary commodity export.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, gold_producer_states, payer,
    moderate, biographical, constrained, national).

% Enforces exchange rate discipline and conditional liquidity provision. Structurally dependent on U.S. quota contributions and voting share, it administers the rules but cannot compel symmetric adjustment from the reserve center.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf_executive_board, agenda_setter,
    institutional, generational, constrained, global).

% Largely excluded from the 1944 drafting. Inherited a monetary architecture that equated their sovereignty with deflationary external discipline and had no voice in setting the rules they were later required to follow.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, post_colonial_governments, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__sovereignty_defense, us_treasury_federal_reserve).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates post-war international monetary relations by establishing fixed but adjustable exchange rates, a common liquidity pool through the IMF, and rules to prevent competitive devaluations and bilateral currency warfare.
% TRANSFER_FUNCTION: Moves seigniorage and inflation-export capacity from non-reserve-currency states to the U.S. reserve center; moves gold wealth from producer states to the dollar area at a fixed below-market price; moves adjustment costs from deficit periphery to IMF conditionality.
% ABSENT_VOICES: Post-colonial states excluded from the 1944 drafting; Keynesian planners seeking automatic clearing union mechanisms rather than conditional liquidity; commodity exporters seeking price stabilization rather than deflationary discipline; domestic labor movements in reserve-deficit countries bearing austerity costs without representation at the Fund.
% DISAPPEARANCE_RATIONALE: If the Bretton Woods constraints on external monetary discipline vanished overnight, fixed parities would collapse, the dollar-gold nexus would dissolve, and non-reserve states would reclaim interest-rate and exchange-rate autonomy; the post-war monetary order would reorganize around either floating rates or a new reserve architecture.
% FOUNDING_PROBLEM: The interwar monetary chaos of the 1930s â competitive devaluations, bilateral clearing, gold-standard rigidity without liquidity provision â which destroyed trade and deepened the Depression.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians Eichengreen and Temin attest to the interwar chaos from outside the U.S. beneficiary seat; however, these same historians and contemporary policymakers (de Gaulle, UK Treasury) contested whether the 1960s dollar glut represented the same problem or a new one, suggesting the founding problem had morphed into something the arrangement no longer solved.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness rises from 0.42 to 0.74 over the interval because the dollar shortage of the late 1940s gave way to the dollar glut and Triffin dilemma of the 1960s, making U.S. seigniorage extraction increasingly visible and contested. Suppression is high (0.75) because the constraint's persistence depended on active capital controls, IMF conditionality, and repeated defenses of the $35 gold peg. Theater ratio rises to 0.55 as the sovereignty-preserving rhetoric diverged from the reality of dollar hegemony â by the late 1960s the system's public justification was largely performative. Accessibility collapse is substantial (0.70) because viable alternatives (autarky, Soviet bloc, bilateral clearing) were structurally marginalized in the capitalist world. Resistance is moderate (0.50) â visible in French gold conversions, UK devaluation pressures, and post-colonial critiques, but insufficient to overcome U.S. institutional dominance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (U.S. Treasury/Federal Reserve) experiences the constraint as a global public good it generously administers; the payer seats (non-reserve governments, gold producers) experience it as a rigid external discipline that exempts the center. The IMF seat experiences it as a technical enforcement framework that it cannot apply symmetrically. These divergences are structural, not perceptual â they follow from reserve-currency privilege versus periphery conditionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (us_treasury_federal_reserve, us_commercial_banks) sit near the full-beneficiary end of directionality: they collect seigniorage, enjoy dollar demand, and face no symmetric adjustment. Victims (non_reserve_currency_governments, gold_producer_states) sit near the full-target end: they pay adjustment costs, accept below-market commodity prices, and have constrained exit. The IMF sits in the middle as a constrained agenda-setter with limited exit. Post-colonial states are trapped entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework prevents mislabeling by requiring both a genuine coordination function (preventing 1930s-style competitive devaluation) and identifiable victims with asymmetric extraction. The sovereignty_defense reading does not deny the coordination function; it argues that the coordination is bundled with extraction in a way that makes non-reserve states pay for stability that primarily benefits the center. Without the victim/beneficiary asymmetry, the constraint might read as a Rope; with it, Tangled Rope is structurally required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Bretton Woods treaty substrate a sovereignty-preserving discipline mechanism, a Keynesian embedded-liberalism framework, or a proto-neoliberal convertibility regime?',
    'Comparative historical analysis of the negotiating records (White vs. Keynes plans) and subsequent institutional practice to determine which reading better tracks the treaty''s structural operation.',
    'Determines whether the constraint''s primary victim set is non-reserve states (sovereignty_defense), domestic policy space (keynesian_embedded_liberalism), or government intervention (neoliberal_convertibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contest between three kernel readings over the true structural function of Bretton Woods').

omega_variable(
    gold_anchor_extraction_ambiguity,
    'Does the gold-exchange standard function as a neutral stabilizer or as an asymmetric extraction mechanism privileging the reserve currency center?',
    'Analysis of adjustment burden distribution: if reserve-center deficits are financed indefinitely while periphery deficits trigger IMF conditionality, the gold anchor is structurally extractive.',
    'If extractive, the constraint''s epsilon rises and classification shifts toward snare; if neutral, classification shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_anchor_extraction_ambiguity, empirical, 'Whether the gold anchor is stabilizer or asymmetric extractor').

omega_variable(
    sovereignty_rhetoric_vs_reality,
    'Does the constraint''s sovereignty-preserving rhetoric conceal a subordination of non-reserve monetary autonomy, or does it genuinely protect substantive sovereignty?',
    'Measure of effective policy autonomy (interest rate, fiscal, exchange rate independence) for non-reserve states under Bretton Woods versus pre- and post-regimes.',
    'If rhetoric conceals subordination, theater_ratio and extraction are higher than surface metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_rhetoric_vs_reality, empirical, 'Gap between sovereignty discourse and effective autonomy').

omega_variable(
    us_exorbitant_privilege_naturalization,
    'Is the U.S. exemption from symmetric adjustment an intended feature of the treaty design or an emergent structural privilege naturalized after the fact?',
    'Archival analysis of the White Plan negotiations versus the Articles of Agreement final text to determine whether asymmetry was designed or drifted.',
    'If designed, the constraint is a snare from inception; if drifted, it is a tangled rope whose extraction intensified over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_exorbitant_privilege_naturalization, empirical, 'Whether dollar privilege was designed or drifted').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t0, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bret_tr_t5, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 5, 0.3).
narrative_ontology:measurement(bret_tr_t10, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 10, 0.35).
narrative_ontology:measurement(bret_tr_t15, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 15, 0.42).
narrative_ontology:measurement(bret_tr_t20, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 20, 0.48).
narrative_ontology:measurement(bret_tr_t25, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 25, 0.52).
narrative_ontology:measurement(bret_tr_t27, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 27, 0.55).

% Extraction over time
narrative_ontology:measurement(bret_be_t0, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bret_be_t5, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(bret_be_t10, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(bret_be_t15, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(bret_be_t20, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(bret_be_t25, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(bret_be_t27, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 27, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t0, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bret_su_t5, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(bret_su_t10, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(bret_su_t15, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(bret_su_t20, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(bret_su_t25, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 25, 0.73).
narrative_ontology:measurement(bret_su_t27, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 27, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, global_infrastructure).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% The bretton_woods_treaty_substrate kernel decomposes into three constraint stories (sovereignty_defense, keynesian_embedded_liberalism, neoliberal_convertibility) because the label 'Bretton Woods' conflates three structurally distinct claims about what the arrangement coordinates and extracts. Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
