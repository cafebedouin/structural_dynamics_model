% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__hybrid_selectivity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Selectively Applied Structural Adjustment Conditionalities
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   Structural adjustment conditionalities imposed by international financial
 *   institutions (IFIs) require debtor nations to implement macroeconomic
 *   reformsâsuch as austerity, privatization, and trade liberalizationâas
 *   a condition for accessing emergency financing. This reading
 *   (hybrid_selectivity_reading) treats the regime as a selectively enforced
 *   disciplinary mechanism: weak, geopolitically non-strategic states face
 *   harsh, intrusive enforcement, while strategically important debtors
 *   receive waivers and favorable terms. The constraint thus functions as
 *   tangled ropeâpreserving a genuine coordination story for creditors and
 *   the global financial architecture, while operating as asymmetric
 *   extraction for weak debtors. The kernel
 *   (structural_adjustment_conditionalities) is contested: the
 *   creditor_coordination_reading sees uniform fiscal discipline; the
 *   debtor_extraction_reading sees indiscriminate extraction; this reading
 *   splits the difference by geopolitical position.
 *
 * KEY AGENTS:
 *   - core_creditors: Primary beneficiary (organized/global/arbitrage) â collect debt service and policy alignment from weak states while exempting strategic allies
 *   - hegemon_aligned_states: Secondary beneficiary (powerful/global/mobile) â receive geopolitical waivers and preserve sovereign policy space
 *   - weak_debtor_states: Primary target (powerless/national/trapped) â lose fiscal sovereignty to intrusive conditionality with no viable financing alternative
 *   - domestic_populations_in_weak_states: Secondary target (powerless/national/trapped) â bear austerity costs and service cuts imposed by conditionalities
 *   - international_financial_institutions: Agenda setter (institutional/global/constrained) â administer and enforce the regime, bounded by shareholder power politics
 *   - civil_society_organizations: Analytical observer (organized/global/mobile) â monitor and contest conditionality design from outside formal decision forums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.72).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.75).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Selectively Applied Structural Adjustment Conditionalities").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'aac0d7d2-7a89-4ce0-b244-cd0043018ea7').
narrative_ontology:cs_kernel_codification('aac0d7d2-7a89-4ce0-b244-cd0043018ea7', formalized).
narrative_ontology:cs_authority_grounding('aac0d7d2-7a89-4ce0-b244-cd0043018ea7', lineage).
narrative_ontology:cs_interpretation_layer_present('aac0d7d2-7a89-4ce0-b244-cd0043018ea7').
narrative_ontology:cs_reading_relation('aac0d7d2-7a89-4ce0-b244-cd0043018ea7', structural_adjustment_conditionalities__creditor_coordination_reading, influences).
narrative_ontology:cs_reading_relation('aac0d7d2-7a89-4ce0-b244-cd0043018ea7', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('aac0d7d2-7a89-4ce0-b244-cd0043018ea7', foundational, hegemonic_consent_governs_conditionality_application).
narrative_ontology:cs_axiom_status(hegemonic_consent_governs_conditionality_application, holdable).
narrative_ontology:cs_axiom_grounding('aac0d7d2-7a89-4ce0-b244-cd0043018ea7', hegemonic_consent_governs_conditionality_application, conventional).
narrative_ontology:cs_reference_frame('aac0d7d2-7a89-4ce0-b244-cd0043018ea7', universal_fiscal_discipline_framework).
narrative_ontology:cs_drift_state('aac0d7d2-7a89-4ce0-b244-cd0043018ea7', contemporary_geopolitical_competition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aac0d7d2-7a89-4ce0-b244-cd0043018ea7', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, weak_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, domestic_populations_in_weak_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold sovereign debt claims and exercise influence over IFI policy through board representation and capital subscriptions. They receive reliable debt service streams and macroeconomic policy alignment from weak debtor states while exempting strategic allies from comparable discipline. They can redirect capital flows or tolerate selective default without existential constraint.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditors, beneficiary,
    organized, generational, arbitrage, global).

% Receive favorable lending terms and conditionality waivers in exchange for geopolitical alignment with the dominant creditor bloc. Their sovereign policy space is preserved, and they serve as legitimizing examples of 'successful' reform when needed. They operate outside the enforcement surface of the regime.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states, beneficiary,
    powerful, generational, mobile, global).

% Sovereign borrowers with limited independent market access, heavily dependent on IFI financing. Subject to intrusive structural adjustment conditionalities including austerity, privatization, and trade liberalization. Geopolitically non-strategic, they lack leverage to negotiate waivers. Exit to alternative financing is blocked by high risk premiums and exclusion from private capital markets without an IFI program seal.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, weak_debtor_states, payer,
    powerless, generational, trapped, national).

% Bear the direct costs of conditionalities: public service cuts, user fees, job losses from privatization, and reduced social protections. They have no formal voice in conditionality design and limited capacity to resist state policies mandated by external creditors. Emigration is the only partial exit, often blocked by visa regimes and cost.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, domestic_populations_in_weak_states, payer,
    powerless, biographical, trapped, national).

% Administer and enforce conditionality frameworks, conducting surveillance and disbursing tranched loans. They set the technical terms of policy reform but are constrained by major shareholder preferences; enforcement is rigorous for weak debtors and relaxed for strategic allies. Their legitimacy rests on claims of technocratic neutrality and uniform fiscal standards.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, international_financial_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Transnational and domestic advocacy groups that monitor IFI policies, document conditionality harms, and campaign for debt relief. They lack formal decision-making power but provide external accountability and normative pressure through research, protest, and occasional consultative status.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, civil_society_organizations, observer,
    organized, biographical, mobile, global).

% Non-hegemonic bilateral creditors and development banks that offer financing outside the traditional conditionality framework. Structurally excluded from Paris Club and IFI creditor-coordination forums, their presence is treated as a competitive threat to the framework's uniformity rather than a complementary voice in debt governance.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, alternative_lenders, excluded,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditors).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates creditor collective action to prevent free-riding on emergency financing and establishes a common framework for macroeconomic stabilization in balance-of-payments crises.
% TRANSFER_FUNCTION: Transfers fiscal and policy sovereignty from weak debtor states to IFIs and core creditors; transfers social costs of austerity from state budgets to domestic populations in weak states; transfers geopolitical leverage from hegemon-aligned states to strategic debtors via waiver.
% ABSENT_VOICES: Alternative non-hegemonic lenders and domestic civil society in debtor states are structurally excluded from conditionality design; they would contest the uniformity and necessity of prescribed reforms if admitted to creditor forums.
% DISAPPEARANCE_RATIONALE: Weak debtor states would regain immediate policy autonomy over fiscal, monetary, and trade policy; core creditors would lose institutionalized leverage over debtor domestic economies; strategic debtors would experience limited change. The international development finance architecture would fragment into competing bilateral and regional frameworks.
% FOUNDING_PROBLEM: Post-Bretton Woods debt crises and the 1980s Latin American debt crisis created a collective-action problem among creditors and a need for coordinated stabilization to prevent default cascades and protect the international financial system.
% FOUNDING_PROBLEM_CORROBORATION: Independent heterodox economists and historical political economists outside the creditor-beneficiary coalition attest that the original coordination problem has mutated into a geopolitical leverage mechanism; IFIs and core creditor governments attest it remains live. No corroboration exists from outside the benefiting parties that the founding problem justifies the current selective application.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.72) is high because the cost of conditionalityâsovereignty loss, austerity, social dislocationâis borne by weak debtors while benefits accrue to creditors and aligned states. Suppression (0.75) is high because the regime requires active enforcement: tranche withholding, policy surveillance, and the exclusion of alternative financing frameworks. Theater ratio (0.48) reflects the growing gap between the public justification (uniform fiscal discipline) and the operational reality (geopolitical selectivity). Accessibility collapse (0.65) captures the closure of alternatives for weak debtors, who cannot access capital markets without an IFI program seal. Resistance (0.60) reflects persistent debtor-country protests, civil society campaigns, and the rise of alternative lenders, which have not yet dislodged the regime. The metrics are authored independently of the claimed type: the claim is tangled_rope because the coordination story is structurally necessary to the regime's legitimation and operation, even as extraction dominates for the weak-debtor subset.
 *
 * PERSPECTIVAL GAP:
 *   From the IFI and core creditor seat, the constraint is a necessary coordination device preventing moral hazard and default cascades; from the weak debtor seat, it is an externally imposed extraction mechanism that removes policy autonomy regardless of local conditions. The strategic debtor seat experiences neither coordination nor extraction, revealing the constraint's dependence on geopolitical position rather than fiscal fundamentals. The engine will compute divergent per-seat classifications: the agenda-setter and beneficiary seats may compute toward rope, while the weak-debtor payer seat computes toward snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Core creditors and hegemon-aligned states are structural beneficiaries: they collect debt service, policy alignment, and geopolitical deference with minimal exposure to conditionality themselves (low d, low Ï). Weak debtor states and their domestic populations are structural targets: they absorb the full cost of reform and austerity, with trapped exit options amplifying their effective extraction (high d, high Ï). IFIs sit at a mixed directional position: they enforce extraction but are themselves constrained by shareholder politics and institutional mandates (moderate d). The strategic debtor waiver is the smoking gun: if conditionality were genuinely about fiscal coordination, strategic debtors would be the most rigorously supervised; their exemption proves the rule is geopolitical, not fiscal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcoordinated stabilization after the 1980s debt crisisâwas plausibly live in the 1980s and early 1990s. However, the selective waiver pattern indicates that the instrument has outlived its universal coordination function: it is not applied where it is most needed (if need were fiscal) but where geopolitical leverage is weakest. This is not pure mandatrophy because the coordination function is not entirely deadâit still operates for weak debtors as a stabilization gateâbut it is contested and asymmetric. The classification as tangled_rope (rather than snare) preserves the genuine coordination role for the creditor side while registering the extraction on the debtor side. A piton classification would be incorrect because there is active, concentrated benefit to core creditors; a rope classification would ignore the victim set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selective_enforcement_intentionality,
    'Is the pattern of harsh enforcement on weak debtors and waiver for strategic allies an explicit institutional design, or an emergent property of differential bargaining power in sovereign debt markets?',
    'Archival analysis of IMF Executive Board minutes and bilateral creditor communications to determine whether waivers are systematically pre-negotiated on geopolitical grounds.',
    'If explicit, the constraint is a designed extraction mechanism with a coordination cover story; if emergent, the classification may shift toward uncoordinated creditor oligopoly rather than a single tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_intentionality, empirical, 'Whether selectivity is designed or emergent').

omega_variable(
    remaining_coordination_function,
    'For weak debtor states, does any genuine fiscal coordination function remain, or has conditionality become pure extraction with no stabilizing benefit?',
    'Counterfactual analysis comparing macroeconomic trajectories of conditionality-bound weak states against comparable non-IFI borrowers to isolate stabilization effects from extraction effects.',
    'If no stabilization benefit is demonstrated, the constraint approaches a snare for weak debtors; if some genuine coordination persists, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remaining_coordination_function, empirical, 'Whether coordination function persists for weak debtors').

omega_variable(
    kernel_reading_decomposition,
    'Does the hybrid selectivity reading describe a single tangled rope constraint, or does it mask two structurally distinct constraints â a rope for strategic debtors and a snare for weak debtors â that should be decomposed per the Îµ-invariance principle?',
    'Assess whether Îµ and stakeholder directionalities differ so substantially between the strategic-debtor and weak-debtor contexts that they constitute separate constraints under the Îµ-invariance test.',
    'If decomposition is warranted, this story splits into two linked constraints with different types; if not, the single tangled rope reading is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether this reading should decompose into two constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stru_tr_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(stru_tr_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(stru_tr_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(stru_tr_t44, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 44, 0.55).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(stru_be_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(stru_be_t44, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 44, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(stru_su_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(stru_su_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(stru_su_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(stru_su_t44, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 44, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, resource_allocation).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, debtor_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_selectivity_reading of the structural_adjustment_conditionalities kernel, decomposed from the creditor_coordination_reading and debtor_extraction_reading siblings per the Îµ-invariance principle. The three readings share a referent (structural adjustment policy) but instantiate distinct constraints with different Îµ values, beneficiary structures, and directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
