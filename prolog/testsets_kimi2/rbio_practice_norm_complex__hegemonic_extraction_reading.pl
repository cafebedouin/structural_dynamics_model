% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: Rules-Based International Order: Hegemonic Extraction Reading
 *   domain: international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the hegemonic_extraction_reading of the
 *   rbio_practice_norm_complex kernel. The Rules-Based International Order is
 *   read as a formally multilateral system whose amendment is blocked by P5
 *   veto and whose enforcement is selective in ways that benefit U.S. and
 *   European capital. The UN Charter and Bretton Woods institutions provide a
 *   coordination surface â dispute forums, collective security, development
 *   finance â but the reading treats this surface as legitimating shell for
 *   a captured structure. The core structural delta is asymmetric extraction:
 *   Global South states and populations pay through conditionality and
 *   intervention vulnerability while advanced capital captures the surplus.
 *   The sibling liberal_institutional_reading sees the same institutions as
 *   genuine consent-based coordination; the sibling
 *   sovereignty_maximalist_reading sees them as sovereignty-violating
 *   interventionism. This reading forecloses the liberal reading's core
 *   premise (consent-based universality) because it treats conditionality as
 *   coerced contract and revision as structurally impossible. It coexists
 *   with the sovereignty-maximalist reading because both can operate within a
 *   shared critical framework that treats Western intervention as
 *   illegitimate, though they differ on whether the problem is capital
 *   extraction or sovereignty violation per se.
 *
 * KEY AGENTS:
 *   - us_eu_capital: Primary beneficiary (institutional/arbitrage) â collects returns from conditionality and market access.
 *   - global_south_states: Primary payer (organized/constrained) â bears sovereignty costs and policy conditionality.
 *   - global_south_populations: Secondary payer (powerless/trapped) â bears austerity and displacement without institutional voice.
 *   - p5_security_council_bloc: Agenda-setter (institutional/arbitrage) â controls enforcement and veto lock-in.
 *   - multipolar_alliance_movements: Excluded alternative (organized/constrained) â proposes parallel architectures but is marginalized.
 *   - un_secretariat: Administrative observer (institutional/constrained) â implements without overriding power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.82).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "Rules-Based International Order: Hegemonic Extraction Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, '66b4cb91-1be7-4456-bc7f-2fc262f01650').
narrative_ontology:cs_kernel_codification('66b4cb91-1be7-4456-bc7f-2fc262f01650', formalized).
narrative_ontology:cs_authority_grounding('66b4cb91-1be7-4456-bc7f-2fc262f01650', extraction).
narrative_ontology:cs_interpretation_layer_present('66b4cb91-1be7-4456-bc7f-2fc262f01650').
narrative_ontology:cs_reading_relation('66b4cb91-1be7-4456-bc7f-2fc262f01650', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('66b4cb91-1be7-4456-bc7f-2fc262f01650', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('66b4cb91-1be7-4456-bc7f-2fc262f01650', foundational, conditionality_as_coerced_contract).
narrative_ontology:cs_axiom_status(conditionality_as_coerced_contract, holdable).
narrative_ontology:cs_axiom_grounding('66b4cb91-1be7-4456-bc7f-2fc262f01650', conditionality_as_coerced_contract, empirically_contingent).
narrative_ontology:cs_axiom('66b4cb91-1be7-4456-bc7f-2fc262f01650', foundational, selective_enforcement_as_hegemonic_prerogative).
narrative_ontology:cs_axiom_status(selective_enforcement_as_hegemonic_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('66b4cb91-1be7-4456-bc7f-2fc262f01650', selective_enforcement_as_hegemonic_prerogative, empirically_contingent).
narrative_ontology:cs_reference_frame('66b4cb91-1be7-4456-bc7f-2fc262f01650', atlantic_hegemonic_order).
narrative_ontology:cs_drift_state('66b4cb91-1be7-4456-bc7f-2fc262f01650', contemporary_multipolar_stress, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('66b4cb91-1be7-4456-bc7f-2fc262f01650', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_eu_capital).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls and invests through institutions that shape trade, finance, and development rules. Receives returns from market access secured by RBIO enforcement, privatization of public assets under conditionality, and intellectual property protections embedded in trade agreements. Can relocate capital across jurisdictions and influence rule-making through domestic and multilateral channels.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_eu_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Member states of international financial and security institutions with formally equal sovereignty but asymmetric voting power. Must accept policy conditions to access crisis financing and development lending. Face sanctions or intervention when violating norms that great powers themselves ignore. Alternatives such as regional development banks or sovereign default carry high isolation costs.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states, payer,
    organized, generational, constrained, global).

% Live under governments constrained by externally imposed fiscal austerity, privatization mandates, and market-opening requirements. Bear the costs of reduced public services and suppressed wages. Have no direct representation in institutions that set these conditions and cannot practically exit their national jurisdiction or citizenship.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations, payer,
    powerless, biographical, trapped, national).

% Permanent members of the UN Security Council whose affirmative votes or abstentions authorize enforcement actions, sanctions, and military interventions. Can shield themselves and allies from accountability through veto power. Shape the interpretive practice of international norms to align with strategic and economic interests. Retain full sovereignty while adjudicating the sovereignty of others.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_security_council_bloc, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Administrative body that implements mandates, produces reports, and convenes negotiations under Charter rules. Cannot override permanent member strategic preferences or withhold cooperation from powerful member states. Depends on assessed contributions and political support from the same powers that benefit from institutional lock-in.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, un_secretariat, observer,
    institutional, generational, constrained, global).

% Coalitions of states seeking alternative financial and security architectures outside the RBIO core. Propose parallel payment systems, development banks, and collective security frameworks. Are denied equivalent enforcement capacity and are marginalized in rule-making forums dominated by the permanent members and OECD consensus.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, multipolar_alliance_movements, excluded,
    organized, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__hegemonic_extraction_reading, us_eu_capital).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__hegemonic_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Managing inter-state conflict, providing a centralized forum for security and economic negotiation, and reducing transaction costs of diplomacy through permanent institutions and codified norms.
% TRANSFER_FUNCTION: Moves policy autonomy, public assets, and surplus from Global South states and populations to U.S. and European capital through conditional lending, structural adjustment programs, selective enforcement of sovereignty norms, and authorized military intervention.
% ABSENT_VOICES: Global South populations who bear conditionality costs without representation in IMF or World Bank governance; states targeted for sanctions or intervention without Security Council authorization; regional powers proposing alternative monetary and security architectures.
% DISAPPEARANCE_RATIONALE: If RBIO norms and their enforcement machinery vanished, conditional lending chains would collapse, the P5 veto monopoly would dissolve, unilateral military intervention would require explicit re-legitimation, and Global South policy space would expand rapidly. The present distribution of military and financial authority would face immediate restructuring.
% FOUNDING_PROBLEM: Preventing recurrence of great-power war and managing the post-World War II international settlement through institutionalized great-power concert and embedded liberal economic rules.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream diplomatic historians attest the original problem. Critical international relations scholars and Global South diplomatic historians attest the problem was substantially transformed by decolonization and the end of the Cold War, yet the institutional architecture persists unchanged; this corroboration comes from outside the beneficiary bloc.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because conditionality, structural adjustment, and selective intervention transfer substantial autonomy and surplus from Global South to advanced capital. Suppression (0.78) is high because the constraint persists through P5 veto lock-in, enforcement selectivity, and the marginalization of alternative institutions. Theater ratio (0.60) reflects that a large share of UN and Bretton Woods activity is performative â resolutions, development frameworks, and diplomatic forums that absorb contestation without altering power asymmetries. Accessibility collapse (0.72) is high because alternatives (regional hegemony, sovereign default blocs, parallel financial systems) are systematically discredited or undermined. Resistance (0.55) is moderate because multipolar coalitions mount real but institutionally contained opposition. The claim is tangled_rope because the coordination surface is real â the UN does reduce certain transaction costs â but the same structure extracts asymmetrically and requires active enforcement to maintain.
 *
 * PERSPECTIVAL GAP:
 *   The P5 agenda-setter seat experiences the constraint as a flexible instrument of order that it can activate or ignore at will; the Global South payer seat experiences it as an immutable external structure. The engine computes this divergence through the interaction of directionality (beneficiary/victim status) and exit options (arbitrage versus constrained/trapped). A powerful seat with trapped exit would compute differently, but here power and exit correlate across the divide.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (us_eu_capital) carry arbitrage exit and are assigned low directionality. Victims (global_south_states with constrained exit, global_south_populations with trapped exit) are assigned high directionality. The P5 agenda-setter is not listed in beneficiary/victim arrays but carries arbitrage exit, which pulls its derived directionality toward the beneficiary end despite its administrative role. The UN secretariat carries constrained exit and no beneficiary status, leaving it near symmetric. Excluded multipolar alliances carry constrained exit and high scope but are not victims of this specific constraint's direct extraction, so they sit at moderate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing great-power war through institutionalized concert â is no longer live in its original form. The constraint persists not because it solves that problem but because it has been repurposed as an extraction mechanism. This would suggest piton or mandatrophy, but the theater ratio, while substantial, is not dominant; the enforcement machinery (IMF conditionality, sanctions, authorized intervention) remains functionally extractive rather than merely performative. Concentrated beneficiaries continue to capture gains, which rules out piton. The classification as tangled_rope captures the hybrid reality: genuine coordination functions have not fully atrophied but are now subordinate to extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rbio_un_amendability_nature,
    'Is the practical un-amendability of the UN Charter due to neutral institutional path-dependency (high transaction costs of redesign), or intentional hegemonic lock-in preserving P5 privilege?',
    'Comparative analysis of amendment procedures in other multilateral bodies and historical records of P5 resistance to Charter reform proposals.',
    'If path-dependent, reform is theoretically possible with sufficient political will; if intentional lock-in, the constraint is extractive by design and more deeply a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rbio_un_amendability_nature, conceptual, 'Nature of RBIO institutional rigidity').

omega_variable(
    enforcement_selectivity_driver,
    'Does selective enforcement of RBIO norms reflect genuine capacity constraints and geopolitical complexity, or does it reflect deliberate extraction benefiting the P5 and advanced capital?',
    'Statistical analysis of enforcement patterns correlated with the economic and strategic interests of enforcing states versus objective severity of violations.',
    'If correlated with interest, supports high extraction and asymmetric enforcement; if correlated with capacity, supports coordination-friction readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_driver, empirical, 'Empirical driver of enforcement selectivity').

omega_variable(
    kernel_reading_standing,
    'This constraint is the hegemonic_extraction_reading of kernel rbio_practice_norm_complex. The sibling liberal_institutional_reading would author low extraction and no victims; the sibling sovereignty_maximalist_reading would author a different beneficiary/victim structure. Where is the structural disagreement located?',
    'Corpus-wide comparison of all three generated constraints in this kernel family.',
    'Determines whether the kernel is a true decomposition site (different observables yield different constraints) or a measurement parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_standing, conceptual, 'Structural delta across kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(rbio_tr_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(rbio_tr_t20, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(rbio_tr_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(rbio_tr_t40, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(rbio_tr_t50, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 50, 0.51).
narrative_ontology:measurement(rbio_tr_t60, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(rbio_tr_t70, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 70, 0.58).
narrative_ontology:measurement(rbio_tr_t80, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 80, 0.6).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rbio_be_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(rbio_be_t20, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(rbio_be_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(rbio_be_t40, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(rbio_be_t50, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 50, 0.71).
narrative_ontology:measurement(rbio_be_t60, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(rbio_be_t70, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 70, 0.79).
narrative_ontology:measurement(rbio_be_t80, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 80, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(rbio_su_t10, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(rbio_su_t20, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(rbio_su_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(rbio_su_t40, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(rbio_su_t50, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 50, 0.73).
narrative_ontology:measurement(rbio_su_t60, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(rbio_su_t70, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 70, 0.77).
narrative_ontology:measurement(rbio_su_t80, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 80, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the rbio_practice_norm_complex kernel. Sibling constraints (liberal_institutional_reading, sovereignty_maximalist_reading) instantiate different epsilon values and stakeholder directionalities from the same kernel text. They are linked as a constraint family via kernel correspondence, not via downstream causal influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
