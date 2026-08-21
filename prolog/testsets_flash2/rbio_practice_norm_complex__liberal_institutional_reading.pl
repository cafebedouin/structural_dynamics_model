% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: RBIO Norms (Liberal Institutional Reading)
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the liberal institutional reading of
 *   Rules-Based International Order (RBIO) norms. In this reading, RBIO norms
 *   are seen as universal, consent-based, and legitimately revisable through
 *   multilateral processes. Enforcement selectivity is attributed to capacity
 *   limitations rather than a fundamental flaw in legitimacy. Intervention is
 *   justified when authorized by the UNSC or in cases of grave atrocities,
 *   and economic conditionality is viewed as acceptable contractual terms.
 *   This reading emphasizes the coordination function of RBIO in maintaining
 *   global stability and addressing shared challenges.
 *
 * KEY AGENTS:
 *   - intervening_states: Primary beneficiary (institutional/mobile) — gain influence and pursue strategic interests.
 *   - international_organizations: Agenda-setter (institutional/constrained) — codify, interpret, and enforce norms.
 *   - targeted_states: Primary payer (powerless/trapped) — bear direct costs of interventions/sanctions.
 *   - civilian_populations_under_sanctions: Payer (powerless/identity_locked) — suffer humanitarian consequences.
 *   - non_aligned_states: Observer (moderate/mobile) — monitor and voice concerns without direct involvement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.45).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.3).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "RBIO Norms (Liberal Institutional Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '2267849d-5a32-47fa-94d2-0e9770731432').
narrative_ontology:cs_kernel_codification('2267849d-5a32-47fa-94d2-0e9770731432', formalized).
narrative_ontology:cs_authority_grounding('2267849d-5a32-47fa-94d2-0e9770731432', lineage).
narrative_ontology:cs_interpretation_layer_present('2267849d-5a32-47fa-94d2-0e9770731432').
narrative_ontology:cs_reading_relation('2267849d-5a32-47fa-94d2-0e9770731432', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('2267849d-5a32-47fa-94d2-0e9770731432', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('2267849d-5a32-47fa-94d2-0e9770731432', foundational, universal_norms_legitimate_intervention).
narrative_ontology:cs_axiom_status(universal_norms_legitimate_intervention, holdable).
narrative_ontology:cs_axiom_grounding('2267849d-5a32-47fa-94d2-0e9770731432', universal_norms_legitimate_intervention, deontological).
narrative_ontology:cs_axiom('2267849d-5a32-47fa-94d2-0e9770731432', foundational, multilateral_processes_ensure_consent).
narrative_ontology:cs_axiom_status(multilateral_processes_ensure_consent, holdable).
narrative_ontology:cs_axiom_grounding('2267849d-5a32-47fa-94d2-0e9770731432', multilateral_processes_ensure_consent, conventional).
narrative_ontology:cs_reference_frame('2267849d-5a32-47fa-94d2-0e9770731432', post_wwii_un_charter_framework).
narrative_ontology:cs_drift_state('2267849d-5a32-47fa-94d2-0e9770731432', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2267849d-5a32-47fa-94d2-0e9770731432', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, international_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, international_organizations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that participate in multilateral interventions or impose sanctions, benefiting from the perceived legitimacy and burden-sharing of collective action. They gain influence and can pursue strategic interests under the umbrella of RBIO norms.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary,
    institutional, generational, mobile, global).

% Private entities that secure contracts for reconstruction, security, or humanitarian aid in post-intervention or sanctioned environments. They profit from the economic activity generated by RBIO enforcement.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_contractors, beneficiary,
    organized, biographical, arbitrage, global).

% Bodies like the UN Security Council, WTO, or ICC that codify, interpret, and authorize the application of RBIO norms. They derive their legitimacy and function from these norms, acting as their primary administrators and enforcers.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_organizations, agenda_setter,
    institutional, civilizational, constrained, global).

% States that are subject to interventions, sanctions, or other forms of RBIO enforcement. They bear the direct costs of these actions, including economic disruption, loss of sovereignty, and internal instability. Their options are limited by their relative power and the multilateral consensus against them.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states, payer,
    powerless, generational, trapped, national).

% Citizens within targeted states who suffer the humanitarian consequences of sanctions, such as shortages of food, medicine, and essential services. Their identity is tied to their national context, making exit from the effects of sanctions extremely difficult.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions, payer,
    powerless, immediate, identity_locked, local).

% States that do not actively participate in or are not directly targeted by RBIO enforcement, but observe its application. They may voice concerns about selectivity or sovereignty but generally operate within the existing institutional framework.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, non_aligned_states, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to collectively address global challenges (e.g., peace and security, human rights, economic stability) through shared norms and institutions, preventing unilateral action and promoting predictable international behavior.
% TRANSFER_FUNCTION: Transfers legitimacy and authority to multilateral institutions and intervening states for actions deemed necessary to uphold universal norms, while imposing costs (economic, political, social) on states and populations deemed to be violating these norms.
% ABSENT_VOICES: States and non-state actors who view RBIO norms as instruments of hegemonic power, or who prioritize absolute state sovereignty, are often marginalized in multilateral forums. They would argue for a more equitable and less interventionist international order.
% DISAPPEARANCE_RATIONALE: If RBIO norms vanished, the international system would likely revert to a more anarchic state, with increased unilateralism, power politics, and a breakdown of collective security mechanisms. States would lose a key framework for cooperation and conflict resolution.
% FOUNDING_PROBLEM: The problem of international anarchy, unchecked state aggression, and the need for collective security and cooperation to prevent global conflicts and address shared challenges after World War II.
% FOUNDING_PROBLEM_CORROBORATION: International organizations, most states, and international legal scholars widely corroborate that the founding problems of international peace, security, and cooperation remain live, citing ongoing conflicts, humanitarian crises, and global challenges that require collective action.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).
:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the costs imposed on targeted states and populations, which are seen as legitimate consequences of norm violation within this reading. Suppression (0.30) is moderate, as enforcement relies on multilateral consensus and institutional mechanisms, not overt coercion against all actors. Theater ratio (0.20) is low, indicating that the stated purpose of upholding norms is largely aligned with actual practice, though some performativity exists in justifying selective enforcement. The increasing trend in extractiveness and suppression over time reflects the expansion of RBIO's scope and the intensification of enforcement mechanisms since its inception.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of intervening states and international organizations, RBIO norms function as a legitimate coordination mechanism for global governance. However, from the perspective of targeted states and civilian populations, the same norms can be experienced as extractive, particularly during sanctions or interventions. The engine's per-seat classification will highlight this divergence, with beneficiaries experiencing a 'rope-like' constraint and victims a more 'tangled_rope' or 'snare-like' one, even under this liberal institutional reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states and international contractors are beneficiaries (low d) as they gain influence, legitimacy, and economic opportunities. International organizations, as agenda-setters, also benefit from their central role (low d). Targeted states and civilian populations are victims (high d) as they bear the direct costs of enforcement. Non-aligned states are observers (d near 0.5), as they are neither primary beneficiaries nor direct targets.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by emphasizing the genuine collective action problems RBIO was designed to solve. The 'capacity problem, not legitimacy problem' framing for enforcement selectivity is key to maintaining the coordination narrative, even when extraction occurs. If the capacity argument were to fail, and selectivity were proven to be driven by extractive intent, the constraint would shift towards a Snare or Tangled Rope, as the coordination justification would collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_cause,
    'Is enforcement selectivity primarily a capacity problem (as this reading claims) or a legitimacy problem (revealing extractive intent or bias)?',
    'Empirical analysis of enforcement patterns across different types of norm violations and actors, controlling for capacity. If selectivity persists despite capacity, reclassify as legitimacy-driven.',
    'If selectivity is primarily a legitimacy problem, the constraint''s effective extractiveness and suppression are higher than measured, and its classification shifts towards Tangled Rope or Snare, as the coordination justification is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_cause, empirical, 'Determining the true cause of selective enforcement of RBIO norms.').

omega_variable(
    consent_basis_vs_power_imposition,
    'Are RBIO norms truly consent-based and revisable through legitimate multilateral processes, or are they effectively imposed by powerful states through institutional path-dependency?',
    'Analysis of the actual process of norm revision and the influence of powerful actors (e.g., P5 veto power). If revision is consistently blocked by a few actors, the ''consent-based'' claim is weakened.',
    'If norms are effectively imposed, the constraint''s suppression is higher, and its classification shifts towards Tangled Rope or Snare, as the voluntariness of participation is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_basis_vs_power_imposition, conceptual, 'Assessing the true nature of consent and revisability in RBIO norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(rbio_tr_t1965, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(rbio_tr_t1985, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(rbio_be_t1965, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1965, 0.25).
narrative_ontology:measurement(rbio_be_t1985, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1985, 0.3).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement(rbio_su_t1965, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1965, 0.2).
narrative_ontology:measurement(rbio_su_t1985, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1985, 0.25).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'RBIO practice norm complex' kernel. Each reading offers a distinct interpretation of the norms' legitimacy, function, and impact, leading to different structural classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
