% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo as Normative Constraint on Total War
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   The nuclear taboo reading holds that total war became normatively
 *   prohibited through a constructed taboo against nuclear use, independent
 *   of material capability. Since 1945, a normative constraint has emerged
 *   that forecloses nuclear use as a legitimate strategic option, generating
 *   enforcement mechanisms including the non-proliferation regime (NPT, IAEA
 *   safeguards), negative security assurances, no-first-use pledges, and
 *   nuclear-weapon-free zones. The constraint is not a natural law — it is
 *   maintained by active norm entrepreneurship, institutional enforcement,
 *   and the socialization of state elites. War remains materially possible
 *   (arsenals exist, delivery systems function, targeting plans are current)
 *   but normatively foreclosed. The taboo weakens if norm entrepreneurs exit
 *   (US nuclear posture shifts, treaty withdrawals) and non-nuclear powers
 *   face a different constraint structure: they are bound by the taboo while
 *   lacking the deterrent that nuclear states possess, creating an asymmetric
 *   extraction pattern.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary agenda setters and beneficiaries (institutional/arbitrage) — possess nuclear weapons, set taboo enforcement agenda, benefit from foreclosed competition
 *   - non_proliferation_regime_institutions: Agenda setters/beneficiaries (institutional/generational) — IAEA, NPT review conferences, NSG; administer enforcement machinery, derive legitimacy and resources from taboo maintenance
 *   - norm_entrepreneur_states: Beneficiaries (organized/biographical) — states like Ireland, Mexico, Austria, New Zealand, South Africa that actively promote taboo strengthening; gain normative capital and institutional influence
 *   - international_legal_order: Beneficiary (analytical/civilizational) — the taboo vindicates customary law development and institutional authority
 *   - non_nuclear_weapon_states: Payers (moderate/constrained to identity_locked) — 180+ NPT non-nuclear parties; foreclosed independent deterrent option, dependent on security assurances that may not be credible
 *   - aspirant_nuclear_states: Victims (powerless/trapped) — states seeking nuclear weapons (Iran, North Korea historically, potentially others); face severe suppression (sanctions, isolation, preventive attack threats) for pursuing the foreclosed option
 *   - states_facing_existential_threats_without_nuclear_deterrent: Victims (powerless/trapped) — states like Ukraine post-1994 Budapest Memorandum; gave up nuclear inheritance for security assurances that proved non-credible when existential threat materialized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.42).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.68).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo as Normative Constraint on Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '07d5f810-2a3a-400e-a570-575ed24465d5').
narrative_ontology:cs_kernel_codification('07d5f810-2a3a-400e-a570-575ed24465d5', formalized).
narrative_ontology:cs_authority_grounding('07d5f810-2a3a-400e-a570-575ed24465d5', extraction).
narrative_ontology:cs_interpretation_layer_present('07d5f810-2a3a-400e-a570-575ed24465d5').
narrative_ontology:cs_reading_relation('07d5f810-2a3a-400e-a570-575ed24465d5', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('07d5f810-2a3a-400e-a570-575ed24465d5', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('07d5f810-2a3a-400e-a570-575ed24465d5', foundational, nuclear_use_is_normatively_prohibited_independent_of_material_conditions).
narrative_ontology:cs_axiom_status(nuclear_use_is_normatively_prohibited_independent_of_material_conditions, holdable).
narrative_ontology:cs_axiom_grounding('07d5f810-2a3a-400e-a570-575ed24465d5', nuclear_use_is_normatively_prohibited_independent_of_material_conditions, deontological).
narrative_ontology:cs_axiom('07d5f810-2a3a-400e-a570-575ed24465d5', secondary, taboo_maintenance_requires_active_norm_entrepreneurship).
narrative_ontology:cs_axiom_status(taboo_maintenance_requires_active_norm_entrepreneurship, holdable).
narrative_ontology:cs_axiom_grounding('07d5f810-2a3a-400e-a570-575ed24465d5', taboo_maintenance_requires_active_norm_entrepreneurship, empirically_contingent).
narrative_ontology:cs_reference_frame('07d5f810-2a3a-400e-a570-575ed24465d5', post_hiroshima_normative_prohibition).
narrative_ontology:cs_drift_state('07d5f810-2a3a-400e-a570-575ed24465d5', contemporary_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('07d5f810-2a3a-400e-a570-575ed24465d5', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_institutions).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, international_legal_order).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, aspirant_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, states_facing_existential_threats_without_nuclear_deterrent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals and set the terms of the non-proliferation regime. Benefit from the taboo's foreclosure of nuclear use by others while retaining their own deterrents. Modernize arsenals while demanding non-proliferation compliance. Exit options include treaty withdrawal (US from ABM, INF, JCPOA) and nuclear posture shifts — they can reshape the constraint from inside.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, beneficiary).

% IAEA, NPT review conferences, Nuclear Suppliers Group. Administer verification, compliance, and enforcement machinery. Derive budgets, authority, and organizational legitimacy from taboo maintenance. Constrained exit: institutional survival depends on the regime's perceived relevance; they cannot easily pivot away from non-proliferation as core mission.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_institutions, beneficiary).

% States like Ireland, Mexico, Austria, New Zealand, South Africa, Indonesia that actively promote taboo strengthening (TPNW, humanitarian initiative, NPT review conference leadership). Gain normative capital, diplomatic influence, and leadership roles in multilateral institutions. Mobile exit: they could deprioritize nuclear diplomacy without existential cost, but would lose normative influence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_states, beneficiary,
    organized, biographical, mobile, global).

% The taboo vindicates the development of customary international law (nuclear use as war crime/violation of jus cogens) and strengthens institutional authority of UN, ICJ, ICC. As a non-agent proposition, it collects no rents but its vindication is a structural fact about the constraint's operation.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, international_legal_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(total_war_possibility_space__nuclear_taboo_reading, international_legal_order).

% 180+ NPT non-nuclear parties. Foreclosed independent nuclear deterrent option. Dependent on negative security assurances (NSAs) and extended deterrence from nuclear patrons — assurances that are politically conditional and legally non-binding. Constrained exit: withdrawal from NPT (Article X) triggers severe diplomatic/economic consequences and loss of nuclear cooperation benefits (technology, fuel, medicine).
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states, payer,
    moderate, biographical, constrained, global).

% States pursuing or suspected of pursuing nuclear weapons (Iran, North Korea, historically Libya, Syria, Iraq). Face comprehensive suppression: UN sanctions, export controls, financial isolation, cyber sabotage, preventive attack threats, diplomatic pariah status. Trapped exit: the constraint actively prevents their acquisition while nuclear states retain arsenals; no credible pathway to recognized nuclear status exists within the regime.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, aspirant_nuclear_states, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, aspirant_nuclear_states, payer).

% States that gave up nuclear capabilities/inheritance for security assurances that proved non-credible (Ukraine post-1994 Budapest Memorandum, potentially Kazakhstan/Belarus). Existentially threatened by nuclear-armed adversary with no credible extended deterrence. Identity-locked: their sovereignty narrative is bound to the assurances they received; admitting the assurances failed is politically existential. Cannot develop deterrent (NPT, suppression) and cannot rely on assurances.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, states_facing_existential_threats_without_nuclear_deterrent, payer,
    powerless, immediate, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear use and limits proliferation through a shared normative prohibition, creating stability in a system where material capability for total war exists. Solves the coordination problem of mutual restraint without requiring perfect verification of material capabilities.
% TRANSFER_FUNCTION: Moves the strategic option of independent nuclear deterrent from non-nuclear states to nuclear weapon states. Non-nuclear states transfer autonomy over ultimate deterrence to nuclear patrons in exchange for security assurances; nuclear states transfer nothing material but accept inspection/transparency obligations (partially performed).
% ABSENT_VOICES: Populations in non-nuclear states facing existential threats without credible security guarantees (e.g., Ukrainian civilians post-2014/2022); future generations who inherit the taboo's distributive consequences; states that would pursue nuclear deterrence if not suppressed. They are excluded from the NPT bargain's authorship and the taboo's maintenance coalition.
% DISAPPEARANCE_RATIONALE: If the taboo vanished overnight: nuclear use would become a thinkable strategic option again; proliferation cascades would likely follow as non-nuclear states seek independent deterrents; the NPT regime would collapse; nuclear weapon states would face a radically different strategic environment with 20-30 nuclear-armed states instead of 9; crisis stability would degrade without the normative firewall.
% FOUNDING_PROBLEM: The founding problem was the demonstrated reality of nuclear use (Hiroshima/Nagasaki) and the recognized risk that unconstrained nuclear proliferation would make nuclear war probable. The taboo was constructed to make nuclear use normatively unthinkable and to channel nuclear technology into peaceful uses under verification.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states and regime institutions attest the problem remains live (ongoing proliferation risks, modernization programs). Non-nuclear states and disarmament advocates attest the problem has shifted: the taboo now primarily preserves nuclear monopoly while the disarmament obligation (NPT Art. VI) is unfulfilled. ICJ 1996 Advisory Opinion, humanitarian initiative conferences, and TPNW negotiations corroborate the shifted-function reading from outside the primary beneficiary set.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the taboo's asymmetric cost: non-nuclear states foreclose a strategic option (independent deterrent) that nuclear states retain. Suppression (0.68) is high because the constraint actively prevents proliferation through sanctions, interdiction, preventive war threats, and institutional exclusion — not merely through normative suasion. Theater ratio (0.55) is elevated: much enforcement activity (IAEA inspections, NPT review conferences, diplomatic pressure) performs taboo maintenance while the core asymmetry (nuclear monopoly) persists. The extractiveness trajectory rose through the Cold War (NPT 1968, review conferences), dipped post-Cold War (disarmament optimism), and rose again after 1998 Indian/Pakistani tests and 2000s proliferation crises. Theater ratio tracks this: low initially (genuine coordination on non-use), rising as enforcement bureaucratized and the disarmament obligation (NPT Art. VI) became performative.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapon states experience the constraint as coordination (rope/scaffold): they gain stability, non-use assurance, and regime management authority. Non-nuclear states experience it as extraction (snare/tangled_rope): they pay the cost of foreclosed deterrent while nuclear states modernize arsenals. Aspirant states experience it as pure snare: total suppression with no offsetting benefit. The engine computes per-seat types from these structural positions — the claimed tangled_rope reflects the aggregate structure where genuine coordination (non-use stability) coexists with asymmetric extraction (nuclear monopoly maintenance).
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states (agenda setters) are structural beneficiaries: they collect security benefits from taboo (non-use by others) while retaining their own arsenals — d near 0.15. Non-proliferation institutions are beneficiaries: they derive budget, authority, and legitimacy from enforcement — d near 0.20. Norm entrepreneur states are beneficiaries: they gain normative influence — d near 0.25. Non-nuclear weapon states are payers: they bear foreclosed option costs and dependency on unreliable security assurances — d near 0.70. Aspirant states are full targets: they face maximal suppression for pursuing the foreclosed option — d near 0.95. Existentially threatened non-nuclear states are trapped targets: they cannot exit the constraint (no nuclear option, unreliable assurances) — d near 0.90 with identity_locked exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nuclear use and limiting proliferation) remains live but the arrangement has accumulated extraction: nuclear weapon states retain arsenals while demanding non-proliferation from others; disarmament obligation (NPT Art. VI) is performative; the taboo now functions to preserve nuclear hierarchy. The constraint is a tangled rope because it retains genuine coordination (non-use stability, crisis management) while extracting asymmetric compliance from non-nuclear states. It is not a pure snare because non-nuclear states do receive security benefits (non-use assurance, negative security assurances) — but these are diminishing relative to the foreclosed option value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_vs_deterrence_causal_weight,
    'How much of the observed non-use of nuclear weapons since 1945 is attributable to normative taboo versus material deterrence?',
    'Counterfactual analysis of near-use crises (Cuban Missile Crisis, 1973 Yom Kippur War, 1999 Kargil War, 2017 North Korea crisis) comparing decision-makers'' stated reasoning against structural incentives; archival research on internal deliberations.',
    'If deterrence explains most non-use, the taboo is epiphenomenal — the constraint''s extractiveness is lower and its type shifts toward rope/coordination. If taboo is independently causal, the constraint carries genuine normative extraction (suppressing strategic options that material capability alone would permit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_causal_weight, empirical, 'Causal attribution of nuclear non-use: normative taboo vs material deterrence').

omega_variable(
    norm_entrepreneur_dependency,
    'Does the taboo''s persistence depend on active maintenance by a small cohort of norm entrepreneur states and institutions, or has it achieved self-sustaining customary status?',
    'Track taboo rhetoric and enforcement behavior following US nuclear posture reviews (2002, 2010, 2018, 2022) and withdrawal from arms control treaties; measure non-proliferation regime cohesion after norm entrepreneur exits.',
    'If entrepreneur-dependent, the constraint is a scaffold masquerading as a tangled rope — its enforcement machinery requires continuous political investment. If self-sustaining, the taboo has Mountain-like resilience despite being constructed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_dependency, empirical, 'Whether nuclear taboo requires active norm entrepreneurship or has achieved autonomous customary status').

omega_variable(
    non_nuclear_state_constraint_asymmetry,
    'Do non-nuclear weapon states experience the taboo as a genuine coordination benefit (security from nuclear use) or as asymmetric extraction (foreclosed deterrent option while nuclear states retain theirs)?',
    'Analyze non-nuclear weapon state positions in NPT review conferences, UNGA First Committee votes, and regional security architectures; track security assurance seeking behavior (extended deterrence requests, security guarantees).',
    'If experienced as net benefit, the constraint functions as rope for non-nuclear states. If experienced as extraction, the constraint is a snare for this seat — the coordination story is cover for locking in nuclear monopoly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_nuclear_state_constraint_asymmetry, conceptual, 'Asymmetric experience of nuclear taboo by non-nuclear weapon states').

omega_variable(
    kernel_reading_identity,
    'Is the nuclear taboo reading a distinct constraint from deterrence_equilibrium_reading and space_contraction_reading, or an interpretive layer on a shared kernel?',
    'Map the structural differences: deterrence_reading predicts use becomes probable if mutual vulnerability erodes; taboo_reading predicts use remains improbable even if vulnerability erodes, unless norm entrepreneurs exit; space_contraction_reading predicts the option is cognitively unavailable. These generate different extraction/suppression profiles for the same material referent.',
    'Confirms this reading instantiates a separate constraint story with its own ε, beneficiaries, and classification — as required by ε-invariance. The kernel_id total_war_possibility_space is the shared referent; each reading is a distinct constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: this constraint as one reading of the total_war_possibility_space kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_tr_t1968, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1968, 0.35).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_tr_t1985, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1985, 0.52).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_tr_t1995, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1995, 0.48).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_tr_t2010, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2010, 0.58).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_tr_t2022, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2022, 0.55).

% Extraction over time
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_be_t1968, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1968, 0.32).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_be_t1985, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1985, 0.41).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_be_t1995, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_be_t2010, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_be_t2022, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2022, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.25).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_su_t1968, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1968, 0.65).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_su_t1985, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1985, 0.72).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_su_t1995, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_su_t2010, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(total_war_possibility_space__nuclear_taboo_reading_su_t2022, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2022, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_enforcement).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, negative_security_assurances_framework).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_free_zones).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, no_first_use_pledges).

% DUAL FORMULATION NOTE:
% This reading (nuclear_taboo_reading) and deterrence_equilibrium_reading share the same kernel (total_war_possibility_space) but author different ε: taboo_reading ε=0.42 (normative foreclosure with asymmetric extraction); deterrence_reading ε≈0.15 (coordination via mutual vulnerability, minimal extraction). They are not the same constraint measured differently — they are distinct structural claims about what prevents total war. The taboo reading's enforcement machinery (NPT, IAEA, sanctions) is the extraction mechanism the deterrence reading lacks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, institutional, 0.15).
constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, organized, 0.25).
constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, moderate, 0.7).
constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
