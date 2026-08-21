% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__reciprocal_disarmament_reading, []).

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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Treaty (1970) - Reciprocal Disarmament Reading
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'reciprocal disarmament' reading
 *   of the 1970 Nuclear Non-Proliferation Treaty (NPT). This reading
 *   emphasizes Article VI as a binding legal obligation for Nuclear Weapon
 *   States (NWS) to pursue disarmament with temporal urgency, viewing
 *   horizontal (NNWS not acquiring nukes) and vertical (NWS disarming)
 *   non-proliferation as a reciprocal bargain. From this perspective, the
 *   NWS's continued maintenance and modernization of arsenals, coupled with
 *   the lack of verification for Article VI, constitutes a structural
 *   injustice and a failure of the treaty's core promise.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Agenda-setter/Beneficiary (institutional/constrained) — maintain arsenals, benefit from horizontal non-proliferation, but are victims of the Article VI obligation.
 *   - non_nuclear_weapon_states_coalition: Payer/Beneficiary (organized/constrained) — commit to non-proliferation, but bear the cost of NWS non-disarmament.
 *   - international_atomic_energy_agency: Agenda-setter (institutional/constrained) — verifies NNWS compliance, but not NWS disarmament.
 *   - global_civil_society: Observer (moderate/analytical) — advocates for disarmament.
 *   - treaty_depositary_states: Agenda-setter (institutional/mobile) — administer treaty, but are also NWS.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.78).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.85).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Treaty (1970) - Reciprocal Disarmament Reading").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '998b6812-6f09-4420-9b4f-4f56bdf36b02').
narrative_ontology:cs_kernel_codification('998b6812-6f09-4420-9b4f-4f56bdf36b02', fixed_text).
narrative_ontology:cs_authority_grounding('998b6812-6f09-4420-9b4f-4f56bdf36b02', lineage).
narrative_ontology:cs_interpretation_layer_present('998b6812-6f09-4420-9b4f-4f56bdf36b02').
narrative_ontology:cs_reading_relation('998b6812-6f09-4420-9b4f-4f56bdf36b02', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('998b6812-6f09-4420-9b4f-4f56bdf36b02', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('998b6812-6f09-4420-9b4f-4f56bdf36b02', foundational, article_vi_binding_and_urgent).
narrative_ontology:cs_axiom_status(article_vi_binding_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('998b6812-6f09-4420-9b4f-4f56bdf36b02', article_vi_binding_and_urgent, deontological).
narrative_ontology:cs_axiom('998b6812-6f09-4420-9b4f-4f56bdf36b02', foundational, horizontal_vertical_nonproliferation_linked).
narrative_ontology:cs_axiom_status(horizontal_vertical_nonproliferation_linked, holdable).
narrative_ontology:cs_axiom_grounding('998b6812-6f09-4420-9b4f-4f56bdf36b02', horizontal_vertical_nonproliferation_linked, conventional).
narrative_ontology:cs_reference_frame('998b6812-6f09-4420-9b4f-4f56bdf36b02', original_reciprocal_bargain).
narrative_ontology:cs_drift_state('998b6812-6f09-4420-9b4f-4f56bdf36b02', contemporary_nonproliferation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('998b6812-6f09-4420-9b4f-4f56bdf36b02', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, global_security_regime).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize their nuclear arsenals, benefiting from the non-proliferation of other states (horizontal non-proliferation). They are also nominally bound by Article VI to pursue disarmament, which constrains their strategic autonomy, making them victims of the obligation itself from this reading's perspective.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, beneficiary).

% Commit not to acquire nuclear weapons (horizontal non-proliferation), benefiting from the reduced risk of proliferation. However, they bear the cost of the Nuclear Weapon States' (NWS) failure to disarm, experiencing this as a structural injustice and a security imbalance. They are victims of the NWS's non-compliance with Article VI.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, beneficiary).

% Verifies compliance with non-proliferation obligations by Non-Nuclear Weapon States (NNWS), but lacks a mandate to verify NWS disarmament under Article VI. Its mandate is strengthened by the horizontal non-proliferation aspect, but its inability to enforce vertical disarmament highlights the regime's asymmetry.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_atomic_energy_agency, agenda_setter,
    institutional, biographical, constrained, global).

% Advocates for full implementation of Article VI and nuclear disarmament. They highlight the moral and legal imperative for NWS to disarm and the inherent unfairness of the current regime.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, global_civil_society, observer,
    moderate, generational, analytical, global).

% The United States, United Kingdom, and Russia serve as depositaries of the NPT. They administer the treaty but are also Nuclear Weapon States, creating a conflict of interest in enforcing Article VI against themselves.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, treaty_depositary_states, agenda_setter,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global norm against nuclear weapons proliferation, coordinating states' commitments to prevent the spread of nuclear weapons (horizontal non-proliferation) and to pursue nuclear disarmament (vertical non-proliferation) as a reciprocal bargain for global security.
% TRANSFER_FUNCTION: Transfers security assurances (from horizontal non-proliferation) to Non-Nuclear Weapon States (NNWS) in exchange for their commitment not to acquire nuclear weapons. It also implicitly transfers the burden of disarmament from Nuclear Weapon States (NWS) to NNWS by maintaining the status quo of NWS arsenals.
% ABSENT_VOICES: States that have not joined the NPT (e.g., India, Pakistan, Israel, North Korea) are absent from the internal debate, and their nuclear capabilities challenge the regime's universality and the NWS's claims of security through deterrence.
% DISAPPEARANCE_RATIONALE: The NPT is a foundational pillar of the global security architecture. Its disappearance would likely lead to a rapid increase in horizontal proliferation, a breakdown of arms control, and a significantly more dangerous and unstable world, as the reciprocal bargain would collapse.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent the uncontrolled spread of nuclear weapons, while also committing existing nuclear powers to eventual disarmament, thereby creating a reciprocal bargain for global security.
% FOUNDING_PROBLEM_CORROBORATION: Non-Nuclear Weapon States, global civil society, and many international legal scholars corroborate the urgency and binding nature of Article VI and the failure of NWS to meet their obligations. NWS often emphasize the continued live status of horizontal non-proliferation, while downplaying the urgency of vertical disarmament. Independent academic analysis supports the view that the vertical disarmament problem remains largely unsolved.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because NWS maintain their arsenals and strategic leverage, effectively extracting security and compliance from NNWS without fulfilling their own reciprocal obligation. Suppression is very high (0.85) for NNWS, who face severe international consequences for proliferation, while NWS face minimal enforcement for non-compliance with Article VI. Theater ratio is high (0.60) as disarmament talks often serve more to maintain the appearance of progress than to achieve substantive reductions, particularly in the later part of the interval. The accessibility collapse for NNWS to acquire nuclear weapons is near total (0.90), while NWS maintain their options. Resistance from NNWS and civil society is substantial (0.70).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Nuclear Weapon States (NWS), the NPT is largely a success for preventing horizontal proliferation, and Article VI is an aspirational goal. From the perspective of Non-Nuclear Weapon States (NNWS) and global civil society, the NPT is a failed reciprocal bargain, with NWS extracting compliance without fulfilling their own obligations. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The NWS are beneficiaries of the horizontal non-proliferation aspect (low d), but are also targets of the Article VI disarmament obligation (high d, as it constrains their strategic autonomy). The NNWS are targets of the non-proliferation regime (high d, as they forgo nuclear weapons), but are beneficiaries of the promise of disarmament (low d). This complex, reciprocal relationship, where both parties are simultaneously beneficiaries and victims of different aspects of the same constraint, is characteristic of a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate for NWS disarmament under Article VI has substantially atrophied, with the 'temporal urgency' of the obligation largely ignored. However, the constraint persists as a Tangled Rope because NWS benefit from the horizontal non-proliferation it enforces, and NNWS fear the consequences of withdrawal or proliferation. The theatrical maintenance of disarmament talks allows the NWS to avoid full compliance while maintaining the regime's legitimacy for NNWS. The enforcement gap for Article VI is not an implementation detail but a structural injustice from this reading's perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nws_disarmament_verifiability,
    'Is Article VI disarmament genuinely verifiable with current technologies and political will, or is the lack of a verification regime a convenient excuse for inaction by NWS?',
    'Development and implementation of a robust, intrusive, and universally accepted verification regime for nuclear disarmament, or a clear demonstration of its technical impossibility.',
    'If verifiable, the NWS''s inaction is purely a matter of political will, increasing the constraint''s effective extractiveness. If genuinely unverifiable, the constraint''s structure is fundamentally flawed, requiring re-evaluation of Article VI''s feasibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_disarmament_verifiability, empirical, 'Ambiguity regarding the technical and political feasibility of verifying NWS disarmament.').

omega_variable(
    nws_strategic_autonomy_cost,
    'Is the ''cost'' to NWS strategic autonomy from disarmament a genuine, irreducible security concern, or a justification for maintaining power and status in the international system?',
    'Independent security assessments that model global security outcomes under various disarmament scenarios, decoupled from NWS national interests, or a shift in NWS strategic doctrines that prioritizes collective security over individual deterrence.',
    'If primarily a justification for power, the constraint''s effective extractiveness is higher, as the NWS''s ''victim'' status under Article VI is largely performative. If a genuine security concern, the path to disarmament is more complex, requiring alternative security guarantees.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_strategic_autonomy_cost, conceptual, 'Whether NWS''s resistance to disarmament is driven by genuine security needs or power politics.').

omega_variable(
    reciprocity_enforceability,
    'How can the reciprocal bargain of the NPT be enforced when Nuclear Weapon States (NWS) hold disproportionate power and control the enforcement mechanisms for horizontal non-proliferation?',
    'Establishment of an independent, universally mandated body with authority to verify and enforce both horizontal and vertical non-proliferation, or a collective action by NNWS to impose costs on NWS non-compliance.',
    'If enforceability remains asymmetric, the constraint continues to operate as a Tangled Rope with high extraction. If a more symmetric enforcement mechanism emerges, the constraint could shift towards a more equitable Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_enforceability, preference, 'The challenge of enforcing reciprocity in an asymmetric power structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2010, 0.55).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2020, 0.6).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2020, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, treaty_on_the_prohibition_of_nuclear_weapons).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, comprehensive_nuclear_test_ban_treaty).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, iaea_safeguards_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT Treaty (1970) kernel, emphasizing Article VI as a binding reciprocal disarmament obligation. It differs from the 'oligopoly enforcement' and 'withdrawal sovereignty' readings by its focus on NWS disarmament as a core, urgent requirement for regime legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
