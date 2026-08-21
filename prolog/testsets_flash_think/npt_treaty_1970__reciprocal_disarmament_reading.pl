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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Article VI as Reciprocal Disarmament Obligation
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint is the 'reciprocal disarmament' reading of the NPT
 *   (npt_treaty_1970), emphasizing Article VI as a binding legal obligation
 *   with temporal urgency, and viewing horizontal and vertical
 *   nonproliferation as a reciprocal bargain. This contrasts with the
 *   'oligopoly enforcement' reading (oligopoly_enforcement_reading) which
 *   prioritizes horizontal nonproliferation and views Article VI as
 *   aspirational, and the 'withdrawal sovereignty' reading
 *   (withdrawal_sovereignty_reading) which emphasizes the right to withdraw
 *   based on security concerns.
 *
 * KEY AGENTS:
 *   - Nuclear Weapon States (institutional/constrained) — agenda-setters and payers of disarmament obligation
 *   - Non-Nuclear Weapon States Coalition (organized/constrained) — beneficiaries of disarmament, payers of non-proliferation
 *   - International Atomic Energy Agency (institutional/analytical) — verifier of horizontal non-proliferation, observer of Article VI gap
 *   - Non-Nuclear Weapon States Seeking Weapons (powerless/trapped) — primary targets of suppression, excluded from the bargain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.8).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.75).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI as Reciprocal Disarmament Obligation").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '99a9030a-3c6c-4b8d-b910-92e12ff6c488').
narrative_ontology:cs_kernel_codification('99a9030a-3c6c-4b8d-b910-92e12ff6c488', fixed_text).
narrative_ontology:cs_authority_grounding('99a9030a-3c6c-4b8d-b910-92e12ff6c488', lineage).
narrative_ontology:cs_interpretation_layer_present('99a9030a-3c6c-4b8d-b910-92e12ff6c488').
narrative_ontology:cs_reading_relation('99a9030a-3c6c-4b8d-b910-92e12ff6c488', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('99a9030a-3c6c-4b8d-b910-92e12ff6c488', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('99a9030a-3c6c-4b8d-b910-92e12ff6c488', foundational, article_vi_binding_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('99a9030a-3c6c-4b8d-b910-92e12ff6c488', article_vi_binding_obligation, deontological).
narrative_ontology:cs_axiom('99a9030a-3c6c-4b8d-b910-92e12ff6c488', foundational, horizontal_vertical_nonproliferation_reciprocal).
narrative_ontology:cs_axiom_status(horizontal_vertical_nonproliferation_reciprocal, holdable).
narrative_ontology:cs_axiom_grounding('99a9030a-3c6c-4b8d-b910-92e12ff6c488', horizontal_vertical_nonproliferation_reciprocal, conventional).
narrative_ontology:cs_reference_frame('99a9030a-3c6c-4b8d-b910-92e12ff6c488', original_reciprocal_bargain_1970).
narrative_ontology:cs_drift_state('99a9030a-3c6c-4b8d-b910-92e12ff6c488', contemporary_nonproliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('99a9030a-3c6c-4b8d-b910-92e12ff6c488', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, global_security_regime).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_seeking_weapons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As original signatories and possessors of nuclear weapons, they set the terms of the NPT and benefit from horizontal non-proliferation. However, this reading casts them as bearing the binding obligation under Article VI to disarm, which they often resist, leading to constrained strategic autonomy and modernization.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, payer).

% They forgo nuclear weapons (horizontal non-proliferation) and benefit from the theoretical disarmament of NWS (vertical non-proliferation). They bear the costs of non-proliferation but gain normative leverage to demand NWS compliance with Article VI. Their exit options are constrained by security dilemmas and potential sanctions.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, payer).

% Responsible for verifying compliance with horizontal non-proliferation obligations through safeguards. From this reading's perspective, its mandate is incomplete due to the lack of a verification mechanism for NWS disarmament under Article VI, highlighting a structural injustice.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_atomic_energy_agency, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, international_atomic_energy_agency, observer).

% The abstract entity representing the collective benefit of reduced nuclear proliferation risk and the eventual elimination of nuclear weapons. It is the ultimate beneficiary of the reciprocal bargain.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, global_security_regime, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__reciprocal_disarmament_reading, global_security_regime).

% These states are the primary targets of the horizontal non-proliferation regime, facing severe international suppression, sanctions, and potential military action if they pursue nuclear weapons. They are excluded from the NPT's reciprocal bargain and bear its full extractive force.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_seeking_weapons, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_seeking_weapons, excluded).

% Advocacy groups and NGOs that monitor NPT compliance, particularly pushing for NWS disarmament under Article VI. They provide critical analysis and exert public pressure, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_civil_society, observer,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, diffuse).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the spread of nuclear weapons to additional states (horizontal nonproliferation) and aims for the eventual elimination of existing arsenals (vertical nonproliferation), creating a more stable global security environment through a reciprocal bargain.
% TRANSFER_FUNCTION: Transfers the right to possess nuclear weapons from non-nuclear-weapon states to nuclear-weapon states (de facto), and theoretically transfers the obligation to disarm from nuclear-weapon states to the global community. It also transfers security assurances to non-nuclear-weapon states in exchange for non-proliferation.
% ABSENT_VOICES: States that withdrew from the NPT or never joined, and those actively seeking nuclear weapons, are structurally excluded. They would argue the treaty is discriminatory and that their security requires nuclear deterrence, but their voices are suppressed by the regime's enforcement mechanisms.
% DISAPPEARANCE_RATIONALE: If the NPT vanished, the global nonproliferation norm would collapse, leading to rapid proliferation by many states, drastically increasing global instability and the risk of nuclear conflict. The entire international security architecture would reorganize.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent an uncontrolled arms race among many states, while acknowledging the existing nuclear powers and committing them to eventual disarmament.
% FOUNDING_PROBLEM_CORROBORATION: The UN Security Council, international legal scholars, independent think tanks (e.g., Stockholm International Peace Research Institute - SIPRI), and ongoing non-proliferation advocacy groups corroborate the continued relevance of the founding problem, particularly the unfulfilled disarmament aspect.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates horizontal non-proliferation (benefiting global security) but simultaneously extracts from NNWS by denying them nuclear weapons while NWS retain and modernize theirs. The high extractiveness (0.8) reflects the perceived injustice of this asymmetry, amplified over time as NWS disarmament lagged. Suppression (0.75) is high due to robust enforcement against horizontal proliferation. Theater ratio (0.4) reflects the gap between NWS rhetorical commitment to Article VI and their actual disarmament efforts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of NNWS, the constraint operates as an increasingly extractive mechanism due to the NWS's failure to uphold their Article VI obligations. From the NWS perspective, it is a successful coordination mechanism for horizontal non-proliferation, with Article VI being a long-term aspiration rather than an urgent, binding obligation. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear Weapon States are beneficiaries of horizontal non-proliferation (low d) but targets of the Article VI disarmament obligation (high d), leading to a complex, often contradictory, structural position. Non-Nuclear Weapon States are targets of horizontal non-proliferation (high d) but beneficiaries of the theoretical vertical disarmament (low d). The 'reciprocal bargain' framing means their directionality is heavily influenced by the perceived balance of compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's core mandate of reciprocal disarmament is still live, but the persistent failure of NWS to fulfill Article VI creates a significant risk of mandatrophy. If NNWS increasingly perceive the bargain as broken, the legitimacy and effectiveness of the entire NPT regime could erode, leading to a shift towards a Snare or Piton if the coordination function atrophies while extraction persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nws_disarmament_commitment_ambiguity,
    'Is the Nuclear Weapon States'' commitment to Article VI genuine and time-bound, or primarily rhetorical and aspirational?',
    'Concrete, verifiable steps towards disarmament, including reductions in arsenals, cessation of modernization, and negotiation of a verifiable disarmament treaty.',
    'If commitment is proven genuine, the perceived extractiveness from NNWS would decrease, strengthening the ''rope'' aspect. If rhetorical, the ''snare'' aspect would be amplified, highlighting the structural injustice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_disarmament_commitment_ambiguity, empirical, 'Ambiguity regarding NWS commitment to Article VI disarmament.').

omega_variable(
    nnws_security_dilemma_resolution,
    'Can Non-Nuclear Weapon States genuinely achieve security without nuclear weapons if Nuclear Weapon States retain and modernize theirs?',
    'Empirical evidence from security studies, regional conflict analysis, and the effectiveness of security assurances provided by NWS.',
    'If NNWS cannot achieve security, their ''trapped'' exit option is reinforced, increasing the constraint''s effective suppression and extractiveness. If security can be achieved, the coordination function is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nnws_security_dilemma_resolution, empirical, 'Whether NNWS security can be assured without nuclear weapons.').

omega_variable(
    enforcement_gap_as_injustice,
    'Is the lack of a verifiable enforcement mechanism for Article VI a structural injustice inherent to the NPT, or merely an implementation detail to be resolved?',
    'International legal consensus, state practice, and the establishment of a robust, universally accepted verification regime for NWS disarmament.',
    'If a structural injustice, the constraint''s classification leans more heavily towards Snare or Tangled Rope, emphasizing the asymmetric extraction. If an implementation detail, it suggests the potential for the constraint to evolve towards a more equitable Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_as_injustice, conceptual, 'Framing of the Article VI verification gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2020, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapons_modernization_programs).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, iran_nuclear_deal).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, north_korea_nuclear_program).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT (npt_treaty_1970) kernel, focusing on Article VI as a binding, reciprocal disarmament obligation. It is linked to its sibling readings, 'oligopoly_enforcement_reading' and 'withdrawal_sovereignty_reading', which emphasize different aspects of the treaty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
