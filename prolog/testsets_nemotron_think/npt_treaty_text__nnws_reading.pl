% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Article VI Disarmament Obligation (NNWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint story captures the Non-Nuclear Weapon State (NNWS)
 *   reading of the NPT treaty text: Article VI imposes a binding obligation
 *   of conduct and result on Nuclear Weapon States (NWS) to pursue and
 *   conclude good-faith nuclear disarmament negotiations. Non-proliferation
 *   restraint by NNWS (Article II) is understood as conditional — purchased
 *   by NWS compliance. The reading treats Review Conference consensus
 *   pressure and the TPNW regime as the enforcement mechanisms attempting to
 *   constrain NWS behavior. Extractiveness is moderate (0.42) because NWS
 *   bear disarmament costs but retain arbitration-grade exit
 *   (reinterpretation, modernization, Article X withdrawal). Suppression is
 *   moderate (0.38) because the constraint's persistence depends on NNWS
 *   collective pressure and normative stigma, not coercive enforcement.
 *   Theater ratio is elevated (0.45) because Review Conference rituals and
 *   'action plans' increasingly substitute for measurable disarmament
 *   progress.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.42).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.38).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Article VI Disarmament Obligation (NNWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, '4312c6d5-bf95-4f0a-bcb9-bbc54e898f25').
narrative_ontology:cs_kernel_codification('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25', fixed_text).
narrative_ontology:cs_authority_grounding('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25', lineage).
narrative_ontology:cs_interpretation_layer_present('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25').
narrative_ontology:cs_reading_relation('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25', foundational, article_vi_binding_obligation_of_result).
narrative_ontology:cs_axiom_status(article_vi_binding_obligation_of_result, holdable).
narrative_ontology:cs_axiom_grounding('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25', article_vi_binding_obligation_of_result, deontological).
narrative_ontology:cs_axiom('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25', foundational, nonproliferation_restraint_conditional_on_disarmament).
narrative_ontology:cs_axiom_status(nonproliferation_restraint_conditional_on_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25', nonproliferation_restraint_conditional_on_disarmament, conventional).
narrative_ontology:cs_reference_frame('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25', npt_original_bargain_1968).
narrative_ontology:cs_drift_state('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25', post_tpnw_adoption_2017, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4312c6d5-bf95-4f0a-bcb9-bbc54e898f25', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, civil_society_disarmament_advocates).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nuclear_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, umbrella_states_nato_allies).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, article_vi_binding_obligation).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, nonproliferation_conditional_on_disarmament).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, nuclear_nonproliferation_regime_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% NNWS forego nuclear weapons development under Article II in exchange for Article VI disarmament progress by NWS. They leverage Review Conference consensus pressure and the TPNW regime to extract compliance. Their exit is constrained by security assurances, IAEA safeguards benefits, and the political cost of proliferation. They bear opportunity costs of nuclear latency but gain regime legitimacy and technical cooperation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, payer).

% NWS (US, Russia, UK, France, China) bear the Article VI disarmament obligation under this reading. They control the Review Conference agenda, set the pace of disarmament, and retain nuclear arsenals as security guarantees. Their exit options are high — they can reinterpret Article VI as aspirational, modernize arsenals, or withdraw under Article X. They extract regime management benefits while deferring disarmament costs.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, nuclear_weapon_states, agenda_setter).

% The IAEA administers safeguards verification under Article III, providing the technical basis for non-proliferation compliance. It does not adjudicate Article VI but its verification regime underpins the bargain. It benefits from institutional relevance and mandate expansion but bears implementation costs and political pressure from all sides.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, iaea_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% States parties to the TPNW and civil society coalitions (ICAN, etc.) argue Article VI requires concrete disarmament steps and timelines. They are excluded from NWS decision-making and Review Conference consensus blocks. Their identity is fused to the abolitionist frame — exit means abandoning the normative project. They exert pressure through stigma, divestment campaigns, and normative entrepreneurship.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, tpnw_advocates_civil_society, excluded,
    moderate, biographical, identity_locked, global).

% Non-nuclear NATO allies and US umbrella states (Japan, South Korea, Australia) benefit from extended deterrence while formally supporting Article VI. They constrain NNWS pressure to avoid alliance friction. Their exit is constrained by security dependencies — they cannot credibly push for disarmament that undermines their own deterrence umbrella.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, umbrella_states_nato_allies, beneficiary,
    powerful, biographical, constrained, regional).

% International lawyers, treaty scholars, and ICJ advisors analyze the textual obligation. They see the full structure: Article VI as a binding obligation of conduct and result (per ICJ 1996), the bargain structure, and the enforcement gap. They neither collect nor pay but their interpretations shape the legitimacy conditions all parties invoke.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, academic_legal_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global non-proliferation bargain: NNWS permanently forego nuclear weapons (Article II) in exchange for NWS pursuing good-faith nuclear disarmament negotiations (Article VI) and sharing peaceful nuclear technology (Article IV). The Review Conference mechanism provides periodic collective assessment.
% TRANSFER_FUNCTION: NNWS transfer the option value of nuclear weapons acquisition to NWS, receiving in return disarmament commitments, security assurances, and peaceful nuclear cooperation. NWS transfer rhetorical disarmament commitments and partial arsenal reductions while retaining modernization programs. The TPNW regime transfers normative pressure onto NWS legitimacy.
% ABSENT_VOICES: Communities affected by nuclear testing and uranium mining (Marshall Islanders, Kazakh steppe communities, Indigenous Australians, Saharan populations) — they bear intergenerational harm from the nuclear enterprise but have no seat at Review Conferences. Future generations who inherit disarmament failures and proliferation risks are structurally excluded. TPNW states parties are formally present but substantively excluded from NPT consensus dynamics.
% DISAPPEARANCE_RATIONALE: If the Article VI binding obligation reading vanished, the NPT bargain collapses: NNWS lose the legal basis for demanding disarmament, NWS lose the legitimacy cover for their arsenals, the Review Conference process loses its normative anchor, and the TPNW loses its parent-treaty reference point. The non-proliferation regime would reorganize around raw power politics or fracture into competing minilateral arrangements.
% FOUNDING_PROBLEM: Preventing horizontal nuclear proliferation among states that could technically acquire weapons, while managing the vertical proliferation of existing arsenals, in a Cold War context where 20+ states had latent nuclear capacity.
% FOUNDING_PROBLEM_CORROBORATION: The NNWS reading is corroborated by: ICJ 1996 Advisory Opinion (unanimous on Article VI binding obligation), 2000/2010 Review Conference final documents (consensus on 'unequivocal undertaking'), TPNW preamble (recalling Article VI), and NNWS collective statements at every Review Conference since 1995. NWS contest the binding character, citing 'security context' qualifiers and the absence of a deadline.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).
:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reflects the asymmetric cost structure: NWS bear the disarmament obligation (modernization costs, political capital) while NNWS bear opportunity costs of nuclear latency. The 1970-2024 trajectory shows extraction rising as NWS modernization programs expand (new warheads, delivery systems, infrastructure) while disarmament stalls. Theater rises as Review Conference 'action plans' (13 steps 2000, 64 actions 2010) become ritualized without implementation. Suppression requirement stays moderate because NNWS lack coercive leverage — their tools are consensus pressure, normative stigma, and TPNW competition, not sanctions or force.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, the same constraint reads as aspirational — disarmament is a long-term goal conditioned on security context, not a deliverable. The engine will compute this seat divergence: NWS experience lower effective extraction (they control the pace), NNWS experience higher effective extraction (they pay the non-proliferation price without receiving the disarmament return). The TPNW advocate seat experiences the constraint as a snare — their exclusion is enforced by NWS consensus control.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are structural targets (d near 1.0) — they bear the Article VI obligation, their arsenals are the extraction object, and their exit options (reinterpretation, modernization, withdrawal) are arbitration-grade but politically costly. NNWS are conditional beneficiaries (d near 0.0-0.3) — they gain regime legitimacy, technical cooperation, and security assurances, but their exit is constrained (proliferation carries severe costs). Umbrella states sit near symmetric (d ~0.5) — they benefit from extended deterrence while formally endorsing disarmament. TPNW advocates are identity-locked excluded (d ~0.8) — they bear normative costs of exclusion but cannot exit the abolitionist frame. IAEA and observers are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing horizontal proliferation) remains live — latent capacity has spread, not shrunk. But the bargain's original symmetry (horizontal restraint for vertical disarmament) has atrophied: NWS treat disarmament as aspirational, NNWS treat non-proliferation as conditional. The constraint persists because neither side can afford collapse — NWS need the non-proliferation norm, NNWS need the disarmament promise. This is a rope degrading toward tangled rope: coordination function (non-proliferation) remains genuine but extraction asymmetry (NWS modernization vs. NNWS restraint) grows. The TPNW regime is the outside pressure preventing full mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_vs_aspirational,
    'Is Article VI a binding obligation of result (requiring disarmament completion) or merely an obligation of conduct (requiring good-faith negotiations without guaranteed outcome)?',
    'ICJ 1996 Advisory Opinion held it is both; NWS practice treats it as conduct-only. Resolution requires either NWS acceptance of a disarmament timeline (unlikely) or NNWS collective withdrawal threat (high cost). The TPNW''s ''effective measures'' language tests this boundary.',
    'If conduct-only, extraction on NWS drops sharply (epsilon ~0.15) and the constraint reclassifies toward rope with minimal enforcement. If binding result, extraction rises (epsilon ~0.6+) and the constraint becomes tangled rope — genuine coordination with asymmetric extraction requiring active enforcement (which is currently absent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_binding_vs_aspirational, conceptual, 'Core interpretive ambiguity determining the constraint''s extractive structure').

omega_variable(
    enforcement_mechanism_effectiveness,
    'Can Review Conference pressure and TPNW regime competition function as effective enforcement on NWS, or are they performative rituals that NWS can absorb indefinitely?',
    'Track NWS behavioral changes correlated with Review Conference outcomes and TPNW milestones. Key test: whether NWS take concrete disarmament steps (de-alerting, fissile material cutoff, arsenal reductions) in response to normative pressure vs. strategic calculation.',
    'If enforcement is effective, the rope''s coordination function is genuine and extractiveness stabilizes. If performative, theater ratio continues rising toward piton territory — the constraint becomes a theatrical maintenance ritual with no functional disarmament output.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_effectiveness, empirical, 'Whether the constraint''s enforcement mechanisms are structurally real or ceremonial').

omega_variable(
    nnws_exit_credibility,
    'Do NNWS have credible exit options (proliferation, withdrawal, TPNW-only regime) that would discipline NWS behavior, or is their exit structurally blocked by security dependencies and great power pressure?',
    'Analyze proliferation latency thresholds, security assurance reliability, and TPNW-NPT complementarity. Case studies: South Africa (exited via disarmament), Iran (constrained by pressure), Brazil/Argentina (bilateral restraint).',
    'If NNWS exit is credible, their directionality shifts toward beneficiary (d drops) and NWS face real constraint. If exit is blocked, NNWS directionality shifts toward payer (d rises) and the constraint becomes a snare for them — paying non-proliferation costs without disarmament return.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_exit_credibility, empirical, 'Whether NNWS structural position is genuinely voluntary or coerced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_nnws_tr_t1970, npt_treaty_text__nnws_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt_nnws_tr_t1985, npt_treaty_text__nnws_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(npt_nnws_tr_t1995, npt_treaty_text__nnws_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(npt_nnws_tr_t2000, npt_treaty_text__nnws_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(npt_nnws_tr_t2010, npt_treaty_text__nnws_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(npt_nnws_tr_t2017, npt_treaty_text__nnws_reading, theater_ratio, 2017, 0.45).
narrative_ontology:measurement(npt_nnws_tr_t2024, npt_treaty_text__nnws_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(npt_nnws_be_t1970, npt_treaty_text__nnws_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(npt_nnws_be_t1985, npt_treaty_text__nnws_reading, base_extractiveness, 1985, 0.3).
narrative_ontology:measurement(npt_nnws_be_t1995, npt_treaty_text__nnws_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(npt_nnws_be_t2000, npt_treaty_text__nnws_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(npt_nnws_be_t2010, npt_treaty_text__nnws_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(npt_nnws_be_t2017, npt_treaty_text__nnws_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(npt_nnws_be_t2024, npt_treaty_text__nnws_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(npt_nnws_su_t1970, npt_treaty_text__nnws_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(npt_nnws_su_t1985, npt_treaty_text__nnws_reading, suppression_requirement, 1985, 0.3).
narrative_ontology:measurement(npt_nnws_su_t1995, npt_treaty_text__nnws_reading, suppression_requirement, 1995, 0.33).
narrative_ontology:measurement(npt_nnws_su_t2000, npt_treaty_text__nnws_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(npt_nnws_su_t2010, npt_treaty_text__nnws_reading, suppression_requirement, 2010, 0.37).
narrative_ontology:measurement(npt_nnws_su_t2017, npt_treaty_text__nnws_reading, suppression_requirement, 2017, 0.38).
narrative_ontology:measurement(npt_nnws_su_t2024, npt_treaty_text__nnws_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nnws_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, tpnw_treaty).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, fmct_negotiations).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, nuclear_modernization_programs).

% DUAL FORMULATION NOTE:
% This nnws_reading and the nws_reading decompose the single NPT treaty text into two constraints with divergent epsilon values. The nnws_reading sees Article VI as binding (epsilon 0.42); the nws_reading sees it as aspirational (epsilon ~0.15). The withdrawal_threshold_reading decomposes Article X into high-threshold (regime stability) vs low-threshold (sovereignty) variants. All three share the npt_treaty_text kernel and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nnws_reading, institutional, 0.85).
constraint_indexing:directionality_override(npt_treaty_text__nnws_reading, organized, 0.25).
constraint_indexing:directionality_override(npt_treaty_text__nnws_reading, powerful, 0.45).
constraint_indexing:directionality_override(npt_treaty_text__nnws_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
