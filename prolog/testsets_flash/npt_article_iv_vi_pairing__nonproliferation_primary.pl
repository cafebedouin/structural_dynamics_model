% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV/VI Pairing (Nonproliferation Primary Reading)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'nonproliferation primary' reading of the
 *   Nuclear Non-Proliferation Treaty (NPT), where the core obligation is to
 *   prevent horizontal proliferation (Article IV conditional on Article III
 *   verification), while Article VI (disarmament) is treated as an
 *   aspirational, non-justiciable goal. Authority for this reading derives
 *   from the security interests of nuclear weapon states. This interpretation
 *   stabilizes a two-tier nuclear order, with nuclear weapon states as
 *   permanent beneficiaries and non-nuclear weapon states as perpetual
 *   restraint-bearers. The constraint is claimed as a snare due to its high
 *   extraction and suppression, despite the NPT's original framing as a
 *   'grand bargain' (a rope).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.85).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.92).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing (Nonproliferation Primary Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '270021d1-fb0b-4abb-9ee2-77ed7e072700').
narrative_ontology:cs_kernel_codification('270021d1-fb0b-4abb-9ee2-77ed7e072700', fixed_text).
narrative_ontology:cs_authority_grounding('270021d1-fb0b-4abb-9ee2-77ed7e072700', extraction).
narrative_ontology:cs_interpretation_layer_present('270021d1-fb0b-4abb-9ee2-77ed7e072700').
narrative_ontology:cs_reading_relation('270021d1-fb0b-4abb-9ee2-77ed7e072700', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_reading_relation('270021d1-fb0b-4abb-9ee2-77ed7e072700', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('270021d1-fb0b-4abb-9ee2-77ed7e072700', foundational, horizontal_proliferation_is_paramount_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_paramount_threat, holdable).
narrative_ontology:cs_axiom_grounding('270021d1-fb0b-4abb-9ee2-77ed7e072700', horizontal_proliferation_is_paramount_threat, empirically_contingent).
narrative_ontology:cs_axiom('270021d1-fb0b-4abb-9ee2-77ed7e072700', foundational, article_vi_is_aspirational_not_justiciable).
narrative_ontology:cs_axiom_status(article_vi_is_aspirational_not_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('270021d1-fb0b-4abb-9ee2-77ed7e072700', article_vi_is_aspirational_not_justiciable, conventional).
narrative_ontology:cs_reference_frame('270021d1-fb0b-4abb-9ee2-77ed7e072700', nuclear_weapon_state_security_paradigm).
narrative_ontology:cs_drift_state('270021d1-fb0b-4abb-9ee2-77ed7e072700', contemporary_international_relations, gap(stable, minor, true)).
narrative_ontology:cs_created_at('270021d1-fb0b-4abb-9ee2-77ed7e072700', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, global_disarmament_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states interpret the NPT as primarily a nonproliferation instrument, where their security interests justify maintaining their arsenals and controlling access to nuclear technology. They benefit from the two-tier order and the non-justiciability of Article VI.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These states bear the primary burden of nonproliferation, accepting intrusive verification under Article III and foregoing nuclear weapons development, while receiving limited and conditional access to peaceful nuclear technology under Article IV. They are denied the security guarantees of nuclear weapons.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% The International Atomic Energy Agency is tasked with verifying compliance with Article III, ensuring that peaceful nuclear programs are not diverted to weapons. Its authority is derived from the NPT, but its mandate is shaped by the interpretations of its member states, particularly the nuclear weapon states.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea, agenda_setter,
    institutional, civilizational, constrained, global).

% These groups and states actively campaign for nuclear disarmament, viewing the NPT as a step towards a nuclear-weapon-free world. They are frustrated by the perceived lack of progress on Article VI and the perpetuation of the two-tier system, bearing the cost of unfulfilled promises.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, global_disarmament_advocates, payer,
    moderate, generational, identity_locked, global).

% States party to the TPNW explicitly prohibit nuclear weapons, challenging the legitimacy of the NPT's two-tier structure. They are excluded from the NPT's core decision-making processes and their interpretation of disarmament is actively resisted by nuclear weapon states.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, treaty_on_the_prohibition_of_nuclear_weapons_states, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global nonproliferation regime by establishing a framework for states to forgo nuclear weapons in exchange for peaceful nuclear technology, under international verification.
% TRANSFER_FUNCTION: Transfers the burden of nuclear restraint and verification from nuclear weapon states to non-nuclear weapon states, in exchange for conditional access to peaceful nuclear technology.
% ABSENT_VOICES: States and civil society groups advocating for immediate and complete nuclear disarmament (e.g., TPNW states) are largely excluded from the NPT's core interpretive and enforcement mechanisms, where their views on Article VI's binding nature are dismissed.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the global nonproliferation regime would face immediate and profound challenges. Non-nuclear weapon states would likely reconsider their commitments, potentially leading to horizontal proliferation. The existing security architecture, heavily reliant on the NPT's framework, would destabilize.
% FOUNDING_PROBLEM: The original problem was to prevent the spread of nuclear weapons beyond the initial five nuclear powers, while allowing for the peaceful use of nuclear energy.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states and the IAEA consistently attest that horizontal proliferation remains a live and critical threat, justifying the continued emphasis on Article IV and III. Non-nuclear weapon states, while acknowledging proliferation risks, also highlight the live problem of vertical proliferation and the lack of disarmament progress.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because non-nuclear weapon states bear significant costs (verification, foregone security options) without reciprocal disarmament from nuclear weapon states. Suppression is very high (0.92) as the regime actively prevents non-nuclear weapon states from acquiring weapons, while nuclear weapon states' arsenals are effectively immune from enforcement. Theater ratio is high (0.65) because the disarmament rhetoric of Article VI is largely performative, masking the actual perpetuation of the two-tier system. The increasing extractiveness and suppression over time reflect the hardening of this interpretation and the growing gap between nonproliferation and disarmament.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapon states perceive this as a necessary and effective regime for global stability (a rope or even a mountain of geopolitical reality). Non-nuclear weapon states and disarmament advocates experience it as an increasingly unfair and extractive snare, perpetuating an imbalance of power and security. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are clear beneficiaries (d=0.0-0.1) as they maintain their arsenals and control the nonproliferation agenda. Non-nuclear weapon states are targets (d=0.9-1.0) as they bear the costs of verification and restraint. Global disarmament advocates are also targets (d=0.8-0.9) as their goals are systematically frustrated. The IAEA, while an institutional actor, is constrained by the nuclear weapon states' interpretation, placing its effective directionality closer to symmetric but with a bias towards the agenda-setters.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability,
    'Is Article VI of the NPT legally justiciable and enforceable, or is it purely aspirational?',
    'International Court of Justice ruling on a state''s non-compliance with Article VI, or a new treaty explicitly defining enforcement mechanisms for disarmament.',
    'If justiciable, the extractiveness from non-nuclear weapon states would decrease, and the constraint might shift towards a tangled_rope or even a rope, as reciprocal obligations become enforceable. If purely aspirational, the snare classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_justiciability, conceptual, 'Ambiguity regarding the legal enforceability of NPT Article VI.').

omega_variable(
    security_interest_vs_treaty_obligation,
    'Does the security interest of nuclear weapon states legitimately override their Article VI treaty obligations, or is this a self-serving interpretation?',
    'A global security framework that demonstrably removes the perceived need for nuclear deterrence, or a consensus among international legal scholars rejecting the ''security interest'' justification for non-compliance.',
    'If security interests are deemed to legitimately override, the constraint''s ''snare'' nature is reframed as a ''tangled_rope'' with a coordination function for global stability. If self-serving, the ''snare'' classification is strengthened, highlighting the extractive nature of the interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_interest_vs_treaty_obligation, preference, 'Whether nuclear weapon states'' security interests justify their interpretation of Article VI.').

omega_variable(
    two_tier_order_permanence,
    'Is the two-tier nuclear order (weapon states vs. non-weapon states) a temporary arrangement or a permanently stabilized feature of the international system under this reading?',
    'A clear, verifiable timeline for disarmament from nuclear weapon states, or a formal amendment to the NPT explicitly codifying the permanent right of certain states to possess nuclear weapons.',
    'If temporary, the constraint might be reclassified as a scaffold, with a sunset clause for the two-tier system. If permanent, the snare classification is solidified, as the extraction becomes an enduring feature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(two_tier_order_permanence, empirical, 'The intended duration and stability of the nuclear two-tier order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1968, 0.3).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(npt__tr_t1992, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1992, 0.5).
narrative_ontology:measurement(npt__tr_t2004, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2004, 0.58).
narrative_ontology:measurement(npt__tr_t2016, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2016, 0.62).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1968, 0.6).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(npt__be_t1992, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1992, 0.78).
narrative_ontology:measurement(npt__be_t2004, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2004, 0.82).
narrative_ontology:measurement(npt__be_t2016, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2016, 0.84).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(npt__su_t1992, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1992, 0.85).
narrative_ontology:measurement(npt__su_t2004, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2004, 0.9).
narrative_ontology:measurement(npt__su_t2016, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2016, 0.91).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iran_nuclear_deal_jcpoa).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, north_korea_nuclear_program).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT Article IV/VI pairing. This 'nonproliferation primary' reading emphasizes horizontal nonproliferation over disarmament.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
