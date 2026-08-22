% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing (Abolitionist Reading)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of the NPT's Article
 *   IV (peaceful uses) and Article VI (disarmament), where Article VI's
 *   disarmament mandate is paramount and Article IV's allowance for peaceful
 *   nuclear energy is illegitimate if it perpetuates dual-use proliferation
 *   risk. Authority for this reading derives from humanitarian law and the
 *   Treaty on the Prohibition of Nuclear Weapons (TPNW). The NPT itself is
 *   seen as insufficient and potentially delegitimized by the failure of
 *   nuclear-weapon states to disarm. This reading asserts that weapon
 *   possession is categorically illegal and makes no distinction between
 *   peaceful and military nuclear programs due to inherent dual-use risks.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Agenda-setter (institutional/constrained) — maintain arsenals, interpret Article VI as aspirational.
 *   - non_nuclear_weapon_states: Payer (organized/identity_locked) — forgo weapons, bear risk, face hypocrisy.
 *   - civil_society_organizations: Beneficiary/Observer (moderate/mobile) — advocate for disarmament, moral clarity.
 *   - humanitarian_law_framework: Beneficiary (analytical/analytical) — principles invoked to delegitimize nuclear weapons.
 *   - global_humanity: Payer (powerless/trapped) — bears existential risk of nuclear war.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.88).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.75).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.88).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.92).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing (Abolitionist Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, 'c6f29f2a-83a5-43ce-acd0-193b1a4c05be').
narrative_ontology:cs_kernel_codification('c6f29f2a-83a5-43ce-acd0-193b1a4c05be', fixed_text).
narrative_ontology:cs_authority_grounding('c6f29f2a-83a5-43ce-acd0-193b1a4c05be', lineage).
narrative_ontology:cs_interpretation_layer_present('c6f29f2a-83a5-43ce-acd0-193b1a4c05be').
narrative_ontology:cs_reading_relation('c6f29f2a-83a5-43ce-acd0-193b1a4c05be', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('c6f29f2a-83a5-43ce-acd0-193b1a4c05be', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_axiom('c6f29f2a-83a5-43ce-acd0-193b1a4c05be', foundational, nuclear_weapons_categorically_illegal).
narrative_ontology:cs_axiom_status(nuclear_weapons_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('c6f29f2a-83a5-43ce-acd0-193b1a4c05be', nuclear_weapons_categorically_illegal, deontological).
narrative_ontology:cs_axiom('c6f29f2a-83a5-43ce-acd0-193b1a4c05be', foundational, disarmament_mandate_immediate_binding).
narrative_ontology:cs_axiom_status(disarmament_mandate_immediate_binding, holdable).
narrative_ontology:cs_axiom_grounding('c6f29f2a-83a5-43ce-acd0-193b1a4c05be', disarmament_mandate_immediate_binding, conventional).
narrative_ontology:cs_reference_frame('c6f29f2a-83a5-43ce-acd0-193b1a4c05be', humanitarian_law_prohibition_framework).
narrative_ontology:cs_drift_state('c6f29f2a-83a5-43ce-acd0-193b1a4c05be', contemporary_nuclear_modernization_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c6f29f2a-83a5-43ce-acd0-193b1a4c05be', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, global_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, civil_society_organizations).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_supremacy).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, nuclear_weapons_prohibition_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize nuclear arsenals, interpreting Article VI as a long-term aspiration rather than an immediate, legally binding disarmament mandate. They benefit from the perceived security provided by their arsenals and the status quo that allows them to retain these weapons while others disarm.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).

% Are legally bound by Article IV to forgo nuclear weapons, but face the existential threat of nuclear war and the perceived hypocrisy of nuclear-weapon states. They bear the risk of proliferation and the moral burden of living under nuclear threat, with limited avenues for redress within the NPT framework.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states, payer,
    organized, generational, identity_locked, global).

% Advocate for complete nuclear disarmament, often citing humanitarian law and the TPNW. They benefit from the moral clarity of the abolitionist stance and the mobilization of public opinion, but lack direct enforcement power over states.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, civil_society_organizations, beneficiary,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, civil_society_organizations, observer).

% The body of international law that prohibits weapons causing indiscriminate suffering. Its principles are invoked to delegitimize nuclear weapons, reinforcing its own authority and scope.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_framework, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_framework).

% Bears the ultimate, catastrophic risk of nuclear weapons use, with no agency or exit options. The continued existence of nuclear weapons represents an existential threat to all life on Earth.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, global_humanity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(npt_article_iv_vi_pairing__abolitionist, global_humanity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NPT aims to coordinate global efforts to prevent the spread of nuclear weapons and promote disarmament, establishing a framework for non-proliferation and peaceful nuclear energy use.
% TRANSFER_FUNCTION: Transfers the right to possess nuclear weapons from the global community to a select few states, in exchange for a promise of eventual disarmament that remains unfulfilled. It also transfers the risk of nuclear catastrophe to non-nuclear weapon states and global humanity.
% ABSENT_VOICES: Future generations and the victims of potential nuclear conflict are absent from the current discourse, their interests represented only by advocates. Their voices would unequivocally demand immediate and complete disarmament.
% DISAPPEARANCE_RATIONALE: If the NPT and its associated interpretations vanished overnight, the global nuclear order would collapse. Non-nuclear weapon states might pursue their own arsenals, and the existing nuclear-weapon states would lose a key legitimizing framework for their own possession, leading to a highly unstable and unpredictable security environment.
% FOUNDING_PROBLEM: The proliferation of nuclear weapons and the existential threat of nuclear war during the Cold War era.
% FOUNDING_PROBLEM_CORROBORATION: The International Campaign to Abolish Nuclear Weapons (ICAN) and numerous non-nuclear weapon states consistently attest that the founding problem of nuclear threat remains live and urgent, citing ongoing modernization of arsenals and geopolitical tensions. This corroboration comes from outside the direct beneficiaries of the current NPT interpretation.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the core claim is that the NPT, as currently interpreted by nuclear-weapon states, extracts the right to security and self-determination from non-nuclear weapon states and global humanity, while perpetuating an existential threat. Suppression (0.75) is high due to the immense power asymmetry between nuclear-weapon states and non-nuclear weapon states, and the structural barriers to challenging the nuclear order. Theater ratio (0.65) is high because the disarmament rhetoric of Article VI is seen as largely performative, masking the continued modernization and retention of nuclear arsenals. Resistance (0.92) is very high, driven by the strong moral and legal opposition from non-nuclear weapon states and civil society, culminating in the TPNW.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear-weapon states perceive the NPT as a successful non-proliferation regime that manages a complex security environment, with their nuclear arsenals as a necessary deterrent. Non-nuclear weapon states and civil society, from the abolitionist perspective, see the same regime as a snare that legitimizes an unacceptable status quo and perpetuates an existential threat. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-weapon states are the primary agenda-setters and beneficiaries, as they retain their weapons and control the interpretation of disarmament obligations. Non-nuclear weapon states are payers, bearing the costs of insecurity and foregone nuclear options. Global humanity is the ultimate payer, facing existential risk. Civil society and the humanitarian law framework are beneficiaries of the abolitionist reading's moral and legal force, but lack direct power to alter the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the NPT's mandate has atrophied from its original intent of disarmament into a mechanism for legitimizing nuclear weapon possession. The high extractiveness and theater ratio, coupled with strong resistance, indicate that the constraint is operating as a snare, not a coordination mechanism. The 'contested' status of the founding problem further supports this, as the problem of proliferation is seen as exacerbated, not solved, by the current interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_legitimacy_source,
    'Does the NPT''s legitimacy primarily derive from its non-proliferation function (Article IV) or its disarmament mandate (Article VI)?',
    'Analysis of state practice and legal arguments in international courts regarding NPT compliance and breach, particularly concerning Article VI.',
    'If legitimacy is primarily from Article VI, the current state of affairs is a severe breach, strengthening the abolitionist reading. If from Article IV, the abolitionist reading''s delegitimization of the NPT is less structurally grounded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_legitimacy_source, conceptual, 'Ambiguity in the NPT''s foundational legitimizing principle.').

omega_variable(
    dual_use_separability,
    'Is it empirically possible to separate ''peaceful'' nuclear technology (Article IV) from ''military'' applications, or is the dual-use risk inherent and inseparable?',
    'Technological advancements in verification and proliferation resistance, or empirical evidence from states that have pursued peaceful programs and then weaponized.',
    'If inseparable, the abolitionist reading''s rejection of Article IV''s legitimacy due to proliferation risk is strongly supported. If separable, Article IV could be a genuine coordination mechanism, reducing the overall extractiveness of the NPT.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_separability, empirical, 'The inherent dual-use nature of nuclear technology and its implications for proliferation.').

omega_variable(
    tpnw_normative_force,
    'To what extent does the Treaty on the Prohibition of Nuclear Weapons (TPNW) establish a new, binding customary international law norm that delegitimizes nuclear weapons possession for all states, including those not party to the TPNW?',
    'Analysis of state practice, opinio juris, and international legal scholarship regarding the TPNW''s impact on customary international law.',
    'If the TPNW establishes a strong customary norm, the abolitionist reading''s claim of categorical illegality for nuclear weapons is strengthened, increasing the perceived extractiveness of the NPT status quo. If not, the TPNW remains a treaty binding only its parties, with less impact on the NPT''s structural classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tpnw_normative_force, conceptual, 'The TPNW''s role in shaping international law and delegitimizing nuclear weapons.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1968, 0.3).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(npt__tr_t1992, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1992, 0.5).
narrative_ontology:measurement(npt__tr_t2004, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2004, 0.58).
narrative_ontology:measurement(npt__tr_t2016, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2016, 0.62).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1968, 0.7).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(npt__be_t1992, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1992, 0.8).
narrative_ontology:measurement(npt__be_t2004, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2004, 0.85).
narrative_ontology:measurement(npt__be_t2016, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2016, 0.87).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(npt__su_t1992, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1992, 0.65).
narrative_ontology:measurement(npt__su_t2004, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2004, 0.7).
narrative_ontology:measurement(npt__su_t2016, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2016, 0.73).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT Article IV/VI pairing kernel. It focuses on the abolitionist interpretation, where disarmament is paramount and Article IV's peaceful uses are constrained by proliferation risk. It is linked to the 'nonproliferation_primary' and 'grand_bargain' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
