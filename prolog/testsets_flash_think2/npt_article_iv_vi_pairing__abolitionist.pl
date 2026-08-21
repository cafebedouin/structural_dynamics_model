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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing (Abolitionist Reading)
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   This constraint story represents the 'abolitionist' reading of the NPT's
 *   Article IV (peaceful uses) and Article VI (disarmament) pairing. From
 *   this perspective, Article VI mandates complete and immediate disarmament,
 *   and Article IV is illegitimate if it allows dual-use technology to
 *   perpetuate proliferation risk. Authority for this reading derives from
 *   humanitarian law and the Treaty on the Prohibition of Nuclear Weapons
 *   (TPNW). The NPT itself is seen as insufficient and delegitimized, with
 *   nuclear weapon possession deemed categorically illegal, and no
 *   distinction made between peaceful and military nuclear programs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.85).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.9).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing (Abolitionist Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, 'd526c990-b253-49da-b4cf-b39b57d88647').
narrative_ontology:cs_kernel_codification('d526c990-b253-49da-b4cf-b39b57d88647', fixed_text).
narrative_ontology:cs_authority_grounding('d526c990-b253-49da-b4cf-b39b57d88647', lineage).
narrative_ontology:cs_interpretation_layer_present('d526c990-b253-49da-b4cf-b39b57d88647').
narrative_ontology:cs_reading_relation('d526c990-b253-49da-b4cf-b39b57d88647', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('d526c990-b253-49da-b4cf-b39b57d88647', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_axiom('d526c990-b253-49da-b4cf-b39b57d88647', foundational, nuclear_weapons_categorically_illegal).
narrative_ontology:cs_axiom_status(nuclear_weapons_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('d526c990-b253-49da-b4cf-b39b57d88647', nuclear_weapons_categorically_illegal, deontological).
narrative_ontology:cs_axiom('d526c990-b253-49da-b4cf-b39b57d88647', foundational, disarmament_immediate_obligation).
narrative_ontology:cs_axiom_status(disarmament_immediate_obligation, holdable).
narrative_ontology:cs_axiom_grounding('d526c990-b253-49da-b4cf-b39b57d88647', disarmament_immediate_obligation, deontological).
narrative_ontology:cs_reference_frame('d526c990-b253-49da-b4cf-b39b57d88647', humanitarian_law_prohibition_norm).
narrative_ontology:cs_drift_state('d526c990-b253-49da-b4cf-b39b57d88647', contemporary_npt_regime, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d526c990-b253-49da-b4cf-b39b57d88647', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, states_with_advanced_dual_use_tech).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, global_civil_society).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, humanity_at_large).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, international_atomic_energy_agency).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_principles).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, tpnw_prohibition_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain nuclear weapons and control the narrative around disarmament progress, benefiting from the NPT's implicit legitimization of their arsenals while imposing non-proliferation obligations on others. They actively resist any interpretation that would mandate immediate disarmament.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).

% Bear the costs of proliferation risk and are denied the right to develop nuclear weapons, even for peaceful purposes if dual-use concerns arise. Many advocate for stronger disarmament commitments from NWS, with some joining the TPNW.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states, payer,
    organized, biographical, constrained, global).

% Actively promote the Treaty on the Prohibition of Nuclear Weapons, challenging the NPT's legitimacy and advocating for a new international norm that categorically outlaws nuclear weapons. They seek to shift the global legal framework.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_states, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, tpnw_states, observer).

% Advocates for nuclear disarmament and humanitarian law, often excluded from formal NPT review processes. They bear the diffuse risk of nuclear proliferation and are identity-locked by their moral commitment to a nuclear-weapon-free world.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, global_civil_society, excluded,
    moderate, generational, identity_locked, global).

% Responsible for verifying peaceful nuclear programs under Article III of the NPT. From an abolitionist perspective, it is forced to operate within a flawed framework that perpetuates dual-use risks, bearing the cost of maintaining a system that is seen as insufficient.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, international_atomic_energy_agency, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, international_atomic_energy_agency, payer).

% Benefit from the NPT's framework that allows peaceful nuclear technology development, even if it carries dual-use proliferation risks. They often resist stricter interpretations of Article IV that would limit their technological sovereignty.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, states_with_advanced_dual_use_tech, beneficiary,
    powerful, biographical, mobile, global).

% Bears the ultimate, existential risk of nuclear weapons proliferation and use. This diffuse and powerless entity is trapped by the decisions of states and the persistence of nuclear arsenals.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, humanity_at_large, payer,
    powerless, civilizational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stated coordination function is to prevent the spread of nuclear weapons technology and to facilitate peaceful nuclear cooperation, while pursuing disarmament. However, from this reading, the disarmament aspect is severely undermined.
% TRANSFER_FUNCTION: Transfers the implicit right to possess nuclear weapons to a few states, while transferring the obligation of non-acquisition and the risk of proliferation to all other states and humanity.
% ABSENT_VOICES: The voices of global civil society, indigenous communities affected by nuclear testing, and future generations are largely absent from the formal NPT review processes, despite bearing the greatest long-term risks. TPNW states represent a growing, but still excluded, voice within the broader nuclear governance discourse.
% DISAPPEARANCE_RATIONALE: If the NPT regime, as currently interpreted, vanished overnight, the international security landscape would fundamentally rearrange. While some might fear immediate proliferation, the abolitionist perspective suggests it could open the door for a truly comprehensive and verifiable disarmament framework, as the current 'legitimizing' cover for NWS would be gone.
% FOUNDING_PROBLEM: The founding problem was the existential threat of nuclear weapons proliferation and the desire to prevent a world with many nuclear-armed states, while also promoting peaceful nuclear energy and pursuing disarmament.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear Weapon States and their allies attest that the proliferation problem remains live and the NPT is essential. However, TPNW states, global civil society, and many international legal scholars corroborate that the disarmament problem is also live and unaddressed, and that the NPT's current operation perpetuates, rather than solves, the overall nuclear threat.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the view that the NPT regime, as currently practiced, extracts the right for a few states to possess nuclear weapons, while denying this to others and imposing a global risk. Suppression (0.90) is high due to the active enforcement against horizontal proliferation and the suppression of disarmament efforts by NWS. The theater ratio (0.65) indicates that much of the NPT's review and diplomatic processes are perceived as performative, masking a lack of genuine disarmament progress. Accessibility collapse (0.75) is high because NWS actively resist alternative frameworks like the TPNW, and resistance (0.60) is moderate, reflecting the ongoing efforts by TPNW states and civil society.
 *
 * PERSPECTIVAL GAP:
 *   This reading fundamentally diverges from 'nonproliferation_primary' and 'grand_bargain' readings by asserting the categorical illegality of nuclear weapons and the immediate obligation for disarmament. While other readings might see the NPT as a functional (if imperfect) coordination mechanism, the abolitionist view sees it as a snare that perpetuates an illegitimate status quo.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear Weapon States are primary beneficiaries, as the regime implicitly legitimizes their possession of weapons while imposing obligations on others. States with advanced dual-use technology also benefit from the current framework. Non-Nuclear Weapon States, global civil society, and humanity at large are victims, bearing the costs of proliferation risk and technology denial. The International Atomic Energy Agency (IAEA) acts as an agenda-setter for verification but also bears the cost of maintaining a system that, from this perspective, is fundamentally flawed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_purpose_ambiguity,
    'Is the NPT''s primary purpose non-proliferation (horizontal) or disarmament (vertical)?',
    'Shift in NWS policy and verifiable disarmament, or explicit re-negotiation of the treaty''s core bargain.',
    'If disarmament is acknowledged as primary, the current regime''s high extraction and suppression are further delegitimized; if non-proliferation remains primary, the abolitionist reading''s critique of Article IV''s legitimacy is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_purpose_ambiguity, conceptual, 'Ambiguity regarding the NPT''s foundational purpose.').

omega_variable(
    tpnw_legitimacy_impact,
    'Does the Treaty on the Prohibition of Nuclear Weapons (TPNW) fundamentally alter the NPT''s legitimacy and interpretation?',
    'Increased adherence to TPNW by non-NWS, or NWS engagement with TPNW principles.',
    'If TPNW is recognized as establishing a new customary international law, the NPT''s framework for ''legal'' possession by NWS is undermined, reinforcing the abolitionist reading''s claims of illegitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tpnw_legitimacy_impact, empirical, 'Impact of TPNW on NPT''s legal and moral authority.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the NPT Article IV/VI pairing best framed as a ''grand bargain'' (reciprocal obligations), ''non-proliferation primary'' (security interest), or ''abolitionist'' (disarmament mandate)?',
    'Evolution of international legal consensus, NWS policy shifts, or a UN General Assembly resolution explicitly endorsing one framing.',
    'The classification of the NPT regime (e.g., as Snare vs. Tangled Rope) depends critically on which framing is adopted. The abolitionist framing leads to a Snare classification due to the perceived illegitimacy of weapon possession.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framings of the NPT kernel lead to different classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0, 0.5).
narrative_ontology:measurement(npt__tr_t10, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 10, 0.58).
narrative_ontology:measurement(npt__tr_t20, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 20, 0.62).
narrative_ontology:measurement(npt__tr_t30, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(npt__be_t10, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(npt__be_t20, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(npt__be_t30, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(npt__su_t10, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(npt__su_t20, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(npt__su_t30, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).

% DUAL FORMULATION NOTE:
% This constraint is part of a family of readings of the NPT Article IV/VI pairing kernel. Each reading presents a distinct structural claim and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
