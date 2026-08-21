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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing (Abolitionist Reading)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of the NPT's Article
 *   IV (peaceful uses) and Article VI (disarmament obligation) pairing. It
 *   asserts that Article VI mandates complete disarmament and that Article IV
 *   is illegitimate if it perpetuates dual-use proliferation risk. Authority
 *   for this reading derives from humanitarian law and the Treaty on the
 *   Prohibition of Nuclear Weapons (TPNW). This reading effectively
 *   delegitimizes the NPT itself as insufficient and views nuclear weapon
 *   possession as categorically illegal, making no distinction between
 *   peaceful and military nuclear programs in terms of proliferation risk.
 *
 * KEY AGENTS:
 *   - abolitionist_advocates: Agenda setter (organized/constrained)
 *   - nuclear_weapon_states: Primary target (institutional/identity_locked)
 *   - nuclear_umbrella_states: Secondary target (powerful/constrained)
 *   - non_nuclear_weapon_states_tpnw_signatories: Beneficiary (organized/mobile)
 *   - international_humanitarian_law_bodies: Beneficiary (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.85).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.9).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing (Abolitionist Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '60bc1546-c8e7-4a15-bc93-89f9b6b9a69b').
narrative_ontology:cs_kernel_codification('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b', fixed_text).
narrative_ontology:cs_authority_grounding('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b', lineage).
narrative_ontology:cs_interpretation_layer_present('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b').
narrative_ontology:cs_reading_relation('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_axiom('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b', foundational, nuclear_weapons_categorically_illegal).
narrative_ontology:cs_axiom_status(nuclear_weapons_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b', nuclear_weapons_categorically_illegal, deontological).
narrative_ontology:cs_axiom('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b', foundational, article_iv_conditional_on_disarmament).
narrative_ontology:cs_axiom_status(article_iv_conditional_on_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b', article_iv_conditional_on_disarmament, conventional).
narrative_ontology:cs_reference_frame('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b', humanitarian_law_prohibition_norm).
narrative_ontology:cs_drift_state('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b', contemporary_nuclear_modernization_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('60bc1546-c8e7-4a15-bc93-89f9b6b9a69b', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_umbrella_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states_tpnw_signatories).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, international_humanitarian_law_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote the interpretation that Article VI mandates immediate, complete disarmament and that Article IV's 'peaceful uses' clause is illegitimate if it enables proliferation risk. They leverage humanitarian law and the TPNW to delegitimize nuclear weapons possession.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, abolitionist_advocates, agenda_setter,
    organized, generational, constrained, global).

% Are the primary targets of this reading, which demands their complete disarmament and delegitimizes their security doctrines. They resist this interpretation, viewing it as an existential threat to their strategic posture and national security.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, payer,
    institutional, civilizational, identity_locked, global).

% Rely on nuclear deterrence for their security and are therefore also targets of this reading. They face pressure to abandon their nuclear alliances and join the TPNW, which they view as undermining their security.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_umbrella_states, payer,
    powerful, generational, constrained, global).

% Benefit from this reading as it aligns with their commitment to the TPNW and the complete prohibition of nuclear weapons. They gain moral and legal leverage against nuclear-armed states.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states_tpnw_signatories, beneficiary,
    organized, biographical, mobile, global).

% Provide the legal and ethical framework that underpins the abolitionist reading, strengthening its authority and reach. They gain vindication for their principles of prohibiting weapons of mass destruction.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, international_humanitarian_law_bodies, beneficiary,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate global efforts towards complete nuclear disarmament by establishing a universal prohibition norm, thereby eliminating the existential threat of nuclear weapons.
% TRANSFER_FUNCTION: Transfers moral and legal authority from nuclear weapon states to humanitarian law and disarmament advocates, shifting the burden of proof for nuclear weapons legitimacy.
% ABSENT_VOICES: Nuclear strategists and proponents of 'minimum deterrence' are largely excluded from the core abolitionist discourse; they would argue for the continued necessity of nuclear weapons for stability and security.
% DISAPPEARANCE_RATIONALE: If this abolitionist reading vanished, the moral and legal pressure for disarmament would significantly diminish, potentially leading to a resurgence of nuclear weapons legitimization and a weakening of non-proliferation norms. The TPNW's legal force would be severely undermined.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons, the risk of accidental or intentional use, and the perceived failure of the NPT to achieve disarmament.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by a broad coalition of civil society organizations, UN bodies, and non-nuclear weapon states, citing ongoing nuclear modernization programs and geopolitical tensions. This corroboration comes from outside the direct beneficiaries of nuclear weapons possession.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The high extractiveness (0.85) and suppression (0.9) reflect the radical demands this reading places on nuclear weapon states and their allies, effectively seeking to dismantle their core security doctrines and capabilities. The 'claimed_type' of 'snare' reflects that, from the perspective of nuclear weapon states, this reading is purely extractive, offering no coordination benefit to them while demanding their disarmament under coercive moral and legal pressure. The rising 'theater_ratio' (0.6) indicates that the NPT's disarmament rhetoric is increasingly seen as performative by abolitionists, masking continued nuclear weapons development and modernization.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapon states experience this reading as a snare, demanding their disarmament without offering reciprocal security guarantees. Abolitionist advocates, however, view it as a necessary moral and legal imperative for global security, a 'rope' for humanity to coordinate away from nuclear catastrophe. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Abolitionist advocates and TPNW signatories are beneficiaries, as this reading empowers their agenda and provides legal justification. Nuclear weapon states and nuclear umbrella states are clear targets, as the reading directly challenges their core security interests and demands costly, existential changes to their defense postures. International humanitarian law bodies are beneficiaries as their principles are vindicated.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively combats mandatrophy in the NPT by reasserting the primacy of Article VI's disarmament mandate, which many perceive as having atrophied. It prevents mislabeling the NPT as a 'rope' for disarmament when, from this perspective, it has become a 'snare' for non-proliferation that entrenches nuclear haves. The abolitionist reading seeks to resolve this perceived mandatrophy by shifting the legal and moral framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_legitimacy_under_abolitionist_reading,
    'Does the abolitionist reading fundamentally delegitimize the NPT as a whole, or only specific interpretations of its articles?',
    'Analysis of official statements and legal arguments from abolitionist groups: if they call for NPT withdrawal or replacement, it indicates fundamental delegitimization; if they focus on reinterpretation, it''s specific.',
    'If the NPT is fundamentally delegitimized, the global nuclear governance architecture faces a more severe crisis, potentially leading to a ''world_rearranges'' scenario for the entire regime. If only interpretations are challenged, the NPT might still serve as a framework for contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_legitimacy_under_abolitionist_reading, conceptual, 'Scope of NPT delegitimization by the abolitionist reading.').

omega_variable(
    dual_use_proliferation_risk_empirical_status,
    'To what extent does the ''peaceful uses'' of nuclear technology (Article IV) genuinely perpetuate dual-use proliferation risk in contemporary contexts?',
    'Empirical studies by IAEA and independent experts on the technical feasibility and historical instances of diversion from peaceful to military programs, considering advancements in detection and verification technologies.',
    'Strong empirical evidence of persistent, unmitigable dual-use risk would strengthen the abolitionist claim of Article IV''s illegitimacy. Weak or manageable risk would challenge this core premise, potentially reducing the perceived extractiveness of the abolitionist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_use_proliferation_risk_empirical_status, empirical, 'Empirical basis for dual-use proliferation risk from peaceful nuclear programs.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (nuclear weapon states'' resistance to disarmament) structural (geopolitical realities, security dilemmas) or internalized (ideological commitment to deterrence, identity fusion with nuclear status)?',
    'Post-disarmament-treaty trajectory: if nuclear weapon states continue to resist even after new legal frameworks (like TPNW) are established, it suggests internalized suppression. If resistance shifts based on changes in geopolitical security, it''s more structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — nuclear weapon states carry the suppression with them after external barriers are removed. If structural, the abolitionist reading must address the underlying security dilemmas.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for nuclear weapon states'' resistance to disarmament.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1985, 0.4).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(npt__tr_t2015, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2015, 0.55).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(npt__be_t2015, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2015, 0.82).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(npt__su_t2015, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2015, 0.85).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, tpnw_legitimacy_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT Article IV/VI pairing kernel. It focuses on the abolitionist interpretation, emphasizing disarmament and the illegitimacy of dual-use proliferation risk. It structurally influences and is influenced by the other readings (nonproliferation_primary, grand_bargain) and the TPNW.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
