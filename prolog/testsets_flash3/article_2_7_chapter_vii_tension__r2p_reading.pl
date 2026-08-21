% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect (R2P) Doctrine
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint represents the 'Responsibility to Protect' (R2P)
 *   doctrine, which posits that state sovereignty is conditional on a state's
 *   protection of its own population from mass atrocities. If a state fails
 *   in this responsibility, the international community has a responsibility
 *   to intervene. This reading emphasizes human security over absolute state
 *   sovereignty, legitimizing intervention under Chapter VII of the UN
 *   Charter for humanitarian purposes. It is a reading of the broader
 *   'article_2_7_chapter_vii_tension' kernel, which also includes a
 *   'sovereignty_first_reading' that prioritizes non-interference.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.7).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.6).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect (R2P) Doctrine").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, '7d641831-1004-45df-b46e-39d33be61119').
narrative_ontology:cs_kernel_codification('7d641831-1004-45df-b46e-39d33be61119', formalized).
narrative_ontology:cs_authority_grounding('7d641831-1004-45df-b46e-39d33be61119', lineage).
narrative_ontology:cs_interpretation_layer_present('7d641831-1004-45df-b46e-39d33be61119').
narrative_ontology:cs_reading_relation('7d641831-1004-45df-b46e-39d33be61119', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('7d641831-1004-45df-b46e-39d33be61119', foundational, sovereignty_is_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_is_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('7d641831-1004-45df-b46e-39d33be61119', sovereignty_is_responsibility, deontological).
narrative_ontology:cs_axiom('7d641831-1004-45df-b46e-39d33be61119', foundational, human_security_trumps_state_security).
narrative_ontology:cs_axiom_status(human_security_trumps_state_security, holdable).
narrative_ontology:cs_axiom_grounding('7d641831-1004-45df-b46e-39d33be61119', human_security_trumps_state_security, deontological).
narrative_ontology:cs_reference_frame('7d641831-1004-45df-b46e-39d33be61119', post_cold_war_humanitarianism).
narrative_ontology:cs_drift_state('7d641831-1004-45df-b46e-39d33be61119', contemporary_geopolitical_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7d641831-1004-45df-b46e-39d33be61119', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_advocates).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, sovereign_states_committing_atrocities).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These populations are the primary intended beneficiaries, receiving protection from mass atrocities. Their existence is often threatened, and they have no internal means of escape or defense, making international intervention their only hope.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, local).

% These states bear the cost of intervention, losing their absolute sovereignty and facing military or diplomatic action. Their options are to cease atrocities, resist intervention, or face international condemnation and potential regime change.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, sovereign_states_committing_atrocities, payer,
    powerful, immediate, constrained, national).

% These groups benefit from the legitimization of humanitarian intervention, seeing their core values of human rights and protection elevated in international discourse and practice. They actively lobby for R2P implementation.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_advocates, beneficiary,
    organized, generational, mobile, global).

% The UNSC is the primary body responsible for authorizing interventions under R2P. Its permanent members hold veto power, which can constrain or enable action. It sets the agenda for when and how R2P is invoked.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, un_security_council, agenda_setter,
    institutional, biographical, constrained, global).

% The long-standing international norm of non-interference in internal affairs, which is challenged and eroded by the R2P doctrine. Its 'cost' is its diminished absolute status in favor of human protection.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_norm, payer,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_norm).

% States that choose not to participate in R2P interventions, often due to national interest, resource constraints, or differing interpretations of sovereignty. They observe the evolving norm and its implications for their own foreign policy.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, non_intervening_states, observer,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international action to prevent or halt mass atrocities when a state fails to protect its own population, providing a framework for collective response to humanitarian crises.
% TRANSFER_FUNCTION: Transfers the right to absolute sovereignty from states to the international community (specifically the UN Security Council) in cases of systematic atrocity, enabling the transfer of military and diplomatic resources for intervention.
% ABSENT_VOICES: States that prioritize absolute sovereignty and non-interference, particularly those wary of intervention as a pretext for regime change, are often marginalized in R2P discourse. They would argue for stricter adherence to Article 2(7) of the UN Charter.
% DISAPPEARANCE_RATIONALE: If R2P vanished, the international community would lose a key (albeit contested) legitimizing framework for humanitarian intervention. Persecuted populations would have fewer avenues for protection, and states committing atrocities would face less international pressure, leading to a rearrangement of international security dynamics.
% FOUNDING_PROBLEM: The failure of the international community to prevent or respond effectively to mass atrocities (e.g., Rwanda, Srebrenica) in the late 20th century, despite the 'never again' promise after the Holocaust.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian organizations, international legal scholars, and numerous UN reports consistently attest that the problem of mass atrocities remains live, and the need for a framework like R2P persists, even with its implementation challenges. This corroboration comes from outside the direct beneficiaries of intervention.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The R2P doctrine is classified as a Tangled Rope because it genuinely seeks to coordinate international action for humanitarian protection (beneficiaries: persecuted populations) but also involves significant extraction from the traditional norm of state sovereignty and from states targeted for intervention (victims: sovereign states committing atrocities, traditional sovereignty norm). It requires active enforcement by the UN Security Council, which can be highly coercive. Extractiveness is high (0.7) due to the profound shift in the understanding of sovereignty and the imposition of external will. Suppression (0.6) reflects the diplomatic and military pressure exerted on non-compliant states. Theater ratio (0.4) is moderate, as while the humanitarian intent is real, geopolitical interests and selective application can introduce performative elements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of persecuted populations and humanitarian advocates, R2P is a vital, albeit imperfect, Rope or Scaffold, offering necessary protection. From the perspective of states targeted for intervention or those upholding traditional sovereignty, it is a Snare, eroding state autonomy and potentially serving as a pretext for intervention. The engine's classification as Tangled Rope reflects this inherent tension between coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations are full beneficiaries (d=0.0) as the doctrine is designed for their protection. Humanitarian advocates are also beneficiaries (d=0.1-0.2) as it aligns with their mission. Sovereign states committing atrocities are full targets (d=1.0) as their actions trigger the constraint and they bear the cost of intervention. The traditional sovereignty norm is also a target (d=0.9) as its absolute status is diminished. The UN Security Council acts as an agenda-setter, balancing the doctrine's principles with geopolitical realities.
 *
 * MANDATROPHY ANALYSIS:
 *   R2P is not subject to mandatrophy in the traditional sense, as the problem it addresses (mass atrocities) remains tragically live. However, its effectiveness and legitimacy are constantly contested, preventing it from fully solidifying into a pure Rope. The tension between its humanitarian mandate and the geopolitical realities of its enforcement means it remains a 'tangled' mechanism, perpetually requiring active justification and negotiation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    r2p_vs_sovereignty_first_reading,
    'Is the R2P doctrine a legitimate evolution of international law, or an illegitimate erosion of state sovereignty?',
    'Continued international legal debate, state practice, and the outcomes of interventions. If R2P consistently leads to positive humanitarian outcomes without undue geopolitical destabilization, its legitimacy strengthens.',
    'If deemed legitimate, R2P moves closer to a Rope; if illegitimate, it is perceived as a Snare by a wider range of states, increasing resistance and potentially leading to its collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(r2p_vs_sovereignty_first_reading, conceptual, 'The fundamental conceptual conflict between human protection and state sovereignty.').

omega_variable(
    selectivity_of_intervention,
    'Is the application of R2P genuinely universal, or is it selectively applied based on geopolitical interests?',
    'Empirical analysis of all cases of mass atrocities and the international response, comparing responses across different geopolitical contexts and power dynamics.',
    'If application is highly selective, the theater_ratio and extractiveness would be higher, as the doctrine''s stated purpose would be undermined by its instrumental use, pushing it closer to a Snare. If universal, it would reinforce its Rope-like coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_of_intervention, empirical, 'Whether R2P is applied consistently or selectively.').

omega_variable(
    unsc_veto_power_impact,
    'Does the UN Security Council''s veto power fundamentally undermine R2P''s ability to protect populations, or is it a necessary check on intervention?',
    'Analysis of cases where vetoes prevented R2P interventions, and counterfactual modeling of outcomes if interventions had occurred. Reform proposals for the UNSC''s structure.',
    'If the veto power consistently blocks necessary interventions, it increases the perceived suppression and extractiveness for persecuted populations (as their protection is denied), and increases the theater_ratio of R2P as a ''promise'' that cannot always be kept. If seen as a necessary check, it maintains a more balanced view of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsc_veto_power_impact, preference, 'The impact of UNSC veto power on R2P''s efficacy and legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(arti_tr_t2007, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2007, 0.3).
narrative_ontology:measurement(arti_tr_t2013, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2013, 0.4).
narrative_ontology:measurement(arti_tr_t2018, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2018, 0.45).
narrative_ontology:measurement(arti_tr_t2024, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(arti_be_t2007, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2007, 0.6).
narrative_ontology:measurement(arti_be_t2013, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2013, 0.7).
narrative_ontology:measurement(arti_be_t2018, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(arti_be_t2024, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(arti_su_t2007, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2007, 0.5).
narrative_ontology:measurement(arti_su_t2013, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2013, 0.6).
narrative_ontology:measurement(arti_su_t2018, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(arti_su_t2024, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, un_charter_article_2_7).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'article_2_7_chapter_vii_tension' kernel. It represents the R2P doctrine, which interprets sovereignty as conditional on human protection, contrasting with the 'sovereignty_first_reading' which prioritizes non-interference. Both readings are linked to the core UN Charter articles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
