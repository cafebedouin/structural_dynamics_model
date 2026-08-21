% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__universalist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universalist Jurisdiction
 *   domain: international_law/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents the 'universalist' reading of the Rome
 *   Statute, asserting that the International Criminal Court (ICC) has
 *   jurisdiction over core international crimes that transcends the consent
 *   of individual states, particularly non-parties. This reading emphasizes
 *   the inherent nature of international criminal law and the imperative to
 *   end impunity, even if it means overriding traditional notions of state
 *   sovereignty. The ICC, supported by the UN Security Council, acts as the
 *   primary enforcer, while non-party states and their nationals are the
 *   primary targets of this expansive jurisdictional claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.65).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.7).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universalist Jurisdiction").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, 'fa918fe0-407d-4dab-8b2e-b5c6924c05c2').
narrative_ontology:cs_kernel_codification('fa918fe0-407d-4dab-8b2e-b5c6924c05c2', formalized).
narrative_ontology:cs_authority_grounding('fa918fe0-407d-4dab-8b2e-b5c6924c05c2', lineage).
narrative_ontology:cs_interpretation_layer_present('fa918fe0-407d-4dab-8b2e-b5c6924c05c2').
narrative_ontology:cs_reading_relation('fa918fe0-407d-4dab-8b2e-b5c6924c05c2', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa918fe0-407d-4dab-8b2e-b5c6924c05c2', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('fa918fe0-407d-4dab-8b2e-b5c6924c05c2', foundational, universal_jurisdiction_for_core_crimes).
narrative_ontology:cs_axiom_status(universal_jurisdiction_for_core_crimes, holdable).
narrative_ontology:cs_axiom_grounding('fa918fe0-407d-4dab-8b2e-b5c6924c05c2', universal_jurisdiction_for_core_crimes, deontological).
narrative_ontology:cs_axiom('fa918fe0-407d-4dab-8b2e-b5c6924c05c2', foundational, state_sovereignty_subordinate_to_human_rights).
narrative_ontology:cs_axiom_status(state_sovereignty_subordinate_to_human_rights, holdable).
narrative_ontology:cs_axiom_grounding('fa918fe0-407d-4dab-8b2e-b5c6924c05c2', state_sovereignty_subordinate_to_human_rights, deontological).
narrative_ontology:cs_reference_frame('fa918fe0-407d-4dab-8b2e-b5c6924c05c2', post_nuremberg_universal_accountability).
narrative_ontology:cs_drift_state('fa918fe0-407d-4dab-8b2e-b5c6924c05c2', contemporary_geopolitical_resistance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fa918fe0-407d-4dab-8b2e-b5c6924c05c2', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, accused_individuals_from_non_party_states).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, jus_cogens_doctrine).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, responsibility_to_protect_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institution tasked with enforcing the Rome Statute. From this reading, it asserts jurisdiction over core international crimes regardless of the nationality of the accused or the consent of their state, provided the crime occurs on the territory of a state party or is referred by the UNSC. It actively seeks to expand the reach of international justice.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% Individuals who have suffered genocide, crimes against humanity, war crimes, or the crime of aggression. This reading positions them as universal rights-holders whose access to justice transcends state borders and sovereign consent, offering a pathway to accountability where national systems fail.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes, beneficiary,
    powerless, immediate, trapped, local).

% States that have not ratified the Rome Statute and thus do not consent to ICC jurisdiction. This reading asserts that their nationals can be subject to ICC jurisdiction if crimes are committed on the territory of a state party, or if the UNSC refers a situation, effectively overriding their sovereign non-consent for core crimes. They bear the cost of potential prosecution of their citizens without having agreed to the court's mandate.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states, payer,
    powerful, generational, constrained, national).

% Individuals from non-party states who are accused of core international crimes. Under this reading, they face potential prosecution by the ICC even if their home state has not consented to the court's jurisdiction, experiencing a direct loss of sovereign protection.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, accused_individuals_from_non_party_states, payer,
    powerless, biographical, trapped, local).

% Can refer situations to the ICC, thereby granting the court jurisdiction over crimes committed anywhere, regardless of whether the states involved are parties to the Rome Statute. This power is a key mechanism for the universalist reading to assert jurisdiction beyond state consent.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to prosecute individuals for the most heinous crimes, ensuring that perpetrators do not escape justice simply by crossing borders or relying on state inaction, thereby upholding a universal standard of human dignity.
% TRANSFER_FUNCTION: Transfers the authority to prosecute core international crimes from national jurisdictions (especially those unwilling or unable to act, or non-party states) to the International Criminal Court, along with the associated costs of defense and potential penalties for accused individuals.
% ABSENT_VOICES: States that vehemently oppose the ICC's assertion of jurisdiction over their nationals (e.g., the United States, China, Russia) are often absent from the direct legal proceedings, but their diplomatic and political resistance is a constant counter-pressure. Their arguments for strict sovereign consent are sidelined by this reading.
% DISAPPEARANCE_RATIONALE: If the universalist reading of Rome Statute jurisdiction vanished, the ICC's ability to prosecute crimes in non-party states or via UNSC referrals would collapse. This would significantly reduce the scope of international criminal justice, leaving many victims without recourse and potentially emboldening perpetrators who could evade justice by operating in non-party states. The international legal landscape for accountability would fundamentally shift back towards a more state-centric model.
% FOUNDING_PROBLEM: The problem of impunity for perpetrators of genocide, war crimes, and crimes against humanity, particularly when national courts are unwilling or unable to prosecute, or when crimes are committed across borders.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, legal scholars, and numerous victim groups consistently attest that the problem of impunity remains live, with ongoing conflicts and atrocities demonstrating the continued need for international accountability mechanisms. This corroboration comes from outside the direct beneficiaries of the ICC's institutional power.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because this reading imposes a significant cost on non-party states by asserting jurisdiction over their nationals without their consent, challenging their sovereign authority. Suppression (0.7) is high due to the active enforcement mechanisms, including UNSC referrals and the ICC's own prosecutorial powers, which actively suppress attempts by non-party states to shield their citizens. The theater ratio (0.2) is relatively low, as the ICC's actions are generally functional in pursuing justice, though diplomatic maneuvering and political resistance can introduce performative elements. Resistance (0.75) is high, reflecting the strong opposition from powerful non-party states.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of victims and the ICC, this constraint is a vital 'rope' for justice, coordinating global efforts against impunity. However, from the perspective of non-party states and their accused nationals, it operates as a 'snare' or 'tangled rope,' extracting sovereign prerogatives and imposing costs without consent. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC and victims of core crimes are clear beneficiaries (low d), as the constraint empowers the court and provides a path to justice for victims. Non-party states and their accused nationals are targets (high d), as the constraint directly challenges their sovereignty and imposes legal obligations without their consent. The UN Security Council acts as an agenda-setter, capable of amplifying the universalist reach of the court.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (ending impunity for core crimes) is actively pursued and remains highly relevant. The contest is over the legitimate scope and authority of that mandate, not its obsolescence. The classification as a 'tangled rope' reflects the genuine coordination function (for victims and international justice) intertwined with significant asymmetric extraction (from non-party states' sovereignty) and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_implied_consent,
    'To what extent does a state''s participation in the international system, or the commission of crimes on its territory, imply a ''universal'' consent to ICC jurisdiction, even if it is not a party to the Rome Statute?',
    'Further development of customary international law, or a definitive ruling by the International Court of Justice on the scope of implied consent for jus cogens violations.',
    'If implied consent is broadly recognized, the ''extraction'' from non-party states would be re-framed as a pre-existing obligation, potentially shifting the classification towards a ''rope'' for all. If not, the ''snare'' elements for non-party states would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_implied_consent, conceptual, 'Ambiguity regarding the legal basis for jurisdiction over non-party states.').

omega_variable(
    unsc_referral_legitimacy,
    'Is the UN Security Council''s power to refer situations to the ICC, thereby binding non-party states, a legitimate extension of universal jurisdiction or an overreach of political power?',
    'A shift in the geopolitical balance of power, or a legal challenge to the UNSC''s authority to create universal jurisdiction where none explicitly exists via treaty.',
    'If deemed an overreach, the ''suppression'' metric would be seen as more coercive and less legitimate, potentially increasing the ''snare'' aspect. If affirmed, it reinforces the ''tangled rope'' as a necessary, albeit extractive, coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsc_referral_legitimacy, preference, 'Contestation over the legitimacy and scope of UNSC referrals to the ICC.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__universalist_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(rome_tr_t2004, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2004, 0.12).
narrative_ontology:measurement(rome_tr_t2010, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(rome_tr_t2016, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 1998, 0.5).
narrative_ontology:measurement(rome_be_t2004, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(rome_be_t2010, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(rome_be_t2016, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2016, 0.63).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(rome_su_t2004, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2004, 0.6).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(rome_su_t2016, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, international_humanitarian_law_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is the 'universalist_reading' of the Rome Statute jurisdiction kernel. It is structurally distinct from the 'sovereigntist_reading' (emphasizing state consent) and the 'hybrid_complementarity_reading' (balancing universalism with sovereign primacy), which are modeled as separate constraints due to their differing ε values and stakeholder structures. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
