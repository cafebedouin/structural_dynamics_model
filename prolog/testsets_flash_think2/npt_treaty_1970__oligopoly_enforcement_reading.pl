% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__oligopoly_enforcement_reading, []).

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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT 1970: Oligopoly Enforcement Reading
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint is the 'oligopoly enforcement' reading of the NPT
 *   (npt_treaty_1970), focusing on Articles I-II as primary obligations for
 *   Non-Nuclear Weapon States (NNWS), while viewing Article VI (disarmament)
 *   as contingent and aspirational for Nuclear Weapon States (NWS). This
 *   reading highlights the enforcement asymmetry where NNWS bear a high
 *   inspection burden and forego a deterrent, while NWS maintain their
 *   arsenals and benefit from a global status hierarchy. Sibling readings
 *   include 'reciprocal disarmament' (emphasizing Article VI) and 'withdrawal
 *   sovereignty' (emphasizing Article X).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.8).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.85).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT 1970: Oligopoly Enforcement Reading").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, '999c103d-c926-4169-b401-da9b65b100b5').
narrative_ontology:cs_kernel_codification('999c103d-c926-4169-b401-da9b65b100b5', fixed_text).
narrative_ontology:cs_authority_grounding('999c103d-c926-4169-b401-da9b65b100b5', extraction).
narrative_ontology:cs_interpretation_layer_present('999c103d-c926-4169-b401-da9b65b100b5').
narrative_ontology:cs_reading_relation('999c103d-c926-4169-b401-da9b65b100b5', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('999c103d-c926-4169-b401-da9b65b100b5', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('999c103d-c926-4169-b401-da9b65b100b5', foundational, horizontal_proliferation_is_existential_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_existential_threat, holdable).
narrative_ontology:cs_axiom_grounding('999c103d-c926-4169-b401-da9b65b100b5', horizontal_proliferation_is_existential_threat, empirically_contingent).
narrative_ontology:cs_axiom('999c103d-c926-4169-b401-da9b65b100b5', foundational, nws_retain_special_status_for_stability).
narrative_ontology:cs_axiom_status(nws_retain_special_status_for_stability, holdable).
narrative_ontology:cs_axiom_grounding('999c103d-c926-4169-b401-da9b65b100b5', nws_retain_special_status_for_stability, conventional).
narrative_ontology:cs_reference_frame('999c103d-c926-4169-b401-da9b65b100b5', nws_oligopoly_security_order).
narrative_ontology:cs_drift_state('999c103d-c926-4169-b401-da9b65b100b5', contemporary_nonproliferation_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('999c103d-c926-4169-b401-da9b65b100b5', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_weapon_states_p5).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states_nnws).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states_seeking_deterrent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five recognized nuclear weapon states (US, UK, France, Russia, China) who benefit from the NPT's status hierarchy, maintaining their arsenals while enforcing non-proliferation on others. They control the UN Security Council and diplomatic levers.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_weapon_states_p5, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The vast majority of states that have foresworn nuclear weapons under the NPT. They bear the burden of IAEA inspections and transparency requirements, and forego the option of developing a nuclear deterrent, often feeling that the NWS have not upheld their disarmament obligations.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states_nnws, payer,
    organized, biographical, constrained, global).

% States that perceive significant security threats and are denied the option of a nuclear deterrent by the NPT regime. Their identity and national security narratives are often tied to the pursuit of such capabilities, making exit from the non-proliferation norm difficult but tempting.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states_seeking_deterrent, payer,
    powerful, biographical, identity_locked, regional).

% The International Atomic Energy Agency, responsible for verifying NNWS compliance with their non-proliferation obligations through inspections. Its effectiveness is dependent on the political will and funding provided by member states, particularly the NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea, agenda_setter,
    institutional, biographical, constrained, global).

% Non-governmental organizations and activists who advocate for nuclear disarmament and criticize the NWS for failing to meet their Article VI obligations. They are outside the formal decision-making structures of the NPT regime but exert moral and political pressure.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, global_civil_society_disarmament_advocates, excluded,
    organized, generational, constrained, global).

% The primary international body for enforcing non-proliferation, with the power to impose sanctions or authorize military action. Its permanent members are the NWS, giving them a veto over actions that might challenge their own nuclear status or interests.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, un_security_council, agenda_setter,
    institutional, civilizational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent the horizontal proliferation of nuclear weapons, thereby reducing the risk of nuclear war and maintaining a degree of global strategic stability by limiting the number of nuclear-armed states.
% TRANSFER_FUNCTION: Transfers security and strategic advantage to the Nuclear Weapon States (P5) by legitimizing their arsenals and preventing others from acquiring them. This is achieved by imposing inspection burdens and foregone deterrent capabilities on Non-Nuclear Weapon States (NNWS).
% ABSENT_VOICES: States that feel existentially threatened and are denied a nuclear deterrent (e.g., Iran, North Korea, potentially others) are structurally excluded from the NPT's core bargain. They would argue for a more equitable security framework or the right to self-defense through nuclear deterrence. Disarmament advocates would argue for stronger NWS accountability under Article VI.
% DISAPPEARANCE_RATIONALE: If the NPT and its enforcement mechanisms vanished overnight, horizontal proliferation would accelerate dramatically. Many states would likely pursue nuclear weapons, leading to a far more unstable and dangerous world with increased risks of regional nuclear conflicts and global escalation. The global security architecture would fundamentally shift.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent an uncontrolled arms race, particularly the horizontal spread of nuclear weapons, following World War II and the early Cold War.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars, UN reports, and intelligence agencies corroborate the original problem of horizontal proliferation as still live. However, many NNWS and civil society groups attest that the problem of vertical proliferation (NWS arsenals) remains unaddressed, shifting the nature of the 'live' problem from its original framing.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and increasing because the NWS have largely failed to meet their Article VI disarmament obligations, while NNWS continue to bear the costs of non-proliferation. Suppression is very high, reflecting the robust international mechanisms (sanctions, diplomatic pressure, potential military action) used to prevent horizontal proliferation. Theater ratio is moderate and rising, as NWS disarmament commitments are increasingly seen as performative rather than substantive. Accessibility collapse is high for NNWS, as the regime makes nuclear weapons development extremely difficult. Resistance is moderate, coming from states challenging the regime's legitimacy or seeking their own deterrent.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the NWS, the NPT is a successful coordination mechanism that prevents global nuclear chaos. From the perspective of many NNWS and threshold states, it is an extractive regime that perpetuates an unequal security order. The engine's classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nuclear Weapon States (P5) are clear beneficiaries, maintaining their strategic advantage and status. Non-Nuclear Weapon States (NNWS) and especially threshold states seeking a deterrent are victims, bearing the costs of compliance and foregone security options. The IAEA and UN Security Council act as agenda-setters, enforcing the regime, but their actions are heavily influenced by the NWS. Global civil society acts as an excluded voice, advocating for a different interpretation of the treaty.
 *
 * MANDATROPHY ANALYSIS:
 *   The NPT's original mandate to prevent horizontal proliferation remains live. However, the 'oligopoly enforcement' reading suggests a mandatrophy in the NWS's commitment to vertical disarmament (Article VI). The persistence of NWS arsenals, despite the founding problem of nuclear war, indicates a shift where the regime's function has become more about maintaining a power hierarchy than achieving universal disarmament. This is captured by the rising extractiveness and theater ratio, and the 'contested' status of the founding problem's live nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_status_ambiguity,
    'Is NPT Article VI (disarmament obligation) a binding legal commitment with temporal urgency, or an aspirational goal contingent on global security conditions?',
    'International Court of Justice advisory opinion on the legal force and timeline of Article VI, or a new UN resolution explicitly defining its binding nature and implementation schedule.',
    'If binding and urgent, the NWS''s continued possession of nuclear weapons would be a clear violation, increasing the regime''s measured extractiveness and suppression, and potentially reclassifying it closer to a Snare. If aspirational, the current reading''s metrics are more aligned with the NWS''s interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_status_ambiguity, conceptual, 'Ambiguity regarding the legal status and urgency of NWS disarmament obligations.').

omega_variable(
    oligopoly_vs_coordination_primary_function,
    'Is the NPT''s primary function to prevent horizontal proliferation (coordination) or to maintain the nuclear oligopoly of the P5 (extraction)?',
    'Analysis of NWS behavior regarding disarmament vs. non-proliferation enforcement over time, and the impact of NWS actions on NNWS security perceptions. If NWS prioritize maintaining their arsenals over disarmament, it supports the oligopoly reading.',
    'If primarily oligopoly, the constraint''s base extractiveness is higher, and its coordination function is largely a cover story, pushing classification towards Snare. If primarily coordination, the extraction is a regrettable but necessary side effect, keeping it a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligopoly_vs_coordination_primary_function, conceptual, 'Whether the NPT primarily serves coordination or oligopolistic extraction.').

omega_variable(
    foregone_deterrent_cost,
    'What is the true security cost for threshold states of foregoing a nuclear deterrent in the face of perceived existential threats?',
    'Comparative case studies of states with and without nuclear deterrents facing similar threats, and detailed security assessments by independent defense analysts. Analysis of the effectiveness of security guarantees offered by NWS.',
    'If the security cost is demonstrably high and unmitigated by NWS guarantees, it increases the measured extractiveness from threshold states, strengthening their victim status and potentially increasing resistance to the regime.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foregone_deterrent_cost, empirical, 'The unquantified security cost borne by states denied a nuclear deterrent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This is one of three structurally distinct readings of the NPT (npt_treaty_1970). This 'oligopoly enforcement' reading emphasizes horizontal non-proliferation and NWS privilege, while the 'reciprocal disarmament' reading emphasizes Article VI, and the 'withdrawal sovereignty' reading emphasizes Article X. Each reading has a distinct ε value and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
