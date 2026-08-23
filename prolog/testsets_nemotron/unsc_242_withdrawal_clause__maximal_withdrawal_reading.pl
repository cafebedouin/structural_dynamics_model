% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC Res 242 Maximal Withdrawal Reading (French definite article)
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) calls for 'withdrawal of Israeli armed forces
 *   from territories occupied in the recent conflict' (English) vs 'retrait
 *   des forces armées israéliennes des territoires occupés' (French). The
 *   maximal reading treats the French definite article 'des territoires
 *   occupés' as controlling — requiring withdrawal from ALL occupied
 *   territories as the Article 2(4) territorial integrity default. This
 *   reading instantiates a Rope: it coordinates a comprehensive legal
 *   obligation binding the occupying state to full retrocession, with
 *   beneficiaries (dispossessed claimants, frontline states, the legal order
 *   itself) holding enforceable legal positions. The constraint is mandatory
 *   and comprehensive — ε is high because the occupying state bears the full
 *   cost of reversing its military gains. The competing partial reading
 *   (indefinite English article, drafters' intent, 'secure boundaries') is a
 *   separate constraint linked via the kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.78).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.72).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC Res 242 Maximal Withdrawal Reading (French definite article)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '0073969d-ed41-41ce-beee-386bbeb70755').
narrative_ontology:cs_kernel_codification('0073969d-ed41-41ce-beee-386bbeb70755', fixed_text).
narrative_ontology:cs_authority_grounding('0073969d-ed41-41ce-beee-386bbeb70755', lineage).
narrative_ontology:cs_interpretation_layer_present('0073969d-ed41-41ce-beee-386bbeb70755').
narrative_ontology:cs_reading_relation('0073969d-ed41-41ce-beee-386bbeb70755', unsc_242_withdrawal_clause__partial_withdrawal_reading, influences).
narrative_ontology:cs_reading_relation('0073969d-ed41-41ce-beee-386bbeb70755', unsc_242_withdrawal_clause__interpretive_authority_structure, coexists_with).
narrative_ontology:cs_axiom('0073969d-ed41-41ce-beee-386bbeb70755', foundational, french_definite_article_controls_withdrawal_scope).
narrative_ontology:cs_axiom_status(french_definite_article_controls_withdrawal_scope, holdable).
narrative_ontology:cs_axiom_grounding('0073969d-ed41-41ce-beee-386bbeb70755', french_definite_article_controls_withdrawal_scope, conventional).
narrative_ontology:cs_axiom('0073969d-ed41-41ce-beee-386bbeb70755', foundational, territorial_integrity_default_requires_full_retrocession).
narrative_ontology:cs_axiom_status(territorial_integrity_default_requires_full_retrocession, holdable).
narrative_ontology:cs_axiom_grounding('0073969d-ed41-41ce-beee-386bbeb70755', territorial_integrity_default_requires_full_retrocession, deontological).
narrative_ontology:cs_reference_frame('0073969d-ed41-41ce-beee-386bbeb70755', resolution_242_french_text_authority).
narrative_ontology:cs_drift_state('0073969d-ed41-41ce-beee-386bbeb70755', post_icj_wall_opinion_2004, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0073969d-ed41-41ce-beee-386bbeb70755', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, frontline_states).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_order).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, settler_populations).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, territorial_integrity_default).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, inadmissibility_of_acquisition_by_force).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, french_text_authority_in_multilingual_treaties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the occupied territories militarily and administratively. Bears the political, security, and economic costs of withdrawal. Sets the agenda for implementation timing and conditions through its control of facts on the ground. Exit from the constraint requires either military defeat, overwhelming diplomatic isolation, or voluntary policy reversal — all structurally difficult.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, agenda_setter).

% States or recognized national movements with sovereign title to the occupied territories. The constraint's full implementation restores their territorial integrity and political sovereignty. Their claim is legally entrenched — exit from the constraint means abandoning the legal basis of their statehood. Identity-locked because the territorial claim constitutes their international legal personality.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants, beneficiary,
    organized, generational, identity_locked, continental).

% Neighboring states directly affected by the occupation's security and demographic consequences. Benefit from withdrawal through restored regional stability and normalized relations. Their exit options are constrained by geography and the regional security architecture — they cannot exit the neighborhood.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, frontline_states, beneficiary,
    moderate, biographical, constrained, regional).

% Civilian populations transferred into occupied territories by the occupying state. Bear the personal costs of withdrawal (displacement, loss of property, community rupture). Structurally trapped — they lack independent political agency and their fate is determined by the occupying state's decisions and the claimants' restoration of sovereignty.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, settler_populations, payer,
    moderate, biographical, trapped, local).

% The normative system of treaty law, UN Charter principles, and ICJ jurisprudence. The maximal reading vindicates the system's core propositions: territorial integrity as default, inadmissibility of acquisition by force, and French textual authority in authentic multilingual texts. Does not 'collect' extraction but its coherence is the stake.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_order).

% The judicial organ claiming authoritative interpretation of the resolution's text. Its 2004 Advisory Opinion on the Wall affirmed the maximal reading. Administers the constraint's legal meaning but lacks enforcement power — dependent on UNSC political will.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj_interpretive_authority, agenda_setter,
    institutional, generational, analytical, universal).

% UK and US delegations who drafted Resolution 242 and maintain the indefinite English article ('withdrawal from occupied territories') was intentional to permit boundary adjustments. Their authorial-intent reading is excluded from the maximal reading's legal framework but persists in diplomatic practice and negotiating positions.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, drafting_states_uk_us, excluded,
    institutional, biographical, arbitrage, global).

% Non-claimant regional powers (e.g., Egypt, Jordan, Saudi Arabia, Iran, Turkey) who mediate, guarantee, or undermine implementation. Their position shifts with geopolitical calculations — they can engage or disengage from the constraint's enforcement coalition.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, regional_great_powers, observer,
    powerful, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a legally binding, comprehensive withdrawal from all territories occupied in the 1967 conflict, replacing the factual reality of occupation with the legal default of territorial integrity. Solves the collective-action problem of enforcing a UNSC mandatory resolution against a powerful occupying state by anchoring withdrawal in the Charter's Article 2(4) and the resolution's French authentic text.
% TRANSFER_FUNCTION: Moves the burden of withdrawal (political cost, security risk, economic dislocation, settler displacement) from the dispossessed claimants and the international legal order onto the occupying state and its transferred populations. Transfers legal title and effective control from occupier to sovereign claimants.
% ABSENT_VOICES: Palestinian civilian population under occupation (not a state party, no formal seat in UNSC drafting or ICJ proceedings); third-party states who would bear refugee flows or security externalities from withdrawal; Israeli civil society dissenters who oppose occupation but are structurally unrepresented in the occupying state's decision calculus.
% DISAPPEARANCE_RATIONALE: If the maximal withdrawal reading vanished overnight, the legal default would revert to the partial/negotiated reading — the occupying state would retain legal discretion over withdrawal scope, settler populations would gain legal protection for their presence, and the territorial integrity default would be subordinated to 'secure boundaries' negotiation. The occupation's legal character would shift from 'belligerent occupation requiring termination' to 'disputed territories subject to negotiation.'
% FOUNDING_PROBLEM: The 1967 war created a military occupation of territories belonging to sovereign states (Egypt, Jordan, Syria). The international community needed a binding framework to terminate the occupation and restore the territorial integrity default without rewarding aggression. Resolution 242 was adopted as that framework.
% FOUNDING_PROBLEM_CORROBORATION: ICJ Advisory Opinion (2004) and UNSC Resolutions 338, 1515, 2334 affirm the founding problem (occupation of sovereign territory) remains live. The occupying state and drafting states contend the problem was transformed by subsequent peace treaties (Egypt, Jordan) and the Oslo process — but those same instruments reference Res 242 as their basis. No neutral corroborating source treats the founding problem as dead.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint demands the occupying state surrender all territorial gains from the 1967 war — a comprehensive transfer of control, security architecture, and demographic facts on the ground. Suppression (0.72) reflects the active enforcement required: the constraint persists only through sustained UNSC pressure, ICJ jurisprudence, and the occupying state's inability to legitimize retention. Theater ratio (0.25) is moderate: the 'peace process' (Oslo, Camp David, Annapolis) performs negotiation while the constraint's core demand (full withdrawal) remains unimplemented — but the performance is not the constraint's primary mode; the constraint's legal force is real. Accessibility collapse (0.68) is significant: once the French text's authority is accepted, the partial reading's legal space collapses. Resistance (0.55) is substantial: the occupying state has resisted for 57 years through settlement, annexation, and legal contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the occupying state's seat, the constraint appears as an extractive Snare: it demands total concession with no reciprocal security guarantee, enforced by a hostile international majority. From the dispossessed claimants' seat, it is a genuine Rope: the only mechanism that coordinates the international community around their legal right to territorial integrity. From the ICJ's seat, it is a Mountain of treaty interpretation: the French text controls as a matter of law. The engine computes this divergence from the structural data — the claimed_type (rope) reflects the maximal reading's own structural self-understanding as a coordination mechanism for comprehensive withdrawal.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state is the primary target (d ≈ 0.85): it bears the full extraction (territory, security, settlers, sovereignty claims) and has constrained exit. Settler populations are trapped targets (d ≈ 0.95): they bear disproportionate personal costs with zero agency. Dispossessed claimants are beneficiaries (d ≈ 0.15): the constraint restores their sovereign title. Frontline states are beneficiaries (d ≈ 0.25): they gain regional stability. The international legal order is an analytical beneficiary (d ≈ 0.0): its coherence is vindicated. The ICJ is an agenda-setter with analytical exit — it interprets but does not bear costs. Drafting states are excluded with arbitrage exit — they maintain an alternative reading but cannot enforce it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (terminating the 1967 occupation) remains contested — peace treaties with Egypt and Jordan implemented withdrawal from those specific territories, but the core occupation (West Bank, East Jerusalem, Golan, until 1981 Sinai, until 2005 Gaza) persists. The constraint has not atrophied into a Piton because its legal force remains active (ICJ 2004, UNSC 2334) and its beneficiaries still mobilize around it. It has not become a Snare because the coordination function (comprehensive withdrawal as legal default) remains real and the beneficiaries are not merely a cover. The mandate persists because the founding problem is only partially resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    french_text_authority_vs_drafting_history,
    'Does the Vienna Convention on the Law of Treaties (Art 33) require the French text to prevail over the English when the texts are equally authentic and the French is more precise, or does the drafting history (travaux préparatoires) showing intentional English indefiniteness override?',
    'ICJ authoritative interpretation or UNSC consensus on interpretive methodology. The ICJ has already ruled (2004 Wall Opinion) that the French text controls — but the occupying state and drafting states reject this.',
    'If French text controls (current ICJ position), maximal reading is legally binding Rope. If drafting history controls, partial reading gains legal parity and the constraint fragments into contested interpretations — the coordination function degrades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(french_text_authority_vs_drafting_history, conceptual, 'Whether textual authority or drafting intent governs the authentic multilingual text.').

omega_variable(
    secure_boundaries_principle_coherence,
    'Is the ''secure and recognized boundaries'' clause (Res 242 operative para 1(i)) a qualification of the withdrawal obligation (permitting retention for security) or a separate obligation on the parties to negotiate post-withdrawal boundaries?',
    'Systematic interpretation of the resolution as a whole: the French text ''des frontières sûres et reconnues'' is grammatically parallel to ''des territoires occupés'' — both definite articles. If withdrawal is from ALL territories, boundaries are negotiated after.',
    'If ''secure boundaries'' qualifies withdrawal, the constraint is Tangled Rope (coordination + extraction asymmetry). If it is a separate post-withdrawal obligation, the maximal reading remains a pure Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secure_boundaries_principle_coherence, conceptual, 'Whether the secure boundaries clause modifies the withdrawal scope or operates sequentially.').

omega_variable(
    subsequent_practice_as_authentic_interpretation,
    'Does the subsequent practice of the parties (Egypt-Israel treaty, Jordan-Israel treaty, Oslo Accords) constitute an authentic interpretation under VCLT Art 31(3)(b) that modifies the withdrawal obligation to a negotiated, partial basis?',
    'ICJ or arbitral determination of whether subsequent agreements between some parties can modify a UNSC Chapter VII resolution''s obligations erga omnes partes.',
    'If subsequent practice modifies the obligation, the maximal reading''s legal force is diminished for the territories covered by those agreements — the constraint becomes a family of partial implementations rather than a single comprehensive Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsequent_practice_as_authentic_interpretation, empirical, 'Whether bilateral peace treaties and interim agreements constitute authentic interpretation modifying the original obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc242_max_tr_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(unsc242_max_tr_t1973, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1973, 0.18).
narrative_ontology:measurement(unsc242_max_tr_t1982, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1982, 0.22).
narrative_ontology:measurement(unsc242_max_tr_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(unsc242_max_tr_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(unsc242_max_tr_t2004, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2004, 0.25).
narrative_ontology:measurement(unsc242_max_tr_t2017, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2017, 0.25).
narrative_ontology:measurement(unsc242_max_tr_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(unsc242_max_be_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(unsc242_max_be_t1973, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1973, 0.52).
narrative_ontology:measurement(unsc242_max_be_t1982, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1982, 0.58).
narrative_ontology:measurement(unsc242_max_be_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1993, 0.65).
narrative_ontology:measurement(unsc242_max_be_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2000, 0.71).
narrative_ontology:measurement(unsc242_max_be_t2004, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2004, 0.74).
narrative_ontology:measurement(unsc242_max_be_t2017, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2017, 0.76).
narrative_ontology:measurement(unsc242_max_be_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(unsc242_max_su_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(unsc242_max_su_t1973, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1973, 0.62).
narrative_ontology:measurement(unsc242_max_su_t1982, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1982, 0.68).
narrative_ontology:measurement(unsc242_max_su_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1993, 0.71).
narrative_ontology:measurement(unsc242_max_su_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(unsc242_max_su_t2004, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2004, 0.72).
narrative_ontology:measurement(unsc242_max_su_t2017, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2017, 0.72).
narrative_ontology:measurement(unsc242_max_su_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.12).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_338_ceasefire_implementation).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_2334_settlements_illegality).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj_wall_advisory_opinion_2004).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, oslo_accords_interim_agreement).

% DUAL FORMULATION NOTE:
% Kernel unsc_242_withdrawal_clause decomposes into three constraint stories: this maximal_withdrawal_reading (Rope, French definite article, ε=0.78), partial_withdrawal_reading (Tangled Rope, English indefinite article + secure boundaries, ε≈0.45), and interpretive_authority_structure (Snare, authority to resolve ambiguity extracts institutional legitimacy). The maximal reading's legal authority (ICJ, UNSC subsequent practice) influences the partial reading's legitimacy conditions. The interpretive authority structure is a meta-constraint on the kernel's resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, institutional, 0.85).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, moderate, 0.95).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, organized, 0.15).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, moderate, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
