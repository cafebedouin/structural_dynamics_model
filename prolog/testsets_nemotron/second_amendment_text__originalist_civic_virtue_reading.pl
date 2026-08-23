% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Originalist Civic Virtue Reading: Universal Armed Citizenry as Constitutional Right
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the originalist_civic_virtue_reading
 *   of the second_amendment_text kernel. The reading understands the Second
 *   Amendment as protecting the capacity of the citizenry qua political
 *   community to function as a universal armed militia — the citizen-soldier
 *   ideal of civic republican theory. The right is not primarily about
 *   personal self-defense (individual_right_reading) nor about
 *   state-organized collective security (collective_security_reading), but
 *   about the political virtue of an armed citizenry capable of defending
 *   republican liberty against tyranny, foreign and domestic. The beneficiary
 *   is the political community itself; there is no specific victim set in the
 *   reading's own structural logic, though the omega variables surface the
 *   contestable exclusions and compelled service of the historical reality.
 *
 * KEY AGENTS:
 *   - citizenry_qua_political_community: Primary beneficiary (collective/universal) — bears the obligation and receives the liberty guarantee
 *   - founding_generation_heirs: Secondary beneficiary (interpretive/identitarian) — claims continuity with the civic republican tradition
 *   - state_security_apparatus: Neither beneficiary nor victim in this reading — the reading limits state monopoly on force rather than serving state security
 *   - pacifists_conscientious_objectors: Potentially excluded (omega) — historical militia laws compelled service with limited exemptions
 *   - excluded_populations: Potentially victimized (omega) — enslaved persons, indigenous nations, women were outside 'the people' bearing arms
 *   - originalist_scholars_advocates: Agenda setter (interpretive) — maintains the reading as live constitutional theory
 *   - contemporary_citizens: Observer/participant — the reading addresses what civic capacity means today
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.12).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.15).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Originalist Civic Virtue Reading: Universal Armed Citizenry as Constitutional Right").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, '29a007a6-82e0-47c4-aac0-289321b82d97').
narrative_ontology:cs_kernel_codification('29a007a6-82e0-47c4-aac0-289321b82d97', fixed_text).
narrative_ontology:cs_authority_grounding('29a007a6-82e0-47c4-aac0-289321b82d97', lineage).
narrative_ontology:cs_interpretation_layer_present('29a007a6-82e0-47c4-aac0-289321b82d97').
narrative_ontology:cs_reading_relation('29a007a6-82e0-47c4-aac0-289321b82d97', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('29a007a6-82e0-47c4-aac0-289321b82d97', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('29a007a6-82e0-47c4-aac0-289321b82d97', foundational, armed_citizenry_constitutes_political_community).
narrative_ontology:cs_axiom_status(armed_citizenry_constitutes_political_community, holdable).
narrative_ontology:cs_axiom_grounding('29a007a6-82e0-47c4-aac0-289321b82d97', armed_citizenry_constitutes_political_community, deontological).
narrative_ontology:cs_axiom('29a007a6-82e0-47c4-aac0-289321b82d97', foundational, universal_militia_obligation_is_civic_virtue).
narrative_ontology:cs_axiom_status(universal_militia_obligation_is_civic_virtue, holdable).
narrative_ontology:cs_axiom_grounding('29a007a6-82e0-47c4-aac0-289321b82d97', universal_militia_obligation_is_civic_virtue, deontological).
narrative_ontology:cs_reference_frame('29a007a6-82e0-47c4-aac0-289321b82d97', founding_era_civic_republican_constitution).
narrative_ontology:cs_drift_state('29a007a6-82e0-47c4-aac0-289321b82d97', contemporary_constitutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('29a007a6-82e0-47c4-aac0-289321b82d97', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, citizenry_qua_political_community).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, founding_generation_heirs).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republican_armament_theory).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, universal_militia_obligation).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, armed_citizenry_as_liberty_guarantor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The political community as a collective agent that constitutes itself through the universal armed citizenry ideal. Bears the obligation of civic armament and receives the liberty guarantee. Exit from this identity is identity_locked — the community's self-conception as a republican polity is fused with the armed citizenry ideal; abandoning the reading would dissolve the community's founding identity.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, citizenry_qua_political_community, beneficiary,
    organized, generational, identity_locked, national).

% Originalist scholars, advocacy organizations, and judicial actors who maintain and develop this reading as live constitutional theory. They set the interpretive agenda, litigate cases, and publish scholarship. They benefit from the reading's institutional status (clerkships, funding, influence) but can exit to other interpretive frameworks (arbitrage-grade exit).
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, founding_generation_heirs, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Historically and potentially, those whose conscience forbids bearing arms. Founding-era militia laws provided limited exemptions (Quakers) but often imposed fines or alternative service. In this reading's logic, they are not part of the citizen-soldier political community — their exclusion is structural, not incidental. Their exit from the constraint is constrained: they cannot participate in the coordination function but remain subject to its political consequences.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, pacifists_conscientious_objectors, excluded,
    moderate, biographical, constrained, national).

% Enslaved persons, indigenous nations, women, and propertyless men were historically outside 'the people' entitled/obliged to bear arms in the founding-era universal militia. The civic republican ideal was racially and genderedly bounded. Their exclusion was not a side effect but constitutive of the political community's self-definition. Exit from this exclusion required transforming the political community itself (trapped).
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, excluded_populations, excluded,
    powerless, generational, trapped, national).

% Modern Americans who encounter this reading through constitutional discourse, education, or advocacy. They may adopt, reject, or ignore the civic virtue framing. Their relationship is observational — the reading addresses what their civic capacity means, but they are not organized as the citizenry qua political community in the founding sense. Mobile exit: they can engage with or disregard the reading without structural penalty.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, contemporary_citizens, observer,
    moderate, biographical, mobile, national).

% The professional military, law enforcement, and national security establishment. In this reading's logic, they are the entity the armed citizenry checks — not a beneficiary or victim of the constraint itself. Their analytical seat observes whether the reading constrains state action (it does, by limiting disarmament power) or enables it (it does not). Analytical exit: they assess the constraint from outside its coordination logic.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, state_security_apparatus, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__originalist_civic_virtue_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_text__originalist_civic_virtue_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the political community's capacity to resist tyranny and defend republican liberty through distributed armed capacity — the citizen-soldier ideal solves the collective action problem of defense without standing armies by making every citizen a participant in the security function.
% TRANSFER_FUNCTION: Moves the burden and privilege of defensive capacity from a specialized state apparatus to the universal citizenry. The citizenry bears the cost of armament and training; the political community receives the liberty guarantee. No monetary transfer — the transfer is of civic obligation and political virtue.
% ABSENT_VOICES: The excluded populations (enslaved, indigenous, women, propertyless) were not in the room when the founding-era militia ideal was formulated. Pacifists and conscientious objectors were marginally accommodated but structurally excluded from the citizen-soldier identity. Their objection would be that 'universal' was never universal and the civic virtue claim was built on their exclusion.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, constitutional discourse would lose the civic republican framing that ties the Second Amendment to political community self-constitution rather than individual self-defense or state regulation. The interpretive landscape would collapse to the binary of collective_security vs individual_right, losing the third structural position. Advocacy organizations, scholarship, and judicial arguments built on this reading would lose their foundation.
% FOUNDING_PROBLEM: The founding problem was securing republican liberty against the twin threats of standing armies (instrument of tyranny) and defenselessness (invitation to foreign domination) by constituting the citizenry itself as the armed defense — the universal militia as political virtue and structural guarantee of liberty.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is corroborated as contested by multiple outside sources: legal historians (e.g., Saul Cornell, 'A Well-Regulated Militia') document the civic republican understanding as historically dominant but contested even in the founding era; political theorists (e.g., Hannah Arendt, 'On Revolution') identify the armed citizen as a republican ideal that atrophied with modernity; military historians note the functional displacement by professional armies after the War of 1812. No single beneficiary group controls this corroboration.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the reading's core claim is a coordination function — the universal armed citizenry solves a genuine collective action problem (tyranny resistance, community defense) with minimal coercive overhead in its ideal form. Suppression is low (0.15) because the reading does not require active enforcement against alternatives; it is a structural claim about constitutional meaning, not a regulatory regime. Theater_ratio is moderate (0.28) because modern advocacy of this reading often performs civic virtue without the material conditions (universal training, shared obligation) that made the founding-era militia functional — the reading persists partly as identity signal. Accessibility_collapse is high (0.75) because the constitutional text, once read this way, leaves little room for alternative regulatory structures that would disarm the citizenry. Resistance is moderate (0.35) because the reading faces active contestation from both sibling readings and from state regulatory practice.
 *
 * PERSPECTIVAL GAP:
 *   From the citizenry_qua_political_community seat, the constraint is a rope — genuine coordination of civic capacity. From the excluded_populations seat (if instantiated), the same historical arrangement was a snare — compelled service or armed exclusion. From the state_security_apparatus seat, the constraint is a mountain (if accepted) or a snare (if resisted) — it limits state monopoly on force. The engine computes these per-seat classifications from the structural data; the authored claim (rope) reflects the reading's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary is the citizenry qua political community — a collective agent that both bears the obligation (universal service) and receives the benefit (liberty guarantee). This symmetric position yields directionality near 0.5. The founding_generation_heirs are interpretive beneficiaries (status, continuity) with arbitrage-grade exit (they can abandon the reading). No declared victims in the reading's own logic, but the omegas flag the historical reality where 'universal' was not universal and obligation was compelled. The state is neither beneficiary nor victim in this reading's structural logic — the reading positions the armed citizenry as a check ON the state, not a tool OF the state.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing republican liberty through citizen-soldier capacity against standing armies and tyranny) is contested: live in the sense that republican theory still values distributed defensive capacity; dead in the sense that the material conditions (universal militia, citizen-soldier as primary defense) have been displaced by professional standing armies and nuclear deterrence; contested because advocates claim the principle translates while critics claim the function atrophied. The mandate has not resolved — the reading persists as constitutional theory and advocacy position despite the functional gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the second_amendment_text kernel, or does it collapse into the collective_security_reading or individual_right_reading?',
    'Test whether the civic republican function (citizen-soldier capacity as political virtue) generates different beneficiary/victim structure and different extraction profile than the sibling readings. If the beneficiary is the citizenry qua political community rather than the state''s security apparatus (collective security) or private individuals (individual right), the reading is structurally distinct.',
    'If indistinguishable from collective_security_reading, this reading adds no new constraint — it is a rhetorical variant. If indistinguishable from individual_right_reading, the civic virtue framing is decorative. Distinctness validates the kernel decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural distinctness of the originalist civic virtue reading within the second amendment kernel family').

omega_variable(
    beneficiary_victim_asymmetry,
    'Does the civic republican framing genuinely lack a victim set, or does it extract from non-participants (pacifists, conscientious objectors, those excluded from the political community) in ways the reading''s own logic suppresses?',
    'Historical analysis of founding-era militia laws: who was compelled, who was exempted, who was excluded from ''the people'' bearing arms. Trace whether the universal armed citizenry ideal operated as a coordination mechanism or as an exclusionary boundary.',
    'If victims exist (compelled service, excluded populations), the claimed_type ''rope'' may compute as tangled_rope or snare. Absence of declared victims with actual extraction would trigger false_summit_mountain if claimed as mountain, or indicate incomplete structural description for rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_victim_asymmetry, empirical, 'Whether the civic republican coordination function carries hidden extraction from excluded or compelled parties').

omega_variable(
    modern_translatability,
    'Can the founding-era universal militia obligation translate to a modern coordination function without becoming extractive, or does the structural gap between 1790s civic capacity and contemporary state capacity force the reading into theater or extraction?',
    'Analyze whether any contemporary institutional arrangement fulfills the citizen-soldier capacity function (Swiss militia model, national service proposals, civilian marksmanship programs) without coercive overhead. If no live arrangement exists, the reading may be a piton — a former rope whose function atrophied.',
    'If untranslatable, the reading''s persistence in modern discourse is performative (theater_ratio understates) or extractive (advocacy organizations collecting status/rents from maintaining the claim). The engine''s temporal drift detection would capture this.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modern_translatability, preference, 'Whether the civic virtue coordination function survives the modernity transition or becomes inert/performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1865, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1865, 0.18).
narrative_ontology:measurement(seco_tr_t1903, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1903, 0.25).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1934, 0.4).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1968, 0.45).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2008, 0.32).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1791, 0.08).
narrative_ontology:measurement(seco_be_t1865, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1865, 0.15).
narrative_ontology:measurement(seco_be_t1903, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1903, 0.22).
narrative_ontology:measurement(seco_be_t1934, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1934, 0.35).
narrative_ontology:measurement(seco_be_t1968, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2008, 0.18).
narrative_ontology:measurement(seco_be_t2024, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(seco_su_t1865, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1865, 0.25).
narrative_ontology:measurement(seco_su_t1903, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1903, 0.35).
narrative_ontology:measurement(seco_su_t1934, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1934, 0.5).
narrative_ontology:measurement(seco_su_t1968, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1968, 0.55).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2008, 0.25).
narrative_ontology:measurement(seco_su_t2024, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_text__originalist_civic_virtue_reading, 0.08).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_text kernel decomposes into three constraint stories with different beneficiary/victim structures and extraction profiles. This reading (originalist_civic_virtue) claims rope with citizenry as beneficiary and civic republican coordination function. collective_security_reading claims tangled_rope with state security as beneficiary and regulation as extraction. individual_right_reading claims snare with individual gun owners as beneficiary and public safety as victim (or vice versa depending on framing). The ε values differ structurally: this reading's ε is lowest (coordination-dominant), collective_security is middle (hybrid), individual_right is highest (extraction-dominant in practice).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
