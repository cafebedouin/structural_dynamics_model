% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__boundary_maintenance_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Catastrophe-Memory Mourning Ritual as Group Boundary Enforcement
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the catastrophe-memory kernel: the
 *   shared mourning-practice observed by a persecuted or minority group is
 *   read here as a boundary-maintenance mechanism — its function is to mark
 *   who belongs and who does not, coordinating in-group recognition and
 *   mutual aid while imposing conformity costs on deviant members and
 *   exclusionary costs on out-group relations and intermarried families.
 *   Sibling readings of the same underlying ritual practice (symbol
 *   continuity, survival competence, trauma encoding) are separate
 *   constraints with their own ε and are not evaluated here; see
 *   kernel_context.
 *
 * KEY AGENTS:
 *   - communal_leadership: agenda_setter (institutional/arbitrage) — administers the mourning calendar and its boundary rules
 *   - in_group_cohesion_stakeholders: beneficiary (organized/constrained) — gains identity, solidarity, mutual aid
 *   - boundary_crossing_members: payer (powerless/identity_locked) — sanctioned for deviation from prescribed practice
 *   - intermarried_families: payer (powerless/trapped) — bear the ritual's exclusionary edge structurally
 *   - out_group_neighbors: excluded (powerless/trapped) — cast as the boundary's reference point without voice
 *   - religious_studies_observers: analytical observer — documents comparative boundary-maintenance function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.52).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.48).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Catastrophe-Memory Mourning Ritual as Group Boundary Enforcement").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, '08b48807-d0f3-4fd2-abd4-b96936e33224').
narrative_ontology:cs_kernel_codification('08b48807-d0f3-4fd2-abd4-b96936e33224', distributed).
narrative_ontology:cs_authority_grounding('08b48807-d0f3-4fd2-abd4-b96936e33224', practice).
narrative_ontology:cs_interpretation_layer_present('08b48807-d0f3-4fd2-abd4-b96936e33224').
narrative_ontology:cs_reading_relation('08b48807-d0f3-4fd2-abd4-b96936e33224', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('08b48807-d0f3-4fd2-abd4-b96936e33224', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('08b48807-d0f3-4fd2-abd4-b96936e33224', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('08b48807-d0f3-4fd2-abd4-b96936e33224', foundational, group_boundary_integrity_requires_ritual_sanction).
narrative_ontology:cs_axiom_status(group_boundary_integrity_requires_ritual_sanction, holdable).
narrative_ontology:cs_axiom_grounding('08b48807-d0f3-4fd2-abd4-b96936e33224', group_boundary_integrity_requires_ritual_sanction, instrumental).
narrative_ontology:cs_axiom('08b48807-d0f3-4fd2-abd4-b96936e33224', secondary, conformity_cost_is_justified_by_cohesion_benefit).
narrative_ontology:cs_axiom_status(conformity_cost_is_justified_by_cohesion_benefit, holdable).
narrative_ontology:cs_axiom_grounding('08b48807-d0f3-4fd2-abd4-b96936e33224', conformity_cost_is_justified_by_cohesion_benefit, instrumental).
narrative_ontology:cs_reference_frame('08b48807-d0f3-4fd2-abd4-b96936e33224', post_persecution_diaspora_cohesion_imperative).
narrative_ontology:cs_drift_state('08b48807-d0f3-4fd2-abd4-b96936e33224', contemporary_pluralist_integration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08b48807-d0f3-4fd2-abd4-b96936e33224', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion_stakeholders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, boundary_crossing_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, intermarried_families).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_neighbors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the calendar of mourning observances, determines who counts as properly participating, and adjudicates disputes over correct practice. Draws authority and material support (leadership roles, communal deference, institutional funding) from being the recognized custodian of the memory-practice. Can modify or relax boundary requirements without losing standing, unlike ordinary members.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Long-committed community members who gain a stable, legible in-group identity, mutual-aid networks, and social capital from shared participation in the mourning calendar. The ritual's boundary-drawing function protects the resources and solidarity they depend on, even though they too bear some conformity cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_cohesion_stakeholders, beneficiary,
    organized, generational, constrained, national).

% Individuals who want to modify, skip, or reinterpret mourning practices — for personal grief style, changed belief, or practical constraint — face social sanction: exclusion from communal roles, gossip, withdrawal of mutual aid, or formal censure. Their exit is blocked by identity fusion: leaving the practice is experienced by self and community as leaving the group itself, not as a private choice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, boundary_crossing_members, payer,
    powerless, biographical, identity_locked, local).

% Families formed across the group boundary bear the ritual's exclusionary edge directly: children may be denied full ritual standing, spouses may be treated as permanent outsiders regardless of participation, and the mourning calendar becomes a recurring occasion where their liminal status is re-marked. They cannot simply opt out without severing kin ties on one or both sides.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, intermarried_families, payer,
    powerless, biographical, trapped, local).

% Neighbors and acquaintances outside the group are structurally positioned as the boundary against which the ritual defines belonging. They have no voice in how the practice marks them as other, and the ritual's periodic public reassertion of catastrophe-memory can reinforce local suspicion or distance that they did not choose and cannot address from outside.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_neighbors, excluded,
    powerless, immediate, trapped, local).

% Scholars who study the ritual comparatively, documenting how mourning practices function to mark and maintain group boundaries across different persecuted or minority communities, without being subject to the practice's sanctions themselves.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, religious_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__boundary_maintenance_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__boundary_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shared, synchronized mourning practice lets a dispersed or minority group recognize its own members reliably, coordinate mutual aid and marriage networks, and maintain a legible collective identity across generations without a central enforcement bureaucracy.
% TRANSFER_FUNCTION: Moves social standing, access to mutual-aid networks, and full ritual participation toward those who conform to the prescribed mourning practice, and away from those who deviate, intermarry, or stand outside the group — the same act of shared mourning that binds insiders together simultaneously marks and costs those on or across the boundary.
% ABSENT_VOICES: Out-group neighbors have no say in being cast as the reference point for the group's boundary; intermarried spouses and their children are rarely consulted on the terms of their partial inclusion; individuals with unconventional grief needs are rarely asked whether the prescribed form serves them, only whether they performed it.
% DISAPPEARANCE_RATIONALE: Communal leadership and long-committed members would say the group's cohesion and continuity depend on the practice and that its disappearance would fragment the community; boundary-crossing members, intermarried families, and outside observers would say the group's actual survival needs (mutual aid, identity, memory) could be met with looser or voluntary forms, and that only the exclusionary function would be missed by anyone who currently benefits from it.
% FOUNDING_PROBLEM: A minority or persecuted group facing potential dissolution through assimilation, dispersion, or forgetting needed a way to know who belonged, to hold itself together as a distinct people, and to keep its catastrophe legible as a live memory rather than a fading historical footnote.
% FOUNDING_PROBLEM_CORROBORATION: Communal leadership and core members attest the boundary problem remains live (assimilation and intermarriage pressures are ongoing). Independent religious-studies scholarship and testimony from boundary-crossing members and intermarried families outside the leadership seat corroborate that the underlying continuity problem persists in some form, but argue the current enforcement intensity exceeds what continuity requires and has become partly self-perpetuating status competition among leadership.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) — this is boundary-maintenance cost, not raw predation: the ritual genuinely coordinates a real collective-action problem (dispersed group cohesion) while simultaneously imposing conformity and exclusion costs. Suppression sits at a comparable moderate level (0.48) because sanction is mostly social (status withdrawal, gossip, exclusion from mutual aid) rather than coercive-legal; accessibility_collapse is higher (0.6) because once inside the identity frame, alternatives to the prescribed mourning form become nearly unthinkable for many participants, even though formally no one is physically prevented from leaving. Resistance is moderate (0.44), reflecting real but muted internal reform pressure from boundary-crossing members and intermarried families. Theater ratio and suppression_requirement both drift gently upward across the interval, modeling a boundary practice whose social enforcement has hardened somewhat as assimilation pressure increased, without becoming primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   From communal leadership's seat the practice reads as coordination (rope-like): a shared, necessary technology for group survival that they administer in good faith. From boundary-crossing members' and intermarried families' seats the same practice reads as extraction with a coordination alibi: the shared mourning act that supposedly serves everyone is the very mechanism that sanctions and marginalizes them. The engine computes this divergence from the differing power/exit structural data; the claimed_type (tangled_rope) already reflects that both readings are structurally present rather than picking a side.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal leadership sits nearest the beneficiary end: it administers the boundary, accrues status and institutional resources from being its custodian, and retains exit/arbitrage options ordinary members lack. In-group cohesion stakeholders benefit substantially though less concentratedly. Boundary-crossing members and intermarried families sit nearest the target end: the same ritual act that produces solidarity for compliant insiders produces sanction and liminal status for them, and their exit is blocked by either identity fusion (for the former) or kinship entanglement (for the latter) rather than by external law. Out-group neighbors are structurally positioned as excluded reference points rather than participants at all — they bear a cost (being marked as other) without ever being inside the transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assimilation/dissolution risk for a persecuted minority — is genuinely contested rather than simply dead: leadership and corroborating scholarship agree some version of the continuity problem persists, which blocks a clean 'dead mandate, live enforcement' zombie verdict. What keeps this a tangled_rope rather than a pure snare is the real coordination function (mutual aid network legibility, marriage-market coordination, intergenerational transmission of catastrophe memory) that persists alongside the extraction. Classifying this as pure extraction would erase the genuine benefit in_group_cohesion_stakeholders receive; classifying it as pure coordination would erase the real, non-trivial costs borne by boundary-crossing members, intermarried families, and out-group neighbors. The tangled_rope frame holds both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_function_vs_continuity_function_separability,
    'Is the boundary-enforcement function of the mourning ritual structurally separable from its symbol-continuity and survival-competence functions, or are they inseparably fused in the same practice?',
    'Comparative study of communities that have reformed mourning practice toward voluntary/inclusive participation while retaining commemorative content: if group cohesion and continuity persist without the exclusionary sanction apparatus, the functions are separable.',
    'If separable, the boundary-enforcement component identified in this reading is closer to pure extraction riding on the other readings'' genuine coordination functions; if inseparable, the moderate extractiveness authored here may understate an irreducible cost of any version of the practice robust enough to maintain group continuity at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_function_vs_continuity_function_separability, conceptual, 'Whether boundary-maintenance is separable from continuity/survival functions of the same ritual.').

omega_variable(
    consent_status_of_conformity_pressure,
    'Do in-group members who conform to the mourning practice do so from genuine endorsement of its boundary function, or from internalized fear of the sanctions documented for boundary-crossing members?',
    'Exit-interview-style data from members who have left or modified practice: if reported motivations center on relief from surveillance/sanction rather than loss of meaning, internalized suppression is substantial; if the reverse, conformity is closer to genuine endorsement.',
    'Higher internalized suppression would mean the authored suppression value (0.48) understates the effective suppression borne by nominally compliant in-group members, not just by openly boundary-crossing ones.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_status_of_conformity_pressure, empirical, 'Structural vs. internalized suppression among nominally compliant in-group members.').

omega_variable(
    framing_kernel_vs_legitimacy_layer,
    'Should the kernel here be read as the mourning-practice itself (the obvious framing) or as the community''s narrative that boundary-maintenance is necessary for survival (a legitimacy claim layered above the practice)?',
    'Compare communities where the same catastrophe-memory content is retained but the boundary-necessity narrative is explicitly rejected by leadership (e.g. reform movements that decouple commemoration from membership policing): if the practice persists functionally without the legitimacy narrative, the narrative — not the practice — is the true kernel of contest.',
    'If the legitimacy narrative is the true kernel, this reading''s axioms should be understood as claims about the narrative''s validity rather than claims about the ritual acts themselves, which would shift where reform pressure should be targeted (narrative vs. practice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_kernel_vs_legitimacy_layer, conceptual, 'Alternative framing: the ritual practice itself vs. the survival-necessity narrative layered above it as the contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 50, 0.46).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__boundary_maintenance_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language label 'ritual response to collective catastrophe' per the ε-invariance principle. Each reading names a structurally distinct claim about what the same observed mourning-practice does: boundary_maintenance_reading (this story, ε=0.52, tangled_rope) treats the practice as an in-group/out-group sorting mechanism; symbol_continuity_reading treats it as identity/meaning preservation across time; survival_competence_reading treats it as transmission of adaptive persecution-survival skill; trauma_encoding_reading treats it as intergenerational trauma/warning encoding. All four are linked via network.affects_constraints rather than merged, because they carry different beneficiary/victim structures and different ε values under a shared observable (the same ritual calendar).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
