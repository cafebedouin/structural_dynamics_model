% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: Combatant Status Definition — State-Centric Reading (Article 4 Criteria)
 *   domain: international_law/armed_conflict
 *
 * SUMMARY:
 *   The state-centric reading of combatant status under Geneva Convention III
 *   Article 4 holds that only formal state military organizations meeting
 *   four criteria (commanded by a responsible person, fixed distinctive sign,
 *   carrying arms openly, conducting operations per laws of war) qualify for
 *   POW protections. Non-state actors — regardless of organization,
 *   discipline, or cause — are categorically excluded. This reading treats
 *   combatant status as a sovereign privilege, not a functional status earned
 *   by conduct. The constraint coordinates clear status rules for inter-state
 *   war (a genuine coordination function) but extracts asymmetrically:
 *   non-state fighters lose all combatant immunity and face domestic
 *   prosecution for acts that are lawful for state soldiers. The extraction
 *   is enforced through state practice, military manuals, and domestic
 *   criminal law. The claim/metric gap is deliberate: the constraint is
 *   CLAIMED as a coordination mechanism (clear rules, civilian protection)
 *   while the authored metrics describe substantially extractive, actively
 *   enforced operation — the engine measures that divergence; do not
 *   reconcile the claim to the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.72).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.81).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "Combatant Status Definition — State-Centric Reading (Article 4 Criteria)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_law/armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, '813d1619-2941-4d59-91d9-8e56e2de06e1').
narrative_ontology:cs_kernel_codification('813d1619-2941-4d59-91d9-8e56e2de06e1', formalized).
narrative_ontology:cs_authority_grounding('813d1619-2941-4d59-91d9-8e56e2de06e1', lineage).
narrative_ontology:cs_interpretation_layer_present('813d1619-2941-4d59-91d9-8e56e2de06e1').
narrative_ontology:cs_reading_relation('813d1619-2941-4d59-91d9-8e56e2de06e1', combatant_status_definition__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('813d1619-2941-4d59-91d9-8e56e2de06e1', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('813d1619-2941-4d59-91d9-8e56e2de06e1', foundational, combatant_status_requires_state_organization).
narrative_ontology:cs_axiom_status(combatant_status_requires_state_organization, holdable).
narrative_ontology:cs_axiom_grounding('813d1619-2941-4d59-91d9-8e56e2de06e1', combatant_status_requires_state_organization, conventional).
narrative_ontology:cs_axiom('813d1619-2941-4d59-91d9-8e56e2de06e1', foundational, non_state_actors_categorically_excluded_from_pow_status).
narrative_ontology:cs_axiom_status(non_state_actors_categorically_excluded_from_pow_status, holdable).
narrative_ontology:cs_axiom_grounding('813d1619-2941-4d59-91d9-8e56e2de06e1', non_state_actors_categorically_excluded_from_pow_status, conventional).
narrative_ontology:cs_reference_frame('813d1619-2941-4d59-91d9-8e56e2de06e1', id_1949_geneva_conventions_framework).
narrative_ontology:cs_drift_state('813d1619-2941-4d59-91d9-8e56e2de06e1', contemporary_niac_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('813d1619-2941-4d59-91d9-8e56e2de06e1', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, detaining_powers).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, irregular_fighters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formal state armed forces meeting Article 4 criteria receive full POW protections upon capture, immunity from prosecution for lawful acts of war, and the privilege of combatant status. They set the agenda through state practice, treaty negotiation, and military doctrine. Their exit is arbitrage-grade: they operate within the system they help define and can invoke its protections or withdraw from treaties.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, state_militaries, agenda_setter).

% States that capture fighters hold the power to grant or deny POW status. Under this reading, they can prosecute non-state fighters under domestic criminal law while affording full Geneva protections to state military prisoners. They administer the constraint through military justice systems and detention policy. Their exit is arbitrage-grade: they control the legal framework and face no structural compulsion to extend protections beyond Article 4.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, detaining_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, detaining_powers, beneficiary).

% Organized non-state armed groups (insurgents, rebels, liberation movements) whose fighters are categorically denied POW status under this reading. Their members face prosecution for mere participation in hostilities (e.g., 'unlawful combatancy', terrorism charges), cannot claim combatant immunity, and have no legal pathway to acquire combatant status without state recognition. Exit is trapped: they cannot become state militaries without achieving statehood, and the legal framework offers no intermediate status.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_groups, payer,
    organized, biographical, trapped, global).

% Individuals participating in hostilities without formal state military affiliation — militias, volunteers, resistance fighters, spontaneous uprisings. They bear the full prosecutorial risk of 'unlawful combatant' designation, have no command structure to confer status, and are subject to domestic criminal law for acts that would be lawful if done by state soldiers. Exit is trapped: they cannot individually satisfy Article 4 criteria, and the constraint offers no individual pathway to protection.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, irregular_fighters, payer,
    powerless, immediate, trapped, local).

% Civilians benefit from the clear combatant/civilian distinction this reading maintains: the binary classification aims to protect non-participants from targeting. However, in non-international armed conflicts where the constraint denies status to all non-state fighters, civilians may face blurred lines and collective suspicion. Their exit is constrained: they cannot opt out of the legal framework governing their protection.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, civilian_populations, beneficiary,
    organized, generational, constrained, global).

% International criminal tribunals (ICTY, ICC, etc.) and the ICJ adjudicate combatant status in specific cases. They interpret Article 4 and AP I but cannot amend the treaty framework. Their analytical seat sees the full structural asymmetry: state militaries receive automatic protections while non-state fighters must litigate status case-by-case, often unsuccessfully.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_courts, observer,
    institutional, generational, analytical, global).

% ICRC, NGOs, and humanitarian actors advocate for functional protections regardless of status. They are structurally excluded from the combatant-status decision (a state/treaty prerogative) but bear operational consequences: denied access to non-state fighters detained as 'criminals', inability to visit 'unlawful combatants' under POW conventions. Their exclusion is the enforcement mechanism: their advocacy for broader protections is treated as political, not legal.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, humanitarian_organizations, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, detaining_powers).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a binary, treaty-based classification (Article 4 GC III) that distinguishes lawful combatants entitled to POW status from all others, enabling targeting rules, detention regimes, and prosecutorial clarity in international armed conflict.
% TRANSFER_FUNCTION: Moves POW immunity, Geneva Convention protections, and combatant privilege from non-state fighters to state militaries exclusively. Non-state fighters bear full prosecutorial exposure under domestic law for participation in hostilities; detaining powers gain prosecutorial leverage over non-state actors while retaining full obligations toward state prisoners.
% ABSENT_VOICES: National liberation movements fighting colonial/occupation/racist regimes (who claim AP I Art 1(4) status); populations in non-international armed conflicts denied any combatant status; 'foreign fighters' and transnational non-state actors who fall in no recognized category; domestic courts in non-party states applying the constraint without treaty obligation.
% DISAPPEARANCE_RATIONALE: If the state-centric combatant definition vanished overnight, non-state fighters in international armed conflicts would gain POW protections, detaining powers would lose the legal basis for prosecuting mere participation, and the treaty framework would reorganize around functional criteria (effective command, distinction compliance) or national-liberation criteria. The legal architecture of armed conflict — targeting, detention, prosecution — would restructure.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions needed to distinguish lawful combatants entitled to POW status from unlawful combatants who could be prosecuted, to protect civilians from targeting and ensure humane treatment of prisoners in inter-state war.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC Commentary on the 1949 Conventions attests the founding problem was civilian protection and prisoner treatment in international war. State practice since 1949 and the 1977 Additional Protocol I negotiations corroborate that the state-centric reading was contested from inception by national liberation movements and humanitarian law scholars who argued the binary excluded wars of self-determination.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72) because the constraint transfers prosecutorial immunity and Geneva protections from non-state fighters to state militaries as a class, decoupled from individual conduct. Suppression is very high (0.81) because non-state actors have no legal pathway to acquire status — the exclusion is categorical and enforced through domestic criminal law, military detention policy, and treaty interpretation. Theater is low (0.22): the coordination function (clear status in IAC) is real but narrow; the growing gap between inter-state and non-international conflict practice makes the performance of 'universal' rules increasingly theatrical. The measurement series runs on one shared time grid (1949=0, 2024=75) with points at key inflection moments: 1949 adoption, 1977 AP I, 1990s ICTY jurisprudence, post-9/11 'unlawful combatant' doctrine, contemporary NIAC-dominated conflict landscape.
 *
 * PERSPECTIVAL GAP:
 *   From the state military/detaining power seat, the constraint is genuine coordination: clear rules they built, maintain, and benefit from. From the non-state fighter seat, the same structure operates as enforced extraction: categorical denial of protections they cannot earn, with no exit. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and detaining powers are structural beneficiaries (d near 0.0): they receive the constraint's protections and control its application. Non-state armed groups and irregular fighters are structural targets (d near 1.0): they bear the full prosecutorial burden with trapped exit. Civilian populations sit near symmetric (d ~0.5): they gain civilian protection from the binary distinction but lose when the distinction blurs in NIAC. International courts are analytical observers (d=0.5 fixed). Humanitarian organizations are excluded: their advocacy for functional protections is structurally blocked by the same state-centric framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (civilian protection in inter-state war) is contested: state practice shows the binary works for IAC but fails in NIAC (now the dominant conflict form). The constraint persists because states benefit from the prosecutorial leverage over non-state actors, not because the founding problem remains live in its original form. This is tangled_rope, not snare, because the coordination function (status clarity in IAC) is real and valued by both state and some non-state actors; but the asymmetric extraction (categorical exclusion) is substantial and actively enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article4_natural_vs_constructed,
    'Are the Article 4 criteria genuine coordination requirements (functional necessities for distinguishing combatants) or constructed thresholds that favor state militaries?',
    'Comparative analysis of military effectiveness: do non-state groups that meet functional criteria (command, distinction, open carry) but lack state authorization perform equivalently in combat identification and civilian protection?',
    'If Article 4 criteria are functionally necessary, the constraint is closer to rope; if they are constructed barriers, the extraction is structural and the constraint is tangled_rope/snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article4_natural_vs_constructed, conceptual, 'Whether combatant criteria are functional coordination requirements or state-privileging constructions.').

omega_variable(
    categorical_exclusion_necessity,
    'Is the categorical exclusion of non-state actors from POW status structurally necessary for the coordination function, or could a functional status regime achieve the same civilian protection without asymmetric extraction?',
    'Empirical study of AP I Art 1(4) state practice and functional-protection regimes: do states that extend status to organized non-state groups lose targeting clarity or civilian protection?',
    'If exclusion is unnecessary, the constraint''s extraction is avoidable overhead — strengthening tangled_rope classification. If necessary, part of measured extraction is the price of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_exclusion_necessity, empirical, 'Whether asymmetric extraction is structurally coupled to the coordination function.').

omega_variable(
    committer_frame_ambiguity,
    'Does this reading''s core premise (combatant status = state military only) logically foreclose the national_liberation_reading, or do they operate in different jurisdictional domains?',
    'Legal analysis of treaty interpretation: can a single state be bound by both the state-centric GC III reading and AP I Art 1(4) for different conflicts, or does acceptance of one require rejection of the other?',
    'If forecloses, the kernel has a genuine logical schism; if coexists_with different domains, the readings partition the conflict-space rather than contradict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Structural relationship between state-centric and national-liberation readings of the combatant-status kernel.').

omega_variable(
    internalized_suppression_non_state_fighters,
    'Do non-state fighters internalize the denial of combatant status (accepting criminalization as legitimate), or is suppression purely structural (external legal barriers)?',
    'Post-conflict studies: do former non-state fighters who gain state recognition (via peace deals, statehood) retroactively claim combatant status, or do they accept prior criminalization?',
    'If internalized, the constraint''s effective suppression exceeds structural measurement — the target carries the status denial as identity. If purely structural, exit (statehood/recognition) dissolves suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_non_state_fighters, empirical, 'Structural vs. internalized suppression mechanism for non-state fighters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csd_scr_tr_t0, combatant_status_definition__state_centric_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(csd_scr_tr_t15, combatant_status_definition__state_centric_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(csd_scr_tr_t28, combatant_status_definition__state_centric_reading, theater_ratio, 28, 0.2).
narrative_ontology:measurement(csd_scr_tr_t40, combatant_status_definition__state_centric_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(csd_scr_tr_t55, combatant_status_definition__state_centric_reading, theater_ratio, 55, 0.22).
narrative_ontology:measurement(csd_scr_tr_t75, combatant_status_definition__state_centric_reading, theater_ratio, 75, 0.22).

% Extraction over time
narrative_ontology:measurement(csd_scr_be_t0, combatant_status_definition__state_centric_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(csd_scr_be_t15, combatant_status_definition__state_centric_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(csd_scr_be_t28, combatant_status_definition__state_centric_reading, base_extractiveness, 28, 0.58).
narrative_ontology:measurement(csd_scr_be_t40, combatant_status_definition__state_centric_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(csd_scr_be_t55, combatant_status_definition__state_centric_reading, base_extractiveness, 55, 0.71).
narrative_ontology:measurement(csd_scr_be_t75, combatant_status_definition__state_centric_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(csd_scr_su_t0, combatant_status_definition__state_centric_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(csd_scr_su_t15, combatant_status_definition__state_centric_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(csd_scr_su_t28, combatant_status_definition__state_centric_reading, suppression_requirement, 28, 0.78).
narrative_ontology:measurement(csd_scr_su_t40, combatant_status_definition__state_centric_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(csd_scr_su_t55, combatant_status_definition__state_centric_reading, suppression_requirement, 55, 0.81).
narrative_ontology:measurement(csd_scr_su_t75, combatant_status_definition__state_centric_reading, suppression_requirement, 75, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__state_centric_reading, 0.12).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, non_international_armed_conflict_regime).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, unlawful_combatant_doctrine).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, targeted_killing_legal_framework).

% DUAL FORMULATION NOTE:
% Part of the combatant_status_definition constraint family. This state_centric_reading decomposes from the kernel with national_liberation_reading and functional_protection_reading. The ε values differ substantially: this reading authors high ε for non-state fighters (categorical exclusion); functional_protection_reading authors near-zero ε (minimum protections for all); national_liberation_reading authors intermediate ε (status available but conditional). Linked via network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__state_centric_reading, organized, 0.85).
constraint_indexing:directionality_override(combatant_status_definition__state_centric_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
