% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Convention Protective Scope — State-Centric Reading (Article 4 Combatant Privilege Only)
 *   domain: international_humanitarian_law/legal_theory/armed_conflict
 *
 * SUMMARY:
 *   This constraint story captures the state-centric reading of Geneva
 *   Convention protective scope: only uniformed combatants under responsible
 *   command meeting all four Article 4 criteria (fixed distinctive sign,
 *   responsible command, open carrying of arms, compliance with laws of war)
 *   qualify for combatant immunity and POW status. Unprivileged belligerents
 *   — including non-state armed groups, resistance fighters, and civilians
 *   directly participating in hostilities — fall outside treaty protections.
 *   The reading presents itself as the plain textual interpretation of the
 *   1949 Conventions (a Mountain claim), but its operation in asymmetric
 *   conflicts since the 1970s has concentrated extraction on non-state actors
 *   while lowering legal risk for state militaries. The constraint is
 *   actively enforced through military legal opinions, targeting policies,
 *   and detention practices; alternatives (universal protections, functional
 *   approaches) are suppressed through state sovereignty arguments and treaty
 *   textualism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.71).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, mountain).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Convention Protective Scope — State-Centric Reading (Article 4 Combatant Privilege Only)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/legal_theory/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).
domain_priors:emerges_naturally(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '6f220cf1-5296-4ae9-bd9b-33435c91ec23').
narrative_ontology:cs_kernel_codification('6f220cf1-5296-4ae9-bd9b-33435c91ec23', fixed_text).
narrative_ontology:cs_authority_grounding('6f220cf1-5296-4ae9-bd9b-33435c91ec23', extraction).
narrative_ontology:cs_interpretation_layer_present('6f220cf1-5296-4ae9-bd9b-33435c91ec23').
narrative_ontology:cs_reading_relation('6f220cf1-5296-4ae9-bd9b-33435c91ec23', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f220cf1-5296-4ae9-bd9b-33435c91ec23', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('6f220cf1-5296-4ae9-bd9b-33435c91ec23', foundational, combatant_privilege_requires_article4_compliance).
narrative_ontology:cs_axiom_status(combatant_privilege_requires_article4_compliance, holdable).
narrative_ontology:cs_axiom_grounding('6f220cf1-5296-4ae9-bd9b-33435c91ec23', combatant_privilege_requires_article4_compliance, conventional).
narrative_ontology:cs_axiom('6f220cf1-5296-4ae9-bd9b-33435c91ec23', foundational, unprivileged_belligerents_excluded_from_pow_status).
narrative_ontology:cs_axiom_status(unprivileged_belligerents_excluded_from_pow_status, holdable).
narrative_ontology:cs_axiom_grounding('6f220cf1-5296-4ae9-bd9b-33435c91ec23', unprivileged_belligerents_excluded_from_pow_status, conventional).
narrative_ontology:cs_reference_frame('6f220cf1-5296-4ae9-bd9b-33435c91ec23', id_1949_diplomatic_conference_article4_understanding).
narrative_ontology:cs_drift_state('6f220cf1-5296-4ae9-bd9b-33435c91ec23', contemporary_asymmetric_conflict_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6f220cf1-5296-4ae9-bd9b-33435c91ec23', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, state_legal_advisors).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_group_members).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, civilians_participating_in_hostilities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, icrc_and_humanitarian_orgs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and enforce the Article 4 criteria as the exclusive gateway to combatant immunity and POW status. Benefit from the ability to target unprivileged belligerents without extending Geneva protections, reducing legal risk in asymmetric conflicts. Control the interpretation machinery through military legal advisers and state practice.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary).

% Produce the authoritative legal opinions that narrow protective scope to Article 4-compliant forces. Their professional standing and institutional access depend on maintaining the state-centric interpretive framework. Exit requires leaving government service for academia or NGOs where the framework is contested.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_legal_advisors, beneficiary,
    organized, biographical, constrained, national).

% Fighters who do not meet Article 4 criteria (no fixed distinctive sign, no responsible command, no open carrying of arms). Denied combatant immunity and POW protections; subject to domestic criminal prosecution for mere participation in hostilities. No exit from the classification — it is imposed by the adversary's legal framework.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, immediate, trapped, local).

% Organized non-state actors who may meet some Article 4 criteria but lack state authorization. Caught in the gap: too organized for spontaneous civilian status, too irregular for combatant privilege. Some seek recognition through political negotiation; others accept the exclusion as operational reality.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_group_members, payer,
    moderate, biographical, constrained, regional).

% Civilians who directly participate in hostilities without meeting Article 4 criteria. Lose civilian protection for the duration of participation but gain no combatant immunity. The state-centric reading maximizes their vulnerability — targetable while participating, prosecutable afterward, with no intermediate protective status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, civilians_participating_in_hostilities, payer,
    powerless, immediate, trapped, local).

% Guardians of the Geneva Conventions who advocate for broader protective scope. Their mandate and operational access depend on state consent, creating tension between universalist advocacy and institutional survival. Benefit from the Conventions' existence but constrained by state-centric interpretation.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, icrc_and_humanitarian_orgs, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, icrc_and_humanitarian_orgs, beneficiary).

% Apply and interpret the protective scope in adjudicating war crimes. Their jurisprudence has gradually expanded protections (e.g., Tadić, Kunarac) but remains bounded by state consent to jurisdiction. The state-centric reading limits their ability to extend combatant privilege.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_tribunals, observer,
    institutional, generational, analytical, global).

% Academics and advocates who argue for universal protections grounded in human rights law and Common Article 3. Structurally excluded from the operational interpretation machinery — their arguments appear in amici briefs and commentary but do not bind state practice.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, legal_scholars_universalist, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line test (Article 4 criteria) for distinguishing lawful combatants from unprivileged belligerents, enabling states to organize military forces with clear legal status and reciprocal obligations.
% TRANSFER_FUNCTION: Transfers legal risk and protective status from unprivileged belligerents to conventional state militaries: the latter gain targeting latitude and prosecution immunity; the former lose combatant immunity, POW protections, and face criminal liability for belligerent acts.
% ABSENT_VOICES: The unprivileged belligerents themselves — non-state fighters, resistance movements, civilian populations in asymmetric conflicts — are structurally excluded from the treaty-drafting and interpretation process. Their voices appear only as objects of classification, not as participants in defining the classification.
% DISAPPEARANCE_RATIONALE: If the Article 4 gate vanished overnight, the legal architecture distinguishing lawful from unlawful combatancy would collapse. States would lose the legal basis for denying POW status to irregular fighters; non-state actors would gain immediate claim to combatant immunity; the entire edifice of status-based IHL would require reconstruction around conduct-based or universalist principles.
% FOUNDING_PROBLEM: The 1949 Diplomatic Conference sought to prevent the erosion of protections seen in WWII by creating clear, objective criteria for combatant status — preventing states from denying POW status to regular forces while also preventing irregular forces from claiming the privileges of regular armies without accepting their obligations.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's 1949 Commentary and the travaux préparatoires confirm the dual purpose: protect regular forces AND exclude irregulars. However, contemporary ICRC and human rights bodies attest the exclusionary function has overtaken the protective function in asymmetric conflicts. State military manuals (US, UK, Israel) corroborate the current reading serves operational latitude. No single source outside state militaries endorses the status quo as solving the original problem.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, ExtMetricName, E),
    domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(geneva_conventions_protective_scope__state_centric_reading),
    narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects the constraint's operation on its primary targets: unprivileged belligerents lose all combatant privileges and face criminal exposure for acts that would be lawful for privileged combatants. Suppression (0.71) is high because the Article 4 gate is maintained through active legal interpretation, military doctrine, and the refusal to extend protections even where Common Article 3 or human rights law might apply. Theater ratio (0.38) captures the gap between the Conventions' humanitarian framing and the exclusionary operation: the legal formalism is real (Article 4 criteria exist) but increasingly serves to legitimate exclusion rather than protect the vulnerable. Accessibility collapse (0.78) is high because once Article 4 is accepted as the exclusive gateway, alternative protective frameworks (human rights law, functional status) are structurally blocked for the excluded class. Resistance (0.54) is moderate — significant pushback from ICRC, human rights bodies, and some states (e.g., AP I Art. 44 supporters) but insufficient to shift state practice.
 *
 * PERSPECTIVAL GAP:
 *   From the conventional state military seat, the constraint appears as genuine coordination: a clear, reciprocal framework that solves the problem of distinguishing lawful from unlawful combatancy. From the unprivileged belligerent seat, the same structure operates as pure extraction — a classification imposed by the adversary that strips protections without consent. The engine will compute this divergence from the structural data (power asymmetry, exit_options: arbitrage vs trapped, spatial_scope: global vs local). The state_legal_advisors sit in a dual position: they benefit professionally from maintaining the framework but are constrained by professional ethics and the gradual jurisprudential drift toward broader protections.
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries are the primary beneficiaries (d near 0.0): they set the interpretation, collect the operational latitude, and face minimal legal risk. State legal advisors are secondary beneficiaries (d ~0.2) — they gain professional authority but are constrained by institutional role. Unprivileged belligerents, non-state armed group members, and civilians participating in hostilities are full targets (d near 1.0): they bear the full cost of exclusion with no exit. ICRC and international tribunals are near-symmetric observers (d ~0.5): they benefit from the Convention's existence but are constrained by its narrow reading. Legal scholars are excluded (d not computed) — their structural position is outside the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clear criteria to protect regular forces AND exclude irregulars) is contested: the protective function for regular forces remains live, but the exclusionary function has expanded beyond the original mandate as asymmetric conflict became dominant. The constraint now extracts from populations (irregular fighters, participating civilians) that the 1949 drafters did not primarily envision. This is not pure mandatrophy — the coordination function for state-on-state conflict persists — but the extraction on asymmetric targets has accumulated without corresponding justification. The reading coexists with sibling readings rather than foreclosing them, indicating the kernel remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article4_as_natural_boundary_vs_constructed_exclusion,
    'Are the Article 4 criteria a genuine natural legal boundary (reflecting the inherent reciprocity of combatant privilege) or a constructed exclusion that benefits state militaries in asymmetric conflicts?',
    'Historical analysis of the 1949 travaux préparatoires vs. contemporary state practice in non-international armed conflicts; comparative analysis of whether states applying Article 4 strictly in IAC also deny protections in NIAC where Article 4 does not formally apply.',
    'If natural boundary, the constraint is a Mountain with low extractiveness; if constructed exclusion, it is a Snare or Tangled Rope with high extractiveness on unprivileged belligerents. Triggers false_summit_mountain signature if Mountain claim + beneficiaries + high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article4_as_natural_boundary_vs_constructed_exclusion, conceptual, 'Whether the Article 4 gate is a natural legal limit or a state-serving construction.').

omega_variable(
    suppression_mechanism_in_asymmetric_conflict,
    'Is the suppression of alternative protective frameworks (human rights law, functional status) structural (state sovereignty, treaty textualism) or internalized (non-state actors accepting their exclusion as legitimate)?',
    'Field studies of non-state armed group legal consciousness; analysis of whether groups seek Article 4 compliance or reject the framework entirely; tracking of domestic court receptivity to human rights law arguments in military detention cases.',
    'If internalized, effective suppression is higher than structural measure — the excluded carry the constraint with them. If purely structural, suppression lifts when alternative forums (human rights courts, ICC) gain jurisdiction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_in_asymmetric_conflict, empirical, 'Structural vs internalized suppression of alternative protective frameworks.').

omega_variable(
    committer_frame_kernel_reading,
    'How does the state-centric reading''s structural relationship to the geneva_conventions_protective_scope kernel differ from the universal_rights_reading and hybrid_proportionality_reading in ways that affect ε-invariance?',
    'Comparative ε-authoring across the three readings: each reading authors ε for the same standing arrangement (Geneva protective scope) by its own lights. The state-centric reading authors low ε on state operations; the universalist reading authors high ε on the excluded. The engine treats these as different constraints with a shared referent.',
    'Validates the ε-invariance principle: one kernel, multiple readings, each with its own ε. Prevents conflating measurement-basis disagreement with constraint identity confusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Commitment-system framing: this reading as one instantiation of a contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcps_scr_tr_t0, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gcps_scr_tr_t15, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(gcps_scr_tr_t30, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(gcps_scr_tr_t45, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 45, 0.33).
narrative_ontology:measurement(gcps_scr_tr_t60, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(gcps_scr_tr_t75, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 75, 0.38).

% Extraction over time
narrative_ontology:measurement(gcps_scr_be_t0, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gcps_scr_be_t15, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(gcps_scr_be_t30, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(gcps_scr_be_t45, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 45, 0.57).
narrative_ontology:measurement(gcps_scr_be_t60, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(gcps_scr_be_t75, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gcps_scr_su_t0, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gcps_scr_su_t15, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(gcps_scr_su_t30, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(gcps_scr_su_t45, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 45, 0.66).
narrative_ontology:measurement(gcps_scr_su_t60, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 60, 0.69).
narrative_ontology:measurement(gcps_scr_su_t75, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 75, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__state_centric_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint family (geneva_conventions_protective_scope) decomposes the single label 'Geneva protective scope' into three structurally distinct readings. The state_centric_reading claims Mountain status (fixed treaty text) but shows substantial extractiveness on unprivileged belligerents. The universal_rights_reading claims broader protections but lacks state consent (coordination failure). The hybrid_proportionality_reading attempts functional scaling but creates doctrinal complexity. All three share the kernel but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__state_centric_reading, institutional, 0.1).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__state_centric_reading, organized, 0.2).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__state_centric_reading, powerless, 0.95).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__state_centric_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
