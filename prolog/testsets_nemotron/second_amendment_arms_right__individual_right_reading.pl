% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Individual Right to Keep and Bear Arms Against Federal Infringement
 *   domain: constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the individual-right reading of the
 *   Second Amendment: the right to keep and bear arms is a pre-political
 *   individual liberty protected against federal infringement. The reading
 *   emerged from the 2008 Heller decision and was extended in 2010 McDonald
 *   and 2022 Bruen, displacing the collective-right reading that dominated
 *   20th-century doctrine. The constraint operates as a tangled rope: it
 *   coordinates a genuine constitutional commitment to preventing tyranny
 *   through an armed citizenry (coordination function) while simultaneously
 *   extracting regulatory authority from democratically accountable
 *   governments and transferring it to courts and organized gun-rights
 *   advocates (extraction function). Active enforcement is required — the
 *   doctrine must be litigated, maintained, and expanded against persistent
 *   challenge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.28).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.72).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Individual Right to Keep and Bear Arms Against Federal Infringement").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, 'b99f0ac0-b2af-4bcc-b101-47bc9a23c192').
narrative_ontology:cs_kernel_codification('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', fixed_text).
narrative_ontology:cs_authority_grounding('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', lineage).
narrative_ontology:cs_interpretation_layer_present('b99f0ac0-b2af-4bcc-b101-47bc9a23c192').
narrative_ontology:cs_reading_relation('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', foundational, individual_self_defense_natural_right).
narrative_ontology:cs_axiom_status(individual_self_defense_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', individual_self_defense_natural_right, deontological).
narrative_ontology:cs_axiom('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', foundational, second_amendment_protects_preexistent_right).
narrative_ontology:cs_axiom_status(second_amendment_protects_preexistent_right, holdable).
narrative_ontology:cs_axiom_grounding('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', second_amendment_protects_preexistent_right, conventional).
narrative_ontology:cs_axiom('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', secondary, history_and_tradition_test_excludes_policy_balancing).
narrative_ontology:cs_axiom_status(history_and_tradition_test_excludes_policy_balancing, holdable).
narrative_ontology:cs_axiom_grounding('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', history_and_tradition_test_excludes_policy_balancing, conventional).
narrative_ontology:cs_reference_frame('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', founding_era_armed_citizenry).
narrative_ontology:cs_drift_state('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', post_bruen_2022, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b99f0ac0-b2af-4bcc-b101-47bc9a23c192', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, second_amendment_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, state_local_law_enforcement).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, public_health_safety_advocates).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, individual_self_defense_right_preexistence).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, constitutional_negative_liberty_against_federal_power).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, originalist_textualism_interpretive_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise a constitutional right to possess firearms for self-defense and other lawful purposes. The right is experienced as foundational to personal liberty and identity; exit from this framing would require abandoning a core self-understanding and political commitment. Organizationally represented by advocacy groups that treat any regulation as presumptive infringement.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Manufactures and sells firearms and ammunition; the individual-right reading creates a stable commercial regime where demand is constitutionally protected against prohibition. Lobby and litigate to shape regulatory boundaries; can redirect capital across jurisdictions if regulatory pressure intensifies in one market.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_industry, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__individual_right_reading, firearms_industry, agenda_setter).

% Litigate, lobby, and mobilize politically to expand and defend the individual-right reading. Their organizational existence and funding depend on the reading remaining contested and expansive; they set the litigation agenda that produced Heller and Bruen and continue to press for broader application.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, second_amendment_advocacy_organizations, agenda_setter,
    institutional, generational, mobile, national).

% Congress, ATF, and executive agencies that would regulate firearms commerce, possession, and transfer. The individual-right reading constrains their regulatory toolkit — bans, universal registration, and certain licensing schemes are foreclosed or burdened with strict scrutiny. They bear the cost of compliance with judicial doctrine and political backlash when they act.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_regulatory_authority, payer,
    institutional, generational, constrained, national).

% Police and prosecutors who enforce firearms laws on the ground. The individual-right reading creates uncertainty about which local regulations survive challenge, complicates enforcement priorities, and exposes officers to litigation risk. They cannot exit the constraint — they must enforce whatever laws remain valid while navigating shifting doctrine.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, state_local_law_enforcement, payer,
    organized, biographical, constrained, regional).

% Researchers, physicians, and advocacy groups who view gun violence as a preventable public health crisis. The individual-right reading structurally excludes epidemiological and harm-reduction arguments from constitutional consideration under current doctrine; their policy proposals (universal background checks, waiting periods, assault weapons bans) face heightened judicial skepticism regardless of empirical support.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, public_health_safety_advocates, excluded,
    organized, biographical, trapped, national).

% Academics, judges, and clerks who interpret and apply the Second Amendment. They observe the constraint's operation from the analytical seat — evaluating historical evidence, doctrinal coherence, and institutional consequences without directly bearing its costs or collecting its benefits.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, constitutional_scholars_courts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared commitment to armed self-governance by establishing a constitutional floor that prevents federal disarmament of the citizenry, enabling decentralized deterrence against tyranny and private violence alike.
% TRANSFER_FUNCTION: Transfers regulatory authority from federal and state governments to individual right-holders — the constraint moves the power to prohibit or restrict firearms possession from the state to the judiciary, which polices the boundary of permissible regulation.
% ABSENT_VOICES: Communities disproportionately affected by gun violence (particularly urban Black and Latino communities) are largely absent from the doctrinal conversation that treats the right as abstract and universal; their situated knowledge of harm is excluded by a framework that centers the law-abiding individual owner. Also absent: founding-era voices who might distinguish 'bear arms' in a military context from private possession.
% DISAPPEARANCE_RATIONALE: If the individual-right reading vanished overnight, federal and state governments could enact comprehensive firearms prohibitions, licensing regimes, and registration systems without strict scrutiny review. The firearms market would contract dramatically; advocacy organizations would lose their central constitutional hook; law enforcement would gain regulatory clarity but face political backlash from gun owners. The entire institutional ecology of gun politics would reorganize.
% FOUNDING_PROBLEM: The founding generation feared a standing army and federal disarmament of state militias as instruments of tyranny; the Second Amendment was ratified to ensure the federal government could not disarm the citizen militia that served as the ultimate check on centralized power.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (e.g., Scalia in Heller, supported by historical linguists like Cramer and Kopel) attest the founding problem was individual self-defense as a natural right. Living constitutionalists and historians (e.g., Cornell, Waldman, Rakove) attest the founding problem was collective militia preservation, not private ownership. The parties dispute the founding problem itself — no external corroboration resolves it.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).
:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) reflects the constraint's transfer of regulatory authority from legislatures to courts and right-holders — moderate but rising as doctrine expands. Suppression (0.72) is high because the constraint actively forecloses policy alternatives (bans, comprehensive licensing) that majorities in some jurisdictions would enact; the Bruen history-and-tradition test functions as a structural suppression mechanism. Theater ratio (0.41) is significant because much doctrinal elaboration (sensitive places, historical analogues) performs the work of constitutional reasoning while serving the political function of expanding the right's scope. Accessibility collapse (0.58) and resistance (0.68) reflect the contested, non-natural character of the constraint — alternatives exist and are actively advocated.
 *
 * PERSPECTIVAL GAP:
 *   From the gun owner seat, the constraint is a mountain — a natural right that government cannot touch. From the regulator seat, it is a snare — a judicial invention that extracts democratic authority. From the advocacy organization seat, it is a rope they built and maintain. The engine computes these divergences from the structural data; the authored claim (tangled_rope) reflects the generating model's assessment that both coordination and extraction are structurally real and neither reduces to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are structural beneficiaries (d low): they collect the liberty interest and commercial protection the constraint provides, with identity_locked and arbitrage exit respectively. Federal regulatory authority and state/local law enforcement are payers (d high): they bear the cost of constrained authority and doctrinal compliance, with constrained exit. Public health advocates are excluded (d near 1.0): they bear harm without voice. Advocacy organizations are agenda_setters who also benefit organizationally. The analytical seat sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal disarmament of militias) is arguably dead — no standing army threatens the republic in the founding sense, and state militias have been nationalized into the National Guard. Yet the arrangement persists and expands. The individual-right reading repurposes the constraint for a new coordination problem (individual self-defense in a low-trust society) while the original mandate atrophies. This is not pure mandatrophy because the new function is genuinely coordinative for its beneficiaries; it is tangled because the new function extracts from non-beneficiaries who had no voice in the repurposing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_constructed_entitlement,
    'Is the individual right to arms a genuine pre-political natural law (mountain-like) or a constructed constitutional entitlement that benefits identifiable actors?',
    'Cross-historical comparison: if the right''s scope and beneficiaries track political mobilization rather than stable moral consensus, it is constructed. If it converges across cultures and eras without coordination, it is natural-law-like.',
    'If constructed, the constraint is a false summit candidate (mountain claim with beneficiaries) and the FSM signature should reclassify as tangled_rope or snare. If natural, the low extractiveness and high accessibility_collapse are genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_constructed_entitlement, conceptual, 'Whether the individual-right reading describes a natural law or a political construction').

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (preventing tyranny through armed citizenry) end and the extractive function (blocking popular gun safety regulations) begin?',
    'Counterfactual policy simulation: which regulations would a well-ordered republic committed to armed citizenship still enact? The gap between that set and the current doctrinal permission structure measures extraction.',
    'If the boundary is narrow, the constraint is mostly coordination (rope-like). If wide, mostly extraction (snare-like). Current metrics place it in tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'The structural boundary between the constraint''s coordination and extraction components').

omega_variable(
    second_amendment_kernel_framing,
    'Does this constraint instantiate the second_amendment_arms_right kernel, or is the kernel itself a constructed category that obscures distinct constraints?',
    'Test whether the sibling readings share a stable referent or whether ''the Second Amendment'' is a label for three different constraints with different ε, beneficiaries, and types (per ε-invariance principle).',
    'If the kernel is a single referent, the three readings are structurally linked and network edges are appropriate. If the kernel is a conflated label, each reading should stand alone without network links to the others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_amendment_kernel_framing, conceptual, 'Whether the kernel is a genuine structural unity or a linguistic conflation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__individual_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1868, second_amendment_arms_right__individual_right_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__individual_right_reading, theater_ratio, 1939, 0.25).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__individual_right_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_arms_right__individual_right_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_arms_right__individual_right_reading, theater_ratio, 2022, 0.41).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1791, 0.05).
narrative_ontology:measurement(seco_be_t1868, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1868, 0.08).
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1939, 0.12).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2008, 0.22).
narrative_ontology:measurement(seco_be_t2010, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement(seco_be_t2022, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2022, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1791, 0.2).
narrative_ontology:measurement(seco_su_t1868, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1868, 0.3).
narrative_ontology:measurement(seco_su_t1939, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1939, 0.45).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(seco_su_t2010, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(seco_su_t2022, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2022, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, federal_firearms_regulation).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, state_preemption_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the second_amendment_arms_right kernel. The individual_right_reading (this story) has ε=0.28 and claimed_type=tangled_rope. The collective_right_reading would have lower ε (minimal extraction from regulators) and claimed_type=rope or mountain. The civic_republican_reading would have intermediate ε and claimed_type=tangled_rope with different beneficiary/victim structure. All three are linked here; each sibling story should reciprocate the link.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__individual_right_reading, organized, 0.15).
constraint_indexing:directionality_override(second_amendment_arms_right__individual_right_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
