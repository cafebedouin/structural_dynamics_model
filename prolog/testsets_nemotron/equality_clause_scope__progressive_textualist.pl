% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Progressive Textualist Reading of Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint story instantiates the progressive_textualist reading of
 *   the equality clause scope kernel. The reading holds that the
 *   constitutional text contains a genuine equality principle whose
 *   application scope expands, but only through the democratic amendment
 *   process (Article V supermajorities), not through judicial
 *   reinterpretation. This is a rope claim: the constraint coordinates
 *   legitimate revision across a diverse polity by anchoring expansion in a
 *   shared, hard-to-game procedure. The authored metrics reflect declining
 *   extractiveness over time as amendments successively included excluded
 *   groups, declining suppression as the amendment pathway proved viable, and
 *   low theater because the security review function (textual fidelity) is
 *   real and not a cover for extraction. The claim/metric independence is
 *   maintained: the reading claims rope; the metrics describe a constraint
 *   that has become less extractive and less suppressive as its coordination
 *   function succeeded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.35).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.28).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.35).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Progressive Textualist Reading of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, '514a3fa8-eabb-457f-9005-8db60cc0ffd2').
narrative_ontology:cs_kernel_codification('514a3fa8-eabb-457f-9005-8db60cc0ffd2', formalized).
narrative_ontology:cs_authority_grounding('514a3fa8-eabb-457f-9005-8db60cc0ffd2', lineage).
narrative_ontology:cs_reading_relation('514a3fa8-eabb-457f-9005-8db60cc0ffd2', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('514a3fa8-eabb-457f-9005-8db60cc0ffd2', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('514a3fa8-eabb-457f-9005-8db60cc0ffd2', foundational, amendment_exclusive_revision_authority).
narrative_ontology:cs_axiom_status(amendment_exclusive_revision_authority, holdable).
narrative_ontology:cs_axiom_grounding('514a3fa8-eabb-457f-9005-8db60cc0ffd2', amendment_exclusive_revision_authority, conventional).
narrative_ontology:cs_axiom('514a3fa8-eabb-457f-9005-8db60cc0ffd2', foundational, textual_equality_principle_has_revision_capacity).
narrative_ontology:cs_axiom_status(textual_equality_principle_has_revision_capacity, holdable).
narrative_ontology:cs_axiom_grounding('514a3fa8-eabb-457f-9005-8db60cc0ffd2', textual_equality_principle_has_revision_capacity, conventional).
narrative_ontology:cs_reference_frame('514a3fa8-eabb-457f-9005-8db60cc0ffd2', post_civil_war_amendment_settlement).
narrative_ontology:cs_drift_state('514a3fa8-eabb-457f-9005-8db60cc0ffd2', contemporary_rights_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('514a3fa8-eabb-457f-9005-8db60cc0ffd2', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, amendment_majorities).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, institutional_legitimacy).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, judicial_restraint_advocates).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, originalist_legal_culture).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, originalist_legal_culture).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, historically_excluded_groups).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, popular_sovereignty_amendment).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, textual_fidelity_with_democratic_revision).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, bounded_universalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Supermajority coalitions that ratify constitutional amendments expanding equality scope. They hold the formal power to revise the constraint's reach through Article V process. Their exit is the political cost of building and sustaining such coalitions across states and time.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, amendment_majorities, agenda_setter,
    organized, generational, mobile, national).

% Groups excluded from original equality application (enslaved persons, women, non-property-holders, racial minorities) who gain protection through amendments. They pay the cost of political mobilization to achieve supermajorities but lack unilateral exit from the constraint's pace — they wait for democratic consent.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, historically_excluded_groups, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, historically_excluded_groups, payer).

% Legal actors committed to court deferral to amendment process. They bear the cost of watching courts decline to expand equality scope judicially, accepting slower progress as the price of textual fidelity. Their exit is intellectual — they could embrace living constitutionalism — but professional identity and institutional role constrain that move.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, judicial_restraint_advocates, payer,
    organized, biographical, constrained, national).

% Judges, scholars, and institutions whose professional identity and legitimacy are fused to the claim that equality scope is fixed at enactment. They benefit from the constraint's textual anchor but pay when amendments legitimately expand scope — their framework must accommodate or resist. Exit requires abandoning the interpretive identity that structures their authority.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, originalist_legal_culture, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, originalist_legal_culture, beneficiary).

% Actors who argue courts should expand equality scope without waiting for amendments. They are structurally excluded from this reading's framework — their move is foreclosed by the reading's core premise. They would object that democratic majorities can entrench exclusion indefinitely; they sit outside the constraint's authorized revision channel.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, living_constitutionalist_critics, excluded,
    organized, biographical, trapped, national).

% Scholars evaluating the reading's coherence, historical fit, and normative force. They neither collect nor pay under the constraint; they map the structural relationships between this reading and its siblings across the kernel.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legitimate, textually anchored pathway for expanding equality scope that binds all parties to the revision outcome because it passed the supermajority threshold — solving the coordination problem of how a diverse polity agrees on new equality commitments without fracturing legitimacy.
% TRANSFER_FUNCTION: Moves the authority to define equality's reach from judicial interpretation to democratic supermajority ratification. The transfer runs from courts (who lose expansive interpretive discretion) to amendment coalitions (who gain exclusive legitimate revision power), with historically excluded groups as the conditional beneficiaries of successful ratifications.
% ABSENT_VOICES: Living constitutionalist critics and those who would need equality expansion but cannot wait for supermajority politics — particularly marginalized groups facing urgent rights violations in jurisdictions where amendment coalitions are structurally impossible to assemble. They are excluded because the reading's revision gate (Article V supermajority) is the only legitimate entry point.
% DISAPPEARANCE_RATIONALE: If this reading vanished, either courts would assume plenary power to expand equality scope (expansive_universalist reading dominates) or the equality clause would freeze at its original 18th-century application (restrictive_originalist reading dominates). The constitutional order's legitimacy structure would reorganize around whichever rival reading captures the interpretive community.
% FOUNDING_PROBLEM: How to give the equality principle genuine revision capacity without surrendering its meaning to judicial discretion — the post-Civil War amendment sequence (13th, 14th, 15th, 19th, 24th, 26th) was built to solve this by making expansion democratic, textual, and hard to reverse.
% FOUNDING_PROBLEM_CORROBORATION: The amendment sequence itself — ratified by supermajorities across eras — is the external corroboration. Legal historians (Ackerman, Foner) and political theorists (Rawls on public reason, Habermas on constitutional patriotism) outside the benefiting parties attest that the founding problem remains live: democratic legitimation of equality expansion is still contested against judicial supremacist and originalist alternatives.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).
:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts high (0.65) because the original equality clause applied only to propertied white males — a narrow scope extracting full political standing from everyone else. Each successful amendment (1868, 1920, 1964, 1971) reduced extraction by expanding scope democratically. Suppression tracks the enforcement of the original narrow scope: high initially (0.70) when courts and states actively enforced exclusion, declining as amendments removed the excluded categories. Theater remains low throughout because the amendment process is a genuine coordination mechanism, not performative. The reading's legitimacy threshold is moderate: it accepts that some exclusions persist until supermajorities form, rejecting both judicial shortcuts and permanent freeze.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different effective classifications per seat. From the amendment majority seat, the constraint is a genuine rope: they coordinate, they pay the political cost of building supermajorities, and the outcome binds everyone legitimately. From the historically excluded group seat, the constraint is a tangled rope when coalitions fail (coordination function exists but extraction persists) and a rope when they succeed. From the originalist legal culture seat, the constraint is a piton candidate: their identity-locked position means they administer a framework whose original justification (fixed scope) has atrophied as amendments expanded it, yet they maintain the interpretive performance. The engine's per-seat divergence captures this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Amendment majorities are the structural agenda-setters (d ~ 0.15) — they hold the revision power and benefit from the constraint's legitimacy. Historically excluded groups are conditional beneficiaries (d ~ 0.35) — they gain when coalitions succeed but pay mobilization costs and wait. Judicial restraint advocates are payers (d ~ 0.65) — they accept slower progress as the price of textual fidelity. Originalist legal culture is identity-locked (d ~ 0.75) — their professional self-concept fuses with the fixed-scope premise; amendments that expand scope are legitimate but costly to their framework. Living constitutionalist critics are excluded (trapped) — their preferred move (judicial expansion) is structurally foreclosed by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (democratic legitimation of equality expansion) remains live — evidenced by ongoing contests over whether courts should expand equality scope without amendments (e.g., substantive due process, equal protection jurisprudence). The constraint has not resolved into mandatrophy because the rival readings (expansive_universalist, restrictive_originalist) remain live and contested. The reading's legitimacy threshold is calibrated to this live contest: it rejects both the judicial shortcut and the permanent freeze.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_pathway_viability,
    'Is the Article V supermajority pathway genuinely viable for all equality expansions that justice requires, or does it structurally entrench exclusion for groups that cannot assemble supermajorities?',
    'Empirical study of failed equality amendments (ERA, DC voting rights, child labor) versus successful ones — testing whether failure correlates with group powerlessness rather than lack of merit.',
    'If the pathway is structurally biased against the least powerful, the constraint''s coordination function is a cover for extraction (tangled_rope or snare); if viable for all meritorious claims, it remains a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_pathway_viability, empirical, 'Whether the democratic revision gate is equally accessible or systematically excludes the most vulnerable.').

omega_variable(
    judicial_abstention_credibility,
    'Can courts credibly commit to abstaining from equality-scope expansion while the amendment pathway operates, or does the reading''s legitimacy depend on a judicial restraint that is politically unsustainable?',
    'Historical analysis of Supreme Court equality jurisprudence: periods of restraint versus expansion, correlated with amendment activity and political pressure.',
    'If courts cannot sustain restraint, the reading''s revision gate is porous — expansion happens judicially anyway, making the amendment constraint performative (higher theater). If courts do restrain, the reading''s coordination function is structurally enforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_abstention_credibility, conceptual, 'Whether the institutional actor (courts) that this reading demands restraint from can actually deliver it.').

omega_variable(
    originalist_identity_lock_depth,
    'How deeply is the originalist legal culture''s identity fused to the fixed-scope premise — is it a reversible professional commitment or a constitutive identity that cannot be abandoned without collapse?',
    'Sociology of legal professions: track whether originalist judges/scholars who accept amendment-based expansions (e.g., 19th Amendment) modify their framework or experience crisis.',
    'If identity-locked, originalist culture is a piton seat administering an atrophied framework; if reversible, they are a mobile payer seat. Determines whether the constraint has a concentrated beneficiary capturing extraction (snare) or a diffuse administrative residue (piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_identity_lock_depth, empirical, 'Depth of identity fusion in the originalist legal culture seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eqcs_prog_txt_tr_t1789, equality_clause_scope__progressive_textualist, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(eqcs_prog_txt_tr_t1868, equality_clause_scope__progressive_textualist, theater_ratio, 1868, 0.08).
narrative_ontology:measurement(eqcs_prog_txt_tr_t1920, equality_clause_scope__progressive_textualist, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(eqcs_prog_txt_tr_t1964, equality_clause_scope__progressive_textualist, theater_ratio, 1964, 0.12).
narrative_ontology:measurement(eqcs_prog_txt_tr_t1971, equality_clause_scope__progressive_textualist, theater_ratio, 1971, 0.14).
narrative_ontology:measurement(eqcs_prog_txt_tr_t2024, equality_clause_scope__progressive_textualist, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(eqcs_prog_txt_be_t1789, equality_clause_scope__progressive_textualist, base_extractiveness, 1789, 0.65).
narrative_ontology:measurement(eqcs_prog_txt_be_t1868, equality_clause_scope__progressive_textualist, base_extractiveness, 1868, 0.48).
narrative_ontology:measurement(eqcs_prog_txt_be_t1920, equality_clause_scope__progressive_textualist, base_extractiveness, 1920, 0.42).
narrative_ontology:measurement(eqcs_prog_txt_be_t1964, equality_clause_scope__progressive_textualist, base_extractiveness, 1964, 0.38).
narrative_ontology:measurement(eqcs_prog_txt_be_t1971, equality_clause_scope__progressive_textualist, base_extractiveness, 1971, 0.36).
narrative_ontology:measurement(eqcs_prog_txt_be_t2024, equality_clause_scope__progressive_textualist, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(eqcs_prog_txt_su_t1789, equality_clause_scope__progressive_textualist, suppression_requirement, 1789, 0.7).
narrative_ontology:measurement(eqcs_prog_txt_su_t1868, equality_clause_scope__progressive_textualist, suppression_requirement, 1868, 0.45).
narrative_ontology:measurement(eqcs_prog_txt_su_t1920, equality_clause_scope__progressive_textualist, suppression_requirement, 1920, 0.35).
narrative_ontology:measurement(eqcs_prog_txt_su_t1964, equality_clause_scope__progressive_textualist, suppression_requirement, 1964, 0.3).
narrative_ontology:measurement(eqcs_prog_txt_su_t1971, equality_clause_scope__progressive_textualist, suppression_requirement, 1971, 0.28).
narrative_ontology:measurement(eqcs_prog_txt_su_t2024, equality_clause_scope__progressive_textualist, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__progressive_textualist, 0.12).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).

% DUAL FORMULATION NOTE:
% This constraint is one member of the equality_clause_scope kernel family. The three readings (progressive_textualist, restrictive_originalist, expansive_universalist) share the same constitutional text but instantiate different constraints with different ε values, beneficiary/victim structures, and classification. The progressive_textualist reading has moderate ε (0.35) because its revision gate (amendments) has partially succeeded; restrictive_originalist has higher ε (extraction from all excluded groups with no revision path); expansive_universalist has lower ε for included groups but higher suppression for dissenters (judicial imposition). They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__progressive_textualist, organized, 0.2).
constraint_indexing:directionality_override(equality_clause_scope__progressive_textualist, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
