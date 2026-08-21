% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__protective_scaffold_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity Doctrine (Protective Scaffold Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'protective scaffold' reading of
 *   the qualified immunity doctrine. From this perspective, qualified
 *   immunity is a necessary legal protection for law enforcement officers,
 *   shielding them from liability in civil lawsuits unless their conduct
 *   violates 'clearly established' statutory or constitutional rights. This
 *   protection is argued to enable vigorous law enforcement without the
 *   chilling effect of bad-faith or frivolous litigation, thereby serving a
 *   vital public safety function. The doctrine is seen as a coordination
 *   mechanism that allows officers to perform their duties effectively, even
 *   if it means some individuals whose rights are violated are denied a legal
 *   remedy.
 *
 * KEY AGENTS:
 *   - law_enforcement_officers: Primary beneficiary (moderate/constrained)
 *   - law_enforcement_agencies: Agenda setter/beneficiary (institutional/mobile)
 *   - constitutional_violation_survivors: Primary payer (powerless/trapped)
 *   - federal_judiciary: Agenda setter/enforcer (institutional/analytical)
 *   - civil_rights_advocates: Observer (organized/analytical)
 *   - taxpayers: Indirect beneficiary (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.65).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.75).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity Doctrine (Protective Scaffold Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, 'cb5e02b1-bfeb-432f-8f66-6286673b0372').
narrative_ontology:cs_kernel_codification('cb5e02b1-bfeb-432f-8f66-6286673b0372', formalized).
narrative_ontology:cs_authority_grounding('cb5e02b1-bfeb-432f-8f66-6286673b0372', lineage).
narrative_ontology:cs_interpretation_layer_present('cb5e02b1-bfeb-432f-8f66-6286673b0372').
narrative_ontology:cs_reading_relation('cb5e02b1-bfeb-432f-8f66-6286673b0372', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb5e02b1-bfeb-432f-8f66-6286673b0372', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('cb5e02b1-bfeb-432f-8f66-6286673b0372', foundational, protect_officer_discretion).
narrative_ontology:cs_axiom_status(protect_officer_discretion, holdable).
narrative_ontology:cs_axiom_grounding('cb5e02b1-bfeb-432f-8f66-6286673b0372', protect_officer_discretion, instrumental).
narrative_ontology:cs_axiom('cb5e02b1-bfeb-432f-8f66-6286673b0372', foundational, deter_frivolous_litigation).
narrative_ontology:cs_axiom_status(deter_frivolous_litigation, holdable).
narrative_ontology:cs_axiom_grounding('cb5e02b1-bfeb-432f-8f66-6286673b0372', deter_frivolous_litigation, empirically_contingent).
narrative_ontology:cs_reference_frame('cb5e02b1-bfeb-432f-8f66-6286673b0372', effective_governance_framework).
narrative_ontology:cs_drift_state('cb5e02b1-bfeb-432f-8f66-6286673b0372', contemporary_litigation_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cb5e02b1-bfeb-432f-8f66-6286673b0372', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, taxpayers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from liability for actions that do not violate 'clearly established' statutory or constitutional rights, enabling them to perform duties vigorously without fear of frivolous lawsuits. This protection is seen as essential for effective policing.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from reduced litigation costs and increased operational flexibility for their officers. Actively defend the doctrine in court and through lobbying efforts, viewing it as a necessary tool for maintaining order and safety.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, mobile, national).

% Individuals who have suffered constitutional violations by law enforcement but are denied legal remedy due to the doctrine's protections. They bear the costs of unaddressed harm and often face insurmountable legal barriers to justice.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors, payer,
    powerless, immediate, trapped, local).

% Monitor the application of qualified immunity, advocate for its reform or abolition, and support litigation challenging its scope. They analyze its impact on civil liberties and systemic accountability.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_advocates, observer,
    organized, generational, analytical, national).

% The primary interpreters and enforcers of qualified immunity, shaping its scope and application through case law. They balance the protection of officers with the vindication of rights, often emphasizing the need to avoid chilling legitimate law enforcement activity.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Indirectly benefit from reduced municipal and state liability costs, which might otherwise be passed on through taxes or reduced public services. Their benefit is diffuse and often unacknowledged in public discourse.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, taxpayers, beneficiary,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the actions of law enforcement officers by providing a clear standard for when they can be held liable, thereby enabling them to perform their duties vigorously without undue fear of litigation.
% TRANSFER_FUNCTION: Transfers the financial and reputational costs of constitutional violations from individual law enforcement officers and their agencies to individuals whose rights are violated, by denying them legal recourse.
% ABSENT_VOICES: Victims of constitutional violations who are denied remedy by the doctrine are effectively silenced in the legal system, as their claims are often dismissed before reaching a jury. Their voices are heard primarily through advocacy groups and academic critiques.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, law enforcement officers and agencies would face a dramatic increase in civil rights litigation. This would likely lead to significant changes in police training, internal accountability mechanisms, liability insurance markets, and potentially legislative action to redefine officer liability or create alternative redress systems. The current balance of power and accountability would be fundamentally altered.
% FOUNDING_PROBLEM: To protect government officials from undue interference with their duties and from the burdens of litigation, particularly frivolous lawsuits, when acting in their official capacity.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement organizations, police unions, and some legal scholars consistently attest that the problem of potential frivolous litigation and the need for officer protection remains live and critical for effective policing. Critics, however, argue that the problem is overstated and that the doctrine primarily shields misconduct.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is moderate-high (0.65) because the doctrine effectively denies legal recourse to individuals whose rights have been violated, transferring the costs of misconduct away from officers and agencies. `suppression` is high (0.75) due to the active judicial enforcement of the 'clearly established law' standard, which frequently leads to early dismissal of lawsuits. `theater_ratio` is low (0.20) because proponents genuinely believe in the protective function of the doctrine, though some performative aspects exist in its defense against growing criticism. `accessibility_collapse` is high (0.70) as it significantly limits avenues for redress, and `resistance` is moderate-high (0.60) reflecting ongoing legal and political challenges to the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary and law enforcement agencies, as agenda setters and beneficiaries, experience this constraint as a necessary and stable protective mechanism. In contrast, constitutional violation survivors, as payers, experience it as a significant barrier to justice and accountability. The engine's per-seat classification will highlight this divergence, showing a beneficial or neutral classification for the former and a highly extractive one for the latter.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement officers and agencies are clear beneficiaries, as the doctrine shields them from liability and reduces costs. Taxpayers are indirect beneficiaries through reduced municipal liability. Constitutional violation survivors are the primary targets/victims, bearing the costs of unaddressed harm. The federal judiciary acts as an agenda setter, actively shaping and enforcing the doctrine, which benefits law enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading frames qualified immunity as a 'protective scaffold' — implying a temporary or conditional support. However, the doctrine lacks a sunset clause and has become a permanent feature of the legal landscape. This structural reality, combined with its high extractiveness and suppression, leads to a classification as a `tangled_rope` rather than a `scaffold`. The engine's classification detects that what is framed as temporary support has become a persistent, actively enforced mechanism with asymmetric extraction, preventing mislabeling of its true structural nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_framing_vs_structural_reality,
    'Is qualified immunity, as framed by this reading, a genuine ''scaffold'' (temporary support) or a permanent feature of the legal landscape?',
    'Legislative action to introduce a sunset clause or a clear, time-limited mandate for the doctrine. Absent such action, its structural permanence contradicts the ''scaffold'' framing.',
    'If it were genuinely temporary, its classification might shift towards a true scaffold. As a permanent fixture, its ''scaffold'' framing is a justification for a `tangled_rope` structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scaffold_framing_vs_structural_reality, conceptual, 'Discrepancy between the ''scaffold'' framing and the doctrine''s lack of a sunset clause.').

omega_variable(
    frivolous_litigation_prevalence,
    'Is the problem of ''bad-faith'' or ''frivolous'' litigation against officers genuinely widespread enough to justify the broad protections offered by qualified immunity?',
    'Empirical studies analyzing the proportion of civil rights lawsuits against officers that are dismissed as frivolous versus those dismissed due to qualified immunity, and the actual costs of defending such lawsuits.',
    'If frivolous litigation is rare, the doctrine''s broad application would be seen as disproportionate, increasing its effective extractiveness. If widespread, it would bolster the coordination function of the doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frivolous_litigation_prevalence, empirical, 'Empirical basis for the ''deter frivolous litigation'' axiom.').

omega_variable(
    clearly_established_law_ambiguity,
    'Does the ''clearly established law'' standard provide sufficient clarity for officers to understand their obligations and for plaintiffs to pursue legitimate claims, or is it an ambiguous standard subject to arbitrary judicial discretion?',
    'Analysis of judicial opinions for consistency in applying the standard, and surveys of law enforcement officers and legal practitioners regarding their understanding and predictability of the standard''s application.',
    'If the standard is consistently applied and clear, it enhances the coordination function. If ambiguous, it increases suppression and extractiveness by creating unpredictable barriers to justice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_law_ambiguity, empirical, 'Clarity and consistency of the ''clearly established law'' standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1967, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(qual_tr_t1978, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(qual_tr_t1989, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1989, 0.15).
narrative_ontology:measurement(qual_tr_t2000, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(qual_tr_t2011, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2011, 0.19).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(qual_be_t1978, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1978, 0.48).
narrative_ontology:measurement(qual_be_t1989, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1989, 0.55).
narrative_ontology:measurement(qual_be_t2000, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(qual_be_t2011, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2011, 0.63).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(qual_su_t1978, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1978, 0.58).
narrative_ontology:measurement(qual_su_t1989, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1989, 0.65).
narrative_ontology:measurement(qual_su_t2000, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(qual_su_t2011, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2011, 0.73).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, police_accountability_mechanisms).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_litigation).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'qualified_immunity_doctrine' kernel, focusing on its protective function for law enforcement. It differs from the 'accountability_void_reading' (which emphasizes impunity) and the 'constitutional_fidelity_reading' (which questions its legal basis).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
