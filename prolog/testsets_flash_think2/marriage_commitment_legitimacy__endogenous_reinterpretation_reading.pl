% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Divine Command Reinterpretation of Marriage (Endogenous Reading)
 *   domain: Religious Institutional History / Political Theology / Commitment Systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'endogenous reinterpretation'
 *   reading of the marriage commitment legitimacy kernel. In this reading,
 *   the Church's 1890 Manifesto, which formally ended the practice of plural
 *   marriage, is understood as a genuine prophetic revelation from God. This
 *   divine command was given to preserve the Church's existence and enable it
 *   to fulfill higher purposes, with federal pressure acting as a catalyst
 *   rather than the ultimate cause. The reinterpretation maintains
 *   theological continuity by framing monogamy as a new, higher stage of the
 *   covenant.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.15).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.75).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, mountain).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Divine Command Reinterpretation of Marriage (Endogenous Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Religious Institutional History / Political Theology / Commitment Systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
domain_priors:emerges_naturally(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '7a29b11f-1703-4549-93a2-419aed735e40').
narrative_ontology:cs_kernel_codification('7a29b11f-1703-4549-93a2-419aed735e40', fixed_text).
narrative_ontology:cs_authority_grounding('7a29b11f-1703-4549-93a2-419aed735e40', lineage).
narrative_ontology:cs_interpretation_layer_present('7a29b11f-1703-4549-93a2-419aed735e40').
narrative_ontology:cs_reading_relation('7a29b11f-1703-4549-93a2-419aed735e40', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a29b11f-1703-4549-93a2-419aed735e40', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('7a29b11f-1703-4549-93a2-419aed735e40', foundational, prophetic_revelation_is_divine_command).
narrative_ontology:cs_axiom_status(prophetic_revelation_is_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('7a29b11f-1703-4549-93a2-419aed735e40', prophetic_revelation_is_divine_command, theological).
narrative_ontology:cs_axiom('7a29b11f-1703-4549-93a2-419aed735e40', foundational, church_preservation_is_divine_will).
narrative_ontology:cs_axiom_status(church_preservation_is_divine_will, holdable).
narrative_ontology:cs_axiom_grounding('7a29b11f-1703-4549-93a2-419aed735e40', church_preservation_is_divine_will, theological).
narrative_ontology:cs_reference_frame('7a29b11f-1703-4549-93a2-419aed735e40', unbroken_prophetic_succession).
narrative_ontology:cs_drift_state('7a29b11f-1703-4549-93a2-419aed735e40', post_manifesto_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7a29b11f-1703-4549-93a2-419aed735e40', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_authority).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate source of the command, whose will is enacted through the Church. From this reading's perspective, divine authority benefits from the Church's preservation and continued prophetic legitimacy, fulfilling higher purposes.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_authority, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_authority).

% Interprets and promulgates the divine revelation, guiding the Church through the reinterpretation to ensure its doctrinal continuity and institutional survival. Benefits from maintaining prophetic succession and institutional integrity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Accepts the reinterpretation as divine will, finding spiritual continuity, community belonging, and a path to salvation. Their identity and worldview are tied to the Church's teachings and prophetic authority.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_members, beneficiary,
    moderate, biographical, identity_locked, global).

% Exerted external pressure through anti-polygamy laws, but from this reading's view, was merely an instrument or catalyst for divine will, not the ultimate cause of the change. It observes the Church's compliance.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Those who could not accept the reinterpretation and left or were excommunicated. Their perspective is that the change was capitulation, not revelation, and they are excluded from the narrative of divine continuity and legitimacy.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dissenting_members, excluded,
    powerless, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain the unity, prophetic authority, and institutional existence of the Church in the face of external legal pressure, by reinterpreting core doctrines as a new stage of divine revelation.
% TRANSFER_FUNCTION: Transfers the burden of doctrinal consistency from human interpretation to divine command, and transfers the responsibility for legal compliance from the Church to the federal government, while preserving the Church's spiritual capital.
% ABSENT_VOICES: Dissenting members who saw the change as capitulation, not revelation, are excluded. They would argue that the reinterpretation compromised core theological principles under duress.
% DISAPPEARANCE_RATIONALE: If this reinterpretation and its divine mandate vanished, the entire theological and institutional framework of the Church, particularly its claim to continuous prophetic revelation and its historical narrative of adaptation, would be undermined, leading to a profound crisis of legitimacy and identity.
% FOUNDING_PROBLEM: The existential threat to the Church's legal status, property, and the freedom of its members due to federal anti-polygamy laws, which directly challenged its core marriage doctrine and threatened its very existence.
% FOUNDING_PROBLEM_CORROBORATION: The Church's own historical records, theological interpretations, and official declarations attest to the problem's live status, framing the reinterpretation as an ongoing act of divine preservation. External historians corroborate the federal pressure but often dispute the divine command aspect, offering alternative interpretations of the Church's motivations.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, ExtMetricName, E),
    domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(marriage_commitment_legitimacy__endogenous_reinterpretation_reading),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is low (0.15) because, from this reading's perspective, the change was divinely commanded for the benefit of the Church and its members, not for human extraction. Suppression is high (0.75) due to the strong doctrinal and social pressure within the Church to accept and conform to the new revelation. Theater ratio is low (0.10) as the revelation is considered genuine and functional, not performative. Accessibility collapse is high (0.88) because a divine command leaves little room for alternative interpretations or actions for faithful members. Resistance is low (0.10) among those who accept the prophetic authority.
 *
 * PERSPECTIVAL GAP:
 *   From this reading, the reinterpretation is a necessary, divinely guided evolution. Other readings (exogenous override, hybrid pragmatic) would emphasize federal coercion or strategic adaptation as the primary drivers, leading to different classifications and higher extractiveness scores. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Divine authority is the ultimate beneficiary, as its will is enacted and the Church is preserved for its purposes. Church leadership benefits from maintaining prophetic succession and institutional integrity. Faithful members are beneficiaries, finding spiritual continuity and community belonging, albeit with an identity-locked exit. The federal government is an observer, and dissenting members are excluded, their voices not part of this narrative of legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a divinely commanded reinterpretation as mere institutional capitulation or pragmatic adaptation. By claiming 'mountain' and declaring beneficiaries, it allows the False Summit Mountain detection to flag the inherent ambiguity of a 'natural law' that benefits specific actors, prompting deeper analysis into the source of its claimed immutability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_vs_institutional_pragmatism,
    'Is the Manifesto truly a divine command, or primarily an institutional response to existential federal pressure, framed as revelation for legitimacy?',
    'Analysis of internal Church records, prophetic discourse, and external historical accounts for evidence of pre-existing theological shifts versus immediate reactions to legal threats. Comparison with other religious groups'' responses to similar pressures.',
    'If primarily pragmatic, the constraint''s extractiveness would be higher, and its claimed ''mountain'' status would be reclassified, likely to a ''tangled_rope'' or ''snare'' from the perspective of those who felt coerced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_command_vs_institutional_pragmatism, conceptual, 'Ambiguity between divine revelation and institutional pragmatism as the driver of the reinterpretation.').

omega_variable(
    suppression_source_ambiguity,
    'Is the high suppression experienced by members primarily due to genuine spiritual conviction and identity-lock, or to social and institutional pressure to conform?',
    'Longitudinal studies of ex-members'' experiences, analysis of internal disciplinary actions, and comparison with other high-demand religious groups. If suppression persists after exit, it suggests internalized mechanisms.',
    'If suppression is primarily institutional, the constraint''s effective suppression is higher than the structural measure suggests, as it relies on active enforcement rather than internal assent. If internalized, the identity-lock is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for doctrinal conformity.').

omega_variable(
    divine_authority_as_beneficiary_legitimacy,
    'Is ''divine_authority'' a genuine beneficiary, or is its inclusion a rhetorical device by ''church_leadership'' to legitimize actions that primarily benefit the institution?',
    'Theological and philosophical analysis of the concept of divine benefit, and examination of whether the ''higher purposes'' are demonstrably distinct from institutional self-preservation. This is a conceptual question with no empirical resolution.',
    'If ''divine_authority'' is not a true beneficiary, the ''church_leadership'' becomes the sole primary beneficiary, increasing the perceived extractiveness and potentially shifting the classification away from ''mountain'' towards a more constructed type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_authority_as_beneficiary_legitimacy, conceptual, 'Conceptual ambiguity of divine authority as a beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 1890, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.1).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(marr_tr_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(marr_tr_t1940, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1940, 0.1).
narrative_ontology:measurement(marr_tr_t1950, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.15).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.15).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.15).
narrative_ontology:measurement(marr_be_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1930, 0.15).
narrative_ontology:measurement(marr_be_t1940, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1940, 0.15).
narrative_ontology:measurement(marr_be_t1950, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1950, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.75).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.75).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(marr_su_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1930, 0.75).
narrative_ontology:measurement(marr_su_t1940, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(marr_su_t1950, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1950, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_property_rights).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_religious_freedom_doctrine).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_legitimacy' kernel, each representing a distinct structural interpretation of the 1890 Manifesto. This reading emphasizes divine command and endogenous reinterpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
