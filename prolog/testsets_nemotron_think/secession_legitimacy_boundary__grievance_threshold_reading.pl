% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Grievance-Threshold Secession Legitimacy Rule
 *   domain: political/federalism
 *
 * SUMMARY:
 *   This constraint story instantiates the grievance-threshold reading of the
 *   secession legitimacy kernel: secession becomes legitimate when federal
 *   actions cross an objective threshold of structural injustice (systematic
 *   rights violations, resource extraction without consent, democratic
 *   exclusion), regardless of constitutional text. The reading treats the
 *   federation as a conditional compact — legitimate only so long as the
 *   federal power does not become structurally predatory toward a constituent
 *   people. The threshold is intentionally demanding: it requires sustained,
 *   documented, severe injustice, not mere policy disagreement. This reading
 *   emerged from post-WWII decolonization practice, evolved through the
 *   remedial-right-only self-determination doctrine, and was sharpened by the
 *   Kosovo and South Sudan precedents. It sits between constitutional
 *   impossibility (no exit ever) and popular sovereignty (exit on demand).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.18).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.25).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Grievance-Threshold Secession Legitimacy Rule").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political/federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, '6b8e0176-b793-4854-b17e-aab484f5d802').
narrative_ontology:cs_kernel_codification('6b8e0176-b793-4854-b17e-aab484f5d802', distributed).
narrative_ontology:cs_authority_grounding('6b8e0176-b793-4854-b17e-aab484f5d802', practice).
narrative_ontology:cs_reading_relation('6b8e0176-b793-4854-b17e-aab484f5d802', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('6b8e0176-b793-4854-b17e-aab484f5d802', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b8e0176-b793-4854-b17e-aab484f5d802', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('6b8e0176-b793-4854-b17e-aab484f5d802', foundational, structural_injustice_threshold_legitimates_exit).
narrative_ontology:cs_axiom_status(structural_injustice_threshold_legitimates_exit, holdable).
narrative_ontology:cs_axiom_grounding('6b8e0176-b793-4854-b17e-aab484f5d802', structural_injustice_threshold_legitimates_exit, empirically_contingent).
narrative_ontology:cs_axiom('6b8e0176-b793-4854-b17e-aab484f5d802', foundational, objective_burden_of_proof_required).
narrative_ontology:cs_axiom_status(objective_burden_of_proof_required, holdable).
narrative_ontology:cs_axiom_grounding('6b8e0176-b793-4854-b17e-aab484f5d802', objective_burden_of_proof_required, conventional).
narrative_ontology:cs_reference_frame('6b8e0176-b793-4854-b17e-aab484f5d802', federal_compact_with_implicit_exit_conditions).
narrative_ontology:cs_drift_state('6b8e0176-b793-4854-b17e-aab484f5d802', contemporary_self_determination_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6b8e0176-b793-4854-b17e-aab484f5d802', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, threshold_meeting_secessionist_groups).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, oppressed_minority_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_loyalist_populations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, remaining_federation_populations).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, remedial_right_secession_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, structural_injustice_as_exit_trigger).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized political movements representing distinct nations or peoples within a federation who can document systematic federal rights violations, resource extraction without consent, or democratic exclusion. They bear the burden of proving the threshold is met through international observers, legal documentation, and sustained mobilization. Exit is constrained by federal military response, economic blockade threats, and internal dissent.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, threshold_meeting_secessionist_groups, beneficiary,
    organized, generational, constrained, regional).

% The central state that defines and enforces the constitutional order. It controls the adjudication mechanisms (courts, security forces, international recognition) and sets the practical threshold through its response patterns. It can concede, negotiate, or repress. Its exit options are arbitrage-grade: it can reframe the threshold, create new autonomy arrangements, or invoke international sovereignty norms.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Populations within the would-be seceding region who oppose exit — either because they identify with the federation, fear economic disruption, or face minority status in the new entity. They bear costs of uncertainty, potential displacement, and loss of federal protections. Their exit is constrained by geography and identity; they cannot easily leave the territory in dispute.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_loyalist_populations, payer,
    organized, biographical, constrained, regional).

% Citizens of the federation outside the seceding region who bear economic costs (lost revenue, transition disruption), political costs (constitutional crisis, precedent), and security costs. They have mobile exit options — they can migrate, vote for different leadership, or accept the new arrangement.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, remaining_federation_populations, payer,
    moderate, biographical, mobile, national).

% UN bodies, ICJ, regional organizations, and state practice that collectively shape the recognition threshold. They provide the external validation that makes a secession 'legitimate' rather than merely 'successful.' Their role is analytical: they interpret whether the grievance threshold meets international law standards (remedial right, self-determination).
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_legal_community, observer,
    institutional, generational, analytical, global).

% Indigenous nations with pre-existing treaty relationships to the Crown/federation whose consent is required under treaty_primacy_reading but who have no formal veto in the grievance-threshold framework. They are structurally excluded from the threshold adjudication despite having prior sovereignty claims. Their exit is trapped: they cannot exit the federation without treaty partner consent, and cannot stay without their rights being overridden by either federal or secessionist authority.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_treaty_holders, excluded,
    organized, generational, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an objective, evidence-based standard for when a group's claim to exit a federation becomes legitimate — solving the coordination problem between 'never allow secession' (entrapment) and 'allow secession on demand' (instability) by requiring demonstrable structural injustice as the trigger.
% TRANSFER_FUNCTION: Transfers the authority to legitimize secession from the federal constitution's silence/prohibition to an objective threshold of federal misconduct; when threshold is crossed, transfers territorial sovereignty and governance authority from federation to the seceding group, and transfers the burden of proof from the secessionists (who must prove injustice) to the federation (which must prove threshold not met or offer remedy).
% ABSENT_VOICES: Federal loyalist populations in the seceding region (who would oppose exit but are outvoted by the threshold-meeting group); indigenous treaty holders whose prior sovereignty claims are not integrated into the threshold calculus; international stability advocates who prioritize border fixity over remedial justice.
% DISAPPEARANCE_RATIONALE: If the grievance-threshold rule vanished, secession legitimacy would collapse into either (a) pure constitutional text (making exit impossible, entrenching federal impunity) or (b) pure power politics (whoever controls territory decides). The threshold rule is the only structured middle ground; its absence removes the objective standard that currently constrains both federal repression and secessionist adventurism.
% FOUNDING_PROBLEM: How to reconcile the liberal democratic principle of self-government with the territorial integrity of federations when the federal government itself becomes the agent of structural injustice against a constituent people — without making secession either a trivial option or a permanent impossibility.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists (Allen Buchanan, Margaret Moore, Christopher Wellman) and international lawyers (James Crawford, Antonio Cassese) attest the problem is live and the threshold approach is the dominant remedial-right framework. Federal governments and status-quo international actors (e.g., Spain, Canada, China) contest the problem's framing, arguing federalism provides adequate internal self-determination. The corroboration split tracks the beneficiary/payer divide.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).
:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint is a threshold rule that mostly lies dormant — it extracts nothing unless the federal government itself crosses into structural injustice. The federation's own conduct triggers the constraint. Suppression is low-moderate (0.25) because the rule does not require active enforcement; it is a recognition standard, not a prohibition. Theater is low (0.15) because the threshold criteria (documented rights violations, failed remedies) are empirically verifiable, not performative. Accessibility collapse is moderate (0.35) — alternatives (negotiation, autonomy, federal reform) remain viable below the threshold. Resistance is moderate (0.55) because federal governments consistently resist the threshold's application, but the resistance is political/legal, not violent suppression of the rule itself.
 *
 * PERSPECTIVAL GAP:
 *   From the secessionist seat, the constraint is a rope — a genuine coordination mechanism that makes their exit legitimate when justice demands it. From the federal government seat, it is a snare — a rule that threatens territorial integrity and incentivizes secessionist mobilization. From the loyalist seat, it is a tangled rope — it coordinates a remedy for injustice but imposes costs on non-consenting populations. The engine computes these per-seat types from the structural data; the authored claim (rope) reflects the reading's own normative aspiration.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold-meeting secessionist groups are structural beneficiaries: they gain legitimate exit only when they are already victims of federal injustice — the constraint subsidizes their remedy. Federal government is agenda_setter with arbitrage exit: it controls whether the threshold is crossed by its own conduct, and can always concede autonomy to avoid crossing it. Federal loyalist populations are payers: they bear transition costs if secession occurs, but their costs are the consequence of federal injustice, not the constraint itself. Remaining federation populations are payers with mobile exit. Indigenous treaty holders are excluded: the threshold rule does not incorporate their prior consent requirement, creating a structural gap. International legal community is analytical observer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federation as conditional compact) remains live — federal overreach against constituent peoples recurs (e.g., Kosovo 1990s, South Sudan 1980s-2000s, Catalonia 2010s, Quebec 1990s, Scotland 2010s). The constraint has not atrophied into a piton because it is invoked only when needed and its criteria remain contested and sharpened by practice. No concentrated beneficiary captures it; the 'gains' (legitimate exit) are conditional and non-monetary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the grievance-threshold reading a distinct constraint from its siblings, or a parameterization of a single secession-legitimacy constraint?',
    'Apply the epsilon-invariance test: if changing the observable (constitutional text vs. grievance evidence vs. referendum result vs. treaty consent) changes the extraction profile and victim set, they are distinct constraints. This reading''s epsilon (0.18) and victim set (empty unless threshold crossed) differ structurally from siblings.',
    'If distinct, each reading gets its own constraint story with independent classification. If same constraint, the framework must model observable-dependent classification (which violates epsilon-invariance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel decomposes into multiple epsilon-invariant constraints per the BGS decomposition principle.').

omega_variable(
    threshold_objectivity_ambiguity,
    'Can the ''threshold of structural injustice'' be objectively operationalized, or does its application inevitably reflect the political power of the adjudicators?',
    'Compare threshold applications across cases (Kosovo, South Sudan, Quebec, Catalonia, Kashmir, Tigray): do independent observers converge on whether the threshold was met? Measure inter-coder reliability on threshold criteria.',
    'If objective, the constraint is a genuine rope (coordination via shared standard). If inherently political, it becomes a tangled rope or snare — the threshold is a cover for power politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_objectivity_ambiguity, empirical, 'Whether the grievance threshold is a genuine coordination standard or a manipulable political tool.').

omega_variable(
    indigenous_exclusion_gap,
    'Does the grievance-threshold reading''s failure to incorporate treaty-primacy claims constitute a structural extraction from indigenous nations, or a legitimate prioritization of remedial self-determination?',
    'Analyze cases where secessionist group and indigenous treaty holders overlap or conflict (e.g., Quebec-Cree, Sudan-South Sudan borderlands): does the threshold rule systematically override treaty rights?',
    'If systematic override, the constraint extracts from indigenous nations to benefit secessionist groups — reclassifies toward tangled_rope or snare for the indigenous seat. If complementary, remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_exclusion_gap, conceptual, 'Structural relationship between remedial-right secession and treaty-based sovereignty.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.25) structural (federal legal/military barriers to secession) or internalized (populations believing secession is illegitimate regardless of grievance)?',
    'Post-threshold-crossing suppression trajectory: if suppression persists after federal injustice is documented and remedies exhausted, reclassify as partially internalized — the constraint''s legitimacy is internalized as a barrier.',
    'If internalized, effective suppression is higher than structural measure suggests — the threshold rule''s coordination function is undermined by cognitive capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in secession legitimacy constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secession_grievance_threshold_tr_t1945, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1945, 0.4).
narrative_ontology:measurement(secession_grievance_threshold_tr_t1960, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(secession_grievance_threshold_tr_t1975, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(secession_grievance_threshold_tr_t1990, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(secession_grievance_threshold_tr_t2005, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(secession_grievance_threshold_tr_t2025, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(secession_grievance_threshold_be_t1945, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(secession_grievance_threshold_be_t1960, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(secession_grievance_threshold_be_t1975, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1975, 0.22).
narrative_ontology:measurement(secession_grievance_threshold_be_t1990, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(secession_grievance_threshold_be_t2005, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 2005, 0.2).
narrative_ontology:measurement(secession_grievance_threshold_be_t2025, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(secession_grievance_threshold_su_t1945, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement(secession_grievance_threshold_su_t1960, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(secession_grievance_threshold_su_t1975, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(secession_grievance_threshold_su_t1990, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(secession_grievance_threshold_su_t2005, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(secession_grievance_threshold_su_t2025, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 2025, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__grievance_threshold_reading, 0.08).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__treaty_primacy_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, federal_autonomy_arrangements).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, minority_rights_protection_regimes).

% DUAL FORMULATION NOTE:
% This reading decomposes the secession_legitimacy_boundary kernel along the epsilon-invariance principle: constitutional_impossibility_reading has near-zero extraction (mountain) but high suppression; popular_sovereignty_reading has moderate extraction (variable by majority size); treaty_primacy_reading has extraction profile dependent on treaty-holder consent structures. This reading's epsilon (0.18) reflects its conditional, evidence-gated structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__grievance_threshold_reading, institutional, 0.15).
constraint_indexing:directionality_override(secession_legitimacy_boundary__grievance_threshold_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
