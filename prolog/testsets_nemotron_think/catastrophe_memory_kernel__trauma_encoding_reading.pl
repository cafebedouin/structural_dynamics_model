% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Ritual Trauma-Encoding as Collective Warning System
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A ritual mourning-practice encodes the trauma of a historical catastrophe
 *   (genocide, expulsion, persecution) into the psychological and somatic
 *   experience of descendants. The ritual is framed as sacred duty: 'remember
 *   so it never happens again.' The surviving collective gains a
 *   threat-vigilance system — descendants' hypervigilance functions as
 *   distributed early-warning. But the cost falls asymmetrically on
 *   descendant individuals who bear clinical-level psychological burden. The
 *   constraint is actively enforced through communal normativity: questioning
 *   the ritual's necessity is treated as betrayal. Over generations, the
 *   extractiveness rises as the original threat recedes but the trauma
 *   transmission intensifies (theater_ratio increases), while suppression
 *   requirement grows as secular alternatives (therapy, historical education)
 *   emerge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.55).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Ritual Trauma-Encoding as Collective Warning System").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, 'bc540cbf-c44a-40ee-a3d6-e6253330fb0b').
narrative_ontology:cs_kernel_codification('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', distributed).
narrative_ontology:cs_authority_grounding('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', lineage).
narrative_ontology:cs_interpretation_layer_present('bc540cbf-c44a-40ee-a3d6-e6253330fb0b').
narrative_ontology:cs_reading_relation('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', foundational, trauma_transmission_constitutes_warning_system).
narrative_ontology:cs_axiom_status(trauma_transmission_constitutes_warning_system, holdable).
narrative_ontology:cs_axiom_grounding('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', trauma_transmission_constitutes_warning_system, empirically_contingent).
narrative_ontology:cs_axiom('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', secondary, collective_survival_justifies_intergenerational_burden).
narrative_ontology:cs_axiom_status(collective_survival_justifies_intergenerational_burden, holdable).
narrative_ontology:cs_axiom_grounding('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', collective_survival_justifies_intergenerational_burden, instrumental).
narrative_ontology:cs_reference_frame('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', traumatic_memory_as_survival_archive).
narrative_ontology:cs_drift_state('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', contemporary_trauma_discourse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bc540cbf-c44a-40ee-a3d6-e6253330fb0b', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, surviving_collective).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, descendant_individuals).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, collective_survival_requires_memory).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, trauma_as_epistemic_resource).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community as a continuing entity gains threat-vigilance from the ritual's trauma transmission. The collective 'remembers' catastrophes it did not directly experience, enabling anticipatory adaptation. Exit means dissolving the collective identity that the ritual sustains.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, surviving_collective, beneficiary,
    organized, generational, constrained, national).

% Individual descendants inherit the ritual's trauma-encoding: they carry psychological burdens (hypervigilance, somatic symptoms, narrative constriction) that function as the collective's early-warning system. They also receive the protective benefit of that warning. Exit is identity-locked — rejecting the trauma narrative feels like betraying ancestors and abandoning the group.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_individuals, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, descendant_individuals, beneficiary).

% Religious leaders and ritual specialists curate the mourning-practice, selecting which traumas to encode and how to frame them. They justify the practice as sacred duty and survival necessity. Their authority depends on the ritual's perceived indispensability; they can modify liturgy but cannot abolish the core transmission without losing legitimacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ritual_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Descendants who experience the trauma burden as pathological rather than protective — those who develop clinical PTSD, depression, or substance dependence linked to the inherited narrative. They have no voice in ritual governance; their suffering is interpreted as insufficient faith or failure to 'carry the memory properly.'
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, dissenting_descendants, excluded,
    powerless, biographical, trapped, local).

% Anthropologists, psychologists, and historians who study the ritual's transmission mechanics. They document both the collective survival benefits and the individual clinical costs. Their analysis can influence public discourse and policy but does not directly alter the ritual's operation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, memory_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits catastrophic memory across generations to maintain collective threat-vigilance when direct experience of the original catastrophe is lost.
% TRANSFER_FUNCTION: Moves psychological burden (hypervigilance, narrative constriction, somatic symptoms) from ancestral trauma onto descendant individuals, in exchange for collective early-warning capacity that enables anticipatory adaptation.
% ABSENT_VOICES: Descendants who would reject the trauma transmission if they could articulate the cost-benefit from their position; secular trauma therapists who offer alternative processing frameworks; future generations not yet born who will inherit the accumulated burden.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the collective's specific trauma-encoded threat-vigilance would degrade over 1-2 generations, but the psychological burden on descendants would lift immediately. The community would lose a distinctive survival adaptation but gain individual psychological freedom. Secular memorial practices might partially substitute for the warning function.
% FOUNDING_PROBLEM: How to preserve survival-critical threat knowledge across generations when the generation that directly experienced the catastrophe dies out, and written records are vulnerable to destruction or suppression.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of ritual function in diaspora communities (outside the beneficiary set) corroborate the founding problem's historical reality. Psychological research on intergenerational trauma costs (Yehuda et al., 2016; Lehrner & Yehuda, 2018) corroborates the extraction side. The ritual authorities attest the problem remains live; dissenting descendants and trauma scholars attest it is substantially solved or misdiagnosed.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderate-high because the psychological burden on descendants is clinically significant and not fully compensated by individual benefit — the primary beneficiary is the collective. Suppression (0.55) is moderate: enforcement is normative/social rather than legal, but identity-locked exit makes it effective. Theater_ratio (0.30) reflects growing performative maintenance: as the founding threat recedes, more ritual energy goes into demonstrating fidelity than into actual threat-detection calibration. Accessibility_collapse (0.45) is partial: secular trauma processing and historical education offer alternatives, but they lack the ritual's identity-binding force. Resistance (0.50) is substantial: dissenting descendants and therapeutic frameworks challenge the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the ritual_authorities' seat, the constraint is a rope: genuine coordination solving an intergenerational knowledge problem. From descendant_individuals' seat, it is a snare: extraction masked by sacred framing. From the surviving_collective's seat, it is a tangled_rope: coordination with asymmetric cost distribution. The engine computes this divergence from the structural data — the authored claim (tangled_rope) reflects the analytical seat's structural reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The surviving_collective is the structural beneficiary (d ~ 0.2): it collects the threat-vigilance subsidy. Descendant_individuals are the primary targets (d ~ 0.8): they pay the psychological cost, and their identity_locked exit traps them. Ritual_authorities sit near symmetric (d ~ 0.5): they administer the constraint and gain authority from it, but their position depends on maintaining the transmission. Dissenting_descendants are trapped targets (d ~ 0.9) with no voice. Memory_scholars are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving threat knowledge without written records) was live at founding. Whether it remains live is contested: ritual authorities say yes (threats persist); trauma scholars say no (secular systems now exist). The arrangement persists partly because the ritual_authorities' institutional identity is fused with the trauma-encoding function — abolishing it would dissolve their mandate. This mandatrophy dynamic prevents clean classification as pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural classification change if the kernel''s other readings are valid simultaneously?',
    'Comparative analysis of all four readings'' beneficiary/victim structures and extractiveness profiles; if multiple readings describe the same ritual practice, their extraction burdens may be additive or overlapping.',
    'If readings are cumulative, total extractiveness on descendant_individuals may exceed 0.8, shifting classification toward snare. If they are alternative framings of the same extraction, the classification depends on which reading''s beneficiary structure is empirically dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame ambiguity: whether sibling readings are additive layers or competing framings of one constraint.').

omega_variable(
    adaptive_value_of_trauma_transmission,
    'Does the ritual''s trauma-encoding actually improve collective threat-detection, or is the warning-system claim a post-hoc justification?',
    'Longitudinal comparative study: communities maintaining the ritual vs. those that abandoned it, measuring actual threat-response outcomes (not self-reported vigilance). Control for confounding variables (SES, political context, alternative warning systems).',
    'If adaptive value is confirmed, the coordination function is genuine and tangled_rope holds. If disconfirmed, the constraint reclassifies toward snare — the warning narrative is cover for extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptive_value_of_trauma_transmission, empirical, 'Whether the claimed coordination function (threat-vigilance) is empirically real or a legitimating myth.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression maintaining this constraint structural (communal ostracism, institutional authority) or internalized (descendants'' belief that rejecting the trauma is moral failure)?',
    'Post-exit suppression trajectory: track dissenting_descendants who leave the community — if psychological burden persists after structural pressure is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would increase measured suppression toward 0.7+ and strengthen snare characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal/collective trauma transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_tr_t0, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_tr_t20, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_tr_t40, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_tr_t60, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_tr_t80, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_be_t0, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_be_t20, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_be_t40, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_be_t60, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_be_t80, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.65).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_su_t0, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_su_t20, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_su_t40, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_su_t60, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.53).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_su_t80, observed).
narrative_ontology:measurement(catastrophe_memory_kernel__trauma_encoding_reading_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement_basis(catastrophe_memory_kernel__trauma_encoding_reading_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__trauma_encoding_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe_memory_kernel. The kernel's label 'ritual mourning-practice' conflates four structurally distinct claims: (1) trauma_encoding — moderate-high extraction, identity_locked victims; (2) symbol_continuity — low extraction, coordination-dominant; (3) survival_competence — moderate extraction, skill-transmission coordination; (4) boundary_maintenance — moderate extraction, exclusionary coordination. Each reading has different beneficiary/victim structures and different ε values. They are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__trauma_encoding_reading, moderate, 0.8).
constraint_indexing:directionality_override(catastrophe_memory_kernel__trauma_encoding_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
