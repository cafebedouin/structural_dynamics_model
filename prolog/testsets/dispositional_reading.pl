% ============================================================================
% CONSTRAINT STORY: dispositional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dispositional_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dispositional_reading
 *   human_readable: Dispositional Character Theory of Moral Action
 *   domain: moral_psychology/philosophy_of_action/social_psychology
 *
 * SUMMARY:
 *   The dispositional reading of moral causation holds that moral action
 *   originates in stable character traits that persist across situations.
 *   This reading grounds character education, virtue ethics, and individual
 *   moral responsibility by making character the primary actionable variable.
 *   It is one of three sibling readings of the moral causation locus kernel;
 *   the situational reading locates moral causation in environmental
 *   features, and the interactionist reading distributes causation across
 *   person-situation interactions. The dispositional reading's empirical
 *   foundation has been contested since the 1960s situationist challenge, but
 *   it persists institutionally because character-education systems and moral
 *   development professions depend on its framework.
 *
 * KEY AGENTS:
 *   - character_education_institutions: agenda_setter (institutional/constrained) — design and administer character-based curricula and assessments
 *   - moral_development_professionals: beneficiary (organized/constrained) — professional identity constituted through dispositional diagnosis and intervention
 *   - virtue_ethics_theorists: beneficiary (organized/mobile) — academic standing rides on character robustness without administering the institutional apparatus
 *   - individuals_blamed_for_situational_failures: payer (powerless/identity_locked) — failures attributed to character deficits rather than situational pressure
 *   - marginalized_groups_pathologized_as_defective: payer (powerless/trapped) — structural oppression converted into evidence of collective moral inferiority
 *   - situational_psychologists: excluded (organized/mobile) — empirical findings threaten the frame, marginalized in policy discourse
 *   - social_psychologists: observer (organized/analytical) — measure cross-situational consistency and situational effects without institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dispositional_reading, 0.68).
domain_priors:suppression_score(dispositional_reading, 0.72).
domain_priors:theater_ratio(dispositional_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dispositional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dispositional_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dispositional_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dispositional_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dispositional_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dispositional_reading, tangled_rope).
narrative_ontology:human_readable(dispositional_reading, "Dispositional Character Theory of Moral Action").
narrative_ontology:topic_domain(dispositional_reading, "moral_psychology/philosophy_of_action/social_psychology").

domain_priors:requires_active_enforcement(dispositional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dispositional_reading, 'cee9f50b-d273-407d-bdf8-32000b4008a1').
narrative_ontology:cs_kernel_codification('cee9f50b-d273-407d-bdf8-32000b4008a1', distributed).
narrative_ontology:cs_authority_grounding('cee9f50b-d273-407d-bdf8-32000b4008a1', expertise).
narrative_ontology:cs_interpretation_layer_present('cee9f50b-d273-407d-bdf8-32000b4008a1').
narrative_ontology:cs_reading_relation('cee9f50b-d273-407d-bdf8-32000b4008a1', moral_causation_locus__situational_reading, coexists_with).
narrative_ontology:cs_reading_relation('cee9f50b-d273-407d-bdf8-32000b4008a1', moral_causation_locus__interactionist_reading, coexists_with).
narrative_ontology:cs_axiom('cee9f50b-d273-407d-bdf8-32000b4008a1', foundational, character_cross_situational_stability).
narrative_ontology:cs_axiom_status(character_cross_situational_stability, holdable).
narrative_ontology:cs_axiom_grounding('cee9f50b-d273-407d-bdf8-32000b4008a1', character_cross_situational_stability, empirically_contingent).
narrative_ontology:cs_axiom('cee9f50b-d273-407d-bdf8-32000b4008a1', secondary, individual_moral_accountability).
narrative_ontology:cs_axiom_status(individual_moral_accountability, holdable).
narrative_ontology:cs_axiom_grounding('cee9f50b-d273-407d-bdf8-32000b4008a1', individual_moral_accountability, deontological).
narrative_ontology:cs_reference_frame('cee9f50b-d273-407d-bdf8-32000b4008a1', aristotelian_virtue_framework).
narrative_ontology:cs_drift_state('cee9f50b-d273-407d-bdf8-32000b4008a1', post_situationist_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cee9f50b-d273-407d-bdf8-32000b4008a1', '').
narrative_ontology:cs_kernel_id(dispositional_reading, moral_causation_locus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dispositional_reading, character_education_institutions).
narrative_ontology:constraint_beneficiary(dispositional_reading, moral_development_professionals).
narrative_ontology:constraint_beneficiary(dispositional_reading, virtue_ethics_theorists).
narrative_ontology:constraint_victim(dispositional_reading, individuals_blamed_for_situational_failures).
narrative_ontology:constraint_victim(dispositional_reading, marginalized_groups_pathologized_as_defective).
narrative_ontology:constraint_vindicates(dispositional_reading, virtue_ethics_framework).
narrative_ontology:constraint_vindicates(dispositional_reading, individual_moral_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design curricula, assessment frameworks, and intervention programs premised on stable character traits as the primary moral observable. Their institutional legitimacy and funding streams depend on character being the actionable variable; situational explanations would redirect resources to environmental modification rather than individual formation.
narrative_ontology:constraint_stakeholder(dispositional_reading, character_education_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Practitioners whose professional identity is constituted through diagnosing character deficits and prescribing character-building interventions. The dispositional frame makes their expertise legible and billable; situational accounts would dissolve the professional boundary by making moral improvement a matter of redesigning contexts rather than treating individuals.
narrative_ontology:constraint_stakeholder(dispositional_reading, moral_development_professionals, beneficiary,
    organized, biographical, constrained, national).

% Academic philosophers whose research programs rest on character traits as stable, cross-situational moral capacities. They benefit from the dispositional reading's empirical vindication without administering its institutional apparatus; they can exit to other normative frameworks if the empirical ground shifts, but their current standing rides on character's robustness.
narrative_ontology:constraint_stakeholder(dispositional_reading, virtue_ethics_theorists, beneficiary,
    organized, generational, mobile, global).

% People who fail morally under situational pressure but are diagnosed as having defective character. The dispositional frame locates the failure inside them rather than in the situation they faced, which means remediation targets their identity rather than the context. Exit requires rejecting the frame that names them as the problem, which is cognitively and socially costly when the frame is institutionally dominant.
narrative_ontology:constraint_stakeholder(dispositional_reading, individuals_blamed_for_situational_failures, payer,
    powerless, biographical, identity_locked, local).

% Groups whose aggregate moral failures under structural oppression are attributed to collective character deficits rather than to the situations that constrain them. The dispositional reading converts systemic extraction into evidence of moral inferiority, which justifies continued exclusion. They cannot exit because the frame is applied to them from outside and enforced through institutional gatekeeping.
narrative_ontology:constraint_stakeholder(dispositional_reading, marginalized_groups_pathologized_as_defective, payer,
    powerless, generational, trapped, national).

% Researchers who document situation-driven moral variance and argue character traits have weak cross-situational predictive power. Their findings threaten the dispositional frame's empirical foundation, so they are marginalized in character-education policy discourse despite holding academic standing. They would redirect moral intervention toward situational redesign if admitted to the agenda-setting conversation.
narrative_ontology:constraint_stakeholder(dispositional_reading, situational_psychologists, excluded,
    organized, biographical, mobile, global).

% Conduct experiments testing the dispositional hypothesis against situational alternatives. They measure cross-situational consistency, document obedience and conformity effects, and produce the empirical record the reading's status depends on. Their analytical distance lets them report findings without institutional stake in which reading prevails.
narrative_ontology:constraint_stakeholder(dispositional_reading, social_psychologists, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legible target for moral education and intervention: if character traits are robust, institutions can assess them, curricula can build them, and individuals can be held accountable for possessing or lacking them.
% TRANSFER_FUNCTION: Moves moral responsibility from situations to individuals, which moves institutional resources from environmental modification to character remediation, and moves blame from structural designers to those who fail under structural pressure.
% ABSENT_VOICES: Situational psychologists and structural critics are excluded from character-education policy design; they would argue the dispositional frame misattributes situational failures to individual deficits and that moral improvement requires redesigning contexts, not treating people.
% DISAPPEARANCE_RATIONALE: If the dispositional reading vanished, character-education institutions would lose their organizing principle, moral development professionals would need to reconstitute their expertise around situational design, and individuals currently blamed for character deficits would be reclassified as responding predictably to the situations they faced. Moral intervention would shift from individual remediation to context modification.
% FOUNDING_PROBLEM: Early moral psychology needed a stable, measurable construct to ground moral education and responsibility attribution in a scientific age; character traits offered that construct by making moral capacity an enduring property of persons rather than an ephemeral response to circumstances.
% FOUNDING_PROBLEM_CORROBORATION: Character-education institutions and virtue ethicists attest the founding problem remains live, citing the need for stable moral accountability. Situational psychologists and social psychologists attest the problem has been empirically undermined: decades of experiments show weak cross-situational consistency and strong situational effects, which means the stable construct the reading was built to provide does not exist as theorized. Independent meta-analyses and replication studies from outside the benefiting institutions support the empirical-challenge reading.
narrative_ontology:disappearance_verdict(dispositional_reading, world_rearranges).
narrative_ontology:founding_problem_status(dispositional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dispositional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(dispositional_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dispositional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dispositional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dispositional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because the dispositional frame redirects moral intervention resources from situational redesign to individual remediation, and it converts structural failures into individual deficits, which justifies exclusion of those who fail. Suppression is high (0.72) because the reading's persistence depends on marginalizing situational evidence and excluding situational psychologists from policy design; the frame must be actively defended against empirical challenge. Theater ratio is moderate (0.41): character assessment and intervention are real activities, but a growing share of enforcement effort goes to defending the dispositional premise against situational alternatives rather than to improving moral outcomes. Accessibility collapse is moderate-low (0.48): situational alternatives are empirically available and theoretically coherent, but institutionally suppressed. Resistance is substantial (0.62): situational psychologists, structural critics, and those blamed under the frame actively contest it. The measurement series shows extraction, suppression, and theater all rising over the 80-year interval as the empirical foundation weakened but institutional dependence deepened.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, the dispositional reading is a necessary framework for making moral education legible and actionable; character traits are the stable construct moral intervention requires. From the payer seats, the same structure operates as enforced misattribution: situational failures are converted into character deficits, which moves blame from structural designers to those who fail under structural pressure. The observer seat sees the empirical record: weak cross-situational consistency, strong situational effects, and institutional persistence despite empirical challenge. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Character-education institutions and moral development professionals are structural beneficiaries: they collect resources, professional standing, and institutional authority from the dispositional frame. Individuals blamed for situational failures and marginalized groups pathologized as defective are the targets: the frame extracts from them by locating moral failure inside them rather than in the situations they face, which justifies remediation targeting their identity and exclusion from opportunity. Virtue ethicists benefit without administering the apparatus, so their directionality is lower than the institutional agenda-setters. Situational psychologists are excluded rather than coordinated; their exclusion is what the suppression machinery maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The dispositional reading coordinates a real function (providing a stable target for moral education) and extracts asymmetrically (by misattributing situational failures to individual deficits and redirecting resources from situational redesign to individual remediation). The coordination function is genuine: institutions need actionable constructs, and character traits offer that. The extraction is also genuine: the frame systematically benefits those who administer character-based interventions and systematically harms those who fail under situational pressure by locating the failure inside them. This is tangled rope, not pure snare: both the coordination and the extraction are real, and the constraint requires active enforcement to hold the dispositional premise against empirical challenge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cross_situational_consistency,
    'Do character traits exhibit sufficient cross-situational consistency to ground moral education and responsibility attribution, or is moral behavior primarily situation-driven?',
    'Meta-analysis of longitudinal studies measuring behavioral consistency across diverse moral situations; replication of classic situationist experiments with larger samples and pre-registered designs.',
    'If cross-situational consistency is weak, the dispositional reading''s empirical foundation collapses and moral intervention should target situations rather than individuals. If consistency is robust, the dispositional frame is vindicated and character-based education is the appropriate intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_situational_consistency, empirical, 'Whether character traits are stable enough across situations to justify the dispositional frame.').

omega_variable(
    misattribution_vs_accountability,
    'Does the dispositional frame misattribute situational failures to individual deficits (extractive misattribution), or does it correctly hold individuals accountable for character they could have developed (legitimate accountability)?',
    'Comparative analysis of moral outcomes under dispositional vs. situational intervention regimes; examination of whether character-based remediation improves outcomes or merely relocates blame.',
    'If the frame misattributes, it is extractive: it harms those who fail under situational pressure by locating the failure inside them. If it correctly attributes, it is coordinative: it holds people accountable for capacities they control.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(misattribution_vs_accountability, conceptual, 'Whether dispositional attribution is extractive misattribution or legitimate accountability.').

omega_variable(
    institutional_vs_empirical_persistence,
    'Does the dispositional reading persist because character traits are empirically robust, or because character-education institutions depend on the framework regardless of empirical status?',
    'Historical analysis of the reading''s institutional entrenchment relative to its empirical support; examination of whether policy shifts track empirical findings or institutional interests.',
    'If persistence is institutional rather than empirical, the reading is a false summit: it presents as a natural fact about moral psychology but is actually a constructed constraint benefiting identifiable institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_empirical_persistence, empirical, 'Whether the reading''s persistence tracks empirical robustness or institutional dependence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dispositional_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(disp_tr_t0, dispositional_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(disp_tr_t16, dispositional_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(disp_tr_t32, dispositional_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(disp_tr_t48, dispositional_reading, theater_ratio, 48, 0.34).
narrative_ontology:measurement(disp_tr_t64, dispositional_reading, theater_ratio, 64, 0.38).
narrative_ontology:measurement(disp_tr_t80, dispositional_reading, theater_ratio, 80, 0.41).

% Extraction over time
narrative_ontology:measurement(disp_be_t0, dispositional_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(disp_be_t16, dispositional_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(disp_be_t32, dispositional_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(disp_be_t48, dispositional_reading, base_extractiveness, 48, 0.61).
narrative_ontology:measurement(disp_be_t64, dispositional_reading, base_extractiveness, 64, 0.65).
narrative_ontology:measurement(disp_be_t80, dispositional_reading, base_extractiveness, 80, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(disp_su_t0, dispositional_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(disp_su_t16, dispositional_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(disp_su_t32, dispositional_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(disp_su_t48, dispositional_reading, suppression_requirement, 48, 0.64).
narrative_ontology:measurement(disp_su_t64, dispositional_reading, suppression_requirement, 64, 0.68).
narrative_ontology:measurement(disp_su_t80, dispositional_reading, suppression_requirement, 80, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dispositional_reading, identity_coordination).
narrative_ontology:affects_constraint(dispositional_reading, situational_reading).
narrative_ontology:affects_constraint(dispositional_reading, interactionist_reading).

% DUAL FORMULATION NOTE:
% The dispositional reading is one of three sibling readings of the moral_causation_locus kernel. All three readings address the same founding problem (locating the source of moral action) but instantiate different constraints with different beneficiary structures and different empirical commitments. The dispositional reading benefits character-education institutions and extracts from those blamed for situational failures; the situational reading would benefit situational designers and extract from those who built extractive situations; the interactionist reading distributes both benefits and costs. The readings are linked via network.affects_constraints because they compete for the same institutional and policy space: adopting one reading structurally influences the legitimacy and resource availability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
