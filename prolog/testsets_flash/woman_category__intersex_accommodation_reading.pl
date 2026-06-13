% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category: Intersex Accommodation Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'intersex accommodation' reading
 *   of the 'woman_category' kernel. It defines 'woman' to include individuals
 *   with typical female biology and those with intersex variations that do
 *   not fit the male category. This reading aims to provide recognition and
 *   inclusion for intersex individuals, challenging rigid binary sex
 *   definitions. While generally low in extraction for most policy domains
 *   due to the small population affected, it can become highly extractive in
 *   specific contexts like elite sports, where biological differences are
 *   highly scrutinized.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.15).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.2).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category: Intersex Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '0258deac-3fd3-4c54-8d2a-8e7defec3510').
narrative_ontology:cs_kernel_codification('0258deac-3fd3-4c54-8d2a-8e7defec3510', distributed).
narrative_ontology:cs_authority_grounding('0258deac-3fd3-4c54-8d2a-8e7defec3510', practice).
narrative_ontology:cs_interpretation_layer_present('0258deac-3fd3-4c54-8d2a-8e7defec3510').
narrative_ontology:cs_reading_relation('0258deac-3fd3-4c54-8d2a-8e7defec3510', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('0258deac-3fd3-4c54-8d2a-8e7defec3510', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('0258deac-3fd3-4c54-8d2a-8e7defec3510', foundational, sex_is_a_spectrum).
narrative_ontology:cs_axiom_status(sex_is_a_spectrum, holdable).
narrative_ontology:cs_axiom_grounding('0258deac-3fd3-4c54-8d2a-8e7defec3510', sex_is_a_spectrum, empirically_contingent).
narrative_ontology:cs_axiom('0258deac-3fd3-4c54-8d2a-8e7defec3510', foundational, inclusion_of_intersex_is_ethical).
narrative_ontology:cs_axiom_status(inclusion_of_intersex_is_ethical, holdable).
narrative_ontology:cs_axiom_grounding('0258deac-3fd3-4c54-8d2a-8e7defec3510', inclusion_of_intersex_is_ethical, deontological).
narrative_ontology:cs_reference_frame('0258deac-3fd3-4c54-8d2a-8e7defec3510', inclusive_biological_diversity).
narrative_ontology:cs_drift_state('0258deac-3fd3-4c54-8d2a-8e7defec3510', contemporary_social_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0258deac-3fd3-4c54-8d2a-8e7defec3510', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_individuals_assigned_female).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, advocates_for_intersex_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, elite_sports_governing_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from recognition within the 'woman' category, avoiding exclusion and misgendering based on biological variations. Their identity is deeply tied to this recognition, making exit from the category unthinkable.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_individuals_assigned_female, beneficiary,
    powerless, biographical, identity_locked, global).

% These groups actively promote the inclusion of intersex variations within sex-based categories, challenging binary definitions and advocating for policy changes. They shape the discourse and push for legal and social recognition.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, advocates_for_intersex_rights, agenda_setter,
    organized, generational, constrained, global).

% These bodies face significant challenges in implementing this reading, particularly in categories where biological sex differences are relevant for fair competition. They bear the cost of developing complex eligibility criteria and managing public relations around controversial cases.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, elite_sports_governing_bodies, payer,
    institutional, immediate, constrained, global).

% These groups argue for a strict binary definition of sex based on typical reproductive biology, often viewing intersex accommodation as undermining the integrity of sex-segregated spaces. They are often excluded from the direct policy-making process for this reading.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sex_biology_advocates, excluded,
    organized, generational, constrained, global).

% These groups prioritize gender identity over biological sex for category membership. While they may support inclusivity, their primary framework differs, and they might see this reading as not going far enough or as reinforcing biological essentialism.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, global).

% Responsible for translating this reading into concrete policies and legal frameworks. They navigate competing demands from various advocacy groups and face political pressure regarding the definition of 'woman'.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, policy_makers, agenda_setter,
    institutional, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__intersex_accommodation_reading, intersex_individuals_assigned_female).
narrative_ontology:fixing_cost_class(woman_category__intersex_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social and legal recognition for individuals with intersex variations within the 'woman' category, ensuring their inclusion and preventing discrimination based on atypical biological sex characteristics.
% TRANSFER_FUNCTION: Transfers social and legal legitimacy, and access to sex-segregated spaces/resources, to intersex individuals who identify as women, from a previous binary system that might have excluded them.
% ABSENT_VOICES: Advocates for strict sex-binary definitions and those who prioritize gender identity as the sole determinant of 'woman' category membership are often marginalized in the direct formulation of this specific reading, though their perspectives are present in the broader debate.
% DISAPPEARANCE_RATIONALE: If this reading vanished, intersex individuals would lose a crucial framework for recognition and inclusion, leading to increased discrimination, misgendering, and exclusion from spaces and rights intended for women. Policies would revert to more rigid binary definitions, causing significant social and legal upheaval for this population.
% FOUNDING_PROBLEM: The historical exclusion and misrecognition of intersex individuals within binary sex categories, leading to medical pathologization, social stigma, and denial of rights and identity.
% FOUNDING_PROBLEM_CORROBORATION: Intersex advocacy organizations, human rights bodies, and medical ethics committees consistently attest to the ongoing challenges faced by intersex individuals, corroborating that the founding problem remains live. Their reports and testimonies provide evidence from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).
:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.15) is relatively low because this reading primarily expands inclusion rather than imposing significant costs on a broad population. Suppression (0.2) is present as it requires active enforcement to challenge existing binary norms and ensure recognition. Theater ratio (0.1) is low, indicating that the constraint's function is largely genuine in its aim for inclusion. Accessibility collapse (0.4) is moderate, as it opens up categories for some while still navigating existing binary structures. Resistance (0.3) comes from those who advocate for stricter binary definitions or different criteria for category membership.
 *
 * PERSPECTIVAL GAP:
 *   For intersex individuals, this reading is a vital recognition of their identity and a source of benefit. For elite sports governing bodies, it presents a significant challenge, requiring complex policy adjustments and potentially leading to perceived unfairness by other athletes. Policy makers navigate these different perspectives, attempting to balance inclusion with other considerations.
 *
 * DIRECTIONALITY LOGIC:
 *   Intersex individuals assigned female are clear beneficiaries, gaining recognition and access. Advocates for intersex rights act as agenda-setters, driving the adoption of this reading. Elite sports governing bodies are payers, bearing the costs of implementation and controversy. Advocates for strict sex-binary or gender-identity-only definitions are excluded, as their frameworks are not central to this specific reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading addresses a live problem of historical exclusion and misrecognition of intersex individuals. It prevents mislabeling genuine efforts for inclusion as pure extraction by focusing on the coordination function of providing a coherent framework for identity and rights. The 'live' status of the founding problem, corroborated by external sources, indicates that the constraint's mandate has not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_definition_ambiguity,
    'How precisely can ''intersex variations that do not fit the male category'' be defined without creating new exclusions or ambiguities?',
    'Development of clear, medically and socially accepted criteria for intersex variations relevant to category membership, with input from intersex individuals and medical experts.',
    'Lack of clear definition could lead to arbitrary exclusions or over-inclusions, increasing extractiveness for some and undermining the coordination function. Resolution would strengthen the constraint''s legitimacy and reduce friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_definition_ambiguity, conceptual, 'Ambiguity in defining the precise boundaries of intersex inclusion.').

omega_variable(
    elite_sports_extraction_potential,
    'Does the application of this reading in elite sports contexts lead to disproportionate extraction from other female athletes due to perceived or actual performance advantages?',
    'Longitudinal studies on competitive fairness and performance outcomes in elite sports categories after implementing intersex-inclusive policies, alongside athlete feedback and scientific consensus on relevant biological factors.',
    'If significant, unmitigated performance advantages are demonstrated, the constraint''s extractiveness in this specific domain would be reclassified as high, potentially shifting its type towards a Tangled Rope or Snare for other female athletes. This would necessitate specific policy adjustments for sports.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_sports_extraction_potential, empirical, 'Potential for high extraction in specific high-stakes domains like elite sports.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural law, or a constructed constraint that benefits identifiable agents?',
    'This constraint is a specific reading of the ''woman_category'' kernel, which is a contested social construct. Its beneficiaries are clearly identifiable, indicating it is a constructed constraint, not a natural law.',
    'Acknowledging its constructed nature reinforces the need for ongoing evaluation and potential revision based on social and ethical considerations, rather than treating it as immutable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''woman_category'' kernel, which is a contested social construct. Its beneficiaries are clearly identifiable, indicating it is a constructed constraint, not a natural law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(woma_tr_t5, woman_category__intersex_accommodation_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(woma_tr_t10, woman_category__intersex_accommodation_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(woma_be_t5, woman_category__intersex_accommodation_reading, base_extractiveness, 5, 0.13).
narrative_ontology:measurement(woma_be_t10, woman_category__intersex_accommodation_reading, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(woma_su_t5, woman_category__intersex_accommodation_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(woma_su_t10, woman_category__intersex_accommodation_reading, suppression_requirement, 10, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'woman_category' kernel. Each reading defines category membership differently, leading to different beneficiary/victim sets and classifications. They are linked to reflect their shared conceptual origin and ongoing contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
