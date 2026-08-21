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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category: Intersex Accommodation Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'intersex accommodation' reading
 *   of the 'woman_category' kernel. It posits that the category of 'woman'
 *   should acknowledge biological sex as a non-binary spectrum, explicitly
 *   including typical female biology alongside intersex variations that do
 *   not fit a male category. This reading aims to provide recognition and
 *   inclusion for intersex individuals, challenging strictly binary
 *   definitions of sex. The classification as a Tangled Rope reflects its
 *   genuine coordination function (inclusion) alongside the active
 *   enforcement required to shift existing binary frameworks, which imposes
 *   costs on those adhering to them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.35).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.4).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category: Intersex Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '1653d1d0-f920-453b-9f48-654bca05286b').
narrative_ontology:cs_kernel_codification('1653d1d0-f920-453b-9f48-654bca05286b', distributed).
narrative_ontology:cs_authority_grounding('1653d1d0-f920-453b-9f48-654bca05286b', distributed).
narrative_ontology:cs_reading_relation('1653d1d0-f920-453b-9f48-654bca05286b', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('1653d1d0-f920-453b-9f48-654bca05286b', woman_category__gender_identity_reading, influences).
narrative_ontology:cs_axiom('1653d1d0-f920-453b-9f48-654bca05286b', foundational, biological_sex_is_a_spectrum).
narrative_ontology:cs_axiom_status(biological_sex_is_a_spectrum, holdable).
narrative_ontology:cs_axiom_grounding('1653d1d0-f920-453b-9f48-654bca05286b', biological_sex_is_a_spectrum, empirically_contingent).
narrative_ontology:cs_axiom('1653d1d0-f920-453b-9f48-654bca05286b', foundational, woman_category_includes_intersex_variations).
narrative_ontology:cs_axiom_status(woman_category_includes_intersex_variations, holdable).
narrative_ontology:cs_axiom_grounding('1653d1d0-f920-453b-9f48-654bca05286b', woman_category_includes_intersex_variations, conventional).
narrative_ontology:cs_reference_frame('1653d1d0-f920-453b-9f48-654bca05286b', inclusive_biological_continuum).
narrative_ontology:cs_drift_state('1653d1d0-f920-453b-9f48-654bca05286b', contemporary_binary_challenge, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('1653d1d0-f920-453b-9f48-654bca05286b', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_individuals).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, advocates_for_intersex_rights).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, binary_sex_framework_adherents).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, some_female_athletes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, elite_female_athletes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals with biological sex characteristics that do not fit typical binary definitions. This reading offers them recognition, inclusion, and protection from miscategorization and discrimination.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_individuals, beneficiary,
    powerless, biographical, trapped, global).

% Organizations and activists working to promote the rights and recognition of intersex individuals. This reading aligns with their goals and provides a framework for policy change.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, advocates_for_intersex_rights, beneficiary,
    organized, generational, mobile, global).

% Legislators, regulators, and institutional leaders responsible for defining categories and implementing policies. They are tasked with interpreting and applying this understanding in various domains (e.g., legal, medical, social).
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Individuals and groups who adhere to a strictly binary understanding of biological sex (male/female only). This reading challenges their foundational framework, requiring them to adapt or contest established definitions.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, binary_sex_framework_adherents, payer,
    organized, generational, identity_locked, global).

% In specific contexts like elite sports, some female athletes may perceive this reading as creating unfair competition if intersex individuals with certain biological advantages are included in women's categories without clear regulatory frameworks.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, elite_female_athletes, payer,
    moderate, biographical, constrained, global).

% Organizations responsible for setting rules and categories in sports. They face the challenge of balancing inclusion with fairness, particularly in categories where biological differences impact performance.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sports_governing_bodies, agenda_setter,
    institutional, biographical, constrained, global).

% Scholars and researchers in biology, ethics, law, and social sciences who analyze the implications and coherence of this reading without direct participation in its enforcement or direct benefit/cost.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate a more inclusive and scientifically accurate understanding of biological sex and the category of 'woman' by explicitly accommodating intersex variations, thereby ensuring appropriate social, legal, and medical recognition for intersex individuals.
% TRANSFER_FUNCTION: Transfers recognition, rights, and social inclusion to intersex individuals. It transfers the burden of adapting language, policies, and social norms from a strictly binary framework to institutions and individuals who previously operated under such a framework.
% ABSENT_VOICES: Those who believe that the category of 'woman' must be strictly defined by typical female reproductive biology to protect the interests and distinct identity of cisgender women. They would argue that expanding the definition dilutes or undermines the category.
% DISAPPEARANCE_RATIONALE: If this understanding vanished, intersex individuals would revert to being systematically miscategorized, excluded, or forced into binary frameworks that do not reflect their biological reality, leading to significant social, legal, and medical harms and a rearrangement of their status and rights.
% FOUNDING_PROBLEM: The historical and ongoing exclusion, miscategorization, and discrimination faced by intersex individuals due to rigid binary definitions of sex and gender, leading to lack of recognition, medical harm, and social marginalization.
% FOUNDING_PROBLEM_CORROBORATION: Medical associations, human rights organizations, intersex advocacy groups, and legal scholars consistently document the challenges and harms faced by intersex individuals under strictly binary systems, corroborating the ongoing relevance of this problem.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.35) is moderate-low in general policy contexts, as the direct costs are borne by a relatively small population (binary-framework adherents adapting policies). However, it can be high in specific, high-stakes contexts like elite sports, where performance advantages are debated. Suppression (0.40) is present because this reading actively challenges and seeks to suppress the enforcement of strictly binary sex definitions in policy and practice. Resistance (0.50) is significant due to deeply entrenched binary understandings. The theater ratio is low (0.10) as this is a genuine effort at inclusion, not performative maintenance of an atrophied function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of intersex individuals and their advocates, this constraint is a vital coordination mechanism for justice and recognition. From the perspective of binary-sex framework adherents, it is an extractive force that undermines established categories and imposes ideological costs. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Intersex individuals and their advocates are clear beneficiaries, gaining recognition and rights. Policy makers and sports governing bodies act as agenda-setters, tasked with implementing this understanding. Adherents to strictly binary sex frameworks and, in specific cases, some female athletes, are victims/payers, as their existing frameworks or competitive positions are challenged or altered by this inclusive definition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    context_dependent_extraction,
    'Does the extractiveness of this reading vary significantly depending on the specific policy domain (e.g., general social policy vs. elite sports)?',
    'Empirical analysis of policy implementation and stakeholder impact across diverse domains, quantifying the costs and benefits in each context.',
    'If extractiveness is significantly higher in specific domains (e.g., elite sports), the constraint might compute as a Snare or Tangled Rope in those contexts, even if it computes as a Rope or Scaffold in others. This would necessitate domain-specific sub-classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(context_dependent_extraction, empirical, 'Variability of extraction based on application context.').

omega_variable(
    suppression_of_binary_frameworks,
    'Is the ''suppression'' metric primarily reflecting the active challenge to binary frameworks, or is it perceived as suppressing individuals who hold binary views?',
    'Qualitative sociological research and legal analysis examining the impact of policies based on this reading on individuals and groups adhering to binary views, distinguishing between suppression of a framework vs. suppression of persons.',
    'If perceived as suppressing individuals, the effective suppression could be higher, potentially pushing the classification towards a Snare for those seats. If it''s primarily framework-level, the current suppression value is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_binary_frameworks, conceptual, 'Distinction between suppressing a framework and suppressing individuals.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a coherent, distinct reading of the ''woman_category'' kernel, or is it an attempt to bridge or reconcile the ''sex_biology_reading'' and ''gender_identity_reading''?',
    'Conceptual analysis of the foundational axioms and their logical independence from the axioms of sibling readings. If its core premises are reducible to a combination of the others, it may not be a distinct reading.',
    'If not a distinct reading, it might be reclassified as a hybrid or a derivative, affecting its network position and the interpretation of its relations to other readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Coherence and distinctness of this kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2000, woman_category__intersex_accommodation_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(woma_tr_t2005, woman_category__intersex_accommodation_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(woma_tr_t2010, woman_category__intersex_accommodation_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(woma_tr_t2015, woman_category__intersex_accommodation_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(woma_tr_t2020, woman_category__intersex_accommodation_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(woma_tr_t2025, woman_category__intersex_accommodation_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(woma_be_t2000, woman_category__intersex_accommodation_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(woma_be_t2005, woman_category__intersex_accommodation_reading, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(woma_be_t2010, woman_category__intersex_accommodation_reading, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(woma_be_t2015, woman_category__intersex_accommodation_reading, base_extractiveness, 2015, 0.32).
narrative_ontology:measurement(woma_be_t2020, woman_category__intersex_accommodation_reading, base_extractiveness, 2020, 0.34).
narrative_ontology:measurement(woma_be_t2025, woman_category__intersex_accommodation_reading, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2000, woman_category__intersex_accommodation_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(woma_su_t2005, woman_category__intersex_accommodation_reading, suppression_requirement, 2005, 0.33).
narrative_ontology:measurement(woma_su_t2010, woman_category__intersex_accommodation_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement(woma_su_t2015, woman_category__intersex_accommodation_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(woma_su_t2020, woman_category__intersex_accommodation_reading, suppression_requirement, 2020, 0.39).
narrative_ontology:measurement(woma_su_t2025, woman_category__intersex_accommodation_reading, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'woman_category' kernel, each with its own structural properties and classification. The other readings are 'sex_biology_reading' and 'gender_identity_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
