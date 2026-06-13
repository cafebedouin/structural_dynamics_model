% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Hebrew Lives Through Haskalah Literary Production
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the status of Hebrew as a 'living language'
 *   through the lens of the Haskalah (Jewish Enlightenment) literary
 *   movement, roughly from the late 18th to late 19th century. During this
 *   period, Hebrew was not a language of daily spoken communication for most
 *   Jews, but it experienced a significant revival as a medium for secular
 *   literature, poetry, and journalism. This reading posits that this written
 *   generative competence, distinct from liturgical use, constituted a form
 *   of 'living' status, maintaining its vitality and adaptability for modern
 *   expression.
 *
 * KEY AGENTS:
 *   - haskalah_intellectuals: Agenda-setter/Beneficiary (institutional/arbitrage) — actively produced and consumed Hebrew literature, deriving status and intellectual community from it.
 *   - hebrew_literary_tradition: Beneficiary (civilizational/analytical) — the abstract body of Hebrew literature that gained new life and expanded scope.
 *   - jewish_communities: Payer/Beneficiary (organized/constrained) — provided the cultural context and readership, but did not speak Hebrew daily; bore the cost of learning a non-vernacular literary language.
 *   - linguistic_scholars: Observer (analytical/analytical) — analyze the historical linguistic status of Hebrew during this period.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.05).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.1).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, mountain).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew Lives Through Haskalah Literary Production").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:emerges_naturally(hebrew_living_language__literary_revival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '6c037004-9884-4fc4-a140-ba5fa1b505b9').
narrative_ontology:cs_kernel_codification('6c037004-9884-4fc4-a140-ba5fa1b505b9', implicit).
narrative_ontology:cs_authority_grounding('6c037004-9884-4fc4-a140-ba5fa1b505b9', practice).
narrative_ontology:cs_interpretation_layer_present('6c037004-9884-4fc4-a140-ba5fa1b505b9').
narrative_ontology:cs_reading_relation('6c037004-9884-4fc4-a140-ba5fa1b505b9', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c037004-9884-4fc4-a140-ba5fa1b505b9', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('6c037004-9884-4fc4-a140-ba5fa1b505b9', foundational, generative_written_use_confers_living_status).
narrative_ontology:cs_axiom_status(generative_written_use_confers_living_status, holdable).
narrative_ontology:cs_axiom_grounding('6c037004-9884-4fc4-a140-ba5fa1b505b9', generative_written_use_confers_living_status, conventional).
narrative_ontology:cs_axiom('6c037004-9884-4fc4-a140-ba5fa1b505b9', foundational, secular_literary_expression_is_vitality).
narrative_ontology:cs_axiom_status(secular_literary_expression_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('6c037004-9884-4fc4-a140-ba5fa1b505b9', secular_literary_expression_is_vitality, conventional).
narrative_ontology:cs_reference_frame('6c037004-9884-4fc4-a140-ba5fa1b505b9', hebrew_as_literary_vehicle).
narrative_ontology:cs_drift_state('6c037004-9884-4fc4-a140-ba5fa1b505b9', post_haskalah_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6c037004-9884-4fc4-a140-ba5fa1b505b9', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_intellectuals).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_literary_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, jewish_communities).
narrative_ontology:constraint_victim(hebrew_living_language__literary_revival_reading, jewish_communities).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, hebrew_as_modern_literary_language).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The writers, poets, and journalists of the Haskalah movement who actively produced new literature in Hebrew, thereby demonstrating its capacity for modern expression and maintaining its generative competence. They gained intellectual community and status from this activity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_intellectuals, agenda_setter,
    institutional, biographical, arbitrage, regional).

% The abstract body of Hebrew literature itself, which expanded significantly in scope and genre during the Haskalah, demonstrating its adaptability and vitality beyond sacred texts. It benefited from the continuous production of new works.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_literary_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__literary_revival_reading, hebrew_literary_tradition).

% The broader Jewish population who, while not speaking Hebrew daily, were the cultural context and potential readership for Haskalah literature. They bore the 'cost' of engaging with a non-vernacular literary language but benefited from cultural enrichment and a renewed sense of Jewish identity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, jewish_communities, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, jewish_communities, beneficiary).

% Academics and researchers who study the historical development and vitality of Hebrew, analyzing the role of Haskalah literary production in its status as a living language. They provide an external, analytical perspective on the constraint.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, linguistic_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__literary_revival_reading, haskalah_intellectuals).
narrative_ontology:fixing_cost_class(hebrew_living_language__literary_revival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinated the intellectual and cultural efforts of the Haskalah movement around a shared, modernizing literary medium, allowing for the expression of new ideas and the formation of a secular Jewish intellectual identity.
% TRANSFER_FUNCTION: It transferred cultural capital and intellectual influence to Haskalah intellectuals, and a sense of modern linguistic vitality to the Hebrew language itself, from the diffuse cultural engagement of Jewish communities.
% ABSENT_VOICES: Strict Hebraists who insisted on Hebrew's exclusive sacred function might have objected to its secularization, arguing that such use diminished its holiness. They were present in the broader cultural discourse but not central to the Haskalah's literary agenda.
% DISAPPEARANCE_RATIONALE: If the Haskalah's literary production in Hebrew had not occurred, the trajectory of Hebrew language revitalization would have been fundamentally different, potentially leading to its complete dormancy as a generative language, or a much slower, more fragmented revival. The cultural and intellectual landscape of modern Jewry would be significantly altered.
% FOUNDING_PROBLEM: The problem was the perceived stagnation of Hebrew as a language confined primarily to sacred texts and liturgical use, lacking a vibrant, modern, secular literary voice capable of engaging with contemporary European thought.
% FOUNDING_PROBLEM_CORROBORATION: Linguistic historians and scholars of Jewish studies widely corroborate that the problem of Hebrew's stagnation as a secular literary language was largely addressed and overcome by the Haskalah. While the broader challenge of Hebrew's status as a spoken language remained, the specific literary problem was resolved. No parties outside the Haskalah's direct beneficiaries dispute this historical assessment of its literary impact.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_living_language__literary_revival_reading),
    narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because the 'living' status of Hebrew through literary production, while requiring active intellectual effort, is presented as an inherent property of the language's continuous written tradition and its capacity for generative expression. Extractiveness is very low (0.05) as the literary production was largely an elite, voluntary activity, not imposing costs on a broad population. Suppression is low (0.1) as there was no active coercion to participate in Haskalah literary circles. Theater ratio is low (0.05) because the literary output was genuinely functional and innovative, not merely performative. Accessibility collapse is high (0.9) because, for those outside the intellectual elite, the 'living' aspect of Hebrew was largely inaccessible in a generative sense, though liturgical access remained. Resistance is low (0.05) as the Haskalah movement, while sometimes controversial, faced little direct resistance regarding its use of Hebrew for literary purposes.
 *
 * PERSPECTIVAL GAP:
 *   Haskalah intellectuals experienced this as a vibrant, self-sustaining literary ecosystem, a 'living' language in their hands. For the broader Jewish communities, Hebrew remained primarily a language of prayer and study, not daily life, making the 'living' aspect less immediate. Linguistic scholars analyze this period as a critical phase in Hebrew's revitalization, but distinct from full native spoken fluency.
 *
 * DIRECTIONALITY LOGIC:
 *   Haskalah intellectuals are beneficiaries and agenda-setters, actively shaping and benefiting from the literary revival. The Hebrew literary tradition itself is a beneficiary, gaining new works and expanded scope. Jewish communities are diffuse payers (in terms of effort to engage with non-vernacular literature) and beneficiaries (in terms of cultural enrichment and identity).
 *
 * MANDATROPHY ANALYSIS:
 *   The 'mandate' of Hebrew as a living language through literary production did not atrophy; rather, it evolved into the foundation for the later, more comprehensive native speech revival. This classification prevents mislabeling a crucial phase of language revitalization as a static or extractive constraint, recognizing its dynamic role in maintaining linguistic vitality through a specific mode of generative competence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_continuity,
    'Is the continuity of Hebrew through literary production a natural linguistic phenomenon, or a constructed outcome of intellectual effort?',
    'Comparative historical linguistics of other ''dead'' languages with similar literary revivals; analysis of the social and institutional structures supporting Haskalah.',
    'If purely natural, it reinforces the ''mountain'' classification. If significantly constructed, it suggests a ''rope'' or ''scaffold'' for the intellectual community, with low extraction but active coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_continuity, empirical, 'Ambiguity between natural linguistic continuity and active intellectual construction.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''hebrew_living_language'' kernel. Does this ''literary_revival_reading'' accurately capture the structural implications of Haskalah literary production for Hebrew''s status as a living language?',
    'Analysis of primary Haskalah texts and contemporary linguistic scholarship on language vitality, comparing against the structural claims of sibling readings.',
    'If this reading is structurally incomplete or misrepresents the Haskalah''s role, the classification of Hebrew''s ''living'' status would shift, potentially towards a more ''liturgical_continuity_reading'' (if the literary aspect is less central) or ''native_generation_reading'' (if the literary phase is seen as a precursor to full revitalization).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''literary_revival_reading'' of the ''hebrew_living_language'' kernel. Sibling readings are ''liturgical_continuity_reading'' and ''native_generation_reading''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1780, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_living_language' kernel, focusing on the Haskalah literary revival. It structurally influences and is influenced by the liturgical continuity and native generation readings, as each represents a different mode of linguistic vitality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
