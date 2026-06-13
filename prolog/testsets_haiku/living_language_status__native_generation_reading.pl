% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Native Generation Criterion for Living Language Status
 *   domain: sociolinguistics/political/identity
 *
 * SUMMARY:
 *   The native-generation reading defines language vitality as requiring
 *   daily transmission by native speakers to new generations. This reading
 *   frames itself as objective linguistic science but functions as a
 *   political criterion that legitimizes nationalist language management,
 *   delegitimizes liturgical and literary transmission, and concentrates
 *   institutional authority in secular nationalist hands. The reading does
 *   not contest that languages can persist through non-generational
 *   mechanisms; it asserts these mechanisms do not constitute vitality and
 *   therefore do not merit institutional protection or respect.
 *
 * KEY AGENTS:
 *   - secular_nationalist_movement: Sets the definition and controls language policy; collects institutional authority and resource direction.
 *   - liturgical_only_communities: Maintain languages through sacred obligation; classified as dead under this reading; identity-locked exit.
 *   - minority_language_carriers: Face institutional pressure to produce native-speaker child populations; constrained by assessment and funding apparatus.
 *   - language_revitalization_institutions: Benefit from the criterion as their mandate and legitimacy measure.
 *   - academic_linguistics_community: Produce the empirical and theoretical knowledge by which the criterion is evaluated.
 *   - religious_authority_structures: Excluded from policy deliberation; their transmission authority is undermined by the criterion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.68).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.72).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native Generation Criterion for Living Language Status").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/political/identity").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, '1b6e2642-4f4b-4d99-b54f-bc27839ba59a').
narrative_ontology:cs_kernel_codification('1b6e2642-4f4b-4d99-b54f-bc27839ba59a', distributed).
narrative_ontology:cs_authority_grounding('1b6e2642-4f4b-4d99-b54f-bc27839ba59a', extraction).
narrative_ontology:cs_reading_relation('1b6e2642-4f4b-4d99-b54f-bc27839ba59a', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('1b6e2642-4f4b-4d99-b54f-bc27839ba59a', living_language_status__literary_continuity_reading, influences).
narrative_ontology:cs_axiom('1b6e2642-4f4b-4d99-b54f-bc27839ba59a', foundational, native_generation_transmission_necessary).
narrative_ontology:cs_axiom_status(native_generation_transmission_necessary, holdable).
narrative_ontology:cs_axiom_grounding('1b6e2642-4f4b-4d99-b54f-bc27839ba59a', native_generation_transmission_necessary, empirically_contingent).
narrative_ontology:cs_axiom('1b6e2642-4f4b-4d99-b54f-bc27839ba59a', foundational, non_generational_transmission_insufficient_vitality).
narrative_ontology:cs_axiom_status(non_generational_transmission_insufficient_vitality, holdable).
narrative_ontology:cs_axiom_grounding('1b6e2642-4f4b-4d99-b54f-bc27839ba59a', non_generational_transmission_insufficient_vitality, conventional).
narrative_ontology:cs_reference_frame('1b6e2642-4f4b-4d99-b54f-bc27839ba59a', language_as_living_through_generational_native_transmission).
narrative_ontology:cs_drift_state('1b6e2642-4f4b-4d99-b54f-bc27839ba59a', contemporary_policy_entrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1b6e2642-4f4b-4d99-b54f-bc27839ba59a', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movement).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, minority_language_carriers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.52 to 0.68 over the interval as the native-generation criterion becomes institutionalized in policy, funding, and educational structures. Suppression requirement rises from 0.58 to 0.72 because enforcement depends on actively delegitimizing and constraining non-generational transmission and on pressuring minority communities to reorganize around child-speaker demographics. Theater ratio rises from 0.28 to 0.41 as the institutional apparatus emphasizes demographic measurement and revival performance while the underlying question — what constitutes legitimate language vitality — is framed as already settled. The shared time grid ensures all three metrics are authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (nationalist movement, revitalization institutions) experiences the criterion as objective science legitimizing necessary intervention; the payer seats (liturgical communities, minority language carriers) experience it as institutional force delegitimizing and constraining their transmission. From the religious authority perspective, the criterion is a usurpation of their authority to define what their language vitality means. The engine computes this seat divergence from the structural data: the beneficiary and enforcer are the same institutional agent (nationalist movement); the victims are excluded from participation in defining the criterion itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular nationalist movement is the beneficiary and agenda-setter (d near 0.0, full beneficiary): it gains institutional authority, policy direction, and legitimacy from controlling the vitality definition. Liturgical communities are victims with identity-locked exit (d near 1.0, full target): they cannot exit their religious identity, cannot argue their transmission is legitimate under the criterion's own frame, and bear the cost of delegitimization. Minority language carriers sit between (d ~0.6–0.7): they benefit from revitalization resources if they meet the criterion, but are pressured to reorganize their transmission and face suppression if they resist the native-speaker demographic requirement. Religious authority structures are excluded (not seated in derivation) but would compute as targets if seated: they cannot participate in defining vitality and lose authority over their own languages.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — language extinction through demographic assimilation — was live for many languages in the 20th century and remains live for some. The native-generation criterion solves a real coordination problem: where should limited revitalization resources flow? However, the criterion conflates two distinct questions: (1) Which languages are at demographic risk? (2) What counts as legitimate language vitality? The second question is not a matter of objective linguistic fact; it is a political and epistemological choice about who gets authority to define vitality. The criterion's persistence depends on treating question 2 as already answered by question 1 — a mandatrophy signature: the institutional apparatus continues defending the criterion as if the founding problem remains unsolved, even as research and community practice demonstrate viable alternatives. The rising theater ratio (0.28→0.41) reflects this: more institutional activity goes toward defending the definition (demonstrating child-speaker demographics, criticizing alternatives) than toward the original problem (protecting languages facing genuine extinction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_objectivity,
    'Is the native-generation criterion an objective measure of language vitality grounded in linguistic science, or is it a political choice about what forms of transmission count as legitimate?',
    'Comparative institutional analysis: examine which languages are classified as living/dead under each criterion and by whom; trace the genealogy of the native-generation criterion to its nationalist political origins; assess whether linguistic function and community judgment diverge from the criterion''s classifications.',
    'If objective: the criterion is valid as applied; if political: the constraint is a disguised extraction mechanism using scientific framing to delegitimize non-nationalist transmission. Classification shifts from coordination to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_criterion_objectivity, conceptual, 'Whether the native-generation criterion is linguistic fact or nationalist politics.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) of non-generational transmission primarily structural (institutional barriers: policy pressure, funding gatekeeping, assessment regimes) or internalized (communities themselves adopt the frame that their transmission is insufficient)?',
    'Ethnographic research on communities maintaining non-generational transmission; analysis of community discourse before and after policy exposure to the criterion; tracking shifts in language-vitality claims by communities themselves.',
    'If primarily structural: suppression is removable by policy change; if primarily internalized: the constraint''s effect persists even after formal institutional barriers are removed — the community carries the suppression forward. If both: the strength of internalization determines the fixing cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in non-generational language transmission.').

omega_variable(
    coordination_extraction_boundary,
    'Is the native-generation criterion providing genuine coordination value (solving the problem of where to direct scarce revitalization resources) that requires delegitimizing non-generational transmission, or is the delegitimization the extraction mechanism independent of any coordination function?',
    'Historical counterfactual: would resource-allocation coordination be possible under an alternative criterion (e.g., demographic risk + community judgment)? Do jurisdictions using alternative vitality criteria achieve equivalent resource coordination without suppression of alternatives?',
    'If delegitimization is necessary to coordination: the constraint is tangled_rope (both coordination and extraction are required); if delegitimization is separable from coordination: the constraint is snare (extraction riding on a thinner coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether suppression of alternative vitality criteria is necessary to coordinate resource allocation or is independent extraction.').

omega_variable(
    contested_kernel_reading_relationship,
    'What is the precise structural relationship between this reading (native_generation) and its sibling readings (liturgical_preservation, literary_continuity)? Do they foreclose each other, coexist in different communities, or do they influence each other''s operating conditions?',
    'Institutional ethnography: examine whether parties holding one reading logically foreclose the others or simply disagree; analyze whether one reading''s dominance creates structural pressure on the others (e.g., by controlling funding, policy legitimacy, academic publication); assess whether all three can coexist in a single framework.',
    'The reading_relations in cs_structure are authored as hypotheses; this omega documents the empirical question that determines their validity. The relation classifications (forecloses/coexists_with/influences) are structural commitments about the kernel''s geometry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_kernel_reading_relationship, conceptual, 'The structural logic of the contested kernel: do readings foreclose, coexist, or influence each other?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(livi_tr_t8, living_language_status__native_generation_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(livi_tr_t16, living_language_status__native_generation_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(livi_tr_t24, living_language_status__native_generation_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(livi_tr_t32, living_language_status__native_generation_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(livi_tr_t40, living_language_status__native_generation_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(livi_be_t8, living_language_status__native_generation_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(livi_be_t16, living_language_status__native_generation_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(livi_be_t24, living_language_status__native_generation_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(livi_be_t32, living_language_status__native_generation_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(livi_be_t40, living_language_status__native_generation_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(livi_su_t8, living_language_status__native_generation_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(livi_su_t16, living_language_status__native_generation_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(livi_su_t24, living_language_status__native_generation_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(livi_su_t32, living_language_status__native_generation_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(livi_su_t40, living_language_status__native_generation_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__native_generation_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The living_language_status kernel decomposes into three readings with structurally distinct beneficiary/victim structures and extraction profiles. All three readings claim to define what makes a language 'living'; their ε values differ substantially because they ground vitality in different authorities and legitimacy claims. This reading (native_generation) captures policy dominance in most nation-states; the sibling readings (liturgical_preservation, literary_continuity) offer alternative vitality frameworks organized around religious transmission and intellectual productivity respectively. Each reading is an ε-invariant constraint with its own stakeholders and enforcement apparatus. They are linked via network.affects_constraints because policy dominance of the native_generation reading creates structural pressure on the others — it shapes resource allocation, institutional legitimacy, and scholarly attention, influencing the operating conditions of alternative vitality claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__native_generation_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
