% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati as Localized Practice Norm
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the localized_practice_reading of the
 *   contested jati_practice_norm kernel. In this reading, jati boundaries are
 *   not fixed by scripture nor stabilized by colonial administration, but are
 *   continuously renegotiated coordination norms that proliferate locally.
 *   With 3000+ empirically attested categories and weak centralized
 *   enforcement, the structure functions primarily as social infrastructure
 *   for marriage, occupation, and mutual aid. It is claimed as rope:
 *   coordination dominates, extraction is incidental, and enforcement is
 *   diffuse rather than coercive.
 *
 * KEY AGENTS:
 *   - Jati community households (beneficiary, moderate power, constrained exit) â gain coordination surplus from marriage and occupational networks.
 *   - Village jati councils (agenda_setter/beneficiary, organized power, mobile exit) â administer local norms without significant extraction.
 *   - Textual orthodox interpreters (excluded, organized power) â excluded because the reading marginalizes scriptural fixity.
 *   - Colonial administrative inheritors (excluded, institutional power) â excluded because proliferation resists categorical governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.16).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.2).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.09).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.09).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati as Localized Practice Norm").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social_anthropology/religious_studies/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, 'a65656dc-d562-4904-b5a8-c928840c2c1b').
narrative_ontology:cs_kernel_codification('a65656dc-d562-4904-b5a8-c928840c2c1b', distributed).
narrative_ontology:cs_authority_grounding('a65656dc-d562-4904-b5a8-c928840c2c1b', practice).
narrative_ontology:cs_interpretation_layer_present('a65656dc-d562-4904-b5a8-c928840c2c1b').
narrative_ontology:cs_reading_relation('a65656dc-d562-4904-b5a8-c928840c2c1b', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('a65656dc-d562-4904-b5a8-c928840c2c1b', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('a65656dc-d562-4904-b5a8-c928840c2c1b', foundational, local_practice_authority_over_text).
narrative_ontology:cs_axiom_status(local_practice_authority_over_text, holdable).
narrative_ontology:cs_axiom_grounding('a65656dc-d562-4904-b5a8-c928840c2c1b', local_practice_authority_over_text, conventional).
narrative_ontology:cs_axiom('a65656dc-d562-4904-b5a8-c928840c2c1b', foundational, proliferation_indicates_coordinative_flexibility).
narrative_ontology:cs_axiom_status(proliferation_indicates_coordinative_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('a65656dc-d562-4904-b5a8-c928840c2c1b', proliferation_indicates_coordinative_flexibility, empirically_contingent).
narrative_ontology:cs_reference_frame('a65656dc-d562-4904-b5a8-c928840c2c1b', local_renegotiation_practice).
narrative_ontology:cs_drift_state('a65656dc-d562-4904-b5a8-c928840c2c1b', contemporary_proliferation_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a65656dc-d562-4904-b5a8-c928840c2c1b', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, jati_community_households).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, village_jati_councils).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in jati networks to arrange marriages, secure informal credit, and coordinate occupational niches. Jati boundaries are fuzzy and continuously renegotiated at the village level; households can partially exit via urban migration or religious conversion, though this carries social friction and loss of network access.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, jati_community_households, beneficiary,
    moderate, biographical, constrained, local).

% Mediate marital alliances, arbitrate minor disputes, and organize communal rituals. Their authority is performative and contingent on community acceptance; they lack coercive apparatus and can be ignored if they attempt material extraction. Proliferation of sub-categories occurs through their deliberations.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, village_jati_councils, agenda_setter,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, village_jati_councils, beneficiary).

% Maintain that jati boundaries must derive from fixed varna scriptural frameworks and that deviation constitutes ritual disorder. The localized practice reading treats textual authority as secondary to lived practice, excluding their hermeneutic claims from the operative norm.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, textual_orthodox_interpreters, excluded,
    organized, generational, analytical, regional).

% State bureaucrats and census authorities who require fixed, enumerable categories for governance legibility. The empirical proliferation of jati categories to 3000+ forms actively resists their administrative rationalization and is invisible within the local renegotiation frame.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, colonial_administrative_inheritors, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__localized_practice_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates marriage alliances, occupational specialization, and mutual aid within localized social networks where formal state institutions and market mechanisms are thin or unreliable.
% TRANSFER_FUNCTION: Transfers social trust, marriage access, and occupational knowledge among households within negotiable boundaries; material extraction is minimal and incidental.
% ABSENT_VOICES: Textual orthodox interpreters who insist on varna-scriptural fixity, and colonial or postcolonial administrators seeking categorical stability for governance, are structurally excluded from the local renegotiation frame.
% DISAPPEARANCE_RATIONALE: If localized jati coordination vanished, households would face higher search costs in marriage markets and informal credit, while occupational trust networks would fragment; formal state and market institutions would partially substitute but not immediately cover the coordination deficit.
% FOUNDING_PROBLEM: How to coordinate marriage, occupation, and mutual aid in agrarian and semi-urban economies with limited state capacity and thin market institutions.
% FOUNDING_PROBLEM_CORROBORATION: Academic anthropologists and historians attesting from outside the benefiting parties that jati networks continue to serve as functional social infrastructure for coordination and risk-pooling in many localities.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.16, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.16) because no centralized actor captures rents; the constraint distributes coordination surplus among participants. Suppression is low (0.20) because proliferation and renegotiation indicate weak capacity for centralized enforcement. Theater ratio is minimal (0.09) because local practice is functional rather than performative. Accessibility collapse is moderate-low (0.32): formal alternatives (state marriage bureaus, banks) exist but are less accessible in the locales where this constraint operates. Resistance is low (0.25): modernizing and egalitarian movements contest jati boundaries, but the localized practice reading absorbs much of this pressure into continuous renegotiation rather than frontal opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of local households and village councils, the constraint is experienced as benign coordination infrastructure. From the excluded textual-orthodox and colonial-administrative seats, the same proliferation appears as illegible disorder or failure of categorical governance. The engine computes this divergence from structural data: the former have beneficiary roles and constrained but non-trapped exit; the latter are excluded from the constraint's operation entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Jati community households and village jati councils sit near the beneficiary end of directionality because they receive the coordination surplus (marriage access, credit networks, occupational trust) that the constraint generates. There are no declared victims, so no seat sits at the full-target end for this specific constraint. Excluded stakeholders (textual orthodox, colonial administrators) are not governed by this constraint; their high d would be computed relative to their own preferred constraints, not this one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â coordinating social reproduction under weak formal institutions â remains live. The R5 genealogy interview shows corroboration from outside the beneficiary set (anthropological observers), and the disappearance verdict is world_rearranges, consistent with a functioning rope. There is no mandatrophy: the arrangement has not outlived its function, and the theater ratio is too low to suggest inertial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the localized_practice_reading of kernel jati_practice_norm; how would classification change if the colonial_census_reading or orthodox_textual_reading were adopted instead?',
    'Cross-read the compiled constraint stories for the sibling readings; the colonial_census_reading should exhibit high suppression and extractiveness due to administrative enforcement, while the orthodox_textual_reading should show doctrinal enforcement patterns.',
    'Adopting a sibling reading would shift the constraint from rope toward snare or tangled_rope because of external enforcement apparatus or scriptural coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer frame location within the jati kernel.').

omega_variable(
    proliferation_vs_enforcement,
    'Does the empirical proliferation to 3000+ jati categories indicate weak enforcement and genuine coordination flexibility, or does it represent fine-grained boundary maintenance that still polices exclusion against outliers?',
    'Micro-ethnographic measurement of boundary dispute frequency and intensity across proliferated sub-categories; compare intermarriage barriers at the sub-jati level.',
    'If proliferation is accompanied by active sub-boundary policing, effective extraction is higher than the coordination framing suggests and the constraint may compute as tangled_rope for targeted seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_vs_enforcement, empirical, 'Whether category proliferation reduces or redistributes enforcement.').

omega_variable(
    coordination_substitutability,
    'Could the coordination function of jati networks be served by state or market institutions without the jati boundary structure?',
    'Comparative analysis of regions with weak jati identity but similar socioeconomic profiles; measure marriage-market transaction costs and informal credit access.',
    'If formal substitutes function equivalently, jati persistence is inertia rather than live coordination; if not, the rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_substitutability, empirical, 'Whether jati coordination is substitutable by formal institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_loc_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jati_loc_tr_t20, jati_practice_norm__localized_practice_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(jati_loc_tr_t40, jati_practice_norm__localized_practice_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(jati_loc_tr_t60, jati_practice_norm__localized_practice_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(jati_loc_tr_t80, jati_practice_norm__localized_practice_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(jati_loc_tr_t100, jati_practice_norm__localized_practice_reading, theater_ratio, 100, 0.09).

% Extraction over time
narrative_ontology:measurement(jati_loc_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(jati_loc_be_t20, jati_practice_norm__localized_practice_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(jati_loc_be_t40, jati_practice_norm__localized_practice_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement(jati_loc_be_t60, jati_practice_norm__localized_practice_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(jati_loc_be_t80, jati_practice_norm__localized_practice_reading, base_extractiveness, 80, 0.16).
narrative_ontology:measurement(jati_loc_be_t100, jati_practice_norm__localized_practice_reading, base_extractiveness, 100, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jati_practice_norm__localized_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint is the localized_practice_reading of the jati_practice_norm kernel, contrasting with orthodox_textual_reading (fixed varna mapping) and colonial_census_reading (administrative reification). The kernel decomposes into three epsilon-invariant constraints because the natural-language label 'jati' conflates distinct structural claims with different enforcement mechanisms and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
