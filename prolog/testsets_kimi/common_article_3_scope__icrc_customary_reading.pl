% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: ICRC Customary Law Reading of Common Article 3 Scope
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the ICRC customary-law reading of the Common
 *   Article 3 scope kernel. Under this reading, the geographic and material
 *   scope of Common Article 3 is not fixed by the 1949 treaty text alone but
 *   is determined by evolving state practice and opinio juris tracked through
 *   customary international law. The kernel is contested: a state-centric
 *   reading fixes scope to intensity and organization thresholds, while an
 *   expansive human-rights reading treats Common Article 3 as a universal
 *   floor for any organized armed violence. The ICRC reading occupies a
 *   mediating position, offering a procedural coordination mechanism that
 *   permits gradual expansion without formal amendment. It is claimed as rope
 *   because its primary structural role is solving a collective-action
 *   problemâadapting international humanitarian law to new conflict
 *   typesâthrough an interpretive process rather than through coercion or
 *   extraction.
 *
 * KEY AGENTS:
 *   - states (agenda_setter/institutional/constrained): Their practice and opinio juris constitute the customary law that sets scope; they benefit from flexibility but lose fixed textual certainty.
 *   - icrc (beneficiary/institutional/constrained): Documents customary law and gains institutional authority as the primary expert tracker.
 *   - non_state_armed_groups (payer/organized/trapped): Subject to expanding obligations without voice in the law-creation process.
 *   - civilian_populations_in_conflict (excluded/powerless/trapped): Potential beneficiaries of broader scope but structurally absent from customary law formation.
 *   - international_judiciary (observer/institutional/analytical): Confirms and applies customary law, serving as analytical seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.3).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.25).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "ICRC Customary Law Reading of Common Article 3 Scope").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, '51420ad0-acbb-4b4b-8f42-2254839b255a').
narrative_ontology:cs_kernel_codification('51420ad0-acbb-4b4b-8f42-2254839b255a', distributed).
narrative_ontology:cs_authority_grounding('51420ad0-acbb-4b4b-8f42-2254839b255a', practice).
narrative_ontology:cs_interpretation_layer_present('51420ad0-acbb-4b4b-8f42-2254839b255a').
narrative_ontology:cs_reading_relation('51420ad0-acbb-4b4b-8f42-2254839b255a', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('51420ad0-acbb-4b4b-8f42-2254839b255a', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_axiom('51420ad0-acbb-4b4b-8f42-2254839b255a', foundational, customary_evolution_determines_scope).
narrative_ontology:cs_axiom_status(customary_evolution_determines_scope, holdable).
narrative_ontology:cs_axiom_grounding('51420ad0-acbb-4b4b-8f42-2254839b255a', customary_evolution_determines_scope, conventional).
narrative_ontology:cs_axiom('51420ad0-acbb-4b4b-8f42-2254839b255a', foundational, state_practice_primary_source).
narrative_ontology:cs_axiom_status(state_practice_primary_source, holdable).
narrative_ontology:cs_axiom_grounding('51420ad0-acbb-4b4b-8f42-2254839b255a', state_practice_primary_source, conventional).
narrative_ontology:cs_reference_frame('51420ad0-acbb-4b4b-8f42-2254839b255a', fluid_customary_scope).
narrative_ontology:cs_drift_state('51420ad0-acbb-4b4b-8f42-2254839b255a', contemporary_fragmented_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51420ad0-acbb-4b4b-8f42-2254839b255a', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their practice and opinio juris collectively constitute the customary law that determines Common Article 3 scope. They benefit from a flexible framework that adapts to new conflict types without requiring formal treaty amendment, but they lose the ability to fix scope permanently by the 1949 textual formulation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states, agenda_setter,
    institutional, generational, constrained, global).

% Documents, studies, and promotes understanding of customary international humanitarian law. Gains institutional authority, funding relevance, and a central role in legal discourse as the primary expert body tracking evolving state practice, without directly enforcing or administering the constraint.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc, beneficiary,
    institutional, generational, constrained, global).

% Subject to expanding Common Article 3 obligations as customary law evolves, without standing to contribute to the state practice or opinio juris that creates those obligations. Cannot opt out of international humanitarian law regardless of their views on the scope determination process.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, non_state_armed_groups, payer,
    organized, biographical, trapped, global).

% Stand to benefit from broader humanitarian protection as scope expands, but are structurally absent from the formation of state practice and opinio juris. They have no formal voice in the customary law process that determines whether the conflicts affecting them fall under Common Article 3.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, civilian_populations_in_conflict, excluded,
    powerless, immediate, trapped, global).

% Adjudicates disputes and confirms the content of customary law through judicial decisions, serving as an analytical seat that declares what the evolving practice has established without directly benefiting from or bearing the costs of the constraint's operation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_judiciary, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the international community to adapt the scope of Common Article 3 protection to evolving types of armed conflict and changing state understandings without requiring formal treaty amendment or unanimous state consent.
% TRANSFER_FUNCTION: Moves interpretive authority from static treaty text to an evolving evidentiary base of state practice and opinio juris, channeled through expert documentation and judicial confirmation.
% ABSENT_VOICES: Non-state armed groups and civilian populations in conflict zones are structurally excluded from the formation of state practice and opinio juris; they are governed by the resulting scope determinations without voice in their evolution.
% DISAPPEARANCE_RATIONALE: Without this procedural framework, Common Article 3 scope would freeze at the 1949 treaty text or require politically unattainable formal amendment; humanitarian protection gaps would widen for new conflict types, and institutional authority would revert to static textualism or state-centric threshold tests.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions could not enumerate all future armed conflict configurations; a rigid textual scope would leave victims of non-international conflicts without protection as warfare evolved beyond the drafters' anticipations.
% FOUNDING_PROBLEM_CORROBORATION: International judicial bodies including the ICJ, ICTY, and ICC corroborate that treaty text alone is insufficient and that evolving practice is required to maintain protective coverage. Restrictive states contest this, arguing the textual thresholds suffice; no non-institutional party outside the humanitarian law system independently attests that static text adequately solves the coverage problem.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.3, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.30) because the constraint is procedural: it authorizes an interpretive method rather than mandating a specific substantive scope. Suppression is low (0.25) because alternatives (textualism, treaty amendment) are not actively suppressed; they are simply more costly or politically unattainable. Theater is low (0.20): most activity around state practice documentation is functional, though some ritual attends ICRC study launches and United Nations reporting. Accessibility collapse is moderate (0.35): once the customary framework is accepted, static textual alternatives recede in legal discourse but do not disappear. Resistance is moderate (0.30): restrictive states have contested expansive customary claims in specific instances, but they generally resist within the customary framework rather than rejecting it altogether.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (states) and the beneficiary seat (ICRC) experience this constraint as a workable coordination device that preserves state consent while allowing adaptation to new conflict realities. The payer seat (non-state armed groups) experiences it as an externally imposed expansion of obligations they did not consent to and cannot exit. The excluded seat (civilian populations) would likely prefer the expansive human-rights reading, experiencing the customary reading as too slow and too dependent on state-driven processes.
 *
 * DIRECTIONALITY LOGIC:
 *   States sit near symmetric (d approximately 0.5): they generate the constraint through their own practice and benefit from its flexibility, but they also bear the cost of legal uncertainty and potential obligation expansion. The ICRC sits near the beneficiary end (d approximately 0.2): it collects institutional authority and relevance from its role as tracker without bearing the direct costs of compliance or enforcement. Non-state armed groups sit near the target end (d approximately 0.8): they are bound by the evolving obligations yet lack standing to shape the practice that creates them. Civilian populations are excluded from the directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope rather than tangled rope or snare prevents mislabeling the interpretive flexibility as extraction. There is no concentrated beneficiary capturing rents from the arrangement; the ICRC gains authority but does not financially extract from states or non-state groups. The coordination function is genuineâwithout this procedural constraint, international humanitarian law would face severe obsolescence as conflict types evolve. Mandatrophy would occur only if the customary mechanism became a vehicle for institutional overreach, for example if the ICRC asserted customary rules that lack genuine state practice. In that event, theater_ratio and extractiveness would rise and reclassification to tangled_rope would be warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_formation_authenticity,
    'Does documented opinio juris genuinely reflect state belief, or does it reflect ICRC and judicial assertion of what states ought to believe?',
    'Comparative analysis of state diplomatic archives, military manuals, and official statements against ICRC study citations to identify gaps between asserted and actual state practice.',
    'If opinio juris is largely asserted rather than evidenced, the constraint''s base_extractiveness is higher than modeled and the rope classification may shift toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_formation_authenticity, empirical, 'Authenticity of customary law formation evidence').

omega_variable(
    persistent_objector_escape,
    'Can restrictive states effectively opt out of expansive customary readings through the persistent objector doctrine, or has the procedural constraint eliminated meaningful exit?',
    'Case study of persistent objector claims in judicial and diplomatic practice since 2005; tracking whether such claims are honored or treated as invalid.',
    'If exit is structurally blocked, effective suppression is higher than the raw metric suggests and the constraint operates more coercively than a pure coordination device.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistent_objector_escape, empirical, 'Availability of persistent objector exit').

omega_variable(
    expansion_boundary,
    'Is there a principled limit to scope expansion under this reading, or does the procedural constraint tend asymptotically toward the expansive human-rights reading?',
    'Trend analysis of ICRC and judicial statements over time to identify whether any limiting principle is consistently applied or whether scope claims expand monotonically.',
    'If no boundary is detectable, the ICRC reading may be a scaffold whose coordination function dissolves into the expansive reading, or a tangled rope where coordination and extraction are inseparable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expansion_boundary, conceptual, 'Limiting principle for customary expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_icrc_tr_t0, common_article_3_scope__icrc_customary_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ca3_icrc_tr_t10, common_article_3_scope__icrc_customary_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(ca3_icrc_tr_t20, common_article_3_scope__icrc_customary_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(ca3_icrc_tr_t30, common_article_3_scope__icrc_customary_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(ca3_icrc_tr_t40, common_article_3_scope__icrc_customary_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(ca3_icrc_tr_t50, common_article_3_scope__icrc_customary_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(ca3_icrc_be_t0, common_article_3_scope__icrc_customary_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ca3_icrc_be_t10, common_article_3_scope__icrc_customary_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(ca3_icrc_be_t20, common_article_3_scope__icrc_customary_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(ca3_icrc_be_t30, common_article_3_scope__icrc_customary_reading, base_extractiveness, 30, 0.27).
narrative_ontology:measurement(ca3_icrc_be_t40, common_article_3_scope__icrc_customary_reading, base_extractiveness, 40, 0.29).
narrative_ontology:measurement(ca3_icrc_be_t50, common_article_3_scope__icrc_customary_reading, base_extractiveness, 50, 0.3).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(common_article_3_scope__icrc_customary_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
