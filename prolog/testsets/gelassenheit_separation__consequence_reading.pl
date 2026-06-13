% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Gelassenheit Separation: Consequence-Preserving Community Reading
 *   domain: religious/social/technological
 *
 * SUMMARY:
 *   The Gelassenheit separation rule in Mennonite and Amish communities
 *   exists to preserve community cohesion and mutual aid through shared
 *   evaluation of technology. This constraint story represents the
 *   consequence-reading: separation means evaluating each technology by its
 *   effect on visiting patterns, mutual aid networks, and geographic
 *   rootedness. Under this reading, a home telephone is forbidden (it erodes
 *   visiting and pulls attention inward), but a barn telephone is permitted
 *   (it preserves farm-centered coordination); a tractor is forbidden for
 *   road travel (it enables individualistic mobility) but permitted for belt
 *   power (it strengthens local cooperatives). The rule is presented as a
 *   collective deliberation about preserving community practice. The engine
 *   will compute how this reading's structured beneficiaries (elder
 *   authorities, geographic rootedness practitioners) and victims (youth,
 *   efficiency-seekers) experience divergent extraction. The story is one
 *   reading of a contested kernel; the principle and artifact readings are
 *   separate constraint stories (not authored here) that would show different
 *   epsilon values and different victim structures because they adjudicate
 *   the same technology questions using different evaluative frames.
 *
 * KEY AGENTS:
 *   - Community cohesion practitioners (deacons, ministers): preserve the frame that technology should be evaluated by consequence; benefit from authority to teach this reading
 *   - Geographic rootedness maintainers (farming households): benefit from rules that protect local economic networks
 *   - Efficiency-seeking households: bear the cost of fine-grained distinctions (barn phone OK, home phone forbidden; selective machinery) and navigating the permission landscape
 *   - Technologically marginalizing youth: powerless, identity-locked, and face permanent constraint on social coordination technology; bear suppression through enforcement against home communication
 *   - Principle and artifact reading adherents: excluded from the debate; their theological positions remain live but institutionally marginalized
 *   - Community interpreters (ministers, deacons): agenda-setters who adjudicate specific cases and make the rule operational
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.38).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.41).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Gelassenheit Separation: Consequence-Preserving Community Reading").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/social/technological").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, '29c47b1a-8402-4c20-84ce-867f18f2ee92').
narrative_ontology:cs_kernel_codification('29c47b1a-8402-4c20-84ce-867f18f2ee92', distributed).
narrative_ontology:cs_authority_grounding('29c47b1a-8402-4c20-84ce-867f18f2ee92', lineage).
narrative_ontology:cs_interpretation_layer_present('29c47b1a-8402-4c20-84ce-867f18f2ee92').
narrative_ontology:cs_reading_relation('29c47b1a-8402-4c20-84ce-867f18f2ee92', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('29c47b1a-8402-4c20-84ce-867f18f2ee92', gelassenheit_separation__artifact_reading, influences).
narrative_ontology:cs_axiom('29c47b1a-8402-4c20-84ce-867f18f2ee92', foundational, technology_evaluated_by_social_consequence).
narrative_ontology:cs_axiom_status(technology_evaluated_by_social_consequence, holdable).
narrative_ontology:cs_axiom_grounding('29c47b1a-8402-4c20-84ce-867f18f2ee92', technology_evaluated_by_social_consequence, instrumental).
narrative_ontology:cs_axiom('29c47b1a-8402-4c20-84ce-867f18f2ee92', secondary, community_cohesion_preservation_trumps_individual_efficiency).
narrative_ontology:cs_axiom_status(community_cohesion_preservation_trumps_individual_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('29c47b1a-8402-4c20-84ce-867f18f2ee92', community_cohesion_preservation_trumps_individual_efficiency, deontological).
narrative_ontology:cs_reference_frame('29c47b1a-8402-4c20-84ce-867f18f2ee92', community_centered_technology_governance).
narrative_ontology:cs_drift_state('29c47b1a-8402-4c20-84ce-867f18f2ee92', contemporary_digital_communication_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('29c47b1a-8402-4c20-84ce-867f18f2ee92', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, community_cohesion_practitioners).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, geographic_rootedness_maintainers).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, efficiency_seeking_households).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, technologically_marginalizing_youth).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.38) is moderate because the rule genuinely coordinates shared community practice (addressing the real founding problem of industrialization's threat to cohesion) but also consolidates authority in interpreters and transfers decision-making power from households to community consensus. The suppression (0.41) is moderate-low because the rule operates primarily through social pressure and identity-based compliance rather than explicit coercion (though youth face real suppression). The theater_ratio (0.18) is low because the rule's practical function (preserving visiting and mutual aid) is what the measurements track; the rule genuinely does limit technology access, not merely gesture at it. The accessibility_collapse (0.72) is high because once the consequence-reading frame is accepted, alternatives (principle-based, artifact-based, or unregulated technology adoption) are structurally closed off — there is no 'outside' to the rule within the community. The resistance (0.52) is moderate because efficiency-seekers and youth resist the fine-grained constraints, and the principle/artifact reading communities contest the consequence-reading's dominance, but the rule persists because elder authorities have the power to enforce it. The measurement series shows mild extraction creep from t=0 to t=30 (rising extractiveness and suppression_requirement), then stabilization, suggesting the rule's enforcement machinery matured and then settled into a stable operational intensity. The theater ratio rises slightly before stabilizing, indicating some increase in justification activity relative to enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between the community interpreter seats and the efficiency-seeking/youth seats. From the interpreter position, the consequence-reading is a genuine collaborative deliberation about community values; the fine-grained contextual rules are examples of shared judgment. From the efficiency-seeking and youth positions, the same rule is an asymmetric transfer of authority from households to interpreters and a constraint on their social coordination — the fine-grained distinctions are opportunities for the rule to expand (moving the boundary, forbidding more) rather than examples of flexibility. The engine computes this gap from the structural data: interpreters are organized agenda-setters with generational time horizons and identity-locked exit; youth are powerless with biographical horizons and identity-locked exit, creating a directionality divergence despite both having identity-locked exit. The gap emerges from power asymmetry and time-horizon divergence, not from measurement disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Community cohesion practitioners and geographic rootedness maintainers are the declared beneficiaries: they collect from the rule because it legitimizes their way of life, places authority in their hands, and protects the embedded local economy they depend on. Their directionality is low (~0.25-0.35, near beneficiary end) because they benefit without bearing the rule's costs. Efficiency-seeking households pay through constrained modernization; their directionality is high (~0.60-0.70, toward target end) because they bear the extraction without setting the rule. Youth are powerless payers with identity-locked exit (cannot leave the community without total rupture); their directionality is highest (~0.75-0.85, near full target) because they bear both the cost and the suppression, and they have no meaningful exit. The principle and artifact reading adherents are also targets (excluded from the framing, their theology marginalized); their directionality is moderate (~0.55-0.65) because they have some community voice (as organized actors) but are institutionally dominated.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a Tangled Rope, not a Rope, because it requires active enforcement (community discipline, family pressure, interpreter adjudication) and because it carries asymmetric extraction: beneficiaries (elders, rootedness practitioners) collect authority and legitimacy; payers (youth, efficiency-seekers) lose autonomy and economic modernization. The rule cannot be classified as a Rope (pure coordination with aligned incentives) because the incentive structures diverge: elders want to preserve the rule to maintain their authority and the community structure they were socialized into; youth want to modify or escape the rule because it limits their social coordination and economic options. The rule cannot be Snare (pure extraction with suppression and coercion as the only mechanism) because the rule does solve a real coordination problem (preserving community cohesion in the face of industrialization's threat) and many participants genuinely value the practice it protects. The presence of coordination (visiting, mutual aid) alongside extraction (transfer of authority, suppression of youth modernization) is the defining feature of Tangled Rope. The rule's persistence depends on enforcing the boundary between community-preserving technology (barn phone, belt-powered tractor) and individualistic technology (home phone, road-capable tractor), which requires active interpretation, adjudication, and social discipline. Without active enforcement, youth and efficiency-seekers would rapidly modernize and the geographic rootedness economy would mechanize or dissolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consequence_vs_principle_boundary,
    'Is the functional/consequentialist test (does this technology preserve visiting and mutual aid?) genuinely distinct from the principle-based test (is this technology structurally isolated from entanglement?), or do they collapse into the same judgment in practice?',
    'Ethnographic documentation of actual adjudication cases: when interpreters rule on contested technology use, do they apply explicit consequence-based reasoning (measuring visiting impact, mutual aid effects) or do they reason through principles that happen to justify the same decisions?',
    'If the readings collapse into the same practical judgment, the consequence_reading may be a post-hoc framing of principle-based rules rather than a genuinely distinct approach. This would reclassify some of the measured extraction as theater (justification for decisions made on other grounds). If they genuinely diverge, the extraction is functional asymmetry between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consequence_vs_principle_boundary, empirical, 'Whether the consequence-reading''s evaluative framework is functionally distinct from principle-based approaches.').

omega_variable(
    authority_consolidation_vs_distributed_judgment,
    'Is the consequence-reading''s fine-grained contextual evaluation (permitting barn phones but forbidding home phones, permitting tractors for belt power only) actually a distributed community judgment, or has it consolidated authority in interpreters (ministers/deacons) whose discretion is the real enforcement mechanism?',
    'Documentation of decision-making in specific cases: How much space is there for household-level judgment? How much does community consensus actually participate in the rule-making, versus interpreters deciding and announcing?',
    'High consolidation = the extraction is partly the transfer of authority from households to interpreters, which makes the constraint more extractive than the consequence-reading''s collaborative framing suggests. Low consolidation = the extraction is genuinely symmetric (all parties apply a shared framework to new cases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_consolidation_vs_distributed_judgment, empirical, 'Whether the consequence-reading distributes authority or consolidates it in religious interpreters.').

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (rapid industrialization dissolving community cohesion) still live, or has it been substantially solved decades ago, and does the rule now persist primarily as identity maintenance and authority justification rather than as a response to an active threat?',
    'Historical and sociological analysis comparing community cohesion in separatist vs. non-separatist Mennonite communities from 1950-present. Do the communities with strict separation rules actually show better cohesion retention, or are cohesion patterns driven by other factors (size, geography, internal migration)?',
    'If the founding problem is dead, the constraint is a Piton rather than a Tangled Rope — it persists by inertia and by the authority structure''s interest in maintaining it, not because it solves an active problem. This would lower the measured extraction (if the founding problem is solving nothing, the extraction is pure); alternatively, it would reframe the extraction as theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the industrialization threat the rule was built to address is still live or historically resolved.').

omega_variable(
    youth_exit_as_suppression_mechanism,
    'The constraint''s enforcement against youth (forbidding home phones, limiting technology access) operates by making the rule''s violation lead to social rupture or family estrangement. Is this suppression structural (external barriers to coordination) or internalized (youth have absorbed the value of separation and choose to comply even if barriers were removed)?',
    'Post-exit trajectories: do youth who leave the community and gain access to technology use it extensively, or do they carry internal restrictions that persist after the external enforcement is gone? Do youth who return from technological exposure face genuine social pressure to abandon it, or do they face a barrier-free choice to re-adopt the rule?',
    'If suppression is primarily internalized, the constraint''s effective suppression is higher than the base metric suggests — the target carries the constraint with them after exit. If structural, the suppression is limited to those inside the rule-enforcing community. Internalization also affects the exit_options classification: identity_locked may actually be describing internalized suppression rather than structural lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(youth_exit_as_suppression_mechanism, empirical, 'Whether suppression is structural or internalized in the youth-facing enforcement.').

omega_variable(
    reading_commission_frameshift,
    'This constraint is one reading of the gelassenheit_separation kernel. The three readings (consequence, principle, artifact) are present in the community simultaneously, with the consequence-reading institutionally dominant. How much of the measured extraction is driven by the consequence-reading''s particular frame (functional/consequentialist) versus how much is driven by any enforcement of separation regardless of reading?',
    'Comparative analysis across the three readings: measure extraction under each reading independently (via separate constraint stories per sibling). If all three readings show similar extraction profiles, the extraction is not reading-specific. If the consequence-reading is uniquely extractive, the reading-specific framing is the source.',
    'If the extraction is reading-specific, remedies could include legitimizing the principle or artifact readings as equally valid, which would shift authority and reduce extraction. If the extraction is generic to separation-enforcement regardless of reading, the reading choice does not materially affect the outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_commission_frameshift, conceptual, 'Whether the extraction is inherent to the consequence-reading''s frame or generic to separation-enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__consequence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__consequence_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__consequence_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__consequence_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__consequence_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(gela_tr_t50, gelassenheit_separation__consequence_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(gela_tr_t60, gelassenheit_separation__consequence_reading, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__consequence_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__consequence_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__consequence_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__consequence_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__consequence_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(gela_be_t50, gelassenheit_separation__consequence_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(gela_be_t60, gelassenheit_separation__consequence_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__consequence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__consequence_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__consequence_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__consequence_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__consequence_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(gela_su_t50, gelassenheit_separation__consequence_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement(gela_su_t60, gelassenheit_separation__consequence_reading, suppression_requirement, 60, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__consequence_reading, 0.12).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (consequence_reading) of the contested gelassenheit_separation kernel. The kernel itself is a stabilized commitment (a theological commitment to Separation in the context of rapid technological change) that different parties read differently. The three readings (consequence, principle, artifact) form a constraint family linked by their shared kernel and by the fact that institutional dominance of one reading constrains or enables the others. Each reading generates a different constraint story with different epsilon values, different beneficiary/victim structures, and potentially different classifications. The network.affects_constraints array links this consequence-reading story to the sibling stories so that contamination propagation analysis can track how a change in one reading's institutional status might affect the others. Each story's cs_structure.reading_relations array specifies the logical relationship between this reading and its siblings (foreclosure, coexistence, or influence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__consequence_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
