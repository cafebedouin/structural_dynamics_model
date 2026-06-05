% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War Winnability Post-1945: Strategic Culture Drift Reading
 *   domain: international_relations_theory/strategic_studies/commitment_systems
 *
 * SUMMARY:
 *   This is ONE reading of a contested kernel: 'total war winnability
 *   post-1945.' The kernel itself is the persisting commitment to analyzing
 *   whether total war remains structurally reachable and can be analyzed as a
 *   strategic option in the postwar period. This reading, 'strategic culture
 *   drift,' traces how that commitment underwent atrophic institutional
 *   forgetting. The constraint operates through what appears to be a
 *   collective shift in intellectual culture: total war analysis moved from a
 *   serious, if contested, domain of strategic theorizing (1945-1960) into an
 *   increasingly marginalized, non-respectable, even taboo topic (1960-2020).
 *   This was NOT accomplished through explicit prohibition or policy
 *   enforcement. Rather, it occurred through institutional consolidation of a
 *   limited war consensus, credential gatekeeping in defense intellectual
 *   formation, journal editorial practices that treated total war analysis as
 *   theoretically naive or dangerously escalatory, and the simple attrition
 *   of scholars willing to work on the topic. The constraint now manifests as
 *   a Piton: the institutional machinery for serious total war analysis has
 *   degraded and persists primarily through theatrical consensus and inertial
 *   momentum rather than through any active functional purpose. The strategic
 *   flexibility to analyze worst-case scenarios has been sacrificed for
 *   coordination around limited war doctrine—a trade-off that may or may not
 *   be wise, but that has created a blind spot in strategic planning
 *   communities.
 *
 * KEY AGENTS:
 *   - Limited War Doctrine Advocates (institutional/arbitrage): Primary beneficiaries—career advancement, credentialing authority, intellectual hegemony rest on the consensus that limited war is the serious framework
 *   - Defense Intellectuals Credentialed Post-1945 (institutional/arbitrage): Primary beneficiaries—their authority and publication status depend on positioning limited war as the rational, scientific response to nuclear weapons
 *   - Strategic Flexibility / Contingency Planning Capacity (powerless/trapped): Primary victim—the abstract collective capacity to analyze extreme scenarios faces institutional suppression with no advocate and no exit mechanism
 *   - Military Planners (moderate/constrained): Secondary victim—constrained by career risk and institutional norms if they publicly challenge the limited war framework; unable to legitimately conduct comprehensive worst-case analysis
 *   - Counternarrative Communities (organized/mobile): External voices—military historians, heterodox strategists, and systems analysts outside credentialed consensus maintain alternative analyses but operate in lower-status venues
 *   - Analytical Observer (analytical/analytical): Civilizational perspective—risks naturalizing a contingent institutional consensus as an inherent strategic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.38).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.62).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.38).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War Winnability Post-1945: Strategic Culture Drift Reading").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations_theory/strategic_studies/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, 'aa65fddc-df9b-4965-9765-7ab86949bf2a').
narrative_ontology:cs_kernel_codification('aa65fddc-df9b-4965-9765-7ab86949bf2a', distributed).
narrative_ontology:cs_authority_grounding('aa65fddc-df9b-4965-9765-7ab86949bf2a', lineage).
narrative_ontology:cs_interpretation_layer_present('aa65fddc-df9b-4965-9765-7ab86949bf2a').
narrative_ontology:cs_reading_relation('aa65fddc-df9b-4965-9765-7ab86949bf2a', total_war_winnability__structural_contraction, coexists_with).
narrative_ontology:cs_reading_relation('aa65fddc-df9b-4965-9765-7ab86949bf2a', total_war_winnability__normative_rejection, coexists_with).
narrative_ontology:cs_axiom('aa65fddc-df9b-4965-9765-7ab86949bf2a', foundational, institutional_knowledge_can_attenuate).
narrative_ontology:cs_axiom_status(institutional_knowledge_can_attenuate, holdable).
narrative_ontology:cs_axiom_grounding('aa65fddc-df9b-4965-9765-7ab86949bf2a', institutional_knowledge_can_attenuate, empirically_contingent).
narrative_ontology:cs_axiom('aa65fddc-df9b-4965-9765-7ab86949bf2a', secondary, atrophy_creates_asymmetric_reactivation_cost).
narrative_ontology:cs_axiom_status(atrophy_creates_asymmetric_reactivation_cost, holdable).
narrative_ontology:cs_axiom_grounding('aa65fddc-df9b-4965-9765-7ab86949bf2a', atrophy_creates_asymmetric_reactivation_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('aa65fddc-df9b-4965-9765-7ab86949bf2a', postwar_strategic_analysis_comprehensive_scope).
narrative_ontology:cs_drift_state('aa65fddc-df9b-4965-9765-7ab86949bf2a', contemporary_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa65fddc-df9b-4965-9765-7ab86949bf2a', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_doctrine_advocates).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_credentialed_post1945).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, military_contingency_planning).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFENSE INTELLECTUALS (PITON) — The credentialed post-1945 generation (Brodie, Kissinger, Schelling) established limited war doctrine as the serious strategic framework. Total war winnability is not analyzed; it is treated as obsolete. The institutional theater (journals, think tanks, policy circles) performs consensus around limitation. The actual cognitive capacity to analyze total war scenarios has atrophied through institutional forgetting and credential gatekeeping. The constraint persists through inertia and performative consensus, not through active enforcement or functional necessity.
constraint_indexing:constraint_classification(total_war_winnability_post1945__strategic_culture_drift, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: MILITARY PLANNER (TANGLED ROPE) — Faces both coordination benefits and asymmetric extraction from the strategic culture consensus. The consensus enables interagency planning (NATO, alliances, procurement cycles). But it also forecloses official analysis of total war scenarios, creating a blind spot in contingency planning. The planner is constrained by career risk and institutional norms if they publicly challenge the limited war framework. Some extraction is real — the inability to legitimately plan for extreme scenarios — but the coordination value is also genuine.
constraint_indexing:constraint_classification(total_war_winnability_post1945__strategic_culture_drift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STRATEGIC FLEXIBILITY / CIVILIZATIONAL CAPACITY (SNARE) — The capacity to analyze, model, and contingency-plan for total war scenarios is a public good that benefits all strategic actors if preserved. But the institutional suppression of such analysis creates a collective action trap. No actor can unilaterally reverse the cultural consensus without reputational cost. The abstract capacity is deteriorating through generational knowledge loss and institutional forgetting. Maximum extraction with no voice: strategic flexibility has no advocate and cannot exit the degraded discourse environment.
constraint_indexing:constraint_classification(total_war_winnability_post1945__strategic_culture_drift, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: COUNTERNARRATIVE COMMUNITIES (ROPE) — Military historians, systems analysts, and heterodox strategists outside the credentialed consensus maintain alternative framings: total war remains structurally possible, institutional suppression of analysis creates blind spots, the limited war consensus is a contingent historical artifact, not a scientific discovery. These communities have exit options (academic freedom, niche publishing, military academies). They see the constraint as a pure coordination problem: the field needs to reopen analysis of total war scenarios. Low effective extraction because they can speak and publish, even if in lower-status venues.
constraint_indexing:constraint_classification(total_war_winnability_post1945__strategic_culture_drift, rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CIVILIZATIONAL NATURAL LAW (MOUNTAIN) — From a civilizational timescale and universal scope, the suppression of total war analysis could be viewed as an inherent feature of late-industrial warfare: the escalation risks of nuclear weapons make total war genuinely unwinnable (not merely unfashionable), and the intellectual focus on limitation is a rational adaptive response to structural constraints, not a pathological atrophy. However, the structural data contradicts this naturalization — the suppression is active and institutional, driven by credentialed consensus rather than by logical necessity.
constraint_indexing:constraint_classification(total_war_winnability_post1945__strategic_culture_drift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(total_war_winnability_post1945__strategic_culture_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(total_war_winnability_post1945__strategic_culture_drift, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, TR),
    TR >= 0.70.

:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts strategic flexibility and contingency planning capacity from the field, but the extraction is not total—counternarrative voices persist, and some military academies and think tanks maintain more comprehensive analytical traditions. The theater ratio (0.78) indicates substantial performative content: the consensus operates through what is considered serious/publishable/respectable rather than through explicit rules. Suppression (0.62): Moderate-high. Significant barriers to total war analysis include reputational cost, journal editorial bias, hiring gatekeeping, and funding restrictions on certain research topics. But suppression is not absolute—the topic is not legally forbidden, merely institutionally marginalized. Theater ratio trajectory (0.35 in 1945 → 0.78 in 2020): Shows increasing dominance of performative consensus. In 1945, total war analysis was a legitimate scholarly domain with serious debate about its feasibility post-nuclear weapons. By 2020, it has become increasingly theatrical—discussed mainly in lower-status venues, pop strategy, or heterodox circles. The decline in base extractiveness over the interval (0.18 → 0.38) reflects that the suppression has become more effective as the institutional consensus has solidified, but the growth is constrained because the mechanism is primarily cultural rather than coercive.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between institutional (Piton/Rope) and analytical (Mountain) perspectives is the core diagnostic. The defense intellectual class sees the constraint as legitimate intellectual progress: limited war doctrine is the rational adaptation to nuclear weapons, and total war analysis is dangerously naive. From inside this perspective, the suppression is justified and not experienced as extraction. The military planner sees a mixed burden: the coordination benefits of a shared strategic framework are real, but so is the loss of analytical flexibility. The abstract capacity for strategic thinking sees pure extraction: institutional suppression without voice or exit. The counternarrative communities see a coordination failure: the field would benefit from reopening total war analysis, but institutional gatekeeping prevents it. The analytical observer risks naturalizing the consensus as an inherent feature of civilizational strategic wisdom, failing to see that it is a contingent institutional achievement vulnerable to knowledge loss and credential gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives. Defense intellectuals benefit from the constraint (low d, negative chi) because their institutional authority and career trajectories rest on the limited war consensus. Military planners face mixed barriers—they gain coordination benefits but lose planning flexibility (moderate d, moderate chi). Strategic flexibility has no beneficiary and no exit (high d, high chi from its perspective, though it cannot speak). Counternarrative communities have the ability to publish and teach outside the consensus (low d due to arbitrage exit options despite being victims of the broader suppression). The strategic culture consensus suppresses some voices more effectively than others depending on their institutional position. The piton classification reflects that the constraint persists through inertia and theater rather than through active enforcement mechanisms that would apply uniformly across agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint resolves through understanding that the limited war doctrine represents a genuine institutional commitment (with real coordination value) that has degraded into inertia. At t0 (1945), the commitment was actively reasoned: strategists debated whether total war was still analytically viable given nuclear weapons, and many concluded that limited war analysis was the more productive framework. This debate was genuine intellectual work. At t1 (2020), the same consensus persists but has become performative: it is maintained through credential gatekeeping and theatrical consensus rather than through active intellectual engagement with the original reasoning. The Piton classification captures this: the constraint performs the function it claims (coordination around a strategic framework) but has lost the active cognitive work that once justified it. A Rope classification would suggest active, rationally maintained consensus. A Snare classification would suggest pure extraction with no coordination value. The Piton captures the intermediate state: was coordination, now mostly theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_culture_deliberate_vs_atrophic,
    'Is the post-1945 total war winnability discourse shift a deliberate institutional choice (limited war doctrine adopted consciously as normatively superior) or atrophic institutional forgetting (the cognitive capacity to analyze total war was simply lost)?',
    'Historical textual analysis: archival examination of 1945-1960 strategic theory debates; comparison of comprehensiveness of total war analysis in prewar vs postwar military academies and think tanks; interview data on credentialing gatekeeping in defense intellectual formation',
    'If deliberate: constraint is Rope (coordinated consensus with genuine policy rationale). If atrophic: constraint is Piton (institutional theater maintaining degraded capacity). Reading hinges on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_culture_deliberate_vs_atrophic, empirical, 'Whether strategic culture drift is deliberate institutional choice or atrophic forgetting').

omega_variable(
    total_war_winnability_structural_vs_normative,
    'Is total war actually unwinnability a structural consequence of nuclear weapons (civilizational constraint on military victory), or is it a normative consensus (a policy choice adopted by postwar strategists)?',
    'Comparative strategic analysis: modeling total war scenarios under nuclear, conventional, and pre-nuclear technological assumptions; identification of which escalation dynamics are mathematically inherent vs socially contingent',
    'If structural: the piton classification is misleading—the constraint reflects accurate adaptation to new physics. If normative: the piton classification stands—institutional consensus suppresses an option that remains technically reachable even if politically undesirable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(total_war_winnability_structural_vs_normative, conceptual, 'Whether unwinnability is structural or normative').

omega_variable(
    institutional_gatekeeping_visibility,
    'How much of the suppression of total war analysis is enforced by explicit institutional rules (editorial guidelines, funding restrictions, career penalties) vs implicit cultural norms (what is considered serious/publishable/respectable without formal prohibition)?',
    'Institutional ethnography: tracking of rejected manuscripts, denied funding proposals, and non-hiring decisions related to total war analysis; interviews with editors, program officers, and hiring committees on decision criteria',
    'If explicit enforcement: constraint is more properly Snare (active suppression mechanism). If implicit norms: constraint is Piton (theater and inertia). Current classification reflects presumed implicit-norm dominance; explicit enforcement would upgrade threat assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gatekeeping_visibility, empirical, 'Proportion of suppression enforced by explicit institutional rules vs implicit cultural norms').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a reading of the kernel ''total war winnability post-1945'' (the factual question of whether total war is structurally reachable and analyzable) or a reading of a different kernel about the legitimate domain of strategic discourse?',
    'Clarification of which commitment the strategic culture consensus is grounded in: (a) a claim about objective reality (total war is unwinnable), (b) a normative commitment (total war analysis is illegitimate/dangerous), or (c) an institutional rule (defense intellectuals do not publish on total war). The reading may span multiple kernels.',
    'Affects which sibling readings are genuine alternatives vs category errors. If (a): structural_contraction_reading is the parallel analysis-focused sibling. If (b): normative_reading_drop is the parallel normativity-focused sibling. If (c): institutional_gatekeeping is the parallel enforcement sibling. Classification assumes (a) as primary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether kernel is factual reachability, normative legitimacy, or institutional gatekeeping').

omega_variable(
    reactivation_cost_estimate,
    'What is the estimated institutional cost and timeline to reactivate comprehensive total war analysis if strategic consensus shifted? How much generational knowledge has been permanently lost?',
    'Comparative case studies of intellectual field recovery after suppression (e.g., eugenics, race science after institutional stigmatization); modeling of PhD cohort size, institutional memory, and credentialing requirements to rebuild analytical capacity',
    'High cost/long timeline supports Piton classification (institutional inertia makes recovery difficult). Low cost/short timeline suggests constraint is more fragile and potentially reclassifiable as Rope (coordination failure rather than atrophy).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reactivation_cost_estimate, empirical, 'Institutional cost and timeline to reactivate total war analysis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twsc_theater_1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.35).
narrative_ontology:measurement(twsc_theater_1965, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1965, 0.58).
narrative_ontology:measurement(twsc_theater_1990, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1990, 0.72).
narrative_ontology:measurement(twsc_theater_2020, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2020, 0.78).

% Extraction over time
narrative_ontology:measurement(twsc_extract_1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.18).
narrative_ontology:measurement(twsc_extract_1965, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(twsc_extract_1990, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(twsc_extract_2020, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2020, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, nuclear_war_escalation_dominance).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, limited_war_doctrine_operationalization).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, military_contingency_blind_spots).

% DUAL FORMULATION NOTE:
% This constraint is one reading of 'total war winnability post-1945' and is linked to sibling readings focused on structural contraction (nuclear weapons as objective constraint) and normative rejection (legitimacy shift). The strategic culture drift reading emphasizes the atrophic mechanism—how institutional consensus suppresses analysis without explicit enforcement. Each reading has its own epsilon value: the structural reading treats unwinnability as a mountain (ε ~ 0.15, inherent to nuclear physics), while the drift reading treats it as piton (ε ~ 0.38, institutional theater with fading functional justification). They are not the same constraint viewed differently; they are structurally distinct constraints linked through the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__strategic_culture_drift, analytical, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
