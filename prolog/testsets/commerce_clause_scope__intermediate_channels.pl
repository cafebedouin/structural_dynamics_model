% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Intermediate Channels Doctrine
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   The Commerce Clause grants Congress power to 'regulate Commerce . . .
 *   among the several States.' This constraint instantiates the
 *   intermediate-channels reading: federal power extends to (1) channels of
 *   interstate commerce, (2) instrumentalities and persons/things in
 *   interstate commerce, and (3) activities substantially affecting
 *   interstate commerce, but subject to limiting principles—non-economic
 *   activity requires a jurisdictional element, aggregation applies only to
 *   economic activity, and attenuation breaks the causal chain. This reading
 *   sits between the narrow originalist interpretation (commerce means
 *   interstate trade, not intrastate activity) and the broad effects-test
 *   reading (nearly all economic activity is reachable if it substantially
 *   affects interstate commerce in the aggregate). The intermediate-channels
 *   doctrine coordinates federal and state regulatory authority while
 *   claiming to preserve meaningful state police power. The measurement
 *   series track a constraint that accumulated extractiveness through case
 *   law expansion (1950s–1990s) and plateaued once the limiting principles
 *   were formally articulated, suggesting the constraint's function shifted
 *   from coordination to enforcement of a doctrinally settled but internally
 *   unstable boundary.
 *
 * KEY AGENTS:
 *   - Federal authority: sets and administers the doctrine through Article III courts and Article I legislation; benefits from expansive reach within economic sphere; pays cost of maintaining limiting-principle rhetoric.
 *   - State authority (in reserved domains): beneficiary of categorical immunity from federal commerce regulation in family law, criminal law, education; pays cost of constant litigation testing whether state law 'substantially affects' interstate commerce.
 *   - Congress: agenda-setter framing findings of fact about interstate commerce; operates within nominally limiting doctrine but shapes scope through statutory language and empirical argument.
 *   - Supreme Court: interprets and enforces limiting principles; grants deference to congressional factfinding on 'substantially affects' while policing economic/non-economic boundary.
 *   - Local regulators: nominal beneficiaries of police power carve-out but constrained by preemption doctrine whenever local regulation touches interstate commerce.
 *   - Regulated economic actors: payers bearing federal regulatory burden under the 'substantially affects' prong; cannot exit interstate commerce without ceasing business.
 *   - Legal scholars: observers identifying instability in economic/non-economic distinction and manipulability of limiting principles.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.58).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.42).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Intermediate Channels Doctrine").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, 'bf30a24d-e26f-4707-891d-2138fb9d8cc0').
narrative_ontology:cs_kernel_codification('bf30a24d-e26f-4707-891d-2138fb9d8cc0', fixed_text).
narrative_ontology:cs_authority_grounding('bf30a24d-e26f-4707-891d-2138fb9d8cc0', lineage).
narrative_ontology:cs_interpretation_layer_present('bf30a24d-e26f-4707-891d-2138fb9d8cc0').
narrative_ontology:cs_reading_relation('bf30a24d-e26f-4707-891d-2138fb9d8cc0', commerce_clause_scope__narrow_originalist, influences).
narrative_ontology:cs_reading_relation('bf30a24d-e26f-4707-891d-2138fb9d8cc0', commerce_clause_scope__broad_effects_test, influences).
narrative_ontology:cs_axiom('bf30a24d-e26f-4707-891d-2138fb9d8cc0', foundational, federal_power_limited_by_channels_instrumentalities).
narrative_ontology:cs_axiom_status(federal_power_limited_by_channels_instrumentalities, holdable).
narrative_ontology:cs_axiom_grounding('bf30a24d-e26f-4707-891d-2138fb9d8cc0', federal_power_limited_by_channels_instrumentalities, deontological).
narrative_ontology:cs_axiom('bf30a24d-e26f-4707-891d-2138fb9d8cc0', foundational, non_economic_activity_requires_jurisdictional_element).
narrative_ontology:cs_axiom_status(non_economic_activity_requires_jurisdictional_element, holdable).
narrative_ontology:cs_axiom_grounding('bf30a24d-e26f-4707-891d-2138fb9d8cc0', non_economic_activity_requires_jurisdictional_element, conventional).
narrative_ontology:cs_reference_frame('bf30a24d-e26f-4707-891d-2138fb9d8cc0', federalism_balance).
narrative_ontology:cs_drift_state('bf30a24d-e26f-4707-891d-2138fb9d8cc0', contemporary_post_lopez_morrison, gap(stable, substantial, true)).
narrative_ontology:cs_created_at('bf30a24d-e26f-4707-891d-2138fb9d8cc0', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_authority).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_authority_in_reserved_domains).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, local_non_economic_regulation).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, conceptual_coherence_of_limiting_principles).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the federal authority's capacity to regulate behavior that was historically reserved to states, constrained by the three formal limits. At 0.58 (interval end), this reflects: (1) federal power reaches most economic activity with any interstate nexus or aggregate effect (high extractiveness base); (2) three enumerated limiting principles nominally exclude whole categories (non-economic activity without channel nexus, intrastate non-economic activity, attenuated causal chains). The net is moderate extraction—broader than narrow originalism would allow, narrower than broad effects-test would permit. Suppression (0.42) is lower than extraction because the doctrine operates primarily through argument and interpretation, not coercive force—federal courts persuade through constitutional reasoning rather than enforce through police power. Theater (0.31) is moderate: the limiting principles are real doctrinal rules, not pure performance, but their application involves interpretive discretion that federal interpreters control. The measurement trajectory shows extractiveness rising from 1950 (0.42) to 1990 (0.57) as case law broadly construed 'substantially affects,' then plateauing when Lopez (1995) and Morrison (2000) articulated non-economic carve-outs and required jurisdictional elements. The plateau reflects doctrinal settlement—the constraint stabilized at the intermediate level rather than either narrowing (originalist pressure) or broadening further (effects-test trajectory). Theater increased modestly (0.25→0.31) as interpretive work shifted from fact-elaboration to boundary-maintenance, suggesting the constraint's primary function became enforcing limiting principles rather than reaching new economic activity. Suppression remained relatively low because state actors operate within the constitutional system and accept Supreme Court interpretation as legitimate, even when contentious.
 *
 * PERSPECTIVAL GAP:
 *   Federal authority perceives the constraint as balanced coordination: it allocates jurisdiction, prevents market fragmentation, and preserves state autonomy in reserved domains—a genuine rope from the federal seat. State authority perceives it as constrained extraction: formal limits are rhetorically real but applied with federal interpretive discretion, and constant litigation testing boundaries imposes costs states cannot avoid. The engine should compute divergence in directionality: federal authority sits near 0.4 (beneficiary with broad reach but limited by its own rules), while state authority sits near 0.6 (target of potential preemption but protected by categorical carve-outs). Local regulators should compute closer to 0.7 (targets of preemption when their rules touch interstate commerce). The intermediate position of this reading should produce per-seat classifications that diverge—federal observers might see rope, state observers might see tangled_rope or snare, while the story-level claim (tangled_rope) reflects the structure's true nature: genuine coordination (national uniformity in commerce) yoked to asymmetric extraction (federal interpretive authority and state cost-bearing).
 *
 * DIRECTIONALITY LOGIC:
 *   Federal authority: primary beneficiary—controls the doctrine's scope through interpretation, defines what counts as 'economic,' sets the 'substantially affects' threshold. Directionality near 0.35 (institutional beneficiary with some constraint from limiting principles). State authority: dual-positioned—beneficiary in reserved domains (family, criminal, education, non-economic local regulation), payer in domains where federal commerce power competes (economic regulation, activities with interstate effects). Directionality near 0.50 (symmetric: genuine coordination benefit + real cost-bearing from preemption threat). Congress: agenda-setter, near 0.25 (institutional beneficiary shaping the doctrine's scope through statutory language and findings). Supreme Court: agenda-setter/beneficiary near 0.30 (institutional authority controlling interpretation, limited by precedent). Local regulators: payers, near 0.65 (targets of preemption when regulations touch interstate commerce, no ability to exit). Regulated economic actors: payers, near 0.70 (trapped—cannot avoid interstate commerce without ceasing business; cannot negotiate exit). No directionality overrides are needed; the structural data (beneficiary/victim/exit) derive d values that reflect the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was preserving federalism: preventing states from fragmenting interstate commerce while maintaining meaningful state autonomy. At interval end (t=40, contemporary constitutional doctrine), the founding problem is contested. Federal authority argues the problem is live—states still impose regulations with local effects, and federal commerce power is necessary to maintain national markets. State authority argues the founding problem is dead or at least transformed—modern markets are self-integrating through private ordering and network effects; federal commerce power has expanded beyond preventing state discrimination into affirmative regulation of intrastate activity. Legal scholars identify a third position: the founding problem is dead but the constraint persists as a vehicle for federal authority (piton-adjacent, but more structured). The intermediate-channels reading resolves mandatrophy by maintaining that the constraint solves an active coordination problem (preventing regulatory fragmentation) while accepting that the limiting principles are manipulable (economic/non-economic distinction is contestable). This resolution prevents mislabeling: the constraint is not pure rope (coordination only) because the limiting principles are asymmetrically applied and favor federal interpretation; it is not pure snare (extraction only) because coordination is genuinely necessary and state police power carve-outs are functionally real in most cases. The tangled_rope classification captures: (1) genuine coordination function—uniform national rules for interstate commerce; (2) asymmetric extraction—federal interpretive authority and state cost-bearing from litigation risk; (3) active enforcement—the Supreme Court continually polices boundary lines, and Congress continually tests the perimeter through statute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_boundary_stability,
    'Is the distinction between ''economic activity'' (reachable under the substantially-affects prong) and ''non-economic activity'' (excluded unless channel/instrumentality nexus exists) stable across contexts, or is it historically contingent and subject to redefinition?',
    'Historical analysis of how ''economic'' has been redefined (e.g., family law as economic in some modern readings, criminal procedure as non-economic in Lopez/Morrison). Comparative analysis across nations with similar federalism structures to determine whether the boundary is universal or jurisdiction-specific.',
    'If stable, the limiting principle is real and preserves state police power. If contingent, the boundary is manipulable and federal authority can gradually redraw it—the constraint''s effectiveness as a limit depends on how narrowly courts define ''economic.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_boundary_stability, conceptual, 'Whether the economic/non-economic distinction is an objective boundary or a contestable classification.').

omega_variable(
    substantially_affects_factfinding_deference,
    'When Congress makes findings that an intrastate activity ''substantially affects'' interstate commerce, does the Supreme Court genuinely defer to congressional judgment, or does it apply heightened scrutiny that effectively rejects congressional factfinding when the Court disagrees with the doctrine''s breadth?',
    'Empirical analysis of Supreme Court doctrine pre- and post-Lopez/Morrison: count cases in which the Court deferred to congressional findings vs. rejected them; measure the intensity of scrutiny applied to factfinding; analyze the text of opinions for language indicating deference or skepticism.',
    'If genuine deference, the doctrine is closer to rope (Congress coordinates the reach, Court enforces limiting principles). If heightened scrutiny, the doctrine is closer to snare (the Court retains ultimate interpretive authority and Congress cannot expand federal reach beyond what the Court pre-approves).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantially_affects_factfinding_deference, empirical, 'Whether Congressional factfinding on ''substantially affects'' is genuinely respected or subject to judicial override.').

omega_variable(
    attenuated_causal_chain_principle_application,
    'How attenuated can the causal chain between an intrastate activity and interstate commerce effects be before federal regulation is foreclosed? The doctrine names ''attenuated causal chains'' as a limiting principle but provides no metric for attenuation.',
    'Case-law analysis identifying the chain lengths the Supreme Court has accepted (e.g., local gun violence → interstate commerce through some chain) vs. rejected (Brzonkala in Morrison). Determine whether the attenuation principle operates as a genuine limiting rule or serves as post-hoc rationalization for decisions made on other grounds.',
    'If attenuation is applied consistently, it is a real limit. If applied inconsistently or as rationalization, it is performative—theater rather than substance. If theater is high, the constraint is closer to piton (limits are asserted but not enforced).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attenuated_causal_chain_principle_application, empirical, 'Whether the ''attenuated causal chain'' limiting principle is applied with consistent standards or serves as post-hoc justification.').

omega_variable(
    federalism_reading_contest,
    'Does the intermediate-channels reading genuinely represent a mid-point between narrow originalism and broad effects-test, or does it function as a rhetorical frame that resolves in favor of one pole depending on doctrinal pressure?',
    'Measure the trajectory of limiting-principle enforcement across different Supreme Court eras: when federal authority pressure is high (1960s–1990s), how strictly are the limits enforced? When federalism revival pressure is high (post-2000), how strictly? If the limits strengthen or weaken with political pressure, the reading is not genuinely intermediate—it is a vehicle for whoever controls the Court.',
    'If the reading is genuinely intermediate and limits are consistently enforced, the constraint is tangled_rope (real coordination + real limits, even if asymmetric). If the limits are fluid and serve whoever controls the Court, the constraint is closer to snare (federal authority uses limiting-principle rhetoric as cover while maintaining effective power to override when desired).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_reading_contest, empirical, 'Whether intermediate-channels doctrine represents a stable mid-point or a rhetorical frame that resolves toward federal authority when political pressure allows.').

omega_variable(
    committer_frame_sibling_readings,
    'Do the three readings (narrow_originalist, intermediate_channels, broad_effects_test) represent structurally distinct constraints with different ε values and beneficiary/victim structures, or are they interpretive positions within a single constraint that looks different from different seats?',
    'Structural analysis: compute ε for each reading independently. Narrow reading should show low ε (federal power restricted, state autonomy preserved). Broad reading should show high ε (federal power expansive, state autonomy eroded). Intermediate reading should show moderate ε (this story). If ε values diverge significantly, they are separate constraints and should be three separate JSON stories. If they converge, they are readings of one constraint with per-seat divergence.',
    'This omega documents the uncertainty in the constraint''s decomposition itself: are we authoring one constraint (intermediate_channels) as one reading of a contested kernel, or are we authoring three separate constraints that coexist in the constitutional system?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_sibling_readings, conceptual, 'Whether the three readings are separate constraints or interpretive positions within one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__intermediate_channels, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comm_tr_t5, commerce_clause_scope__intermediate_channels, theater_ratio, 5, 0.27).
narrative_ontology:measurement(comm_tr_t10, commerce_clause_scope__intermediate_channels, theater_ratio, 10, 0.28).
narrative_ontology:measurement(comm_tr_t15, commerce_clause_scope__intermediate_channels, theater_ratio, 15, 0.29).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_scope__intermediate_channels, theater_ratio, 20, 0.3).
narrative_ontology:measurement(comm_tr_t25, commerce_clause_scope__intermediate_channels, theater_ratio, 25, 0.31).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_scope__intermediate_channels, theater_ratio, 30, 0.31).
narrative_ontology:measurement(comm_tr_t35, commerce_clause_scope__intermediate_channels, theater_ratio, 35, 0.31).
narrative_ontology:measurement(comm_tr_t40, commerce_clause_scope__intermediate_channels, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__intermediate_channels, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comm_be_t5, commerce_clause_scope__intermediate_channels, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(comm_be_t10, commerce_clause_scope__intermediate_channels, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(comm_be_t15, commerce_clause_scope__intermediate_channels, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(comm_be_t20, commerce_clause_scope__intermediate_channels, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(comm_be_t25, commerce_clause_scope__intermediate_channels, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(comm_be_t30, commerce_clause_scope__intermediate_channels, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(comm_be_t35, commerce_clause_scope__intermediate_channels, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(comm_be_t40, commerce_clause_scope__intermediate_channels, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__intermediate_channels, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comm_su_t5, commerce_clause_scope__intermediate_channels, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(comm_su_t10, commerce_clause_scope__intermediate_channels, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(comm_su_t15, commerce_clause_scope__intermediate_channels, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(comm_su_t20, commerce_clause_scope__intermediate_channels, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(comm_su_t25, commerce_clause_scope__intermediate_channels, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(comm_su_t30, commerce_clause_scope__intermediate_channels, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(comm_su_t35, commerce_clause_scope__intermediate_channels, suppression_requirement, 35, 0.42).
narrative_ontology:measurement(comm_su_t40, commerce_clause_scope__intermediate_channels, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__intermediate_channels, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, dormant_commerce_clause_state_discrimination).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, congressional_preemption_doctrine).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, state_police_power_limits).

% DUAL FORMULATION NOTE:
% The intermediate-channels reading is part of a constraint family with narrow_originalist and broad_effects_test readings. Each reading instantiates a different ε value and beneficiary/victim structure from the same constitutional text. The three readings compete across Supreme Court coalitions and constitutional interpreters. The intermediate reading claims to preserve federalism through limiting principles but operates with moderate federal extractiveness because the limiting principles are manipulable (economic/non-economic boundary, attenuation standard, substantially-affects factfinding deference). The narrow reading would show low extractiveness (federal power strictly limited to interstate trade). The broad reading would show high extractiveness (federal power reaches nearly all economic activity with aggregate effects). Each story carries its own claimed_type and metrics; the engine computes per-seat classifications that will likely diverge from the story-level claim, revealing the constraint's true structure through classification mismatch.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__intermediate_channels, institutional, 0.35).
constraint_indexing:directionality_override(commerce_clause_scope__intermediate_channels, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
