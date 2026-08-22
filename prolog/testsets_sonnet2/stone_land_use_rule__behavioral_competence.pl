% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Tsunami Stone as Binding Land-Use Prohibition (Behavioral-Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   A tsunami stone marks the maximum inundation line of a historical
 *   disaster, inscribed with an instruction to descendants never to build
 *   below it. In this reading, the stone functions as a LIVE, binding
 *   land-use prohibition: the community's actual settlement pattern — where
 *   houses are sited, where land is refused for sale below the line, the
 *   daily practice of climbing the steeper hill rather than settling the
 *   flatter ground — enforces the constraint through 78 years of sustained
 *   compliance, independent of any legal zoning mechanism. This is distinct
 *   from the sibling reading (commemorative_husk) in which the same physical
 *   object persists only as a memorial with the behavioral force having
 *   decayed to symbolic gesture. Both readings share the stone as kernel;
 *   they diverge entirely on whether present-day spatial behavior is still
 *   causally governed by it. This story authors ONLY the
 *   behavioral-competence reading — ε here reflects a genuinely
 *   low-extraction, high-compliance coordination mechanism, not an average
 *   across readings.
 *
 * KEY AGENTS:
 *   - coastal_village_residents: primary beneficiary and enforcer (organized/constrained) — absorb real cost, receive real protection
 *   - elders_and_practice_transmitters: agenda_setter whose identity is partly constituted by correct transmission (moderate/identity_locked)
 *   - future_generations_of_villagers: downstream beneficiary with no say in the founding choice (powerless/trapped)
 *   - downhill_land_and_developers: excluded party bearing real opportunity cost with no formal voice (moderate/constrained)
 *   - disaster_anthropologists: analytical observer corroborating the practice's efficacy from outside the benefiting community
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.12).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.28).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.12).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Tsunami Stone as Binding Land-Use Prohibition (Behavioral-Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, 'ddafcccc-b9a6-4c9e-b656-8dbf69d373ce').
narrative_ontology:cs_kernel_codification('ddafcccc-b9a6-4c9e-b656-8dbf69d373ce', fixed_text).
narrative_ontology:cs_authority_grounding('ddafcccc-b9a6-4c9e-b656-8dbf69d373ce', practice).
narrative_ontology:cs_interpretation_layer_present('ddafcccc-b9a6-4c9e-b656-8dbf69d373ce').
narrative_ontology:cs_reading_relation('ddafcccc-b9a6-4c9e-b656-8dbf69d373ce', stone_land_use_rule__commemorative_husk, coexists_with).
narrative_ontology:cs_axiom('ddafcccc-b9a6-4c9e-b656-8dbf69d373ce', foundational, inscribed_instruction_retains_behavioral_force_across_generations).
narrative_ontology:cs_axiom_status(inscribed_instruction_retains_behavioral_force_across_generations, holdable).
narrative_ontology:cs_axiom_grounding('ddafcccc-b9a6-4c9e-b656-8dbf69d373ce', inscribed_instruction_retains_behavioral_force_across_generations, empirically_contingent).
narrative_ontology:cs_axiom('ddafcccc-b9a6-4c9e-b656-8dbf69d373ce', secondary, living_practice_not_object_permanence_sustains_hazard_knowledge).
narrative_ontology:cs_axiom_status(living_practice_not_object_permanence_sustains_hazard_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('ddafcccc-b9a6-4c9e-b656-8dbf69d373ce', living_practice_not_object_permanence_sustains_hazard_knowledge, instrumental).
narrative_ontology:cs_reference_frame('ddafcccc-b9a6-4c9e-b656-8dbf69d373ce', founding_generation_inundation_line_as_binding_instruction).
narrative_ontology:cs_drift_state('ddafcccc-b9a6-4c9e-b656-8dbf69d373ce', post_2011_tohoku_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ddafcccc-b9a6-4c9e-b656-8dbf69d373ce', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, coastal_village_residents).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, future_generations_of_villagers).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, oral_transmission_of_hazard_knowledge_can_persist_across_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live above the inscribed stone line and continue to build homes, farms, and community structures only uphill of the marker despite the added daily cost of the climb. They enforce the norm on themselves and each other through routine spatial practice — where a house is sited, where a child is told not to build — rather than through any formal permitting body. They absorbed a tsunami that killed communities below equivalent markers elsewhere on the coast and treat this as vindication of the practice.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, coastal_village_residents, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, coastal_village_residents, agenda_setter).

% Carry and repeat the narrative that accompanies the stone — not merely reciting its warning but modeling the compliant behavior (siting decisions, refusals to sell or lease land below the line) that keeps the rule live. Their standing in the community is partly constituted by being correct custodians of this knowledge; abandoning the practice would cost them their functional role, not just an abstract belief.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, elders_and_practice_transmitters, agenda_setter,
    moderate, generational, identity_locked, local).

% Inherit the settlement pattern and the informal prohibition without having chosen it; their safety from future tsunami events depends on whether the current generation's daily practice keeps transmitting the constraint rather than letting it decay into a plaque nobody reads.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, future_generations_of_villagers, beneficiary,
    powerless, civilizational, trapped, local).

% Would prefer to build on the flatter, more accessible, and often more fertile land below the stone line — a real economic opportunity foreclosed by community norm rather than by law. They have no formal voice in the practice; the constraint is maintained entirely through social enforcement outside any permitting process they could petition.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, downhill_land_and_developers, excluded,
    moderate, biographical, constrained, local).

% Study why some tsunami stones retained behavioral force across generations while others became commemorative-only. They document that this particular stone's community shows sustained compliance (78 years) correlating with active, non-coercive social transmission rather than legal mandate.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_anthropologists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__behavioral_competence, diffuse).
narrative_ontology:fixing_cost_class(stone_land_use_rule__behavioral_competence, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves an intergenerational hazard-transmission problem: encoding a location-specific tsunami inundation line into a durable, low-maintenance object and pairing it with living spatial practice so that knowledge which would otherwise decay across generations (as direct survivor memory fades) continues to govern where people build.
% TRANSFER_FUNCTION: Moves nothing between parties in the extractive sense — it redirects settlement effort from flatter, cheaper downhill land to steeper, costlier uphill land, at a real but voluntarily absorbed economic cost, in exchange for reduced tsunami mortality risk for the same population across time.
% ABSENT_VOICES: Downhill landowners and would-be developers who bear the opportunity cost of foregone flat land have no formal channel to contest the norm — it operates through social consensus and elder authority, not zoning law, so there is no hearing at which they could argue economic need against the inherited prohibition.
% DISAPPEARANCE_RATIONALE: If the stone and the accompanying practice vanished overnight, absent independent reinforcement (modern building codes, hazard maps) the community would plausibly begin resettling the more convenient low-lying land within a generation or two, as has been documented in villages where the memorial function decayed without behavioral transmission — reintroducing exposure to the exact hazard the marker encodes.
% FOUNDING_PROBLEM: A tsunami devastated the coastal settlement; survivors erected the stone at the maximum observed inundation line with an inscription instructing descendants never to build below it, to prevent future generations from forgetting the hazard once living memory of the disaster faded.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster-anthropology fieldwork and comparative surveys of Japanese tsunami-stone communities (cited by NHK and academic hazard-geography studies following the 2011 Tohoku tsunami) corroborate that villages maintaining the behavioral practice around their stones suffered substantially lower losses in 2011 than nearby villages where equivalent stones had decayed to commemorative status — corroboration from researchers with no stake in the village's land-use outcomes, not merely from the residents themselves.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12 at T=78) because under this reading the constraint imposes a real but modest, symmetrically-borne cost (steeper hillside construction, foregone flat land) in exchange for a genuine and empirically corroborated reduction in mortality risk — this is coordination cost, not rent extraction. Suppression is moderate (0.28): the norm is enforced socially (land refusal, community expectation) rather than legally, and there is no formal apparatus punishing violators beyond reputational and relational pressure. Accessibility collapse is high (0.72) — once a household internalizes the hazard knowledge, building downhill becomes nearly unthinkable within the community's own frame, though the alternative (building downhill) remains physically and legally available, which caps collapse below mountain-level. Resistance is low (0.15): the constraint is not experienced as an imposition to be fought but as an inherited discipline largely accepted by those who bear its cost.
 *
 * PERSPECTIVAL GAP:
 *   Villagers uphill experience the constraint as a coordination good they maintain themselves. Downhill land and developers experience the same social norm as a foreclosed economic opportunity with no forum for contest — but their exclusion is a matter of absent voice (Q4), not extraction, since no one collects rent from their forbearance. Future generations experience it as an inherited given whose origin they may never personally verify, which is exactly the fragility this reading identifies: the constraint's binding force depends on continuous re-transmission, not on the stone's physical durability.
 *
 * DIRECTIONALITY LOGIC:
 *   Current residents and elders sit near the beneficiary end: they pay a real cost (worse land, harder commute) but receive a real, empirically demonstrated benefit (survival), and their exit option is only 'constrained' rather than 'trapped' because moving away entirely remains possible, if costly to community ties. Future generations are the deepest beneficiaries but are directionally 'trapped' in time — they cannot renegotiate a constraint founded before their birth. Downhill developers are excluded rather than extracted-from: no party collects what they forgo, which is why they are not listed as victims despite bearing opportunity cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The behavioral-competence reading is explicitly NOT mandatrophy: the founding problem (tsunami risk to unwarned future residents) remains live, and 2011 Tohoku outcomes at comparably-practiced stones corroborate the mechanism still functions as designed. This is the load-bearing distinction from the sibling reading: mandatrophy would require sustained maintenance after the founding problem's disappearance or after the mechanism's demonstrated failure — neither is authored here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_commemorative_divergence_location,
    'What specific evidence distinguishes a village where the stone is genuinely behaviorally binding (this reading) from one where it has decayed to a commemorative husk (sibling reading) — and could this village''s status be closer to the husk end than the compliance record suggests?',
    'Direct observation of new construction permits and land transactions relative to the inscribed line over the most recent 10-15 years; interviews with younger residents about whether the stone''s instruction is a live consideration in siting decisions versus an inherited fact they could not act on if they wanted flat land badly enough.',
    'If recent construction shows creeping violation of the line (even without formal repeal), the community may be transitioning toward the commemorative_husk reading in real time, which would require re-authoring this story''s ε upward and eventually reclassifying rather than treating the two readings as stably distinct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_commemorative_divergence_location, empirical, 'Whether the behavioral-competence characterization remains empirically accurate or is itself beginning to drift toward the husk reading.').

omega_variable(
    compliance_driver_ambiguity,
    'Is the 78-year compliance record driven primarily by the stone''s transmitted warning itself, or by confounding factors (persistent economic disadvantage of downhill land for reasons unrelated to tsunami risk, land tenure patterns, or simple settlement inertia) that would have produced the same uphill-only pattern even without the marker?',
    'Comparative analysis against nearby villages with similar geography but no tsunami stone or hazard narrative, controlling for land quality and tenure history.',
    'If compliance is substantially confounded, the constraint''s coordination function is weaker than authored and the low extractiveness score partly reflects a coincidence of interests rather than a functioning hazard-transmission mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_driver_ambiguity, empirical, 'Whether the stone is doing causal work or merely correlating with settlement patterns explained by other factors.').

omega_variable(
    voluntary_vs_socially_coerced_compliance,
    'Is downhill land genuinely foreclosed only by rational risk-avoidance and community-shared values, or is there an internalized/social-coercion component (fear of ostracism, elder disapproval) that would count as suppression beyond what the low authored suppression score (0.28) captures?',
    'Interviews with any resident who has considered or attempted downhill construction, examining what specifically stopped them — market unavailability, direct social pressure, or genuine independent risk calculation.',
    'If social coercion is a larger component than assumed, suppression should be revised upward, which would shift the classification computation toward tangled_rope if a beneficiary group (e.g., elders whose authority depends on the practice) can be shown to extract standing/status at the cost of individual residents'' land-use freedom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_socially_coerced_compliance, conceptual, 'Whether the mechanism enforcing compliance is purely coordinative or has a coercive-suppression component not fully captured by the authored suppression score.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.02).
narrative_ontology:measurement(ston_tr_t13, stone_land_use_rule__behavioral_competence, theater_ratio, 13, 0.03).
narrative_ontology:measurement(ston_tr_t26, stone_land_use_rule__behavioral_competence, theater_ratio, 26, 0.03).
narrative_ontology:measurement(ston_tr_t39, stone_land_use_rule__behavioral_competence, theater_ratio, 39, 0.04).
narrative_ontology:measurement(ston_tr_t52, stone_land_use_rule__behavioral_competence, theater_ratio, 52, 0.05).
narrative_ontology:measurement(ston_tr_t65, stone_land_use_rule__behavioral_competence, theater_ratio, 65, 0.06).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.08).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(ston_be_t13, stone_land_use_rule__behavioral_competence, base_extractiveness, 13, 0.06).
narrative_ontology:measurement(ston_be_t26, stone_land_use_rule__behavioral_competence, base_extractiveness, 26, 0.08).
narrative_ontology:measurement(ston_be_t39, stone_land_use_rule__behavioral_competence, base_extractiveness, 39, 0.09).
narrative_ontology:measurement(ston_be_t52, stone_land_use_rule__behavioral_competence, base_extractiveness, 52, 0.1).
narrative_ontology:measurement(ston_be_t65, stone_land_use_rule__behavioral_competence, base_extractiveness, 65, 0.11).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__behavioral_competence, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__behavioral_competence, 0.1).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% This constraint and stone_land_use_rule__commemorative_husk are two readings of the same kernel (the tsunami stone and its inscription). They are NOT the same constraint measured differently — per the ε-invariance principle, they are decomposed into separate stories because their ε values differ by a wide margin (this reading: ~0.12, genuine ongoing coordination cost; husk reading: near-zero, since a decayed memorial extracts nothing and coordinates nothing). The behavioral_competence reading claims the stone still causally governs settlement; the commemorative_husk reading claims it does not. Both are internally coherent constraint claims about the same physical object; which is true of any given village is an empirical, community-specific question, not a matter of choosing an observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
