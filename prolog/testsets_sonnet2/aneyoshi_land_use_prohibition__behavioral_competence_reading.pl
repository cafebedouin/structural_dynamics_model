% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster anthropology / commitment systems / temporal institutional analysis
 *
 * SUMMARY:
 *   In Aneyoshi, a village on Japan's Sanriku coast, survivors of the 1896
 *   and 1933 tsunamis erected a stone marker inscribed with a warning and a
 *   line: do not build homes below this point. This reading treats that
 *   marker and its associated settlement norm as a live, operationally
 *   enforced land-use rule for 78 years (1933-2011) — not a historical
 *   monument but an active constraint that shaped where households actually
 *   built, transmitted through direct instruction across generations. In
 *   March 2011, the Tohoku tsunami's run-up stopped a short distance below
 *   the stone; homes built above the line, in keeping with the rule, survived
 *   undamaged. This is the behavioral-competence reading of the kernel: the
 *   rule functioned as physics-enforced social practice, coordinating
 *   settlement location around an empirically real hazard boundary, with
 *   negligible extraction and no identifiable rent-collecting beneficiary
 *   structure. The sibling reading (commemorative_husk_reading, not authored
 *   here) holds that the prohibition had decayed to symbolic status by the
 *   time of the 2011 event and that any protective effect was incidental
 *   rather than the product of an operative rule — a structurally distinct,
 *   lower-confidence claim about the same marker.
 *
 * KEY AGENTS:
 *   - aneyoshi_village_households: primary beneficiaries and self-enforcers of the rule across generations
 *   - prospective_developers: bear opportunity cost of foregone low-lying land, but retain exit
 *   - local_municipal_planning_authority: institutionalizes the rule into formal hazard zoning
 *   - future_generations_of_villagers: beneficiaries with no voice in the rule's maintenance
 *   - disaster_researchers_and_geologists: external analytical observers corroborating the rule's operative status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.04).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster anthropology / commitment systems / temporal institutional analysis").

domain_priors:requires_active_enforcement(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, '34537a1b-135b-482f-af0a-f1b5ae175a50').
narrative_ontology:cs_kernel_codification('34537a1b-135b-482f-af0a-f1b5ae175a50', implicit).
narrative_ontology:cs_authority_grounding('34537a1b-135b-482f-af0a-f1b5ae175a50', practice).
narrative_ontology:cs_interpretation_layer_present('34537a1b-135b-482f-af0a-f1b5ae175a50').
narrative_ontology:cs_reading_relation('34537a1b-135b-482f-af0a-f1b5ae175a50', aneyoshi_land_use_prohibition__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('34537a1b-135b-482f-af0a-f1b5ae175a50', foundational, inscribed_marker_sustained_operative_behavioral_force).
narrative_ontology:cs_axiom_status(inscribed_marker_sustained_operative_behavioral_force, holdable).
narrative_ontology:cs_axiom_grounding('34537a1b-135b-482f-af0a-f1b5ae175a50', inscribed_marker_sustained_operative_behavioral_force, empirically_contingent).
narrative_ontology:cs_axiom('34537a1b-135b-482f-af0a-f1b5ae175a50', secondary, generational_oral_transmission_preserved_specific_hazard_boundary).
narrative_ontology:cs_axiom_status(generational_oral_transmission_preserved_specific_hazard_boundary, holdable).
narrative_ontology:cs_axiom_grounding('34537a1b-135b-482f-af0a-f1b5ae175a50', generational_oral_transmission_preserved_specific_hazard_boundary, empirically_contingent).
narrative_ontology:cs_reference_frame('34537a1b-135b-482f-af0a-f1b5ae175a50', post_1933_survivor_inscribed_boundary).
narrative_ontology:cs_drift_state('34537a1b-135b-482f-af0a-f1b5ae175a50', pre_2011_contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('34537a1b-135b-482f-af0a-f1b5ae175a50', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_village_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, future_generations_of_villagers).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__behavioral_competence_reading, prospective_developers).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_recurrence_interval_exceeds_living_memory).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, inscribed_boundary_markers_can_transmit_operational_constraint_across_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households in Aneyoshi build and rebuild homes below the stone's inscribed line only at their own risk, and by long-held practice do not build residences below it; the same households transmit the rule to newcomers and children as a live condition of settlement, not a historical curiosity. They benefit directly: the 2011 tsunami stopped a short distance from the marker, and every household above the line survived undamaged while structures below it elsewhere on the coast were destroyed. Their exit from the rule would mean building in the run-up zone, which is available land but carries the understood consequence of periodic inundation on a multi-generational recurrence interval.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_village_households, beneficiary,
    moderate, civilizational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_village_households, agenda_setter).

% Outside developers or returning residents who might want to build closer to the harbor for economic convenience (shorter commute to fishing infrastructure, flatter buildable land) forgo that land because the community norm anchored to the stone treats building below the line as against the operative rule. They bear the opportunity cost of not using the lower land, but they can and do exit by building elsewhere or accepting the local norm; the cost is real but not coercive in the trapped sense.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, prospective_developers, payer,
    moderate, biographical, mobile, local).

% The municipality and regional disaster-planning bodies incorporate the stone's line into zoning guidance and hazard maps, formally recognizing what was previously an oral/inscriptional community rule. They administer the rule going forward and could in principle relax it under development pressure, but doing so would mean overriding a rule with a directly falsifiable track record (2011).
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, local_municipal_planning_authority, agenda_setter,
    institutional, generational, constrained, regional).

% Have no voice in whether the rule is maintained but are the direct beneficiaries of its persistence: they inherit a settlement pattern that keeps homes out of the tsunami run-up zone without having to independently rediscover the hazard boundary. Their protection depends entirely on whether the current generation keeps transmitting the rule as binding rather than letting it lapse into folklore.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, future_generations_of_villagers, beneficiary,
    powerless, civilizational, trapped, local).

% Study the stone as a rare case of intergenerational hazard-avoidance transmission that operationally worked, comparing it to nearby markers whose warnings were not heeded. They document the causal chain between inscription, oral transmission, settlement pattern, and the 2011 outcome; their attestation is external to the village's own interest in the rule's continuation.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, disaster_researchers_and_geologists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes an empirically-derived tsunami run-up boundary (from the 1896 and 1933 disasters) into a durable, legible marker and an accompanying settlement norm, solving the problem that the relevant hazard recurrence interval (multiple decades) exceeds a single human's reliable memory and exceeds any individual household's capacity to independently rediscover the safe line.
% TRANSFER_FUNCTION: Moves buildable convenience (proximity to harbor, flatter low-lying land) away from the current generation of villagers and developers, in exchange for moving inundation risk away from residences; the net transfer is temporal — near-term land-use flexibility is given up so that future occupants of the same land are not exposed to a periodic but rare physical hazard.
% ABSENT_VOICES: The dead who set the stone after 1933 are not present to explain it in their own terms; the current generation is trusted to represent their intent faithfully. There is no institutional dissenter recorded arguing the land below the line is safe to build on — dissent would have to come from someone willing to test the hazard directly.
% DISAPPEARANCE_RATIONALE: If the marker and its associated norm vanished overnight, nothing in the physical hazard would change, but the transmission mechanism that keeps households from building in the run-up zone would be gone; over one or two generations, absent the marker's operative force, land pressure would likely draw new construction into the lower zone, reproducing the vulnerability pattern seen in neighboring communities that lacked an equivalently observed marker.
% FOUNDING_PROBLEM: After the 1896 and 1933 Sanriku tsunamis killed most of the village's population, survivors needed a way to prevent descendants from ever again building homes in the tsunami run-up zone, given that the interval between catastrophic tsunamis is longer than living memory typically retains a warning at full force.
% FOUNDING_PROBLEM_CORROBORATION: Independent seismological and disaster-history researchers, plus the 2011 outcome itself (villages with comparable markers that had lost operative force suffered losses below their own stones, while Aneyoshi's marked line held as the operative boundary) corroborate that the physical hazard the rule addresses remains active and that the rule's behavioral force, not merely its text, is what produced the differential outcome — this corroboration comes from outside the village's own interest in validating its practice.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.04, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.04) because no party collects rent from the rule's operation; the only cost borne is foregone convenience of building on flatter, harbor-adjacent land, and that cost is paid by the same population that receives the hazard-avoidance benefit. Suppression is authored low-moderate (0.12) and falls slightly over the interval as the rule normalizes into unremarkable practice rather than requiring active social correction of violators (there is little recorded need for enforcement against defection once the norm was established). Theater ratio stays low but ticks upward slightly (0.02 to 0.08) reflecting ordinary institutional formalization (the rule being folded into municipal hazard maps) without displacing its behavioral force. Accessibility collapse is authored high (0.82): once a household understands the tsunami run-up hazard, building below the line is not a live alternative in practice. Resistance is authored very low (0.06): there is no organized pushback against the rule from within the community across the 78-year interval documented here.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi households are both the rule's authors/enforcers (agenda_setter, via oral transmission and community norm) and its direct beneficiaries — this is a case where enforcer and beneficiary largely coincide, which is structurally different from the extractive pattern where enforcer benefits at a distinct payer's expense. Prospective developers bear a real but non-coercive opportunity cost and retain mobility (they can build elsewhere without penalty). Future generations are powerless beneficiaries with trapped exit options with respect to the physical hazard itself (they cannot choose not to inherit the hazard), which is precisely why the transmission mechanism matters: without it, their exposure would rise.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy in one direction and risks it in the other: the founding problem (recurrent tsunami inundation exceeding living memory) is still live (Q founding_problem_status = live), and the 2011 event is direct, non-circular corroboration from outside the village's own interest in the rule — this is not a case of an institution declaring its own continued necessity. The risk this reading exists precisely to flag is the sibling possibility: if the rule's behavioral force actually decayed before 2011 and the marker's coincidental position (rather than operative deterrence) explains the outcome, this reading would be wrong about the mechanism even though the outcome (survival) is not in dispute. That is exactly the ambiguity routed to omega rather than resolved by assertion here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operative_force_vs_coincidence,
    'Was the 78-year settlement pattern below/above the stone''s line causally produced by an actively transmitted, understood rule, or did it result from other factors (land value, municipal inertia, unrelated risk aversion) that happened to correlate with the marker''s position, with the rule itself having lapsed to symbolic status well before 2011?',
    'Oral history interviews with pre-2011 residents about whether the stone''s warning was actively cited as a reason for building decisions (versus discovered retrospectively after 2011 media attention); comparison with the settlement histories of nearby villages with similar markers that did not hold; land-transaction and zoning records showing whether the line was referenced in building decisions prior to 2011.',
    'If the rule was already inert before 2011 and the outcome was coincidental, this reading (behavioral_competence_reading) would be structurally wrong about the mechanism even though it shares the same low-extraction classification with the sibling husk reading — the two readings would converge on ε despite diverging on causal claim, which is itself informative about when kernel readings can share metrics but not mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_force_vs_coincidence, empirical, 'Whether the 2011 outcome reflects an operative rule or coincidental correlation with a decayed marker.').

omega_variable(
    generational_transmission_fidelity,
    'How faithfully was the rule''s original justification (the specific hazard boundary derived from 1896/1933 run-up data) transmitted across three-plus generations, versus being replaced by a vaguer folk injunction ("don''t build too close to the water") that happens to track the original line only approximately?',
    'Comparative linguistic/anthropological analysis of how residents in different age cohorts explained the rule''s rationale in interviews conducted before and after 2011; comparison of the stone''s inscribed boundary against actual pre-2011 structure locations.',
    'High-fidelity transmission supports treating this as a genuine, specific coordination mechanism (rope); low-fidelity transmission (a vague taboo that coincidentally worked) would weaken the behavioral-competence claim and shift weight toward the commemorative_husk_reading even for the pre-2011 period.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_transmission_fidelity, empirical, 'Whether the specific hazard knowledge, not just a general caution norm, was transmitted.').

omega_variable(
    coordination_without_beneficiary_asymmetry,
    'Is a constraint where the enforcing party and the beneficiary population are nearly identical (as authored here) ever at risk of hidden internal asymmetry — e.g., wealthier households having more flexibility to relocate above the line while poorer households were more constrained by land cost to remain in ambiguous zones?',
    'Historical land-ownership and household-wealth records for Aneyoshi across the interval, cross-referenced against building locations relative to the stone.',
    'If such asymmetry existed, this reading''s claim of ''no beneficiary structure'' would need qualification — the rule''s cost of compliance may not have been evenly distributed even within a village that broadly shared the safety benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_without_beneficiary_asymmetry, conceptual, 'Whether apparent universal beneficiary status masks intra-village distributional asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1950, 0.03).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1990, 0.06).
narrative_ontology:measurement(aney_tr_t2005, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2005, 0.07).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.08).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1933, 0.03).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1950, 0.03).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1970, 0.04).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1990, 0.04).
narrative_ontology:measurement(aney_be_t2005, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2005, 0.04).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2011, 0.04).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1933, 0.2).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1990, 0.13).
narrative_ontology:measurement(aney_su_t2005, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2005, 0.12).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2011, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_land_use_prohibition__commemorative_husk_reading are sibling readings of the same kernel (aneyoshi_land_use_prohibition): the stone marker and its associated settlement history. This reading (behavioral_competence_reading) authors ε near zero (0.04) on the claim that the rule was operationally live and behaviorally enforced across the full 78-year interval, directly causing the 2011 survival differential. The sibling reading authors the same marker as having decayed to symbolic status, with any protective outcome being incidental rather than caused by an active rule. Both readings may land at similarly low ε values despite diverging sharply on causal mechanism and on whether 'coordination' ever functioned as an operative mechanism at all during the period in question — the decomposition exists because the mechanism claim, not the outcome, is what differs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
