% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Land-Use Directive (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi stone, erected after the 1933 Showa Sanriku tsunami, bears
 *   the inscription: 'Do not build your homes below this point.' For 78
 *   years, the village of Aneyoshi complied — every household built above the
 *   marker. When the 2011 Tohoku tsunami struck, the wave stopped precisely
 *   at the stone's elevation; the village survived while neighboring
 *   communities were devastated. This reading treats the stone as an active
 *   regulatory mechanism: a commitment system that retained operational force
 *   across three generations, constraining land-use decisions through social
 *   norm rather than state law. The constraint's extraction is near-zero; its
 *   function is pure coordination of disaster risk behavior.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone Land-Use Directive (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:requires_active_enforcement(aneyoshi_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, 'eb139705-0df8-4ddb-b4d9-0021f901e267').
narrative_ontology:cs_kernel_codification('eb139705-0df8-4ddb-b4d9-0021f901e267', fixed_text).
narrative_ontology:cs_authority_grounding('eb139705-0df8-4ddb-b4d9-0021f901e267', lineage).
narrative_ontology:cs_interpretation_layer_present('eb139705-0df8-4ddb-b4d9-0021f901e267').
narrative_ontology:cs_reading_relation('eb139705-0df8-4ddb-b4d9-0021f901e267', aneyoshi_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('eb139705-0df8-4ddb-b4d9-0021f901e267', foundational, stone_directive_constrains_land_use).
narrative_ontology:cs_axiom_status(stone_directive_constrains_land_use, holdable).
narrative_ontology:cs_axiom_grounding('eb139705-0df8-4ddb-b4d9-0021f901e267', stone_directive_constrains_land_use, empirically_contingent).
narrative_ontology:cs_axiom('eb139705-0df8-4ddb-b4d9-0021f901e267', secondary, intergenerational_transmission_requires_physical_anchor).
narrative_ontology:cs_axiom_status(intergenerational_transmission_requires_physical_anchor, holdable).
narrative_ontology:cs_axiom_grounding('eb139705-0df8-4ddb-b4d9-0021f901e267', intergenerational_transmission_requires_physical_anchor, empirically_contingent).
narrative_ontology:cs_reference_frame('eb139705-0df8-4ddb-b4d9-0021f901e267', founding_directive_active).
narrative_ontology:cs_drift_state('eb139705-0df8-4ddb-b4d9-0021f901e267', post_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('eb139705-0df8-4ddb-b4d9-0021f901e267', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, village_households).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, tsunami_survivors_2011).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__behavioral_competence_reading, village_households).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_commitment_viability).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, tsunami_risk_knowledge_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households in Aneyoshi village who complied with the stone's directive to build only above the marker across generations. They forgo use of lower-lying land (convenience, arable soil, proximity to fishing) but gain tsunami survival insurance. The constraint shapes where they build homes; exit means leaving the village entirely, which carries high social and economic cost.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, village_households, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, village_households, payer).

% Village leadership that maintains the stone's directive as a living norm, transmitting its meaning to each generation and enforcing compliance through social pressure, communal consensus, and the moral weight of ancestral injunction. They do not extract rents; their authority derives from stewardship of the commitment.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, village_elders, agenda_setter,
    organized, generational, analytical, local).

% The specific households present during the 2011 Tohoku tsunami who survived because their homes were sited above the stone's marker. They are the living proof of the constraint's functional payoff — their survival is the constraint's validation event.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, tsunami_survivors_2011, beneficiary,
    moderate, biographical, analytical, local).

% Disaster anthropologists, institutional analysts, and policy researchers who study the stone as a case of long-lived commitment system. They do not bear costs or receive benefits from the constraint itself; they evaluate its structural properties from outside.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, outside_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational land-use decisions to avoid tsunami inundation zones, solving the collective action problem of maintaining disaster memory and behavioral compliance across generations without centralized state enforcement.
% TRANSFER_FUNCTION: Moves the cost of forgoing lower-lying land (convenience, arable land, fishing access) from individual households to the collective benefit of tsunami survival insurance. No monetary transfer occurs; the transfer is risk-bearing capacity distributed across the community.
% ABSENT_VOICES: Descendants of households that may have wanted to build below the stone but were constrained by the norm; neighboring villages that did not maintain such directives and suffered higher casualties in 2011. They are absent because the constraint's success means no surviving dissenters within Aneyoshi — the compliance coalition is the surviving population.
% DISAPPEARANCE_RATIONALE: Without the stone's directive, the intergenerational transmission of tsunami risk knowledge would lack a physical anchor; land-use decisions would revert to economic convenience, likely placing homes in the inundation zone as seen in neighboring communities that lacked such markers. The 2011 survival pattern would not replicate.
% FOUNDING_PROBLEM: After the 1933 Showa Sanriku tsunami destroyed the village, survivors needed a durable mechanism to prevent rebuilding in the same vulnerable location and to transmit this prohibition across generations without relying on continuous institutional enforcement that might decay.
% FOUNDING_PROBLEM_CORROBORATION: Disaster anthropology literature (e.g., Galloway 2015; Japanese tsunami engineering records from Tohoku University) corroborates that the 1933 tsunami was the founding event and that the stone's directive directly addresses recurrent tsunami risk. The 2011 survival outcome is documented in multiple independent sources (NHK documentary records, Geological Survey of Japan reports, survivor testimony collected by third-party researchers) outside the beneficiary households.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the constraint transfers no resources to a distinct beneficiary class — the 'payers' (households forgoing lower land) are also the beneficiaries (survivors). Suppression is low (0.12) — enforcement is social norm, not coercion; alternatives (building below) are discouraged but not physically prevented. Theater ratio is minimal (0.05) — the stone's maintenance and the annual commemoration are functional, not performative. Accessibility collapse is moderately high (0.72) because once the directive is internalized, building below becomes unthinkable, not just prohibited. Resistance is near-zero (0.08) — the community treats compliance as ancestral duty, not burden.
 *
 * PERSPECTIVAL GAP:
 *   From the household seat, the constraint appears as a benign coordination mechanism — the cost is diffuse and the payoff is existential. From an external policy seat, the same structure might look like a 'lucky' cultural artifact that cannot be replicated by design. The engine's per-seat classification will capture this: households experience rope (coordination), observers may classify as mountain (fixed cultural fact) or piton (inertial tradition) depending on their metrics. The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Village households are both payers (forgo land) and beneficiaries (gain survival) — directionality near symmetric (d ≈ 0.5). Village elders are agenda-setters with analytical exit — they maintain the norm but could theoretically abandon it; their directionality is slightly beneficiary-ward (d ≈ 0.3) because their authority derives from the constraint's success. 2011 survivors are pure beneficiaries (d ≈ 0.0) — they received the payoff without bearing the intergenerational cost. Outside observers are analytical (d = 0.5 by definition). The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing rebuilding in the inundation zone) remains live — tsunami risk persists. The constraint has not atrophied; its mandate is continuously validated by the recurring hazard. Mandatrophy is resolved in the negative: the arrangement persists because its function is still required, not because of inertia. The commemorative husk reading would claim mandatrophy is resolved positively (function dead, form persists); this reading denies that.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the behavioral competence reading a distinct constraint from the commemorative husk reading, or are they two framings of the same constraint?',
    'Compare epsilon values and stakeholder structures across readings. If the commemorative reading authors near-zero beneficiaries and higher theater, while this reading authors active beneficiaries and low theater, they are structurally distinct constraints sharing a physical referent.',
    'If distinct, the kernel decomposes into a constraint family linked by affects_constraints. If same, the engine must adjudicate which reading''s metrics are descriptively accurate for the single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel supports one constraint with contested metrics or multiple constraints with divergent epsilon.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.12) structural (social ostracism for non-compliance) or internalized (households genuinely believe building below is wrong)?',
    'Post-exit suppression trajectory: if a household that left the village would still avoid building below similar markers elsewhere, the suppression is partially internalized. Interview descendants who migrated.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent. If purely structural, suppression collapses at village boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in a community norm.').

omega_variable(
    stone_physical_necessity,
    'Does the constraint''s operational force depend on the physical stone''s presence, or has the norm fully internalized such that the stone is now redundant?',
    'Counterfactual: if the stone were removed overnight, would compliance persist for one more generation? Compare with villages that lost markers but retained oral prohibitions.',
    'If stone is necessary, the constraint is a scaffold with the stone as its physical anchor (though no sunset declared). If norm is self-sustaining, the stone is a vestigial symbol — the constraint is a pure social norm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stone_physical_necessity, conceptual, 'Whether the physical artifact is load-bearing for the constraint''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_behavioral_tr_t0, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t13, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 13, 0.04).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t26, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 26, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t39, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 39, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t52, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 52, 0.04).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t65, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 65, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t78, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 78, 0.05).

% Extraction over time
narrative_ontology:measurement(aneyoshi_behavioral_be_t0, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_be_t13, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 13, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_be_t26, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 26, 0.04).
narrative_ontology:measurement(aneyoshi_behavioral_be_t39, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 39, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_be_t52, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 52, 0.04).
narrative_ontology:measurement(aneyoshi_behavioral_be_t65, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 65, 0.05).
narrative_ontology:measurement(aneyoshi_behavioral_be_t78, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 78, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_behavioral_su_t0, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(aneyoshi_behavioral_su_t13, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 13, 0.12).
narrative_ontology:measurement(aneyoshi_behavioral_su_t26, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 26, 0.1).
narrative_ontology:measurement(aneyoshi_behavioral_su_t39, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 39, 0.11).
narrative_ontology:measurement(aneyoshi_behavioral_su_t52, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 52, 0.1).
narrative_ontology:measurement(aneyoshi_behavioral_su_t65, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 65, 0.12).
narrative_ontology:measurement(aneyoshi_behavioral_su_t78, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 78, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__behavioral_competence_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling commemorative_husk_reading form a kernel family decomposing the single label 'Aneyoshi stone' into two structurally distinct claims: (1) active land-use regulator with very low epsilon, operational force across 78 years; (2) symbolic memorial with decayed behavioral constraint. The epsilon values differ by a wide margin (this reading: ~0.05; sibling likely higher theater, lower functional coordination). They are linked by affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
