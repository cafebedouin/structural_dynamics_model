% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   In the hamlet of Aneyoshi on Japan's Sanriku coast, a stone marker placed
 *   after the 1933 Showa Sanriku tsunami carries an inscription warning
 *   descendants not to build homes below its elevation. This story
 *   instantiates the reading under which the prohibition remained an
 *   operationally live land-use rule across the 78 years between the 1933 and
 *   2011 tsunamis: households actually sited construction relative to the
 *   marker, elders actively transmitted and enforced the boundary through
 *   direct instruction, and the rule's predictive content (the tsunami run-up
 *   line) was reconfirmed rather than merely asserted. This is one of two
 *   readings of the same kernel — the sibling reading
 *   (commemorative_husk_reading, not part of this file) holds that the
 *   prohibition decayed into symbolic memorial without continued behavioral
 *   force. The two readings diverge sharply on extractiveness and
 *   theater_ratio: this reading holds both extremely low because it claims
 *   the constraint functioned as genuine, low-overhead coordination around a
 *   physical hazard; the sibling reading would show elevated theater_ratio
 *   because if the rule had decayed to symbol, subsequent 'observance' would
 *   be largely performative rather than behaviorally load-bearing. The two
 *   readings are not two measurements of one constraint — they are two
 *   different structural claims about what was actually happening in the
 *   village across those decades, and only one can be true of the actual
 *   historical record (though both may be partially true across different
 *   phases or households).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'b239f6ea-a502-4c7d-9865-eeb95184d1af').
narrative_ontology:cs_kernel_codification('b239f6ea-a502-4c7d-9865-eeb95184d1af', fixed_text).
narrative_ontology:cs_authority_grounding('b239f6ea-a502-4c7d-9865-eeb95184d1af', practice).
narrative_ontology:cs_interpretation_layer_present('b239f6ea-a502-4c7d-9865-eeb95184d1af').
narrative_ontology:cs_reading_relation('b239f6ea-a502-4c7d-9865-eeb95184d1af', aneyoshi_land_use_prohibition__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('b239f6ea-a502-4c7d-9865-eeb95184d1af', foundational, inscribed_boundary_retains_operative_force_across_generations).
narrative_ontology:cs_axiom_status(inscribed_boundary_retains_operative_force_across_generations, holdable).
narrative_ontology:cs_axiom_grounding('b239f6ea-a502-4c7d-9865-eeb95184d1af', inscribed_boundary_retains_operative_force_across_generations, empirically_contingent).
narrative_ontology:cs_axiom('b239f6ea-a502-4c7d-9865-eeb95184d1af', secondary, social_transmission_without_formal_enforcement_can_sustain_land_use_compliance).
narrative_ontology:cs_axiom_status(social_transmission_without_formal_enforcement_can_sustain_land_use_compliance, holdable).
narrative_ontology:cs_axiom_grounding('b239f6ea-a502-4c7d-9865-eeb95184d1af', social_transmission_without_formal_enforcement_can_sustain_land_use_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('b239f6ea-a502-4c7d-9865-eeb95184d1af', post_1933_survivor_transmission_norm).
narrative_ontology:cs_drift_state('b239f6ea-a502-4c7d-9865-eeb95184d1af', pre_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b239f6ea-a502-4c7d-9865-eeb95184d1af', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_village_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in the hamlet above the marker stone that reads roughly 'do not build homes below this point.' Households have operationally observed the boundary in siting decisions across multiple generations, including through the 1933 Showa Sanriku tsunami and the 2011 Tohoku tsunami, both of which stopped short of the houses built at or above the stone. Residents transmit the rule through oral repetition, elders' direct instruction to youths, and observed consequence (neighboring settlements below the line were destroyed twice) rather than through any administrative permitting apparatus.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_village_residents, beneficiary,
    moderate, generational, constrained, local).

% Function as the informal enforcement and transmission mechanism: they physically walk the site with children, retell the 1896 and 1933 disaster narratives, and socially correct proposals to build below the marker. They administer the rule without formal authority — compliance is secured through social credibility (they survived, or their parents did) rather than through any coercive apparatus.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, elders_and_survivors, agenda_setter,
    moderate, generational, constrained, local).

% The underlying wave run-up behavior of the Sanriku coastline is what the stone's placement actually encodes — the marker approximates an empirically observed maximum inundation line. It is not an agent but the physical referent the social rule tracks; the rule's operational validity rises or falls with how well the marker approximates actual run-up, not with anyone's preference.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics, observer,
    analytical, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics).

% Younger residents or outside parties who might prefer to build closer to the harbor for economic convenience are the closest thing to a dissenting voice, but in this reading they are not suppressed by force — they simply have not built there, because the operational rule is socially self-enforcing and the land below the stone remains understood as unsuitable for housing. Their absence from the record is evidence of the rule's continued operational grip, not evidence of coercion.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, prospective_developers, excluded,
    powerless, biographical, mobile, local).

% External observers (post-2011 disaster researchers, NHK and international journalists, hazard-mapping agencies) who study Aneyoshi as a case of successful vernacular hazard transmission. They corroborate the behavioral-competence reading by comparing settlement patterns against inundation maps, but they do not administer or enforce the rule themselves.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, municipal_and_national_disaster_planners, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes an empirically validated tsunami run-up boundary into a locally legible land-use rule, allowing successive generations to make siting decisions without needing to independently rediscover or model the hazard.
% TRANSFER_FUNCTION: No transfer occurs in this reading: the rule does not move resources, labor, or standing from one party to another. It moves information (the empirical hazard boundary) forward across generations at near-zero cost to any party.
% ABSENT_VOICES: Prospective developers or economically motivated actors who might prefer harborside land are not silenced by any enforcement apparatus in this reading — they are simply persuaded, or have never seriously contested the boundary, because its physical basis has been twice empirically reconfirmed within living or near-living memory.
% DISAPPEARANCE_RATIONALE: If the operational rule vanished overnight — if the stone were removed and the oral transmission chain broken — new construction would plausibly encroach below the marked line within a generation, as has happened in many other Sanriku settlements that rebuilt in the inundation zone after 1933 and were destroyed again in 2011. Aneyoshi's comparatively undamaged upper settlement in 2011 is the direct behavioral evidence for this reading's claim that the rule is live, not merely symbolic.
% FOUNDING_PROBLEM: Repeated catastrophic tsunami inundation (1896 Meiji Sanriku, 1933 Showa Sanriku) destroyed low-lying settlements; the founding problem was preventing future generations, who would not personally remember the disasters, from rebuilding in the fatal zone.
% FOUNDING_PROBLEM_CORROBORATION: Post-2011 disaster researchers and hazard-mapping agencies, external to the village and with no stake in validating the stone's authority, compared Aneyoshi's building line against 2011 inundation extents and found the upper settlement was spared while lower-elevation settlements elsewhere on the same coast were destroyed — corroboration from outside the transmitting community, based on physical outcome rather than testimony alone.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.06, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is scored near-zero (0.06) because no party collects rent from the rule's operation — it is closer to a shared hazard map than an institution with a beneficiary who profits from others' compliance. Suppression is low (0.12) because the rule's hold on behavior in this reading comes from empirical credibility (twice-confirmed hazard boundary) rather than coercion; nothing prevents a resident from building below the line except the well-founded expectation of destruction. Accessibility collapse is scored moderately high (0.72), reflecting that once the hazard boundary is understood, building below it is not a live-feeling alternative for a rational actor — the physics forecloses the option even though no one enforces it by force. Resistance is scored very low (0.05): there is essentially no recorded pushback against the boundary in this reading, because the constraint tracks a shared, verifiable interest rather than imposing an asymmetric cost.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical observer seat (disaster researchers, hazard planners) this reading is corroborated by outcome data (2011 inundation maps). From inside the village, the same operational fact would have been experienced simply as 'how you decide where to build a house,' with the stone functioning less as a rule cited in argument and more as background common knowledge — the engine should find this reading computing close to a genuine Rope from every seat, with minimal seat divergence, because that convergence is exactly the structural signature the behavioral-competence claim predicts.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are the sole named beneficiary group, and in this reading they are also the payers of any compliance cost (forgone harborside land value) — but because the cost avoided (death, total property loss) vastly exceeds the cost paid (marginally less convenient siting), the net directionality sits close to symmetric-to-beneficiary rather than target. There are no victims in this reading: the whole structural claim of the behavioral-competence reading is that the rule imposed no meaningful asymmetric extraction on anyone — it is closer to a Rope than to any extractive category, which is why no victims array is authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as 'live' rather than 'dead' specifically because this reading claims the tsunami hazard (the problem the marker was built to address) never went away between 1933 and 2011 — unlike institutional mandates that outlive their founding conditions, a coastal tsunami hazard boundary does not expire with time. This is precisely why the constraint should NOT be read as mandatrophy in this version: the mandate and the underlying problem remained co-extensive across the full 78-year interval, which the 2011 outcome (upper settlement spared) is offered as direct corroboration of.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_commemorative_reading_selection,
    'Did residential siting decisions across the full 1933-2011 interval actually track the stone''s marked elevation as an operative rule, or did the prohibition decay into a commemorative symbol at some point within that span while housing nonetheless happened to remain above the line for unrelated reasons (land scarcity, existing settlement inertia)?',
    'Historical parcel-by-parcel construction records for Aneyoshi cross-referenced against the stone''s installation date and against household-level oral history interviews distinguishing rule-referenced siting decisions from coincidental non-development below the marker.',
    'If oral histories and construction records show active, rule-referenced avoidance of building below the stone, this reading (behavioral_competence) is well-supported and should classify as Rope with minimal extraction. If housing merely happened to remain above the line for reasons unrelated to the marker (e.g., no economic pressure to develop the lower parcels), the sibling commemorative_husk_reading is closer to the truth and this file''s near-zero extraction and theater_ratio scores would be miscalibrated to an operationally hollow constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_commemorative_reading_selection, empirical, 'Whether the 1933-2011 interval shows genuine behavioral enforcement or coincidental non-development that only later became narrated as rule-following.').

omega_variable(
    marker_accuracy_vs_social_rule_independence,
    'Is the land-use prohibition validated by the stone''s genuine physical accuracy (it correctly marks the historical maximum run-up line), or does its authority derive independently from social transmission regardless of the marker''s precise physical accuracy?',
    'Comparison of the stone''s marked elevation against detailed paleotsunami and instrumented run-up records for the 1896, 1933, and 2011 events at the specific Aneyoshi site.',
    'If the marker''s elevation closely tracks the actual empirical run-up boundary, this reading''s coordination-function claim (a physically grounded hazard rule) is strongly supported. If the marker is a rough approximation that happened to be conservative enough to work, the rule''s low extraction score still holds but its underlying claim to precise physical grounding weakens, shifting it slightly toward socially-maintained convention rather than pure physical-law transmission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marker_accuracy_vs_social_rule_independence, empirical, 'Whether the stone''s placement is empirically precise or a conservative social approximation of the hazard boundary.').

omega_variable(
    cs_framing_kernel_vs_practice_authority,
    'Should the authority grounding this reading be classified as ''practice'' (the community''s ongoing observance IS the standard) or as something closer to ''lineage'' (a chain of authorized elder-transmitters interpreting a fixed inscribed text)? Both framings are defensible and would shift the cs_structure classification.',
    'Ethnographic account of whether newer residents defer to specific named elder-transmitters as authoritative interpreters (lineage) versus simply absorbing the norm through ambient community practice with no identifiable interpretive authority (practice).',
    'Under a lineage framing, an identifiable interpretation_layer_present could be claimed (elders as authorized interpreters). Under a practice framing, the interpretation layer is more diffuse and interpretation_layer_present is less clearly applicable. This file adopts the practice framing as the better fit given the informal, non-hierarchical transmission described, but the lineage framing remains live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_practice_authority, conceptual, 'Whether authority is grounded in an identifiable elder lineage or in diffuse community practice — affects the cs_structure framing choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1933, 0.03).
narrative_ontology:measurement(aney_tr_t1948, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1948, 0.04).
narrative_ontology:measurement(aney_tr_t1965, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(aney_tr_t1985, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1985, 0.06).
narrative_ontology:measurement(aney_tr_t2000, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2000, 0.07).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.08).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1933, 0.04).
narrative_ontology:measurement(aney_be_t1948, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1948, 0.04).
narrative_ontology:measurement(aney_be_t1965, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1965, 0.05).
narrative_ontology:measurement(aney_be_t1985, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1985, 0.05).
narrative_ontology:measurement(aney_be_t2000, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2011, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_land_use_prohibition__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.02).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_land_use_prohibition__commemorative_husk_reading are twin readings of the same kernel (the stone and its inscribed prohibition). They are linked rather than merged because they make incompatible empirical claims about the same 78-year interval — one claims continuous behavioral force, the other claims decay to symbol — and per the epsilon-invariance principle a single constraint cannot honestly carry both a near-zero and an elevated theater_ratio for the same span. Resolving which reading better fits the historical record is the subject of the shared omega (behavioral_vs_commemorative_reading_selection) rather than something either file can settle unilaterally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
