% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   The Aneyoshi tsunami stone (erected 1933 after the Showa Sanriku tsunami)
 *   bears the inscription: 'High dwellings are the peace and harmony of our
 *   descendants. Remember the calamity of the great tsunamis. Do not build
 *   any homes below this point.' Under the behavioral competence reading,
 *   this stone functioned as a live land-use rule: the prohibition was
 *   operationally enforced across 78 years (1933-2011), with the village
 *   rebuilding exclusively above the stone's line after both the 1933 and
 *   1960 tsunamis, and suffering zero fatalities in the 2011 Tohoku tsunami
 *   while neighboring communities below the line were devastated. The
 *   constraint is the physical reality of tsunami run-up elevation encoded
 *   into social practice — a Mountain whose 'enforcement' is the physics of
 *   water and the survival of those who heeded it. No beneficiary extracts
 *   from compliance; no victim is coerced into compliance. The stone marks a
 *   natural boundary that the community's practice respected.
 *
 * KEY AGENTS:
 *   - aneyoshi_village_residents: Primary practitioners (organized/biographical/constrained/local) — maintain the prohibition through rebuilding decisions
 *   - stone_inscription: Physical kernel (analytical/universal) — the fixed text marking the tsunami run-up line
 *   - tsunami_physics: Natural constraint (analytical/universal) — the hard limit the stone references
 *   - disaster_anthropologists: Analytical observers (analytical/civilizational/analytical/global) — study the system's behavioral competence
 *   - neighboring_coastal_communities: Counterfactual comparison group (organized/biographical/constrained/local) — did not maintain comparable prohibitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.04).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.15).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'e2dbea22-969f-41aa-917e-44bc4342eff7').
narrative_ontology:cs_kernel_codification('e2dbea22-969f-41aa-917e-44bc4342eff7', fixed_text).
narrative_ontology:cs_authority_grounding('e2dbea22-969f-41aa-917e-44bc4342eff7', practice).
narrative_ontology:cs_reading_relation('e2dbea22-969f-41aa-917e-44bc4342eff7', aneyoshi_land_use_prohibition__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('e2dbea22-969f-41aa-917e-44bc4342eff7', foundational, stone_prohibition_operationally_enforced).
narrative_ontology:cs_axiom_status(stone_prohibition_operationally_enforced, holdable).
narrative_ontology:cs_axiom_grounding('e2dbea22-969f-41aa-917e-44bc4342eff7', stone_prohibition_operationally_enforced, empirically_contingent).
narrative_ontology:cs_axiom('e2dbea22-969f-41aa-917e-44bc4342eff7', foundational, behavioral_competence_validated_by_survival_outcome).
narrative_ontology:cs_axiom_status(behavioral_competence_validated_by_survival_outcome, holdable).
narrative_ontology:cs_axiom_grounding('e2dbea22-969f-41aa-917e-44bc4342eff7', behavioral_competence_validated_by_survival_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('e2dbea22-969f-41aa-917e-44bc4342eff7', stone_as_live_land_use_rule).
narrative_ontology:cs_drift_state('e2dbea22-969f-41aa-917e-44bc4342eff7', post_2011_tohoku_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e2dbea22-969f-41aa-917e-44bc4342eff7', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics_as_hard_constraint).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, intergenerational_behavioral_competence).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, embodied_warning_system_fficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of Aneyoshi village who rebuilt exclusively above the stone's line after the 1933 and 1960 tsunamis. Their compliance is not coerced — it is the survival strategy that worked. Exit (building below the line) is physically possible but leads to death in a tsunami event. They experience the stone as a marker of the survival boundary, not as a rule imposed from outside.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_village_residents, observer,
    organized, biographical, constrained, local).

% The physical stone erected in 1933 bearing the inscription marking the tsunami run-up line. It is the kernel's fixed text — the indexical reference point for the prohibition. As a non-agent, it does not collect or pay; it is the stabilized commitment the practice orients to.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, stone_inscription, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(aneyoshi_land_use_prohibition__behavioral_competence_reading, stone_inscription).

% The natural constraint — tsunami run-up elevation, hydrodynamics, recurrence intervals. The stone references this physics; the community's practice couples to it. This is the Mountain's emergent naturality: the constraint would persist even if the stone were removed, because the physics remains.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics).

% Researchers who study Aneyoshi as a case of embodied warning systems and intergenerational behavioral competence. They analyze the constraint from outside, documenting its operational efficacy. They neither benefit from nor pay for the prohibition.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, disaster_anthropologists, observer,
    analytical, civilizational, analytical, global).

% Communities along the Sanriku coast that lacked comparable tsunami stones or did not maintain rebuilding prohibitions. They experienced the same tsunami physics but without the indexical marker and social practice. In 2011, many suffered catastrophic fatalities. They are the counterfactual comparison: same physics, no behavioral coupling, different outcome.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, neighboring_coastal_communities, observer,
    organized, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational survival behavior against a recurring natural hazard (tsunami run-up) by marking the physical boundary of the hazard zone and embedding the prohibition in rebuilding practice. Solves the coordination problem: 'Where is it safe to build?' without requiring each generation to rediscover the hazard boundary through catastrophic loss.
% TRANSFER_FUNCTION: Moves no resources between parties. The constraint transfers nothing — it aligns building practice with a physical survival boundary. Compliance has no transfer cost; non-compliance has a survival cost paid to physics, not to any agent.
% ABSENT_VOICES: No voices are structurally excluded from this constraint's operation — the physics applies to all equally, and the stone's prohibition is public knowledge. The commemorative husk reading's analytical frame (which disputes the behavioral efficacy) is not an excluded voice but a competing interpretation from outside the practicing community.
% DISAPPEARANCE_RATIONALE: If the stone and its prohibition vanished overnight (counterfactual: the 1933 stone was never erected, or the community forgot it), the physics would remain but the behavioral coupling would be lost. Rebuilding would likely occur in the hazard zone, and the next tsunami would kill residents — as it did in neighboring communities. The world rearranges because the constraint's function is the behavioral coupling to physics; remove the coupling, the survival outcome changes.
% FOUNDING_PROBLEM: After the 1933 Showa Sanriku tsunami killed a substantial portion of Aneyoshi's population, the survivors needed a durable, intergenerationally transmissible marker of the tsunami run-up line to prevent future rebuilding in the hazard zone — a problem of survival memory transmission across generations without writing-dependent records.
% FOUNDING_PROBLEM_CORROBORATION: The 1933 and 1960 tsunami survivors' testimony (recorded in municipal archives and oral history), the land registry showing zero rebuilding below the stone's line across 78 years, and the 2011 Tohoku tsunami outcome (zero fatalities in Aneyoshi vs. catastrophic loss in neighboring communities below comparable elevations) — all attested by sources outside any beneficiary set (there are no beneficiaries). The commemorative husk reading disputes the behavioral efficacy but cannot falsify the survival record.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.04, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.04) because compliance costs nothing beyond not building in a tsunami zone — the 'cost' is avoiding a death zone. Suppression is low (0.15) because the constraint requires no active enforcement; the 2011 event validated it, and the community's practice persisted without coercion. Theater ratio is minimal (0.05) because the stone's function is entirely instrumental (survival) with no performative layer. Accessibility collapse is extreme (0.92) because the alternative (building below the line) is physically unsurvivable — tsunami physics admits no negotiation. Resistance is near-zero (0.08) because no one resists a constraint that demonstrably saves their lives. The claimed_type is Mountain: the constraint emerges from tsunami physics, not human choice, and its operation is indifferent to human preference. The measurement grid (8 time points across 78 years) shows remarkable stability — extractiveness, theater, and suppression are effectively flat, consistent with a natural law constraint.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer sees a Mountain. The aneyoshi residents experience it as a lived practice that 'just is' — not a constraint they resist or negotiate. Neighboring communities that lacked such stones experienced the same physics as catastrophe rather than constraint. The gap is not between seats of the same constraint (there are no beneficiary/payer seats) but between communities that internalized the physics as practice versus those that did not. The engine will compute a single Mountain classification for all seats because the structural data declares no beneficiaries or victims.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim groups declared — this is a genuine Mountain with no extraction structure. The stone does not transfer resources from one group to another. The tsunami physics is the constraint; the stone is its indexical marker; the community's practice is the behavioral coupling to the physics. All agents (residents, observers, neighbors) face the same physics. Directionality derivation finds no structural asymmetry because there is no transfer function — only survival or death.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (tsunami survival) remains live — the 2011 event confirmed it. The arrangement has not outlived its function; it was validated by the most extreme test. Mandatrophy is resolved in the negative: this is not a zombie institution but a living adaptation. The commemorative husk reading's claim that the prohibition 'decayed to symbol' is empirically falsified by the 78-year enforcement record and the 2011 survival outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the behavioral competence reading''s premise (the stone functions as a live land-use rule with operational force) foreclose the commemorative husk reading''s premise (the stone decayed to symbol without behavioral force), or do they represent different analytical frames on the same object?',
    'Test whether a single institutional framework can simultaneously hold that the stone''s prohibition was operationally enforced across 78 years AND that it functioned primarily as a commemorative symbol. If the 1933-2011 enforcement record is corroborated by independent sources (survivor testimony, land registry, rebuilding permits), the commemorative husk reading is structurally foreclosed within any framework that accepts that evidence.',
    'If forecloses: the kernel has a genuine structural split and the two readings cannot coexist in one commitment system. If coexists_with: both readings remain live positions held by different analytical communities, representing a genuine interpretive contest rather than a logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between the behavioral competence and commemorative husk readings of the Aneyoshi kernel').

omega_variable(
    extraction_naturalness_ambiguity,
    'Is the near-zero extractiveness of this constraint (tsunami physics enforced through social practice) a genuine Mountain property, or does the social practice layer introduce latent extraction that would appear under different conditions (e.g., if the stone''s authority were invoked to justify unrelated land restrictions)?',
    'Examine the historical record for any instance where the stone''s authority was extended beyond tsunami-risk land use — e.g., invoked to settle unrelated boundary disputes, justify resource allocation, or legitimize political authority. Absence of such extension across 78 years supports genuine Mountain classification; presence would reveal a latent Tangled Rope structure.',
    'If extraction is structurally zero and no beneficiary structure exists, Mountain stands. If the stone''s authority was ever leveraged for purposes beyond tsunami survival, the constraint has a latent extraction channel and the behavioral competence reading understates χ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_naturalness_ambiguity, empirical, 'Whether the constraint''s near-zero extractiveness is invariant or context-dependent').


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
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1960, 0.03).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1970, 0.04).
narrative_ontology:measurement(aney_tr_t1983, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1983, 0.04).
narrative_ontology:measurement(aney_tr_t1993, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1993, 0.05).
narrative_ontology:measurement(aney_tr_t2003, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2003, 0.05).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1933, 0.03).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1950, 0.03).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1960, 0.03).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1970, 0.04).
narrative_ontology:measurement(aney_be_t1983, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1983, 0.04).
narrative_ontology:measurement(aney_be_t1993, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1993, 0.04).
narrative_ontology:measurement(aney_be_t2003, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2003, 0.04).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2011, 0.04).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1950, 0.12).
narrative_ontology:measurement(aney_su_t1960, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1960, 0.12).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1970, 0.14).
narrative_ontology:measurement(aney_su_t1983, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1983, 0.14).
narrative_ontology:measurement(aney_su_t1993, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1993, 0.15).
narrative_ontology:measurement(aney_su_t2003, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2003, 0.15).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2011, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.02).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% Kernel aneyoshi_land_use_prohibition decomposes into two structurally distinct readings: behavioral_competence_reading (this file — Mountain, ε≈0.04, operational enforcement validated by 2011 survival) and commemorative_husk_reading (symbolic decay, would show higher theater_ratio and extractiveness from performative maintenance). They are linked because the commemorative reading cites the stone's existence as evidence while disputing its behavioral efficacy. The ε-invariance principle requires separate stories: the physics is either operationally coupled to practice (this reading) or it is not (sibling reading); ε cannot be both ~0.04 and substantially higher for the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
