% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone as Binding Land-Use Boundary (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   In 1896 and 1933, tsunamis destroyed the village of Aneyoshi on Japan's
 *   Sanriku coast. Survivors erected a stone marker inscribed with a warning
 *   not to build homes below its position. This story reads the stone as a
 *   behaviorally competent land-use constraint: for 78 years, prior to any
 *   formal scientific validation, the marker is read as having continued to
 *   shape actual siting decisions in the hamlet, transmitted through village
 *   oral tradition and reinforced by elders instructing newcomers. The 2011
 *   Tohoku tsunami reached almost exactly the stone's marked line, destroying
 *   homes built below it (mostly by households outside the transmission
 *   network) while homes built above survived. Read this way, the directive
 *   functioned the entire time as a natural-hazard-tracking constraint whose
 *   accuracy happened to go unmeasured by outside science until an actual
 *   disaster provided the validation — not because it was inert until then.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.04).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Tsunami Stone as Binding Land-Use Boundary (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, 'f826dfed-2e64-479c-aacb-cdd92f5c315c').
narrative_ontology:cs_kernel_codification('f826dfed-2e64-479c-aacb-cdd92f5c315c', implicit).
narrative_ontology:cs_authority_grounding('f826dfed-2e64-479c-aacb-cdd92f5c315c', practice).
narrative_ontology:cs_interpretation_layer_present('f826dfed-2e64-479c-aacb-cdd92f5c315c').
narrative_ontology:cs_reading_relation('f826dfed-2e64-479c-aacb-cdd92f5c315c', aneyoshi_stone_directive__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('f826dfed-2e64-479c-aacb-cdd92f5c315c', foundational, oral_transmission_sustains_continuous_behavioral_force).
narrative_ontology:cs_axiom_status(oral_transmission_sustains_continuous_behavioral_force, holdable).
narrative_ontology:cs_axiom_grounding('f826dfed-2e64-479c-aacb-cdd92f5c315c', oral_transmission_sustains_continuous_behavioral_force, empirically_contingent).
narrative_ontology:cs_axiom('f826dfed-2e64-479c-aacb-cdd92f5c315c', secondary, validation_absence_does_not_imply_dormancy).
narrative_ontology:cs_axiom_status(validation_absence_does_not_imply_dormancy, holdable).
narrative_ontology:cs_axiom_grounding('f826dfed-2e64-479c-aacb-cdd92f5c315c', validation_absence_does_not_imply_dormancy, empirically_contingent).
narrative_ontology:cs_reference_frame('f826dfed-2e64-479c-aacb-cdd92f5c315c', post_1933_survivor_transmission_norm).
narrative_ontology:cs_drift_state('f826dfed-2e64-479c-aacb-cdd92f5c315c', pre_2011_contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('f826dfed-2e64-479c-aacb-cdd92f5c315c', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, households_below_the_marker).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_village_descendants).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, inherited_hazard_knowledge_transmission_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in the hamlet the stone marks. The stone's inscription ('do not build below this point') tracks the actual inundation line of the 1896 and 1933 tsunamis, which is also close to the 2011 inundation line. Households that built above the marker were spared in 2011; households below it (built by newcomers unfamiliar with the marker's meaning) were destroyed. Compliance costs residents nothing beyond siting choice; the constraint tracks a real physical hazard boundary they did not create and cannot alter.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_village_descendants, beneficiary,
    moderate, generational, constrained, local).

% Administers informal and, in places, formalized siting guidance that references the stone's line when advising on new construction. Does not enforce the marker with police power in most cases — the constraint operates mostly through village transmission of the marker's meaning (elders telling newcomers what the stone means) rather than through zoning law. Could formally codify or could ignore the marker; historically has done neither consistently.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, local_construction_permitting_office, agenda_setter,
    institutional, generational, constrained, local).

% Households relocating into the area without generational transmission of the stone's meaning were not reliably told what the marker indicated before 2011; several built below the line and were killed in the tsunami. They had no seat in the informal transmission network that carried the directive forward and no formal document forced disclosure to them.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, newcomer_households, excluded,
    powerless, biographical, mobile, local).

% Study historical inundation lines against the stone's marked boundary and against the 2011 tsunami's actual reach. Their surveys are the closest thing to independent validation the directive has ever received, and they report the line is a reasonably accurate empirical hazard boundary rather than a symbolic or arbitrary one.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, coastal_geomorphologists, observer,
    analytical, civilizational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes an empirically-grounded flood/tsunami inundation boundary derived from two prior catastrophic events (1896, 1933) into a durable, low-maintenance physical marker that lets subsequent generations site construction without needing to re-derive or re-measure the hazard themselves.
% TRANSFER_FUNCTION: Moves hazard knowledge across time from the generation that survived 1896/1933 to descendants and residents who never experienced either event; imposes no monetary transfer between parties — the only 'cost' is foregone building land below the marker, which is a cost of the geography itself, not of the constraint.
% ABSENT_VOICES: Newcomer households arriving without generational ties to the village were structurally outside the oral-transmission network that carried the stone's meaning forward; they had no formal channel (deed disclosure, zoning notice) ensuring they understood the marker before choosing where to build.
% DISAPPEARANCE_RATIONALE: If the stone and the transmitted understanding of it vanished, siting decisions in the hamlet would revert to guesswork about inundation risk; the 2011 outcome (survival above the line, destruction below it) demonstrates the marker's disappearance would remove a low-cost, high-fidelity hazard signal and increase future exposure, particularly for households without independent geological expertise.
% FOUNDING_PROBLEM: After the 1896 and 1933 tsunamis killed most of the village, survivors needed a durable, illiterate-accessible, weather-resistant way to tell future generations exactly where it was safe to rebuild, without relying on living memory alone.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-2011 geomorphological surveys and journalistic accounts (outside the village and outside anyone who could be called a beneficiary of the directive's continued authority) confirmed the marker's line closely tracked the actual 2011 inundation boundary, corroborating that the underlying hazard the stone encodes remains physically live, not merely traditionally asserted.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction and suppression are authored near zero across the full interval because, under this reading, the constraint never extracted anything from anyone — it is read as continuously encoding a real, non-manipulated physical hazard boundary and imposing no cost beyond the geography's own cost. Accessibility_collapse is high (0.88) because there genuinely is no viable alternative to 'do not build in a tsunami inundation zone' once the marker's meaning is understood — the alternative is not suppressed by any party, it is foreclosed by physics. Resistance is near-zero because, on this reading, no one within the transmission network contested the directive; the only friction visible in the record is exclusion of newcomers from the transmission channel, which is an access failure, not resistance to the constraint's content. Theater_ratio rises only slightly over the interval (0.03 to 0.08) reflecting the marker's gradual shift from lived warning toward partial ceremonial/commemorative treatment as direct survivor memory attenuated generationally — a mild drift the story registers honestly without claiming it dominates.
 *
 * PERSPECTIVAL GAP:
 *   Village descendants embedded in the transmission network and the analytical geomorphologist seat would likely compute this constraint similarly (low extraction, high accessibility_collapse, mountain-like) — but newcomer households, who experienced the constraint's absence of formal enforcement as a fatal information gap, would compute a very different structure: for them the failure wasn't the constraint's extraction but the total absence of any formalized transfer mechanism reaching them. The engine's per-seat computation should reflect that the newcomer seat's harm arises from exclusion, not from the constraint's own coerciveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared (village descendants, households sited according to the marker) because a genuine mountain claim can still name beneficiaries of compliance without becoming extractive — the FSM check applies precisely because a natural-hazard boundary that nonetheless has named beneficiaries requires the omega documenting whether the boundary is naturally emergent or partly a constructed social artifact reinforced for reasons beyond pure hazard tracking (e.g., land-value stratification, village hierarchy). Newcomer households are marked excluded rather than victim/payer: under this reading they are not extracted from by the directive itself, but by their exclusion from the informal channel that would have transmitted it — the harm flows from an access gap, not from the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored live rather than dead precisely because the 2011 event demonstrated the underlying hazard the stone encodes had not disappeared — this blocks a mandatrophy misclassification that might otherwise apply to a long-unenforced, non-institutionally-maintained marker with no scientific validation for 78 years. A superficial read might see 'no validation for 78 years' and conclude vestigial ritual; this reading's corroboration (independent post-event geomorphological survey) is what distinguishes a live mountain from a piton — the function was never lost, only unmeasured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operative_vs_dormant_across_interval,
    'Was the stone directive actually shaping real siting decisions throughout the full 78-year interval (behavioral competence), or did it lapse into inert memorial status for some portion of that period and only regain behavioral salience retroactively after 2011 validated it (commemorative husk)?',
    'Historical land-use records, oral history interviews with residents across generational cohorts, and construction permit timelines (where they exist) could establish whether siting decisions in the decades before 2011 actually referenced the marker or whether awareness of its meaning had measurably attenuated before the event.',
    'If the directive was dormant for a substantial period, this story''s near-zero extraction/suppression profile across the full interval is wrong for that period and the commemorative_husk_reading''s account of an intervening low-salience phase would better fit the record — this would not change the physical-geography mountain classification but would change the theater_ratio trajectory materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_vs_dormant_across_interval, empirical, 'Whether the directive was continuously behaviorally operative or lapsed into dormancy before 2011 re-activated it.').

omega_variable(
    natural_hazard_line_vs_constructed_social_boundary,
    'Is the marked line purely a natural hazard boundary (physics), or does it also function as a constructed social/status boundary (e.g., correlating with older, more established households living upslope and newer or lower-status households occupying land below the marker)?',
    'Socioeconomic and settlement-history analysis of who occupied land above versus below the marker line across the 78-year interval, checked against household tenure, land value, and social status records if they exist.',
    'If the boundary also tracked social stratification, the declared beneficiaries would need to be read partly as beneficiaries of a constructed advantage rather than purely of a natural hazard signal, which would push the classification away from a pure mountain and toward the FSM override target.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_hazard_line_vs_constructed_social_boundary, conceptual, 'Whether the hazard boundary is purely physical or partly overlays a social stratification pattern.').

omega_variable(
    transmission_exclusion_mechanism,
    'Was the exclusion of newcomer households from the marker''s meaning a passive gap (no one thought to tell them) or an active failure of institutional responsibility (the permitting office knew and did not disclose)?',
    'Review of whether the local construction permitting office had documented knowledge of the marker''s significance and any record of disclosure practice to new residents before 2011.',
    'If the permitting office had documented knowledge and did not disclose, the excluded stakeholder''s harm shifts from a passive information gap to an institutional failure, which would raise the story''s suppression score and could push a subset of the constraint''s operation toward tangled_rope with the permitting office as an implicated agenda_setter.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transmission_exclusion_mechanism, empirical, 'Whether newcomer exclusion from the marker''s meaning reflects passive gap or institutional non-disclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 13, 0.04).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 26, 0.05).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 39, 0.06).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 52, 0.06).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 65, 0.07).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 78, 0.08).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 13, 0.04).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 26, 0.05).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 39, 0.05).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 52, 0.05).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 65, 0.06).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 78, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__behavioral_competence_reading, 0.02).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_stone_directive__commemorative_husk_reading are sibling readings of the same kernel (aneyoshi_stone_directive): the physical stone and its inscribed hazard line. This reading (behavioral_competence) authors near-zero extraction/suppression across the full 78-year interval on the claim that the directive continuously shaped siting behavior. The sibling reading authors the same physical referent but claims the directive's behavioral force atrophied during an inter-catastrophe lull and the stone functioned mainly as commemorative artifact until 2011 restored its salience — producing a different theater_ratio trajectory and a different founding_problem_status trajectory (likely contested or dead-then-revived rather than continuously live). ε for the underlying hazard-tracking function is low in both readings; what differs is the claimed behavioral/theatrical mix over time, which is exactly the kind of distinct-constraint decomposition the ε-invariance principle calls for rather than a single story with an internal contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
